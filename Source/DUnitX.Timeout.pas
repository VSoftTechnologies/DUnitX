{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2017 Vincent Parrett & Contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DUnitX.Timeout;

interface

{$I DUnitX.inc}

uses
{$IFDEF USE_NS}
  System.Classes;
{$ELSE}
  Classes;
{$ENDIF}

//TODO : This is currently only supported on Windows, need to investigate osx etc.

type
  ITimeout = interface(IUnknown)
    ['{0A380F7B-9CEE-4FD7-9D86-60CE05B97C1A}']
    procedure Stop;
  end;

function InitialiseTimeout(const ATime : cardinal) : ITimeout;

implementation

uses
{$IFDEF USE_NS}
  WinAPI.Windows,
  System.Diagnostics,
  System.SysUtils,
{$ELSE}
  Windows,
  Diagnostics,
  SysUtils,
{$ENDIF}
  DUnitX.ResStrs,
  DUnitX.TestFramework,
  DUnitX.Exceptions;

// The following TimeOut code is based on the code found at
// https://code.google.com/p/delphitimeouts/
// DelphiTimeouts version 1.1
// Copyright (c) 2007-2008 Szymon Jachim

const
  TIMEOUT_RUNNING = 0;
  TIMEOUT_STOPPED = 1;
  TIMEOUT_FIRED = 2;

type
  TTimeoutThread = class(TThread)
  private
    procedure TimeoutThread;
  public
    ThreadHandle : THandle;
    Timeout : Cardinal;
    //TIMEOUT_RUNNING until either the test stops the timeout or the timeout fires; only one of them can move it on.
    State : Integer;
    procedure Execute; override;
  end;

  TTimeout = class(TInterfacedObject, ITimeout)
  private
    FTimeoutThread : TTimeoutThread;
  public
    constructor Create(const ATimeout : Cardinal; AThreadHandle : THandle);
    destructor Destroy; override;
    procedure Stop;
  end;

  TCancelSynchronousIo = function(hThread : THandle) : BOOL; stdcall;
  PTimeoutContext = ^_CONTEXT;

var
  //Vista and later; nil before, when a test blocked in a synchronous read is not interrupted until the read ends.
  _CancelSynchronousIo : TCancelSynchronousIo;
{$IFDEF CPUX64}
  //A page that can never be executed.  On Win64 the timed-out thread is sent here, to fault (see TimeoutThread).  nil if
  //it could not be allocated, when a Win64 test is not interrupted.
  _TimeoutFaultAddress : Pointer;
  _PreviousExceptObjProc : Pointer;
  _PreviousExceptClsProc : Pointer;
{$ENDIF}

function InitialiseTimeout(const ATime : cardinal) : ITimeout;
var
  ThisThreadHandle : THandle;
  Timeout : TTimeout;
begin
  DuplicateHandle(GetCurrentProcess, GetCurrentThread, GetCurrentProcess, @ThisThreadHandle, 0, False, DUPLICATE_SAME_ACCESS);
  Timeout := TTimeout.Create(ATime, ThisThreadHandle);
  Result := Timeout;
  //Only now: had it fired before Result was set, nothing would have freed the timeout.
  Timeout.FTimeoutThread.Start;
end;

{$IFDEF CPUX86}
procedure RaiseTimeOutException;
begin
  raise ETimedOut.Create(SOperationTimedOut);
end;
{$ENDIF}

{$IFDEF CPUX64}
//The fault at _TimeoutFaultAddress is reported as ETimedOut; every other exception is mapped as before.
function TimeoutExceptObjProc(P : PExceptionRecord) : Pointer;
begin
  if P^.ExceptionAddress = _TimeoutFaultAddress then
    Result := ETimedOut.Create(SOperationTimedOut)
  else if _PreviousExceptObjProc <> nil then
    Result := TExceptObjProc(_PreviousExceptObjProc)(P)
  else
    Result := nil;
end;

//The class the RTL matches "on E : ETimedOut do" against.
function TimeoutExceptClsProc(P : PExceptionRecord) : Pointer;
begin
  if P^.ExceptionAddress = _TimeoutFaultAddress then
    Result := ETimedOut
  else if _PreviousExceptClsProc <> nil then
    Result := TExceptClsProc(_PreviousExceptClsProc)(P)
  else
    Result := nil;
end;

//True if the thread's stack can take the return address at AAddress (it is not, say, in the guard page).
function StackSlotWritable(const AAddress : NativeUInt) : Boolean;
var
  MemoryInfo : TMemoryBasicInformation;
begin
  Result := (VirtualQuery(Pointer(AAddress), MemoryInfo, SizeOf(MemoryInfo)) = SizeOf(MemoryInfo)) and
    (MemoryInfo.State = MEM_COMMIT) and (MemoryInfo.Protect and PAGE_GUARD = 0) and
    (MemoryInfo.Protect and (PAGE_READWRITE or PAGE_EXECUTE_READWRITE) <> 0);
end;
{$ENDIF}

//Queued to the timed-out thread so that an alertable wait it is blocked in (a blocking Winsock call is one) ends.
procedure WakeAlertableWait(Param : ULONG_PTR); stdcall;
begin
end;

procedure TTimeoutThread.TimeoutThread;
var
  //GetThreadContext needs the context 16-byte aligned on Win64, which a local record is not always.
  CtxBuffer : array [0 .. SizeOf(_CONTEXT) + 15] of Byte;
  Ctx : PTimeoutContext;
  Redirected : Boolean;
begin
  Redirected := False;
  Ctx := PTimeoutContext((NativeUInt(@CtxBuffer) + 15) and not NativeUInt(15));
  if SuspendThread(ThreadHandle) = Cardinal(-1) then
    exit;
  try
    Ctx^.ContextFlags := CONTEXT_FULL;
    //Also waits until the thread is actually suspended, so it cannot stop the timeout while State is checked below.
    if not GetThreadContext(ThreadHandle, Ctx^) then
      exit;
    //If the test stopped the timeout first, it finished in time: leave it alone.
    if InterlockedCompareExchange(State, TIMEOUT_FIRED, TIMEOUT_RUNNING) <> TIMEOUT_RUNNING then
      exit;
{$IFDEF CPUX86}
    Ctx^.Eip := Cardinal(@RaiseTimeOutException);
{$ELSE}
{$IFDEF CPUX64}
    //Making the thread call a raise does not work on Win64: unwinding needs a return address and the stack aligned for
    //a call, and a thread stopped inside a system call has neither.  Instead it is made to fault, as if it had called
    //_TimeoutFaultAddress from where it stopped; Windows unwinds from a fault, and TimeoutExceptObjProc makes it
    //ETimedOut.  A debugger stops on it as a first-chance access violation.
    if (_TimeoutFaultAddress = nil) or not StackSlotWritable(Ctx^.Rsp - SizeOf(NativeUInt)) then
      exit;
    Ctx^.Rsp := Ctx^.Rsp - SizeOf(NativeUInt);
    PNativeUInt(Ctx^.Rsp)^ := Ctx^.Rip;
    Ctx^.Rip := NativeUInt(_TimeoutFaultAddress);
{$ELSE}
    exit;
{$ENDIF}
{$ENDIF}
    if not SetThreadContext(ThreadHandle, Ctx^) then
      exit;
    Redirected := True;
    //A thread blocked in the kernel only reaches the raise when the call returns, so end the call.  The wake-up is
    //queued while the thread is still suspended, so only a wait it is already in is ended.
    QueueUserAPC(@WakeAlertableWait, ThreadHandle, 0);
  finally
    ResumeThread(ThreadHandle);
  end;
  //CancelSynchronousIo waits for the call to end, so the thread must be running.  It is called at once, so the read or
  //write ended is the one the thread was blocked in, not one in the code handling the timeout.
  if Redirected and Assigned(_CancelSynchronousIo) then
    _CancelSynchronousIo(ThreadHandle);
end;

{ TTimeout }

procedure TTimeout.Stop;
begin
  InterlockedCompareExchange(FTimeoutThread.State, TIMEOUT_STOPPED, TIMEOUT_RUNNING);
  FTimeoutThread.Terminate;
end;

constructor TTimeout.Create(const ATimeout : Cardinal; AThreadHandle : THandle);
begin
  FTimeoutThread := TTimeoutThread.Create(true);
  FTimeoutThread.FreeOnTerminate := false;
  FTimeoutThread.ThreadHandle := AThreadHandle;
  FTimeoutThread.Timeout := ATimeout;
  FTimeoutThread.State := TIMEOUT_RUNNING;
end;

destructor TTimeout.Destroy;
begin
  //Unwinding and we need to stop the thread, as it may still raise an exception
  Stop;
  FTimeoutThread.WaitFor;
  CloseHandle(FTimeoutThread.ThreadHandle);
  //Runs the wake-up queued to this (the test's) thread if no alertable wait has, so it cannot cut short one in the next
  //test.  Any other user APC already queued to the thread runs here too.
  if FTimeoutThread.State = TIMEOUT_FIRED then
    SleepEx(0, True);
  FTimeoutThread.Free;
  inherited;
end;

{ TTimeoutThread }

procedure TTimeoutThread.Execute;
var
  elapsedTime : Int64;
  stopwatch : TStopWatch;
begin
  inherited;
  stopwatch := TStopWatch.Create;
  stopwatch.Reset;
  stopwatch.Start;
{$IFNDEF DELPHI_XE101_UP}               // <- H2077 Value assigned to 'elapsedTime' never used 10.1 Berlin and up
  elapsedTime := 0;
{$ENDIF}
  if Terminated then
    exit;
  repeat
    //Give some time back to the system to process the test.
    Sleep(20);
    if Terminated then
      Break;
    elapsedTime := stopwatch.ElapsedMilliseconds;
  until (elapsedTime >= Timeout);

  //If we haven't been terminated then we have timed out.
  if not Terminated then
    TimeoutThread;
end;

initialization
  @_CancelSynchronousIo := GetProcAddress(GetModuleHandle(kernel32), 'CancelSynchronousIo');
{$IFDEF CPUX64}
  _TimeoutFaultAddress := VirtualAlloc(nil, 4096, MEM_COMMIT or MEM_RESERVE, PAGE_NOACCESS);
  if _TimeoutFaultAddress <> nil then
  begin
    _PreviousExceptObjProc := ExceptObjProc;
    ExceptObjProc := @TimeoutExceptObjProc;
    _PreviousExceptClsProc := ExceptClsProc;
    ExceptClsProc := @TimeoutExceptClsProc;
  end;
{$ENDIF}

finalization
{$IFDEF CPUX64}
  if ExceptObjProc = @TimeoutExceptObjProc then
    ExceptObjProc := _PreviousExceptObjProc;
  if ExceptClsProc = @TimeoutExceptClsProc then
    ExceptClsProc := _PreviousExceptClsProc;
  if _TimeoutFaultAddress <> nil then
    VirtualFree(_TimeoutFaultAddress, 0, MEM_RELEASE);
{$ENDIF}

end.
