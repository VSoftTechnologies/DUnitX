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

  function InitialiseTimeout(const ATime: cardinal): ITimeout;

implementation

uses
  {$IFDEF USE_NS}
  WinAPI.Windows,
  System.Diagnostics,
  {$ELSE}
  Windows,
  Diagnostics,
  {$ENDIF}
  DUnitX.ResStrs,
  DUnitX.TestFramework,
  DUnitX.Exceptions;

// The following TimeOut code is based on the code found at
// https://code.google.com/p/delphitimeouts/
// DelphiTimeouts version 1.1
// Copyright (c) 2007-2008 Szymon Jachim

type
  TTimeoutThread = class(TThread)
  private
    procedure TimeoutThread;
  public
    ThreadHandle: Cardinal;
    Timeout: Cardinal;
    procedure Execute; override;
  end;

  TTimeout = class(TInterfacedObject, ITimeout)
  private
    FTimeoutThread: TTimeoutThread;
  public
    constructor Create(const ATimeout: Cardinal; AThreadHandle: THandle);
    destructor Destroy; override;

    procedure Stop;
  end;

function InitialiseTimeout(const ATime: cardinal): ITimeout;
var
  ThisThreadHandle: THandle;
begin
  DuplicateHandle(GetCurrentProcess, GetCurrentThread, GetCurrentProcess, @ThisThreadHandle, 0, True, DUPLICATE_SAME_ACCESS);
  Result := TTimeout.Create(ATime, ThisThreadHandle);
end;

procedure RaiseTimeOutException;
begin
  raise ETimedOut.Create(SOperationTimedOut);
end;

procedure TTimeoutThread.TimeoutThread;
var
  Ctx: _CONTEXT;
begin
  SuspendThread(ThreadHandle);
  Ctx.ContextFlags := CONTEXT_FULL;
  GetThreadContext(ThreadHandle, Ctx);

  {$IFDEF CPUX64}
  Ctx.Rip := Cardinal(@RaiseTimeOutException);
  {$ELSE}
  Ctx.Eip := Cardinal(@RaiseTimeOutException);
  {$ENDIF}
  SetThreadContext(ThreadHandle, Ctx);
  ResumeThread(ThreadHandle);
end;

{ TTimeout }

procedure TTimeout.Stop;
begin
  FTimeoutThread.Terminate;
end;

constructor TTimeout.Create(const ATimeout: Cardinal; AThreadHandle: THandle);
begin
  FTimeoutThread := TTimeoutThread.Create(true);
  FTimeoutThread.FreeOnTerminate := false;
  FTimeoutThread.ThreadHandle := AThreadHandle;
  FTimeoutThread.Timeout := ATimeout;
  FTimeoutThread.Start;
end;

destructor TTimeout.Destroy;
begin
  //Unwinding and we need to stop the thread, as it may still raise an exception
  Stop;
  FTimeoutThread.WaitFor;
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

  //Not sure why this was removed in previous revision, but it causes W1036 as
  //not being initialized if not present.
  elapsedTime := 0;

  if Terminated then
     exit;
  repeat
    //Give some time back to the system to process the test.
    Sleep(20);

    if Terminated then
      Break;

    elapsedTime :=  stopwatch.ElapsedMilliseconds;
  until (elapsedTime >= Timeout);

  //If we haven't been terminated then we have timed out.
  if not Terminated then
    TimeoutThread;
end;


end.
