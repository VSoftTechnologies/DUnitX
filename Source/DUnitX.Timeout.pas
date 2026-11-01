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

type
  /// <summary>
  ///   Times a test against its [MaxTime].  The test is not interrupted: once
  ///   it returns, the caller stops the timer and asks whether it overran.
  /// </summary>
  /// <remarks>
  ///   This used to suspend the test thread from a watchdog thread and rewrite
  ///   its instruction pointer so that it jumped into a raise.  On Win64 the
  ///   address was truncated to 32 bits, so the thread faulted when it
  ///   resumed; and even with the full address, a frame injected that way has
  ///   no return address and no unwind data, which x64 exception handling
  ///   needs.  Timing the test and checking it afterwards is safe on every
  ///   platform, but cannot stop a test that never returns.
  /// </remarks>
  ITimeout = interface(IUnknown)
    ['{7708FBA2-9790-421B-A780-9E50363C8EBD}']
    /// <summary>Stops timing.  Safe to call more than once.</summary>
    procedure Stop;
    /// <summary>True when more than the allowed time elapsed before Stop.</summary>
    function TimedOut : boolean;
    /// <summary>Raises ETimedOut when TimedOut is true.</summary>
    procedure RaiseIfTimedOut;
  end;

/// <summary>Starts timing, allowing ATime milliseconds.</summary>
function InitialiseTimeout(const ATime : cardinal) : ITimeout;

implementation

uses
{$IFDEF USE_NS}
  System.Diagnostics,
{$ELSE}
  Diagnostics,
{$ENDIF}
  DUnitX.ResStrs,
  DUnitX.Exceptions;

type
  TTimeout = class(TInterfacedObject, ITimeout)
  private
    FTimeout : Cardinal;
    FStopwatch : TStopwatch;
  public
    constructor Create(const ATimeout : Cardinal);

    procedure Stop;
    function TimedOut : boolean;
    procedure RaiseIfTimedOut;
  end;

function InitialiseTimeout(const ATime : cardinal) : ITimeout;
begin
  Result := TTimeout.Create(ATime);
end;

{ TTimeout }

constructor TTimeout.Create(const ATimeout : Cardinal);
begin
  inherited Create;
  FTimeout := ATimeout;
  FStopwatch := TStopwatch.StartNew;
end;

procedure TTimeout.Stop;
begin
  FStopwatch.Stop;
end;

function TTimeout.TimedOut : boolean;
begin
  Result := FStopwatch.ElapsedMilliseconds > FTimeout;
end;

procedure TTimeout.RaiseIfTimedOut;
begin
  if TimedOut then
    raise ETimedOut.Create(SOperationTimedOut);
end;

end.
