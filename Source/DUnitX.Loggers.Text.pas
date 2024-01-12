{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2015 Vincent Parrett & Contributors               }
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

unit DUnitX.Loggers.Text;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Classes,
  {$ELSE}
  Classes,
  {$ENDIF}
  DUnitX.TestFramework;


type
  // Simple text file logger.
  TDUnitXTextFileLogger = class(TInterfacedObject, ITestLogger)
  private
    //FFileName : string;
  protected
    procedure OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount: Cardinal);

    procedure OnStartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);

    procedure OnSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnEndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);

    procedure OnBeginTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure OnSetupTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure OnExecuteTest(const threadId: TThreadID; const Test: ITestInfo);


    procedure OnTestSuccess(const threadId: TThreadID; const Test: ITestResult);
    procedure OnTestError(const threadId: TThreadID; const Error: ITestError);
    procedure OnTestFailure(const threadId: TThreadID; const Failure: ITestError);
    procedure OnTestIgnored(const threadId: TThreadID; const AIgnored: ITestResult);
    procedure OnTestMemoryLeak(const threadId: TThreadID; const Test: ITestResult);

    procedure OnLog(const logType: TLogLevel; const msg : string);

    procedure OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndTeardownTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure OnEndTest(const threadId: TThreadID; const Test: ITestResult);


    procedure OnTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnEndTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);

    procedure OnEndTestFixture(const threadId: TThreadID; const results: IFixtureResult);

    procedure OnTestingEnds(const RunResults: IRunResults);
  public
    constructor Create(const AFileName: string; const overwrite : boolean = true);
  end;

implementation

uses
  {$IFDEF DELPHI_2010}
  DUnitX.Exceptions, //ENotImplemented is not in SysUtils in D2010
  {$ENDIF}
  {$IFDEF USE_NS}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}


{ TDUnitXTextFileLogger }

constructor TDUnitXTextFileLogger.Create(const AFileName : string; const overwrite : boolean);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnEndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnEndTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnEndTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnEndTest(const threadId: TThreadID; const Test: ITestResult);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnEndTestFixture(const threadId: TThreadID; const results: IFixtureResult);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnExecuteTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnTestError(const threadId: TThreadID; const Error: ITestError);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnTestFailure(const threadId: TThreadID; const Failure: ITestError);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnTestIgnored(const threadId: TThreadID; const AIgnored: ITestResult);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnLog(const logType : TLogLevel; const msg: string);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnSetupTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnBeginTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnStartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  raise ENotImplemented.Create('Not Implemented');
end;


procedure TDUnitXTextFileLogger.OnTestSuccess(const threadId: TThreadID; const Test: ITestResult);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnTestingEnds(const RunResults: IRunResults);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TDUnitXTextFileLogger.OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount : Cardinal);
begin
  raise ENotImplemented.Create('Not Implemented');
end;


procedure TDUnitXTextFileLogger.OnTestMemoryLeak(const threadId: TThreadID; const Test: ITestResult);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

end.
