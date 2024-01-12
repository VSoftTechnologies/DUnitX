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

unit DUnitX.Loggers.Null;

interface

uses
  DUnitX.TestFramework;

type
  ///  A Base class for loggers that do not need to use every interface method.
  TDUnitXNullLogger = class(TInterfacedObject,ITestLogger)
  protected
    procedure OnBeginTest(const threadId: TThreadID; const Test: ITestInfo); virtual;
    procedure OnEndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo); virtual;
    procedure OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo); virtual;
    procedure OnEndTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo); virtual;
    procedure OnEndTeardownTest(const threadId: TThreadID; const Test: ITestInfo); virtual;
    procedure OnEndTest(const threadId: TThreadID; const Test: ITestResult); virtual;
    procedure OnEndTestFixture(const threadId: TThreadID; const results: IFixtureResult); virtual;
    procedure OnExecuteTest(const threadId: TThreadID; const Test: ITestInfo); virtual;
    procedure OnLog(const logType: TLogLevel; const msg: string); virtual;
    procedure OnSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo); virtual;
    procedure OnSetupTest(const threadId: TThreadID; const Test: ITestInfo); virtual;
    procedure OnStartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo); virtual;
    procedure OnTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo); virtual;
    procedure OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo); virtual;
    procedure OnTestError(const threadId: TThreadID; const Error: ITestError); virtual;
    procedure OnTestFailure(const threadId: TThreadID; const Failure: ITestError); virtual;
    procedure OnTestMemoryLeak(const threadId: TThreadID; const Test: ITestResult); virtual;
    procedure OnTestIgnored(const threadId: TThreadID; const AIgnored: ITestResult); virtual;
    procedure OnTestSuccess(const threadId: TThreadID; const Test: ITestResult); virtual;
    procedure OnTestingEnds(const RunResults: IRunResults); virtual;
    procedure OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount: Cardinal); virtual;
  end;

implementation

{ TDUnitXNullLogger }

procedure TDUnitXNullLogger.OnBeginTest(const threadId: TThreadID; const Test: ITestInfo);
begin

end;

procedure TDUnitXNullLogger.OnEndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXNullLogger.OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo);
begin

end;

procedure TDUnitXNullLogger.OnEndTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXNullLogger.OnEndTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
begin

end;

procedure TDUnitXNullLogger.OnEndTest(const threadId: TThreadID; const Test: ITestResult);
begin

end;

procedure TDUnitXNullLogger.OnEndTestFixture(const threadId: TThreadID; const results: IFixtureResult);
begin

end;

procedure TDUnitXNullLogger.OnExecuteTest(const threadId: TThreadID; const Test: ITestInfo);
begin

end;

procedure TDUnitXNullLogger.OnLog(const logType: TLogLevel; const msg: string);
begin

end;

procedure TDUnitXNullLogger.OnSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXNullLogger.OnSetupTest(const threadId: TThreadID; const Test: ITestInfo);
begin

end;

procedure TDUnitXNullLogger.OnStartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXNullLogger.OnTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXNullLogger.OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
begin

end;

procedure TDUnitXNullLogger.OnTestError(const threadId: TThreadID; const Error: ITestError);
begin

end;

procedure TDUnitXNullLogger.OnTestFailure(const threadId: TThreadID; const Failure: ITestError);
begin

end;

procedure TDUnitXNullLogger.OnTestIgnored(const threadId: TThreadID; const AIgnored: ITestResult);
begin

end;

procedure TDUnitXNullLogger.OnTestingEnds(const RunResults: IRunResults);
begin

end;

procedure TDUnitXNullLogger.OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount: Cardinal);
begin

end;

procedure TDUnitXNullLogger.OnTestMemoryLeak(const threadId: TThreadID; const Test: ITestResult);
begin

end;

procedure TDUnitXNullLogger.OnTestSuccess(const threadId: TThreadID; const Test: ITestResult);
begin

end;


end.
