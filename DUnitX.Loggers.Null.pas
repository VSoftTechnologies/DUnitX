{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2013 Vincent Parrett                              }
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
    procedure OnBeginTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndSetupTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnEndTest(const threadId: Cardinal; const Test: ITestResult);
    procedure OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);
    procedure OnExecuteTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnLog(const logType: TLogLevel; const msg: string);
    procedure OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnSetupTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnTestError(const threadId: Cardinal; const Error: ITestError);
    procedure OnTestFailure(const threadId: Cardinal; const Failure: ITestError);
    procedure OnTestMemoryLeak(const threadId : Cardinal; const Test: ITestResult);
    procedure OnTestIgnored(const threadId: Cardinal; const AIgnored: ITestResult);
    procedure OnTestSuccess(const threadId: Cardinal; const Test: ITestResult);
    procedure OnTestingEnds(const RunResults: IRunResults);virtual;
    procedure OnTestingStarts(const threadId: Cardinal; const testCount: Cardinal; const testActiveCount: Cardinal);
  end;

implementation

{ TDUnitXNullLogger }

procedure TDUnitXNullLogger.OnBeginTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXNullLogger.OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXNullLogger.OnEndSetupTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXNullLogger.OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXNullLogger.OnEndTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXNullLogger.OnEndTest(const threadId: Cardinal; const Test: ITestResult);
begin

end;

procedure TDUnitXNullLogger.OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);
begin

end;

procedure TDUnitXNullLogger.OnExecuteTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXNullLogger.OnLog(const logType: TLogLevel; const msg: string);
begin

end;

procedure TDUnitXNullLogger.OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXNullLogger.OnSetupTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXNullLogger.OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXNullLogger.OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXNullLogger.OnTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXNullLogger.OnTestError(const threadId: Cardinal; const Error: ITestError);
begin

end;

procedure TDUnitXNullLogger.OnTestFailure(const threadId: Cardinal; const Failure: ITestError);
begin

end;

procedure TDUnitXNullLogger.OnTestIgnored(const threadId: Cardinal; const AIgnored: ITestResult);
begin

end;

procedure TDUnitXNullLogger.OnTestingEnds(const RunResults: IRunResults);
begin

end;

procedure TDUnitXNullLogger.OnTestingStarts(const threadId, testCount, testActiveCount: Cardinal);
begin

end;

procedure TDUnitXNullLogger.OnTestMemoryLeak(const threadId: Cardinal; const Test: ITestResult);
begin

end;

procedure TDUnitXNullLogger.OnTestSuccess(const threadId: Cardinal; const Test: ITestResult);
begin

end;


end.
