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

unit DUnitX.Loggers.Text;

interface

uses
  DUnitX.TestFramework,
  classes;

{$I DUnitX.inc}

type
  // Simple text file logger.
  TDUnitXTextFileLogger = class(TInterfacedObject, ITestLogger)
  private
    //FFileName : string;
  protected
    procedure OnTestingStarts(const threadId, testCount, testActiveCount: Cardinal);

    procedure OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnBeginTest(const threadId: Cardinal; const Test: ITestInfo);

    procedure OnSetupTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnEndSetupTest(const threadId: Cardinal; const Test: ITestInfo);

    procedure OnExecuteTest(const threadId : Cardinal; const Test: ITestInfo);


    procedure OnTestSuccess(const threadId: Cardinal; const Test: ITestResult);
    procedure OnTestError(const threadId: Cardinal; const Error: ITestError);
    procedure OnTestFailure(const threadId: Cardinal; const Failure: ITestError);
    procedure OnTestIgnored(const threadId: Cardinal; const AIgnored: ITestResult);
    procedure OnTestMemoryLeak(const threadId : Cardinal; const Test: ITestResult);

    procedure OnLog(const logType: TLogLevel; const msg : string);

    procedure OnTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnEndTeardownTest(const threadId: Cardinal; const Test: ITestInfo);

    procedure OnEndTest(const threadId: Cardinal; const Test: ITestResult);


    procedure OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);

    procedure OnTestingEnds(const RunResults: IRunResults);
  public
    constructor Create(const AFileName: string; const overwrite : boolean = true);
  end;

implementation


{ TDUnitXTextFileLogger }

constructor TDUnitXTextFileLogger.Create(const AFileName : string; const overwrite : boolean);
begin

end;

procedure TDUnitXTextFileLogger.OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnEndSetupTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnEndTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnEndTest(const threadId: Cardinal; const Test: ITestResult);
begin

end;

procedure TDUnitXTextFileLogger.OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);
begin

end;

procedure TDUnitXTextFileLogger.OnExecuteTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnTestError(const threadId: Cardinal; const Error: ITestError);
begin

end;

procedure TDUnitXTextFileLogger.OnTestFailure(const threadId: Cardinal; const Failure: ITestError);
begin

end;

procedure TDUnitXTextFileLogger.OnTestIgnored(const threadId: Cardinal; const AIgnored: ITestResult);
begin

end;

procedure TDUnitXTextFileLogger.OnLog(const logType : TLogLevel; const msg: string);
begin

end;

procedure TDUnitXTextFileLogger.OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnSetupTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnBeginTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;


procedure TDUnitXTextFileLogger.OnTestSuccess(const threadId: Cardinal; const Test: ITestResult);
begin

end;

procedure TDUnitXTextFileLogger.OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnTestingEnds(const RunResults: IRunResults);
begin

end;

procedure TDUnitXTextFileLogger.OnTestingStarts(const threadId, testCount, testActiveCount : Cardinal);
begin

end;


procedure TDUnitXTextFileLogger.OnTestMemoryLeak(const threadId: Cardinal; const Test: ITestResult);
begin

end;

end.
