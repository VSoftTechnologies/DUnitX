{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2012 Vincent Parrett                              }
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

    procedure OnBeginTest(const threadId: Cardinal; Test: ITestInfo);

    procedure OnSetupTest(const threadId: Cardinal; Test: ITestInfo);
    procedure OnEndSetupTest(const threadId: Cardinal; Test: ITestInfo);

    procedure OnExecuteTest(const threadId : Cardinal; Test: ITestInfo);


    procedure OnTestSuccess(const threadId: Cardinal; Test: ITestResult);
    procedure OnTestWarning(const threadId: Cardinal; AWarning: ITestResult);
    procedure OnTestError(const threadId: Cardinal; Error: ITestError);
    procedure OnTestFailure(const threadId: Cardinal; Failure: ITestError);

    procedure OnLog(const logType: TLogLevel; const msg : string);

    procedure OnTeardownTest(const threadId: Cardinal; Test: ITestInfo);
    procedure OnEndTeardownTest(const threadId: Cardinal; Test: ITestInfo);

    procedure OnEndTest(const threadId: Cardinal; Test: ITestResult);


    procedure OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);

    procedure OnTestingEnds(const TestResult: ITestResults);
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

procedure TDUnitXTextFileLogger.OnEndSetupTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnEndTeardownTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnEndTest(const threadId: Cardinal; Test: ITestResult);
begin

end;

procedure TDUnitXTextFileLogger.OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);
begin

end;

procedure TDUnitXTextFileLogger.OnExecuteTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnTestError(const threadId: Cardinal; Error: ITestError);
begin

end;

procedure TDUnitXTextFileLogger.OnTestFailure(const threadId: Cardinal; Failure: ITestError);
begin

end;

procedure TDUnitXTextFileLogger.OnLog(const logType : TLogLevel; const msg: string);
begin

end;

procedure TDUnitXTextFileLogger.OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnSetupTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnBeginTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;


procedure TDUnitXTextFileLogger.OnTestSuccess(const threadId: Cardinal; Test: ITestResult);
begin

end;

procedure TDUnitXTextFileLogger.OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnTeardownTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXTextFileLogger.OnTestingEnds(const TestResult: ITestResults);
begin

end;

procedure TDUnitXTextFileLogger.OnTestingStarts(const threadId, testCount, testActiveCount : Cardinal);
begin

end;

procedure TDUnitXTextFileLogger.OnTestWarning(const threadId: Cardinal; AWarning: ITestResult);
begin

end;

end.
