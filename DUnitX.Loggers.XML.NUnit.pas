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

unit DUnitX.Loggers.XML.NUnit;

interface

uses
  DUnitX.TestFramework,
  classes;

{$I DUnitX.inc}

type
  IDUnitXXMLNUnitLogger = interface(ITestLogger)
    ['{18886A0C-1937-4ADA-A926-396019E570AE}']
    procedure SetFilename(const filename: string);
    function GetFilename : string;
    property Filename : string read GetFilename write SetFilename;
  end;

  TDUnitXXMLNUnitLogger = class(TInterfacedObject, IDUnitXXMLNUnitLogger)
  private

  protected
    procedure SetFilename(const filename: string);
    function GetFilename: string;

    procedure OnTestingStarts(const threadId: Cardinal);

    procedure OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnBeginTest(const threadId: Cardinal; Test: ITestInfo);

    procedure OnSetupTest(const threadId: Cardinal; Test: ITestInfo);
    procedure OnEndSetupTest(const threadId: Cardinal; Test: ITestInfo);

    procedure OnExecuteTest(const threadId: Cardinal; Test: ITestInfo);

    procedure OnTestSuccess(const threadId: Cardinal; Test: ITestResult);
    procedure OnTestWarning(const threadId: Cardinal; AWarning: ITestResult);
    procedure OnTestError(const threadId: Cardinal; Error: ITestError);
    procedure OnTestFailure(const threadId: Cardinal; Failure: ITestResult);

    procedure OnLog(const logType: TLogLevel; const msg: string);

    procedure OnTeardownTest(const threadId: Cardinal; Test: ITestInfo);
    procedure OnEndTeardownTest(const threadId: Cardinal; Test: ITestInfo);

    procedure OnEndTest(const threadId: Cardinal; Test: ITestResult);

    procedure OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);

    procedure OnTestingEnds(const TestResult: ITestResults);
  public
    constructor Create(const AFileName : string; const overwrite : boolean = true); overload;
    constructor Create(const AFile : TFileStream); overload;

    property Filename : string read GetFilename write SetFilename;
  end;

implementation

{ TDUnitXTextFileLogger }

constructor TDUnitXXMLNUnitLogger.Create(const AFileName: string; const overwrite: boolean);
begin

end;

constructor TDUnitXXMLNUnitLogger.Create(const AFile: TFileStream);
begin

end;

function TDUnitXXMLNUnitLogger.GetFilename: string;
begin

end;

procedure TDUnitXXMLNUnitLogger.OnBeginTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnEndSetupTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnEndTeardownTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnEndTest(const threadId: Cardinal; Test: ITestResult);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnExecuteTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnLog(const logType: TLogLevel; const msg: string);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnSetupTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnTeardownTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnTestError(const threadId: Cardinal; Error: ITestError);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnTestFailure(const threadId: Cardinal; Failure: ITestResult);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnTestingEnds(const TestResult: ITestResults);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnTestingStarts(const threadId: Cardinal);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnTestSuccess(const threadId: Cardinal; Test: ITestResult);
begin

end;

procedure TDUnitXXMLNUnitLogger.OnTestWarning(const threadId: Cardinal; AWarning: ITestResult);
begin

end;

procedure TDUnitXXMLNUnitLogger.SetFilename(const filename: string);
begin

end;

end.
