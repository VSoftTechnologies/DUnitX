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

unit DUnitX.Loggers.Console;

interface
uses
  DUnitX.ConsoleWriter.Base,
  DUnitX.TestFramework,
  classes;

{$I DUnitX.inc}

type
  ///
  ///  Writes nicely formatted and colored messages to the console window.
  ///
  TDUnitXConsoleLogger = class(TInterfacedObject, ITestLogger)
  private
    FConsoleWriter : IDUnitXConsoleWriter;
    FQuietMode : boolean;
  protected
    procedure OnTestingStarts(const threadId, testCount, testActiveCount : Cardinal);

    procedure OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnBeginTest(const threadId: Cardinal; Test: ITestInfo);

    procedure OnSetupTest(const threadId: Cardinal; Test: ITestInfo);
    procedure OnEndSetupTest(const threadId: Cardinal; Test: ITestInfo);

    procedure OnExecuteTest(const threadId : Cardinal; Test: ITestInfo);


    procedure OnTestWarning(const threadId: Cardinal; AWarning: ITestResult);
    procedure OnTestError(const threadId: Cardinal; Error: ITestError);
    procedure OnTestFailure(const threadId: Cardinal; Failure: ITestError);
    procedure OnTestSuccess(const threadId: Cardinal; Test: ITestResult);
    procedure OnLog(const logType : TLogLevel; const msg : string);

    procedure OnTeardownTest(const threadId: Cardinal; Test: ITestInfo);
    procedure OnEndTeardownTest(const threadId: Cardinal; Test: ITestInfo);

    procedure OnEndTest(const threadId: Cardinal; Test: ITestResult);

    procedure OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);

    procedure OnTestingEnds(const TestResults: ITestResults);

  public
    constructor Create(const quietMode : boolean = false);
    destructor Destroy;override;
  end;


implementation

uses
  DUnitX.IoC,
  SysUtils;


{ TDUnitXConsoleLogger }

constructor TDUnitXConsoleLogger.Create(const quietMode : boolean = false);
begin
  FQuietMode := quietMode;
  FConsoleWriter := TDUnitXIoC.DefaultContainer.Resolve<IDUnitXConsoleWriter>();
  if FConsoleWriter = nil then
    raise Exception.Create('No ConsoleWriter Class is registered.' + #13#10 +
                           'You will need to include DUnitX.Windows.Console or DUnitX.MACOS.Console in you application');
end;

destructor TDUnitXConsoleLogger.Destroy;
begin

  inherited;
end;

procedure TDUnitXConsoleLogger.OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.Outdent(1);
  FConsoleWriter.WriteLn;
end;

procedure TDUnitXConsoleLogger.OnEndSetupTest(const threadId: Cardinal; Test: ITestInfo);
begin
  if FQuietMode then
    exit;

//  FConsoleWriter.WriteLn;
//  FConsoleWriter.Outdent(1);
  FConsoleWriter.WriteLn;

end;

procedure TDUnitXConsoleLogger.OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXConsoleLogger.OnEndTeardownTest(const threadId: Cardinal; Test: ITestInfo);
begin

end;

procedure TDUnitXConsoleLogger.OnEndTest(const threadId: Cardinal; Test: ITestResult);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.Outdent(1);
  FConsoleWriter.WriteLn;
end;

procedure TDUnitXConsoleLogger.OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.Outdent(3);
  FConsoleWriter.WriteLn;
end;

procedure TDUnitXConsoleLogger.OnExecuteTest(const threadId: Cardinal; Test: ITestInfo);
begin
  if FQuietMode then
  begin
    FConsoleWriter.Write('.');
    exit;
  end;


  //FConsoleWriter.SetColour(ccBrightAqua);
  //FConsoleWriter.Indent(1);
  FConsoleWriter.WriteLn('Executing Test : ' + Test.Name);
  FConsoleWriter.WriteLn;
  //FConsoleWriter.SetColour(ccDefault);
end;

procedure TDUnitXConsoleLogger.OnTestError(const threadId: Cardinal; Error: ITestError);
begin
  if FQuietMode then
  begin
    FConsoleWriter.Write('E');
    exit;
  end;

end;

procedure TDUnitXConsoleLogger.OnTestFailure(const threadId: Cardinal; Failure: ITestError);
begin
  if FQuietMode then
  begin
    FConsoleWriter.Write('F');
    exit;
  end;

end;

procedure TDUnitXConsoleLogger.OnLog(const logType: TLogLevel; const msg: string);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.Indent(2);
  try

    case logType  of
      ltInformation: FConsoleWriter.SetColour(ccDefault);
      ltWarning: FConsoleWriter.SetColour(ccBrightYellow);
      ltError: FConsoleWriter.SetColour(ccBrightRed);
    end;

    FConsoleWriter.WriteLn(msg);
  finally
    FConsoleWriter.Outdent(2);
    FConsoleWriter.SetColour(ccDefault);
  end;
end;

procedure TDUnitXConsoleLogger.OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.Indent(1);
  FConsoleWriter.WriteLn('Running Fixture Setup Method : ' + fixture.SetupFixtureMethodName);

end;

procedure TDUnitXConsoleLogger.OnSetupTest(const threadId: Cardinal; Test: ITestInfo);
begin
  if FQuietMode then
    exit;
//  FConsoleWriter.Indent(1);
  FConsoleWriter.SetColour(ccBrightPurple);
  FConsoleWriter.WriteLn('Running Setup for : ' + Test.Name);
end;

procedure TDUnitXConsoleLogger.OnBeginTest(const threadId: Cardinal; Test: ITestInfo);
begin
  if FQuietMode then
    exit;


  FConsoleWriter.SetColour(ccBrightAqua);
  FConsoleWriter.Indent(1);
  FConsoleWriter.WriteLn('Test : ' +  Test.FullName);
  FConsoleWriter.WriteLn('-------------------------------------------------');
  FConsoleWriter.SetColour(ccDefault);
end;

procedure TDUnitXConsoleLogger.OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.SetColour(ccBrightYellow);
  FConsoleWriter.Indent(2);
  FConsoleWriter.WriteLn('Fixture : ' + fixture.FullName);
  FConsoleWriter.WriteLn('-------------------------------------------------');
  FConsoleWriter.Indent(1);
  FConsoleWriter.SetColour(ccDefault);
end;


procedure TDUnitXConsoleLogger.OnTestSuccess(const threadId: Cardinal; Test: ITestResult);
begin
  if FQuietMode then
  begin
    FConsoleWriter.Write('.');
    exit;
  end;
  FConsoleWriter.Indent(2);
  FConsoleWriter.WriteLn('Success.');
  FConsoleWriter.Outdent(2);
end;

procedure TDUnitXConsoleLogger.OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.WriteLn('Running Fixture Teardown Method : ' + fixture.TearDownFixtureMethodName);
  FConsoleWriter.WriteLn;
end;

procedure TDUnitXConsoleLogger.OnTeardownTest(const threadId: Cardinal; Test: ITestInfo);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.WriteLn;
  FConsoleWriter.Indent(1);
  FConsoleWriter.WriteLn('Running Teardown for Test : ' + test.Name);
  FConsoleWriter.WriteLn;
  FConsoleWriter.Outdent(1);
end;

procedure TDUnitXConsoleLogger.OnTestingEnds(const TestResults: ITestResults);
var
  testResult: ITestResult;
begin
  if FQuietMode then
  begin
      FConsoleWriter.WriteLn;
      FConsoleWriter.WriteLn;
  end
  else
  begin
    FConsoleWriter.OutDent(1);
    FConsoleWriter.WriteLn('Done testing.');
  end;


  FConsoleWriter.SetColour(ccBrightWhite);
  FConsoleWriter.WriteLn(Format('Tests Run     : %d',[TestResults.Count]));

  if TestResults.PassCount > 0 then
    FConsoleWriter.SetColour(ccBrightGreen)
  else
    FConsoleWriter.SetColour(ccDefault );
  FConsoleWriter.WriteLn(Format('Tests Passed  : %d',[TestResults.PassCount]));

  if TestResults.FailureCount > 0 then
    FConsoleWriter.SetColour(ccBrightRed)
  else
    FConsoleWriter.SetColour(ccDefault );
  FConsoleWriter.WriteLn(Format('Tests Failed  : %d',[TestResults.FailureCount]));

  if TestResults.ErrorCount > 0 then
    FConsoleWriter.SetColour(ccBrightRed)
  else
    FConsoleWriter.SetColour(ccDefault );
  FConsoleWriter.WriteLn(Format('Tests Errored : %d',[TestResults.ErrorCount]));


  if TestResults.FailureCount > 0  then
  begin
    FConsoleWriter.SetColour(ccBrightRed);
    FConsoleWriter.WriteLn;
    FConsoleWriter.WriteLn('Failing Tests');
    FConsoleWriter.WriteLn;
    FConsoleWriter.SetColour(ccDefault );

    for testResult in TestResults.GetResults do
    begin
      if testResult.ResultType = TTestResultType.Failure then
      begin
        FConsoleWriter.WriteLn(testResult.Test.Fixture.Name + '.' + testResult.Test.Name);
      end;
      
    end;

  end;
  FConsoleWriter.SetColour(ccDefault );

end;

procedure TDUnitXConsoleLogger.OnTestingStarts(const threadId, testCount, testActiveCount : Cardinal);
begin
  if FQuietMode then
  begin
    FConsoleWriter.WriteLn(Format('DUnitX - [%s] - Starting Tests.',[ExtractFileName(ParamStr(0))]));
    FConsoleWriter.WriteLn;
    exit;
  end;

  if TDUnitX.CommandLine.HideBanner then
    exit;

  FConsoleWriter.SetColour(ccBrightWhite);
  FConsoleWriter.WriteLn('**********************************************************************');
  FConsoleWriter.WriteLn('               DUnitX - (c) 2013 Vincent Parrett                      ');
  FConsoleWriter.WriteLn('                     vincent@finalbuilder.com                         ');
  FConsoleWriter.WriteLn('                                                                      ');
  FConsoleWriter.WriteLn('         License - http://www.apache.org/licenses/LICENSE-2.0         ');
  FConsoleWriter.WriteLn('**********************************************************************');
  FConsoleWriter.WriteLn();
  FConsoleWriter.SetColour(ccDefault);
  FConsoleWriter.Indent(1);
end;

procedure TDUnitXConsoleLogger.OnTestWarning(const threadId: Cardinal; AWarning: ITestResult);
begin
  if FQuietMode then
    FConsoleWriter.Write('W');
end;

end.

