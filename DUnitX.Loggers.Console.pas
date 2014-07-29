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
  strict protected
    procedure SetConsoleDefaultColor(); virtual;
    procedure SetConsoleErrorColor(); virtual;
    procedure SetConsolePassColor(); virtual;
    procedure SetConsoleSetupTestColor(); virtual;
    procedure SetConsoleRunTestColor(); virtual;
    procedure SetConsoleSummaryColor(); virtual;
    procedure SetConsoleWarningColor(); virtual;
  protected
    procedure OnTestingStarts(const threadId, testCount, testActiveCount : Cardinal);

    procedure OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnBeginTest(const threadId: Cardinal; const Test: ITestInfo);

    procedure OnSetupTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnEndSetupTest(const threadId: Cardinal; const Test: ITestInfo);

    procedure OnExecuteTest(const threadId : Cardinal; const Test: ITestInfo);

    procedure OnTestMemoryLeak(const threadId : Cardinal; const Test: ITestResult);
    procedure OnTestIgnored(const threadId: Cardinal;const  AIgnored: ITestResult);
    procedure OnTestError(const threadId: Cardinal; const Error: ITestError);
    procedure OnTestFailure(const threadId: Cardinal; const Failure: ITestError);
    procedure OnTestSuccess(const threadId: Cardinal; const Test: ITestResult);
    procedure OnLog(const logType : TLogLevel; const msg : string);

    procedure OnTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnEndTeardownTest(const threadId: Cardinal; const Test: ITestInfo);

    procedure OnEndTest(const threadId: Cardinal; const Test: ITestResult);

    procedure OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);

    procedure OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);

    procedure OnTestingEnds(const RunResults: IRunResults);
  public
    constructor Create(const quietMode : boolean = false);
    destructor Destroy;override;
  end;

implementation

uses
  DUnitX.AutoDetect.Console,
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

procedure TDUnitXConsoleLogger.OnEndSetupTest(const threadId: Cardinal; const Test: ITestInfo);
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

procedure TDUnitXConsoleLogger.OnEndTeardownTest(const threadId: Cardinal; const  Test: ITestInfo);
begin

end;

procedure TDUnitXConsoleLogger.OnEndTest(const threadId: Cardinal; const  Test: ITestResult);
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

procedure TDUnitXConsoleLogger.OnExecuteTest(const threadId: Cardinal; const  Test: ITestInfo);
begin
  if FQuietMode then
  begin
    FConsoleWriter.Write('.');
    exit;
  end;


  //SetConsoleRunTestColor();
  //FConsoleWriter.Indent(1);
  FConsoleWriter.WriteLn('Executing Test : ' + Test.Name);
  FConsoleWriter.WriteLn;
  //SetConsoleDefaultColor();
end;

procedure TDUnitXConsoleLogger.OnTestError(const threadId: Cardinal; const  Error: ITestError);
begin
  if FQuietMode then
  begin
    FConsoleWriter.Write('E');
    exit;
  end;
end;

procedure TDUnitXConsoleLogger.OnTestFailure(const threadId: Cardinal; const  Failure: ITestError);
begin
  if FQuietMode then
  begin
    FConsoleWriter.Write('F');
    exit;
  end;
end;

procedure TDUnitXConsoleLogger.OnTestIgnored(const threadId: Cardinal; const AIgnored: ITestResult);
begin
  if FQuietMode then
    FConsoleWriter.Write('I');

end;

procedure TDUnitXConsoleLogger.OnLog(const logType: TLogLevel; const msg: string);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.Indent(2);
  try

    case logType  of
      TLogLevel.Information: SetConsoleDefaultColor();
      TLogLevel.Warning: SetConsoleWarningColor();
      TLogLevel.Error: SetConsoleErrorColor();
    end;

    FConsoleWriter.WriteLn(msg);
  finally
    FConsoleWriter.Outdent(2);
    SetConsoleDefaultColor();
  end;
end;

procedure TDUnitXConsoleLogger.OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.Indent(1);
  FConsoleWriter.WriteLn('Running Fixture Setup Method : ' + fixture.SetupFixtureMethodName);

end;

procedure TDUnitXConsoleLogger.OnSetupTest(const threadId: Cardinal; const  Test: ITestInfo);
begin
  if FQuietMode then
    exit;

  SetConsoleSetupTestColor();
  FConsoleWriter.WriteLn('Running Setup for : ' + Test.Name);
end;

procedure TDUnitXConsoleLogger.OnBeginTest(const threadId: Cardinal; const  Test: ITestInfo);
begin
  if FQuietMode then
    exit;

  SetConsoleRunTestColor();
  FConsoleWriter.Indent(1);
  FConsoleWriter.WriteLn('Test : ' +  Test.FullName);
  FConsoleWriter.WriteLn('-------------------------------------------------');
  SetConsoleDefaultColor();
end;

procedure TDUnitXConsoleLogger.OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin
  if FQuietMode then
    exit;

  SetConsoleWarningColor();
  FConsoleWriter.Indent(2);
  FConsoleWriter.WriteLn('Fixture : ' + fixture.FullName);
  FConsoleWriter.WriteLn('-------------------------------------------------');
  FConsoleWriter.Indent(1);
  SetConsoleDefaultColor();
end;


procedure TDUnitXConsoleLogger.OnTestSuccess(const threadId: Cardinal; const Test: ITestResult);
begin
  if FQuietMode then
  begin
    FConsoleWriter.Write('.');
    exit;
  end;
  FConsoleWriter.Indent(2);
  SetConsolePassColor;
  FConsoleWriter.WriteLn('Success.');
  SetConsoleDefaultColor;
  FConsoleWriter.Outdent(2);
end;

procedure TDUnitXConsoleLogger.OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.WriteLn('Running Fixture Teardown Method : ' + fixture.TearDownFixtureMethodName);
  FConsoleWriter.WriteLn;
end;

procedure TDUnitXConsoleLogger.OnTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.WriteLn;
  FConsoleWriter.Indent(1);
  FConsoleWriter.WriteLn('Running Teardown for Test : ' + test.Name);
  FConsoleWriter.WriteLn;
  FConsoleWriter.Outdent(1);
end;

procedure TDUnitXConsoleLogger.OnTestingEnds(const RunResults: IRunResults);
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

  SetConsoleSummaryColor();
  FConsoleWriter.WriteLn(Format('Tests Found   : %d',[RunResults.TestCount]));

  if RunResults.IgnoredCount > 0 then
    SetConsoleWarningColor()
  else
    SetConsoleDefaultColor();
  FConsoleWriter.WriteLn(Format('Tests Ignored : %d',[RunResults.IgnoredCount]));


  if RunResults.PassCount > 0 then
    SetConsolePassColor()
  else
    SetConsoleDefaultColor();
  FConsoleWriter.WriteLn(Format('Tests Passed  : %d',[RunResults.PassCount]));

  if RunResults.MemoryLeakCount > 0 then
    SetConsoleWarningColor()
  else
    SetConsoleDefaultColor();
  FConsoleWriter.WriteLn(Format('Tests Leaked  : %d',[RunResults.MemoryLeakCount]));

  if RunResults.FailureCount > 0 then
    SetConsoleErrorColor()
  else
    SetConsoleDefaultColor();
  FConsoleWriter.WriteLn(Format('Tests Failed  : %d',[RunResults.FailureCount]));

  if RunResults.ErrorCount > 0 then
    SetConsoleErrorColor()
  else
    SetConsoleDefaultColor();
  FConsoleWriter.WriteLn(Format('Tests Errored : %d',[RunResults.ErrorCount]));


  if RunResults.FailureCount > 0  then
  begin
    SetConsoleErrorColor();
    FConsoleWriter.WriteLn;
    FConsoleWriter.WriteLn('Failing Tests');
    FConsoleWriter.WriteLn;
    SetConsoleDefaultColor();


    for testResult in RunResults.GetAllTestResults do
    begin
      if testResult.ResultType = TTestResultType.Failure then
      begin
        SetConsoleErrorColor();
        FConsoleWriter.WriteLn('  ' + testResult.Test.FullName);
        SetConsoleDefaultColor();
        FConsoleWriter.WriteLn('  Message: ' + testResult.Message);
        FConsoleWriter.WriteLn;
      end;
    end;
    FConsoleWriter.WriteLn;
  end;

  if RunResults.ErrorCount > 0  then
  begin
    SetConsoleErrorColor();
    FConsoleWriter.WriteLn;
    FConsoleWriter.WriteLn('Tests With Errors');
    FConsoleWriter.WriteLn;
    SetConsoleDefaultColor();

    for testResult in RunResults.GetAllTestResults do
    begin
      if testResult.ResultType = TTestResultType.Error then
      begin
        SetConsoleErrorColor();
        FConsoleWriter.WriteLn('  ' + testResult.Test.FullName);
        SetConsoleDefaultColor();
        FConsoleWriter.WriteLn('  Message: ' + testResult.Message);
        FConsoleWriter.WriteLn;
      end;
    end;
    FConsoleWriter.WriteLn;
  end;

  if RunResults.MemoryLeakCount > 0  then
  begin
    SetConsoleWarningColor();
    FConsoleWriter.WriteLn;
    FConsoleWriter.WriteLn('Tests With Memory Leak');
    FConsoleWriter.WriteLn;
    SetConsoleDefaultColor();

    for testResult in RunResults.GetAllTestResults do
    begin
      if testResult.ResultType = TTestResultType.MemoryLeak then
      begin
        SetConsoleWarningColor();
        FConsoleWriter.WriteLn('  ' + testResult.Test.FullName);
        SetConsoleDefaultColor();
        FConsoleWriter.WriteLn('  Message: ' + testResult.Message);
        FConsoleWriter.WriteLn;
      end;
    end;
    FConsoleWriter.WriteLn;
  end;

  SetConsoleDefaultColor();
end;

procedure TDUnitXConsoleLogger.OnTestingStarts(const threadId, testCount, testActiveCount : Cardinal);
begin
  if FQuietMode then
  begin
    FConsoleWriter.WriteLn(Format('DUnitX - [%s] - Starting Tests.',[ExtractFileName(ParamStr(0))]));
    FConsoleWriter.WriteLn;
    exit;
  end;
end;


procedure TDUnitXConsoleLogger.OnTestMemoryLeak(const threadId: Cardinal; const Test: ITestResult);
begin
  if FQuietMode then
    FConsoleWriter.Write('M');
end;

procedure TDUnitXConsoleLogger.SetConsoleDefaultColor();
begin
  FConsoleWriter.SetColour(ccDefault);
end;

procedure TDUnitXConsoleLogger.SetConsoleErrorColor();
begin
  FConsoleWriter.SetColour(ccBrightRed, ccBlack);
end;

procedure TDUnitXConsoleLogger.SetConsolePassColor();
begin
  FConsoleWriter.SetColour(ccBrightGreen, ccBlack);
end;

procedure TDUnitXConsoleLogger.SetConsoleRunTestColor();
begin
  FConsoleWriter.SetColour(ccBrightAqua, ccBlack);
end;

procedure TDUnitXConsoleLogger.SetConsoleSetupTestColor();
begin
  FConsoleWriter.SetColour(ccBrightPurple, ccBlack);
end;

procedure TDUnitXConsoleLogger.SetConsoleSummaryColor();
begin
  FConsoleWriter.SetColour(ccBrightWhite, ccBlack);
end;

procedure TDUnitXConsoleLogger.SetConsoleWarningColor();
begin
  FConsoleWriter.SetColour(ccBrightYellow, ccBlack);
end;

end.

