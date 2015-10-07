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

unit DUnitX.Loggers.Console;

interface

{$I DUnitX.inc}

uses
  DUnitX.ConsoleWriter.Base,
  DUnitX.TestFramework;

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
    procedure OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount: Cardinal);

    procedure OnStartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);

    procedure OnSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnEndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);

    procedure OnBeginTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure OnSetupTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure OnExecuteTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure OnTestMemoryLeak(const threadId : TThreadID; const Test: ITestResult);
    procedure OnTestIgnored(const threadId: TThreadID; const  AIgnored: ITestResult);
    procedure OnTestError(const threadId: TThreadID; const Error: ITestError);
    procedure OnTestFailure(const threadId: TThreadID; const Failure: ITestError);
    procedure OnTestSuccess(const threadId: TThreadID; const Test: ITestResult);
    procedure OnLog(const logType : TLogLevel; const msg : string);

    procedure OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndTeardownTest(const threadId: TThreadID; const Test: ITestInfo);

    procedure OnEndTest(const threadId: TThreadID; const Test: ITestResult);

    procedure OnTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnEndTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);

    procedure OnEndTestFixture(const threadId: TThreadID; const results: IFixtureResult);

    procedure OnTestingEnds(const RunResults: IRunResults);
  public
    constructor Create(const quietMode : boolean = false);
    destructor Destroy;override;
  end;

implementation

uses
  DUnitX.ResStrs,
  {$IFDEF USE_NS}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  DUnitX.AutoDetect.Console,
  DUnitX.IoC;

{ TDUnitXConsoleLogger }

constructor TDUnitXConsoleLogger.Create(const quietMode : boolean = false);
begin
  FQuietMode := quietMode;
  FConsoleWriter := TDUnitXIoC.DefaultContainer.Resolve<IDUnitXConsoleWriter>();
  if FConsoleWriter = nil then
    raise Exception.Create(SNoConsoleWriterClassRegistered);
end;

destructor TDUnitXConsoleLogger.Destroy;
begin

  inherited;
end;

procedure TDUnitXConsoleLogger.OnEndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.Outdent(1);
  FConsoleWriter.WriteLn;
end;

procedure TDUnitXConsoleLogger.OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  if FQuietMode then
    exit;

//  FConsoleWriter.WriteLn;
//  FConsoleWriter.Outdent(1);
  FConsoleWriter.WriteLn;
end;

procedure TDUnitXConsoleLogger.OnEndTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXConsoleLogger.OnEndTeardownTest(const threadId: TThreadID; const  Test: ITestInfo);
begin

end;

procedure TDUnitXConsoleLogger.OnEndTest(const threadId: TThreadID; const  Test: ITestResult);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.Outdent(1);
  FConsoleWriter.WriteLn;
end;

procedure TDUnitXConsoleLogger.OnEndTestFixture(const threadId: TThreadID; const results: IFixtureResult);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.Outdent(3);
  FConsoleWriter.WriteLn;
end;

procedure TDUnitXConsoleLogger.OnExecuteTest(const threadId: TThreadID; const  Test: ITestInfo);
begin
  if FQuietMode then
  begin
    FConsoleWriter.Write('.');
    exit;
  end;


  //SetConsoleRunTestColor();
  //FConsoleWriter.Indent(1);
  FConsoleWriter.WriteLn(SExecutingTest + Test.Name);
  FConsoleWriter.WriteLn;
  //SetConsoleDefaultColor();
end;

procedure TDUnitXConsoleLogger.OnTestError(const threadId: TThreadID; const  Error: ITestError);
begin
  if FQuietMode then
  begin
    FConsoleWriter.Write('E');
    exit;
  end;
end;

procedure TDUnitXConsoleLogger.OnTestFailure(const threadId: TThreadID; const  Failure: ITestError);
begin
  if FQuietMode then
  begin
    FConsoleWriter.Write('F');
    exit;
  end;
end;

procedure TDUnitXConsoleLogger.OnTestIgnored(const threadId: TThreadID; const AIgnored: ITestResult);
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

procedure TDUnitXConsoleLogger.OnSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.Indent(1);
  FConsoleWriter.WriteLn(SRunningFixtureSetup + fixture.SetupFixtureMethodName);

end;

procedure TDUnitXConsoleLogger.OnSetupTest(const threadId: TThreadID; const  Test: ITestInfo);
begin
  if FQuietMode then
    exit;

  SetConsoleSetupTestColor();
  FConsoleWriter.WriteLn(SRunningSetup + Test.Name);
end;

procedure TDUnitXConsoleLogger.OnBeginTest(const threadId: TThreadID; const  Test: ITestInfo);
begin
  if FQuietMode then
    exit;

  SetConsoleRunTestColor();
  FConsoleWriter.Indent(1);
  FConsoleWriter.WriteLn(STest +  Test.FullName);
  FConsoleWriter.WriteLn('-------------------------------------------------');
  SetConsoleDefaultColor();
end;

procedure TDUnitXConsoleLogger.OnStartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  if FQuietMode then
    exit;

  SetConsoleWarningColor();
  FConsoleWriter.Indent(2);
  FConsoleWriter.WriteLn(SFixture + fixture.FullName);
  FConsoleWriter.WriteLn('-------------------------------------------------');
  FConsoleWriter.Indent(1);
  SetConsoleDefaultColor();
end;


procedure TDUnitXConsoleLogger.OnTestSuccess(const threadId: TThreadID; const Test: ITestResult);
begin
  if FQuietMode then
  begin
    FConsoleWriter.Write('.');
    exit;
  end;
  FConsoleWriter.Indent(2);
  SetConsolePassColor;
  FConsoleWriter.WriteLn(SSuccess);
  SetConsoleDefaultColor;
  FConsoleWriter.Outdent(2);
end;

procedure TDUnitXConsoleLogger.OnTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.WriteLn(SRunningFixtureTeardown + fixture.TearDownFixtureMethodName);
  FConsoleWriter.WriteLn;
end;

procedure TDUnitXConsoleLogger.OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  if FQuietMode then
    exit;

  FConsoleWriter.WriteLn;
  FConsoleWriter.Indent(1);
  FConsoleWriter.WriteLn(SRunningTestTeardown + test.Name);
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
    FConsoleWriter.WriteLn(SDoneTesting);
  end;

  SetConsoleSummaryColor();
  FConsoleWriter.WriteLn(Format(STestsFound, [RunResults.TestCount]));

  if RunResults.IgnoredCount > 0 then
    SetConsoleWarningColor()
  else
    SetConsoleDefaultColor();
  FConsoleWriter.WriteLn(Format(STestsIgnored, [RunResults.IgnoredCount]));


  if RunResults.PassCount > 0 then
    SetConsolePassColor()
  else
    SetConsoleDefaultColor();
  FConsoleWriter.WriteLn(Format(STestsPassed, [RunResults.PassCount]));

  if RunResults.MemoryLeakCount > 0 then
    SetConsoleWarningColor()
  else
    SetConsoleDefaultColor();
  FConsoleWriter.WriteLn(Format(STestsLeaked, [RunResults.MemoryLeakCount]));

  if RunResults.FailureCount > 0 then
    SetConsoleErrorColor()
  else
    SetConsoleDefaultColor();
  FConsoleWriter.WriteLn(Format(STestsFailed, [RunResults.FailureCount]));

  if RunResults.ErrorCount > 0 then
    SetConsoleErrorColor()
  else
    SetConsoleDefaultColor();
  FConsoleWriter.WriteLn(Format(STestsErrored,[RunResults.ErrorCount]));


  if RunResults.FailureCount > 0  then
  begin
    SetConsoleErrorColor();
    FConsoleWriter.WriteLn;
    FConsoleWriter.WriteLn(SFailingTests);
    FConsoleWriter.WriteLn;
    SetConsoleDefaultColor();


    for testResult in RunResults.GetAllTestResults do
    begin
      if testResult.ResultType = TTestResultType.Failure then
      begin
        SetConsoleErrorColor();
        FConsoleWriter.WriteLn('  ' + testResult.Test.FullName);
        SetConsoleDefaultColor();
        FConsoleWriter.WriteLn(SMessage + testResult.Message);
        FConsoleWriter.WriteLn;
      end;
    end;
    FConsoleWriter.WriteLn;
  end;

  if RunResults.ErrorCount > 0  then
  begin
    SetConsoleErrorColor();
    FConsoleWriter.WriteLn;
    FConsoleWriter.WriteLn(STestsWithErrors);
    FConsoleWriter.WriteLn;
    SetConsoleDefaultColor();

    for testResult in RunResults.GetAllTestResults do
    begin
      if testResult.ResultType = TTestResultType.Error then
      begin
        SetConsoleErrorColor();
        FConsoleWriter.WriteLn('  ' + testResult.Test.FullName);
        SetConsoleDefaultColor();
        FConsoleWriter.WriteLn(SMessage + testResult.Message);
        FConsoleWriter.WriteLn;
      end;
    end;
    FConsoleWriter.WriteLn;
  end;

  if RunResults.MemoryLeakCount > 0  then
  begin
    SetConsoleWarningColor();
    FConsoleWriter.WriteLn;
    FConsoleWriter.WriteLn(STestsWithLeak);
    FConsoleWriter.WriteLn;
    SetConsoleDefaultColor();

    for testResult in RunResults.GetAllTestResults do
    begin
      if testResult.ResultType = TTestResultType.MemoryLeak then
      begin
        SetConsoleWarningColor();
        FConsoleWriter.WriteLn('  ' + testResult.Test.FullName);
        SetConsoleDefaultColor();
        FConsoleWriter.WriteLn(SMessage + testResult.Message);
        FConsoleWriter.WriteLn;
      end;
    end;
    FConsoleWriter.WriteLn;
  end;

  SetConsoleDefaultColor();
end;

procedure TDUnitXConsoleLogger.OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount : Cardinal);
begin
  if FQuietMode then
  begin
    FConsoleWriter.WriteLn(Format(SStartingTests, [ExtractFileName(ParamStr(0))]));
    FConsoleWriter.WriteLn;
    exit;
  end;
end;


procedure TDUnitXConsoleLogger.OnTestMemoryLeak(const threadId: TThreadID; const Test: ITestResult);
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

