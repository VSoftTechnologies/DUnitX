{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2016 Vincent Parrett                              }
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

unit DUnitX.Loggers.GUI.VCL.RichEdit;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  Vcl.ComCtrls,
  Vcl.Graphics,
  System.Generics.Collections,
  {$ELSE}
  ComCtrls,
  Graphics,
  Generics.Collections,
  {$ENDIF}
  DUnitX.ConsoleWriter.Base,
  DUnitX.TestFramework;

type
  TTestBookmarkList = TDictionary<string,integer>;

  TDUnitXGUIVCLRichEditLogger = class(TInterfacedObject, ITestLogger)
  private
    FRichEdit: TRichEdit;
    FIndent: integer;
    FCurrColor: TColor;
    FTestBookmarkList: TTestBookmarkList;
    procedure Write(const Msg: string);
    procedure WriteBlankLine;
    procedure SetColor(const Color: TColor);
    procedure Indent(const Value: integer = 1);
    procedure Outdent(const Value: integer = 1);
  strict protected
    procedure SetConsoleDefaultColor; virtual;
    procedure SetConsoleErrorColor; virtual;
    procedure SetConsolePassColor; virtual;
    procedure SetConsoleRunTestColor; virtual;
    procedure SetConsoleSetupTestColor; virtual;
    procedure SetConsoleSummaryColor; virtual;
    procedure SetConsoleWarningColor; virtual;
  protected
    procedure OnBeginTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnEndTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndTest(const threadId: TThreadID; const Test: ITestResult);
    procedure OnEndTestFixture(const threadId: TThreadID; const results: IFixtureResult);
    procedure OnExecuteTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnLog(const logType : TLogLevel; const msg : string);
    procedure OnSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnSetupTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnStartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnTestError(const threadId: TThreadID; const Error: ITestError);
    procedure OnTestFailure(const threadId: TThreadID; const Failure: ITestError);
    procedure OnTestIgnored(const threadId: TThreadID; const AIgnored: ITestResult);
    procedure OnTestingEnds(const RunResults: IRunResults);
    procedure OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount: Cardinal);
    procedure OnTestMemoryLeak(const threadId : TThreadID; const Test: ITestResult);
    procedure OnTestSuccess(const threadId: TThreadID; const Test: ITestResult);
  public
    constructor Create(const RichEdit: TRichEdit; const TestBookmarkList: TTestBookmarkList);
  end;

implementation

uses
  {$IFDEF USE_NS}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  DUnitX.ResStrs;

constructor TDUnitXGUIVCLRichEditLogger.Create(const RichEdit: TRichEdit; const TestBookmarkList: TTestBookmarkList);
begin
  FRichEdit := RichEdit;
  FTestBookmarkList := TestBookmarkList;

  FIndent := 0;
end;

procedure TDUnitXGUIVCLRichEditLogger.Indent(const Value: integer);
begin
  Inc(FIndent, Value);
end;

procedure TDUnitXGUIVCLRichEditLogger.OnBeginTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  FTestBookmarkList.Add(Test.FullName, FRichEdit.Lines.Count);

  SetConsoleRunTestColor();
  Indent(1);
  Write(STest +  Test.FullName);
  Write('-------------------------------------------------');
  SetConsoleDefaultColor();
end;

procedure TDUnitXGUIVCLRichEditLogger.OnEndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  Outdent(1);
end;

procedure TDUnitXGUIVCLRichEditLogger.OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  //Empty
end;

procedure TDUnitXGUIVCLRichEditLogger.OnEndTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  //Empty
end;

procedure TDUnitXGUIVCLRichEditLogger.OnEndTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  //Empty
end;

procedure TDUnitXGUIVCLRichEditLogger.OnEndTest(const threadId: TThreadID; const Test: ITestResult);
begin
  Outdent(1);
end;

procedure TDUnitXGUIVCLRichEditLogger.OnEndTestFixture(const threadId: TThreadID; const results: IFixtureResult);
begin
  Outdent(3);
  WriteBlankLine;
end;

procedure TDUnitXGUIVCLRichEditLogger.OnExecuteTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  Write(SExecutingTest + Test.Name);
  WriteBlankLine;
end;

procedure TDUnitXGUIVCLRichEditLogger.OnLog(const logType: TLogLevel; const msg: string);
begin
  Indent(2);
  try
    case logType  of
      TLogLevel.Information: SetConsoleDefaultColor();
      TLogLevel.Warning: SetConsoleWarningColor();
      TLogLevel.Error: SetConsoleErrorColor();
    end;

    Write(msg);
  finally
    Outdent(2);
    SetConsoleDefaultColor();
  end;
end;

procedure TDUnitXGUIVCLRichEditLogger.OnSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  Indent(1);
  Write(SRunningFixtureSetup + fixture.SetupFixtureMethodName);
end;

procedure TDUnitXGUIVCLRichEditLogger.OnSetupTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  SetConsoleSetupTestColor();
  Write(SRunningSetup + Test.Name);
end;

procedure TDUnitXGUIVCLRichEditLogger.OnStartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  SetConsoleWarningColor();
  Indent(2);
  Write(SFixture + fixture.FullName);
  Write('-------------------------------------------------');
  Indent(1);
  SetConsoleDefaultColor();
end;

procedure TDUnitXGUIVCLRichEditLogger.OnTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  Write(SRunningFixtureTeardown + fixture.TearDownFixtureMethodName);
  WriteBlankLine;
end;

procedure TDUnitXGUIVCLRichEditLogger.OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  WriteBlankLine;
  Indent(1);
  Write(SRunningTestTeardown + test.Name);
  WriteBlankLine;
  Outdent(1);
end;

procedure TDUnitXGUIVCLRichEditLogger.OnTestError(const threadId: TThreadID; const Error: ITestError);
begin
  //Empty
end;

procedure TDUnitXGUIVCLRichEditLogger.OnTestFailure(const threadId: TThreadID; const Failure: ITestError);
begin
  //Empty
end;

procedure TDUnitXGUIVCLRichEditLogger.OnTestIgnored(const threadId: TThreadID; const AIgnored: ITestResult);
begin
  //Empty
end;

procedure TDUnitXGUIVCLRichEditLogger.OnTestingEnds(const RunResults: IRunResults);
var
  TestResult: ITestResult;
begin
  OutDent(1);
  Write(SDoneTesting);

  SetConsoleSummaryColor;
  Write(Format(STestsFound, [RunResults.TestCount]));

  if RunResults.IgnoredCount > 0 then
    SetConsoleWarningColor()
  else
    SetConsoleDefaultColor;
  Write(Format(STestsIgnored, [RunResults.IgnoredCount]));

  if RunResults.PassCount > 0 then
    SetConsolePassColor()
  else
    SetConsoleDefaultColor;
  Write(Format(STestsPassed, [RunResults.PassCount]));

  if RunResults.MemoryLeakCount > 0 then
    SetConsoleWarningColor()
  else
    SetConsoleDefaultColor;
  Write(Format(STestsLeaked, [RunResults.MemoryLeakCount]));

  if RunResults.FailureCount > 0 then
    SetConsoleErrorColor()
  else
    SetConsoleDefaultColor;
  Write(Format(STestsFailed, [RunResults.FailureCount]));

  if RunResults.ErrorCount > 0 then
    SetConsoleErrorColor()
  else
    SetConsoleDefaultColor;
  Write(Format(STestsErrored,[RunResults.ErrorCount]));

  if RunResults.FailureCount > 0  then
  begin
    SetConsoleErrorColor;
    WriteBlankLine;
    Write(SFailingTests);
    WriteBlankLine;
    SetConsoleDefaultColor;

    for TestResult in RunResults.GetAllTestResults do
    begin
      if TestResult.ResultType = TTestResultType.Failure then
      begin
        SetConsoleErrorColor;
        Write('  ' + TestResult.Test.FullName);
        SetConsoleDefaultColor;
        Write(SMessage + TestResult.Message);
        WriteBlankLine;
      end;
    end;
    WriteBlankLine;
  end;

  if RunResults.ErrorCount > 0  then
  begin
    SetConsoleErrorColor;
    WriteBlankLine;
    Write(STestsWithErrors);
    WriteBlankLine;
    SetConsoleDefaultColor;

    for TestResult in RunResults.GetAllTestResults do
    begin
      if TestResult.ResultType = TTestResultType.Error then
      begin
        SetConsoleErrorColor;
        Write('  ' + TestResult.Test.FullName);
        SetConsoleDefaultColor;
        Write(SMessage + TestResult.Message);
        WriteBlankLine;
      end;
    end;
    WriteBlankLine;
  end;

  if RunResults.MemoryLeakCount > 0  then
  begin
    SetConsoleWarningColor;
    WriteBlankLine;
    Write(STestsWithLeak);
    WriteBlankLine;
    SetConsoleDefaultColor;

    for TestResult in RunResults.GetAllTestResults do
    begin
      if TestResult.ResultType = TTestResultType.MemoryLeak then
      begin
        SetConsoleWarningColor;
        Write('  ' + TestResult.Test.FullName);
        SetConsoleDefaultColor;
        Write(SMessage + TestResult.Message);
        WriteBlankLine;
      end;
    end;
    WriteBlankLine;
  end;

  SetConsoleDefaultColor;
end;

procedure TDUnitXGUIVCLRichEditLogger.OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount : Cardinal);
begin
  FRichEdit.Clear;
  FTestBookmarkList.Clear;

  SetConsoleDefaultColor;
  Write(Format(SStartingTests, [ExtractFileName(ParamStr(0))]));
  WriteBlankLine;
end;

procedure TDUnitXGUIVCLRichEditLogger.OnTestMemoryLeak(const threadId: TThreadID; const Test: ITestResult);
begin
  //Empty
end;

procedure TDUnitXGUIVCLRichEditLogger.OnTestSuccess(const threadId: TThreadID; const Test: ITestResult);
begin
  Indent(2);
  SetConsolePassColor;
  Write(SSuccess);
  SetConsoleDefaultColor;
  Outdent(2);
end;

procedure TDUnitXGUIVCLRichEditLogger.Outdent(const Value: integer);
begin
  Dec(FIndent, Value);
end;

procedure TDUnitXGUIVCLRichEditLogger.SetColor(const Color: TColor);
begin
  FCurrColor := Color;
end;

procedure TDUnitXGUIVCLRichEditLogger.SetConsoleDefaultColor;
begin
  SetColor(clBlack);
end;

procedure TDUnitXGUIVCLRichEditLogger.SetConsoleErrorColor;
begin
  SetColor(clRed);
end;

procedure TDUnitXGUIVCLRichEditLogger.SetConsolePassColor;
begin
  SetColor(clGreen);
end;

procedure TDUnitXGUIVCLRichEditLogger.SetConsoleRunTestColor;
begin
  SetColor(clNavy);
end;

procedure TDUnitXGUIVCLRichEditLogger.SetConsoleSetupTestColor;
begin
  SetColor(clPurple);
end;

procedure TDUnitXGUIVCLRichEditLogger.SetConsoleSummaryColor;
begin
  SetColor(clBlack);
end;

procedure TDUnitXGUIVCLRichEditLogger.SetConsoleWarningColor;
begin
  SetColor(clOlive);
end;

procedure TDUnitXGUIVCLRichEditLogger.Write(const Msg: string);
begin
  FRichEdit.SelAttributes.Color := FCurrColor;
  FRichEdit.SelText := StringOfChar(' ', FIndent) + Msg + #13#10;
end;

procedure TDUnitXGUIVCLRichEditLogger.WriteBlankLine;
begin
  FRichEdit.SelText := #13#10;
end;

end.
