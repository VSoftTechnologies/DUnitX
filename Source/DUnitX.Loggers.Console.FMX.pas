{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2015 Vincent Parrett & Contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
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

unit DUnitX.Loggers.Console.FMX;

interface

{$I DUnitX.inc}

uses
{$IFDEF USE_NS}
  System.Classes,
  System.SysUtils,
  System.UITypes,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Memo,
  FMX.Objects,
  FMX.Controls.Presentation,
{$ELSE}
  Classes,
  SysUtils,
  UITypes,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Memo,
  FMX.Objects,
{$ENDIF}
  DUnitX.TestFramework,
  DUnitX.ResStrs;

type
  /// <summary>
  ///   Console-style ITestLogger that appends lines to a TStrings (typically a TMemo.Lines).
  ///   Mirrors TDUnitXConsoleLogger message text. Never writes to a real console.
  /// </summary>
  TDUnitXFMXConsoleLogger = class(TInterfacedObject, ITestLogger)
  private
    FLines : TStrings;
    FQuietMode : boolean;
    FIndent : Integer;
    FPendingLine : string;
    FHasPending : boolean;
    procedure Indent(const value : Integer = 1);
    procedure Outdent(const value : Integer = 1);
    procedure Write(const s : string);
    procedure WriteLn; overload;
    procedure WriteLn(const s : string); overload;
  protected
    procedure OnTestingStarts(const threadId : TThreadID; testCount, testActiveCount : Cardinal);
    procedure OnStartTestFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
    procedure OnSetupFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
    procedure OnEndSetupFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
    procedure OnBeginTest(const threadId : TThreadID; const Test : ITestInfo);
    procedure OnSetupTest(const threadId : TThreadID; const Test : ITestInfo);
    procedure OnEndSetupTest(const threadId : TThreadID; const Test : ITestInfo);
    procedure OnExecuteTest(const threadId : TThreadID; const Test : ITestInfo);
    procedure OnTestMemoryLeak(const threadId : TThreadID; const Test : ITestResult);
    procedure OnTestIgnored(const threadId : TThreadID; const AIgnored : ITestResult);
    procedure OnTestError(const threadId : TThreadID; const Error : ITestError);
    procedure OnTestFailure(const threadId : TThreadID; const Failure : ITestError);
    procedure OnTestSuccess(const threadId : TThreadID; const Test : ITestResult);
    procedure OnLog(const logType : TLogLevel; const msg : string);
    procedure OnTeardownTest(const threadId : TThreadID; const Test : ITestInfo);
    procedure OnEndTeardownTest(const threadId : TThreadID; const Test : ITestInfo);
    procedure OnEndTest(const threadId : TThreadID; const Test : ITestResult);
    procedure OnTearDownFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
    procedure OnEndTearDownFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
    procedure OnEndTestFixture(const threadId : TThreadID; const results : IFixtureResult);
    procedure OnTestingEnds(const RunResults : IRunResults);
  public
    constructor Create(const ALines : TStrings; const quietMode : boolean = False);
  end;

  /// <summary>
  ///   Code-created FMX host (no .fmx). Memo + status label; auto-runs the suite on first show.
  /// </summary>
  TDUnitXFMXConsoleHost = class(TForm)
  private
    FStatus : TLabel;
    FRunAgain : TRectangle;
    FRunAgainLabel : TText;
    FMemo : TMemo;
    FHasAutoRun : Boolean;
    FRunning : Boolean;
    procedure ApplyConsoleText(const ATextSettings : TTextSettings);
    procedure BuildUI;
    procedure HandleMemoApplyStyleLookup(Sender : TObject);
    procedure HandleShow(Sender : TObject);
    procedure HandleRunAgainClick(Sender : TObject);
    procedure RunTests;
  public
    constructor CreateNew(AOwner : TComponent; Dummy : NativeInt = 0); override;
  end;

/// <summary>
///   FMX entry point analogous to DUnitX.Loggers.GUI.VCL.Run.
///   Creates a code-built host form and runs the registered suite.
/// </summary>
procedure Run;

implementation

{ TDUnitXFMXConsoleLogger }

constructor TDUnitXFMXConsoleLogger.Create(const ALines : TStrings; const quietMode : boolean);
begin
  inherited Create;
  if ALines = nil then
    raise Exception.Create('ALines must not be nil');
  FLines := ALines;
  FQuietMode := quietMode;
  FIndent := 0;
  FHasPending := False;
end;

procedure TDUnitXFMXConsoleLogger.Indent(const value : Integer);
begin
  Inc(FIndent, value);
  if FIndent < 0 then
    FIndent := 0;
end;

procedure TDUnitXFMXConsoleLogger.Outdent(const value : Integer);
begin
  Dec(FIndent, value);
  if FIndent < 0 then
    FIndent := 0;
end;

procedure TDUnitXFMXConsoleLogger.Write(const s : string);
begin
  if not FHasPending then
  begin
    FPendingLine := StringOfChar(' ', FIndent) + s;
    FHasPending := True;
  end
  else
    FPendingLine := FPendingLine + s;
end;

procedure TDUnitXFMXConsoleLogger.WriteLn;
begin
  WriteLn('');
end;

procedure TDUnitXFMXConsoleLogger.WriteLn(const s : string);
begin
  if FHasPending then
  begin
    FLines.Add(FPendingLine + s);
    FPendingLine := '';
    FHasPending := False;
  end
  else
    FLines.Add(StringOfChar(' ', FIndent) + s);
end;

procedure TDUnitXFMXConsoleLogger.OnTestingStarts(const threadId : TThreadID; testCount, testActiveCount : Cardinal);
begin
  if FQuietMode then
  begin
    WriteLn(Format(SStartingTests, [ExtractFileName(ParamStr(0))]));
    WriteLn;
  end;
end;

procedure TDUnitXFMXConsoleLogger.OnStartTestFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
begin
  if FQuietMode then
    Exit;

  Indent(2);
  WriteLn(SFixture + fixture.FullName);
  WriteLn('-------------------------------------------------');
  Indent(1);
end;

procedure TDUnitXFMXConsoleLogger.OnSetupFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
begin
  if FQuietMode then
    Exit;

  Indent(1);
  WriteLn(SRunningFixtureSetup + fixture.SetupFixtureMethodName);
end;

procedure TDUnitXFMXConsoleLogger.OnEndSetupFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
begin
  if FQuietMode then
    Exit;

  Outdent(1);
  WriteLn;
end;

procedure TDUnitXFMXConsoleLogger.OnBeginTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  if FQuietMode then
    Exit;

  Indent(1);
  WriteLn(STest + Test.FullName);
  WriteLn('-------------------------------------------------');
end;

procedure TDUnitXFMXConsoleLogger.OnSetupTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  if FQuietMode then
    Exit;

  WriteLn(SRunningSetup + Test.Name);
end;

procedure TDUnitXFMXConsoleLogger.OnEndSetupTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  if FQuietMode then
    Exit;

  WriteLn;
end;

procedure TDUnitXFMXConsoleLogger.OnExecuteTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  if FQuietMode then
  begin
    Write('.');
    Exit;
  end;

  WriteLn(SExecutingTest + Test.Name);
  WriteLn;
end;

procedure TDUnitXFMXConsoleLogger.OnTestError(const threadId : TThreadID; const Error : ITestError);
begin
  if FQuietMode then
    Write('E');
end;

procedure TDUnitXFMXConsoleLogger.OnTestFailure(const threadId : TThreadID; const Failure : ITestError);
begin
  if FQuietMode then
    Write('F');
end;

procedure TDUnitXFMXConsoleLogger.OnTestIgnored(const threadId : TThreadID; const AIgnored : ITestResult);
begin
  if FQuietMode then
    Write('I');
end;

procedure TDUnitXFMXConsoleLogger.OnTestMemoryLeak(const threadId : TThreadID; const Test : ITestResult);
begin
  if FQuietMode then
    Write('M');
end;

procedure TDUnitXFMXConsoleLogger.OnTestSuccess(const threadId : TThreadID; const Test : ITestResult);
var
  sMessage : string;
begin
  if FQuietMode then
  begin
    Write('.');
    Exit;
  end;

  Indent(2);
  if Test.Message <> '' then
    sMessage := SSuccess + ' : ' + Test.Message
  else
    sMessage := SSuccess + '.';
  WriteLn(sMessage);
  Outdent(2);
end;

procedure TDUnitXFMXConsoleLogger.OnLog(const logType : TLogLevel; const msg : string);
begin
  if FQuietMode then
    Exit;

  Indent(2);
  try
    WriteLn(msg);
  finally
    Outdent(2);
  end;
end;

procedure TDUnitXFMXConsoleLogger.OnTeardownTest(const threadId : TThreadID; const Test : ITestInfo);
begin
  if FQuietMode then
    Exit;

  WriteLn;
  Indent(1);
  WriteLn(SRunningTestTeardown + Test.Name);
  WriteLn;
  Outdent(1);
end;

procedure TDUnitXFMXConsoleLogger.OnEndTeardownTest(const threadId : TThreadID; const Test : ITestInfo);
begin
end;

procedure TDUnitXFMXConsoleLogger.OnEndTest(const threadId : TThreadID; const Test : ITestResult);
begin
  if FQuietMode then
    Exit;

  Outdent(1);
  WriteLn;
end;

procedure TDUnitXFMXConsoleLogger.OnTearDownFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
begin
  if FQuietMode then
    Exit;

  WriteLn(SRunningFixtureTeardown + fixture.TearDownFixtureMethodName);
  WriteLn;
end;

procedure TDUnitXFMXConsoleLogger.OnEndTearDownFixture(const threadId : TThreadID; const fixture : ITestFixtureInfo);
begin
end;

procedure TDUnitXFMXConsoleLogger.OnEndTestFixture(const threadId : TThreadID; const results : IFixtureResult);
begin
  if FQuietMode then
    Exit;

  Outdent(3);
  WriteLn;
end;

procedure TDUnitXFMXConsoleLogger.OnTestingEnds(const RunResults : IRunResults);
var
  testResult : ITestResult;
begin
  if FQuietMode then
  begin
    WriteLn;
    WriteLn;
  end
  else
  begin
    Outdent(1);
    WriteLn(SDoneTesting);
  end;

  WriteLn(Format(STestsFound, [RunResults.TestCount]));
  WriteLn(Format(STestsIgnored, [RunResults.IgnoredCount]));
  WriteLn(Format(STestsPassed, [RunResults.PassCount]));
  WriteLn(Format(STestsLeaked, [RunResults.MemoryLeakCount]));
  WriteLn(Format(STestsFailed, [RunResults.FailureCount]));
  WriteLn(Format(STestsErrored, [RunResults.ErrorCount]));

  if RunResults.FailureCount > 0 then
  begin
    WriteLn;
    WriteLn(SFailingTests);
    WriteLn;
    for testResult in RunResults.GetAllTestResults do
    begin
      if testResult.ResultType = TTestResultType.Failure then
      begin
        WriteLn('  ' + testResult.Test.FullName);
        WriteLn(SMessage + testResult.Message);
        WriteLn;
      end;
    end;
    WriteLn;
  end;

  if RunResults.ErrorCount > 0 then
  begin
    WriteLn;
    WriteLn(STestsWithErrors);
    WriteLn;
    for testResult in RunResults.GetAllTestResults do
    begin
      if testResult.ResultType = TTestResultType.Error then
      begin
        WriteLn('  ' + testResult.Test.FullName);
        WriteLn(SMessage + testResult.Message);
        WriteLn;
      end;
    end;
    WriteLn;
  end;

  if RunResults.MemoryLeakCount > 0 then
  begin
    WriteLn;
    WriteLn(STestsWithLeak);
    WriteLn;
    for testResult in RunResults.GetAllTestResults do
    begin
      if testResult.ResultType = TTestResultType.MemoryLeak then
      begin
        WriteLn('  ' + testResult.Test.FullName);
        WriteLn(SMessage + testResult.Message);
        WriteLn;
      end;
    end;
    WriteLn;
  end;
end;

{ TDUnitXFMXConsoleHost }

constructor TDUnitXFMXConsoleHost.CreateNew(AOwner : TComponent; Dummy : NativeInt);
begin
  inherited CreateNew(AOwner, Dummy);
  Caption := 'DUnitX';
  Width := 700;
  Height := 500;
  FHasAutoRun := False;
  FRunning := False;
  BuildUI;
  OnShow := HandleShow;
end;

function ConsoleFontFamily : string;
begin
{$IFDEF MSWINDOWS}
  Result := 'Consolas';
{$ELSE}
{$IFDEF ANDROID}
  Result := 'monospace';
{$ELSE}
{$IFDEF IOS}
  Result := 'Menlo';
{$ELSE}
  Result := 'Courier New';
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

procedure TDUnitXFMXConsoleHost.ApplyConsoleText(const ATextSettings : TTextSettings);
begin
  ATextSettings.Font.Family := ConsoleFontFamily;
  ATextSettings.Font.Size := 12;
  ATextSettings.FontColor := $FFD4D4D4;
end;

procedure TDUnitXFMXConsoleHost.HandleMemoApplyStyleLookup(Sender : TObject);
const
  cConsoleBg = $FF0C0C0C;
var
  LBackground : TFmxObject;
  LRect : TRectangle;
  i : Integer;
begin
  // Default FMX memo skin is TActiveStyleObject (bitmap), not TRectangle.
  LBackground := FMemo.FindStyleResource('background');
  if LBackground = nil then
    Exit;
  for i := 0 to LBackground.ChildrenCount - 1 do
    if LBackground.Children[i].StyleName = 'consolebg' then
      Exit;

  LRect := TRectangle.Create(LBackground);
  LRect.StyleName := 'consolebg';
  LRect.Parent := LBackground;
  LRect.Align := TAlignLayout.Contents;
  LRect.HitTest := False;
  LRect.Stroke.Kind := TBrushKind.None;
  LRect.Fill.Kind := TBrushKind.Solid;
  LRect.Fill.Color := cConsoleBg;
  LRect.SendToBack;
end;

procedure TDUnitXFMXConsoleHost.BuildUI;
begin
  // Entire UI is created in code - no .fmx (see Docs/FMX-Pseudo-Console.md).
  Fill.Kind := TBrushKind.Solid;
  Fill.Color := $FF0C0C0C;

  FStatus := TLabel.Create(Self);
  FStatus.Parent := Self;
  FStatus.Align := TAlignLayout.Top;
  FStatus.Height := 28;
  FStatus.Margins.Left := 8;
  FStatus.Margins.Right := 8;
  FStatus.StyledSettings := [];
  ApplyConsoleText(FStatus.TextSettings);
  FStatus.Text := 'Ready';

  // TLabel/TLayout: styled TText still has HitTest=True and swallows the click.
  // TRectangle + primitive TText (HitTest=False) is a real mouse target.
  FRunAgain := TRectangle.Create(Self);
  FRunAgain.Parent := Self;
  FRunAgain.Align := TAlignLayout.Top;
  FRunAgain.Height := 24;
  FRunAgain.Margins.Left := 8;
  FRunAgain.Margins.Right := 8;
  FRunAgain.Margins.Bottom := 4;
  FRunAgain.Fill.Kind := TBrushKind.Solid;
  FRunAgain.Fill.Color := $FF0C0C0C;
  FRunAgain.Stroke.Kind := TBrushKind.None;
  FRunAgain.HitTest := True;
  FRunAgain.AutoCapture := True;
  FRunAgain.Cursor := crHandPoint;
  FRunAgain.OnClick := HandleRunAgainClick;

  FRunAgainLabel := TText.Create(FRunAgain);
  FRunAgainLabel.Parent := FRunAgain;
  FRunAgainLabel.Align := TAlignLayout.Client;
  FRunAgainLabel.HitTest := False;
  FRunAgainLabel.HorzTextAlign := TTextAlign.Leading;
  FRunAgainLabel.VertTextAlign := TTextAlign.Center;
  FRunAgainLabel.TextSettings.Font.Family := ConsoleFontFamily;
  FRunAgainLabel.TextSettings.Font.Size := 12;
  FRunAgainLabel.TextSettings.FontColor := $FFD4D4D4;
  FRunAgainLabel.Text := '> Run again';

  FMemo := TMemo.Create(Self);
  FMemo.OnApplyStyleLookup := HandleMemoApplyStyleLookup;
  FMemo.Parent := Self;
  FMemo.Align := TAlignLayout.Client;
  FMemo.Margins.Left := 8;
  FMemo.Margins.Right := 8;
  FMemo.Margins.Bottom := 8;
  FMemo.ReadOnly := True;
  FMemo.WordWrap := False;
  FMemo.StyledSettings := [];
  ApplyConsoleText(FMemo.TextSettings);
end;

procedure TDUnitXFMXConsoleHost.HandleShow(Sender : TObject);
begin
  if not FHasAutoRun then
  begin
    FHasAutoRun := True;
    RunTests;
  end;
end;

procedure TDUnitXFMXConsoleHost.HandleRunAgainClick(Sender : TObject);
begin
  RunTests;
end;

procedure TDUnitXFMXConsoleHost.RunTests;
var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
begin
  if FRunning then
    Exit;
  FRunning := True;
  FRunAgain.Enabled := False;
  FRunAgain.Opacity := 0.45;
  try
    try
      FMemo.Lines.Clear;
      FStatus.Text := 'Running...';

      runner := TDUnitX.CreateRunner;
      runner.UseRTTI := True;
      runner.FailsOnNoAsserts := False;

      logger := TDUnitXFMXConsoleLogger.Create(FMemo.Lines, False);
      runner.AddLogger(logger);

      results := runner.Execute;

      if results.AllPassed then
        FStatus.Text := Format('Done - all passed (%d)', [results.PassCount])
      else
        FStatus.Text := Format('Done - failed: %d  errored: %d  ignored: %d  passed: %d',
          [results.FailureCount, results.ErrorCount, results.IgnoredCount, results.PassCount]);
    except
      on E : Exception do
      begin
        FMemo.Lines.Add(E.ClassName + ': ' + E.Message);
        FStatus.Text := 'Error - ' + E.Message;
      end;
    end;
  finally
    FRunning := False;
    FRunAgain.Enabled := True;
    FRunAgain.Opacity := 1;
  end;
end;

procedure Run;
var
  host : TDUnitXFMXConsoleHost;
begin
  TDUnitX.CheckCommandLine;

  Application.Initialize;
  host := TDUnitXFMXConsoleHost.CreateNew(Application);
  Application.MainForm := host;
  host.Show;
  Application.Run;
end;

end.
