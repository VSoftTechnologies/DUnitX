{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2017 Vincent Parrett                              }
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

unit DUnitX.Loggers.GUI.VCL;

interface

{$I DUnitX.inc}

// Be aware that editing this in the higher versions of RAD Studio (10 Seattle for sure)
// results in the form designer continuously adding units to the uses that are
// already there.  I have been unable to find a workaround short of editing the
// file outside of the IDE...  The units are: System.ImageList and System.Actions

uses
{$IFDEF USE_NS}
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ActnList,
  Vcl.StdActns,
  Vcl.ActnCtrls,
  Vcl.ToolWin,
  Vcl.ActnMan,
  Vcl.ActnMenus,
  Vcl.ImgList,
  Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.Menus,
  Vcl.ActnPopup,
  Vcl.XPMan,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.IniFiles,
  System.Actions, //IDE keeps adding this at the end even if it's in the infdef'd section. make sure it's not there when committing!
{$ELSE}
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ActnList,
  StdActns,
  ActnCtrls,
  ToolWin,
  ActnMan,
  ActnMenus,
  ImgList,
  PlatformDefaultStyleActnCtrls,
  ExtCtrls,
  ComCtrls,
  StdCtrls,
  Menus,
  ActnPopup,
  XPMan,
  Generics.Defaults,
  Generics.Collections,
  IniFiles,
{$ENDIF}
  DUnitX.TestFrameWork,
  DUnitX.Extensibility,
  DUnitX.InternalInterfaces,
  DUnitX.ResStrs,
  DUnitX.ComparableFormat,
  DUnitX.ComparableFormat.Xml,
  DUnitX.ComparableFormat.Csv,
  DUnitX.Loggers.GUI.VCL.RichEdit;

const
  WM_LOAD_TESTS = WM_USER + 123;
  WM_TESTPOPUP = WM_USER + 124;
  WM_RESULTPOPUP = WM_USER + 125;

type
  TTestNode = class(TTreeNode)
  private
    FIsFixtureNode: boolean;
  public
    property IsFixtureNode: boolean read FIsFixtureNode;
    procedure InvertState;
    class function ToWeight(const ImageIndex: integer): integer;
    class function ToImageIndex(const Weight: integer): integer;
  end;

  TResultNode = class(TTreeNode)
  public
    TestNode: TTreeNode;
    Messages: string;
    LogMessages: TLogMessageArray;
    IsComparable: boolean;
    Expected: string;
    Actual: string;
    Format: TDUnitXComparableFormatClass;

    procedure AfterConstruction; override;
  end;

  TGUIVCLTestRunner = class(TForm, ITestLogger)
    ActionManager: TActionManager;
    ActionImages: TImageList;
    actRunSelected: TAction;
    actRun: TAction;
    pnlFilter: TPanel;
    lblFilter: TLabel;
    StateImages: TImageList;
    ResultImages: TImageList;
    pnlClient: TPanel;
    tbrTests: TActionToolBar;
    actTestsSelectAll: TAction;
    actTestsUnselectAll: TAction;
    splMain: TSplitter;
    pnlTests: TPanel;
    tvwTests: TTreeView;
    pnlResults: TPanel;
    tbrResults: TActionToolBar;
    actCompare: TAction;
    pgeResults: TPageControl;
    tabStructured: TTabSheet;
    tvwResults: TTreeView;
    stsMain: TStatusBar;
    popResults: TPopupActionBar;
    itmComparePop: TMenuItem;
    popTests: TPopupActionBar;
    RunAll1: TMenuItem;
    RunAll2: TMenuItem;
    actTestsInvert: TAction;
    pnlTestsTop: TPanel;
    lblTestsHeader: TLabel;
    pnlResultsTop: TPanel;
    lblResultsHeader: TLabel;
    actResultsExpandAll: TAction;
    actResultsCollapseAll: TAction;
    tabText: TTabSheet;
    rchText: TRichEdit;
    pnlText: TPanel;
    actFindInText: TAction;
    itmFindInText: TMenuItem;
    N1: TMenuItem;
    actFindInStructured: TAction;
    itmFindInStructured: TMenuItem;
    imgTests: TImage;
    imgResults: TImage;
    edtFilter: TButtonedEdit;
    actCopyActual: TAction;
    actCopyExpected: TAction;
    N2: TMenuItem;
    itmCopyExpected: TMenuItem;
    itmCopyActual: TMenuItem;
    pgResults: TProgressBar;
    txtStatus: TLabel;
    pnlSummary: TPanel;
    lblIgnored: TLabel;
    lblPassed: TLabel;
    txtPassed: TLabel;
    txtIgnored: TLabel;
    lblFailed: TLabel;
    txtFailed: TLabel;
    lblErrored: TLabel;
    txtErrored: TLabel;
    lblMemLeak: TLabel;
    txtMemLeak: TLabel;
    procedure actTestsUnselectAllExecute(Sender: TObject);
    procedure actCompareExecute(Sender: TObject);
    procedure actCopyActualExecute(Sender: TObject);
    procedure actCopyExpectedExecute(Sender: TObject);
    procedure actResultsCollapseAllExecute(Sender: TObject);
    procedure actResultsExpandAllExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actRunSelectedExecute(Sender: TObject);
    procedure actFindInTextExecute(Sender: TObject);
    procedure actFindInStructuredExecute(Sender: TObject);
    procedure actTestsInvertExecute(Sender: TObject);
    procedure actTestsSelectAllExecute(Sender: TObject);
    procedure edtFilterRightButtonClick(Sender: TObject);
    procedure edtFilterKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure tvwTestsCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure tvwTestsKeyPress(Sender: TObject; var Key: Char);
    procedure tvwTestsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tvwResultsChange(Sender: TObject; Node: TTreeNode);
    procedure tvwResultsCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure tvwResultsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvwTestsCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvwTestsChange(Sender: TObject; Node: TTreeNode);
  private
    FFixtureList: ITestFixtureList;
    FLastResult: IRunResults;
    FTestBookmarkList: TTestBookmarkList;
    FRootNodes : array[TTestResultType] of TTreeNode;
    FRunning : boolean;
    FFixtureCount: integer;

    procedure LoadTests;
    procedure BuildTestTreeNode(const FixtureList: ITestFixtureList;
      const ParentNode: TTreeNode);
    procedure ClearResults;
    procedure RunExecute;
    procedure Invert;
    procedure SelectAll;
    procedure UnselectAll;
    function NeedRunner: ITestRunner;
    function GetNodeByTestFullName(const TestFullName: string): TTreeNode;
    procedure SetNodeTestResult(const Node: TTreeNode; const ImageIndex: integer);
    procedure ProcessTestResult(const Test: ITestResult);
    procedure ProcessMessagesForNode(const ResultNode: TResultNode; const Test: ITestResult);
    procedure UpdateFormActions;
    function GetSelectedResultsNode: TResultNode;
    function GetComparableResultsNode: TResultNode;
    function GetRootNode(const nodeType : TTestResultType): TTreeNode;
    procedure UpdateGUIForComplete(const RunResults: IRunResults);
    procedure UpdateRootNodeCaption(const nodeType : TTestResultType);
    procedure UpdateGUIForRun(const TestCount: integer);
    procedure UpdateGUIForProgress(const CurrTestCount: integer; const CurrTestName: string);
  protected
    procedure OnBeginTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnEndTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnEndTest(const threadId: TThreadID; const Test: ITestResult);
    procedure OnEndTestFixture(const threadId: TThreadID; const results: IFixtureResult);
    procedure OnExecuteTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnLog(const logType: TLogLevel; const msg: string);
    procedure OnSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnSetupTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnStartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
    procedure OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
    procedure OnTestError(const threadId: TThreadID; const Error: ITestError);
    procedure OnTestFailure(const threadId: TThreadID; const Failure: ITestError);
    procedure OnTestSuccess(const threadId: TThreadID; const Test: ITestResult);
    procedure OnTestMemoryLeak(const threadId: TThreadID; const Test: ITestResult);
    procedure OnTestIgnored(const threadId: TThreadID; const Ignored: ITestResult);
    procedure OnTestingEnds(const RunResults: IRunResults);
    procedure OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount: Cardinal);

    procedure WMTestPopUp(var message : TMessage); message WM_TESTPOPUP;
    procedure WMResultPopUp(var message : TMessage); message WM_RESULTPOPUP;

    procedure WMLoadTests(var message : TMessage); message WM_LOAD_TESTS;
    procedure Loaded; override;
  end;

var
  GUIVCLTestRunner: TGUIVCLTestRunner;

procedure Run;

implementation

{$R *.dfm}

uses
  {$IFDEF USE_NS}
  System.IOUtils,
  System.StrUtils,
  System.Math,
  Vcl.Clipbrd,
  {$ELSE}
  IOUtils,
  StrUtils,
  Math,
  Clipbrd,
  {$ENDIF}
  DUnitX.Filters,
  DUnitX.Exceptions;

const
  cTestNodeStateUnchecked = 1;
  cTestNodeStateChecked = 2;
  cTestNodeStatePartialChecked = 3;

  cTestResultImageMap: array[TTestResultType] of integer = (1, 2, 2, 0, 2, 3);

  cLogLevelImageMap: array[TLogLevel] of integer = (6, 7, 8);

  cRootNodeStrings : array[TTestResultType] of string = (STestsPassed,STestsFailed,STestsErrored,STestsIgnored,STestsWithLeak,STestsWarning);

  cTestSetup = 'TestSetup';

type
  //can't return "array of string" on function (at least in XE)
  TStringArray = array of string;

  PathUtils = record
  public
    class function FindBeyondCompare: string; static;
    class function IsWow64: boolean; static;
    class function GetProgramFilesX64: string; static;
    class function GetProgramFilesX86: string; static;
    class function GetProgramFiles: TStringArray; static;
    class function MakeTempFile(const Contents: string; const FileExtNoDot: string = ''): string; static;
    class function CreateProcessAndWait(const CmdLine: string): boolean; static;
    class procedure Compare(const ExpFile, ActFile: string); static;
  end;

  TTreeViewIterateProc = reference to procedure(const Node: TTreeNode; var Stop: boolean);

  TTreeViewHelper = class helper for TTreeView
  public
    function IterateAll(const Proc: TTreeViewIterateProc): boolean;
  end;

  TTreeNodeHelper = class helper for TTreeNode
  public
    function IterateAllChilds(const Proc: TTreeViewIterateProc): boolean;
    function IterateChilds(const Proc: TTreeViewIterateProc): boolean;
    function IterateParents(const Proc: TTreeViewIterateProc): boolean;
  end;


procedure Run;
begin
  TDUnitX.CheckCommandLine;

  Application.Initialize;
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;
end;

procedure TGUIVCLTestRunner.actTestsUnselectAllExecute(Sender: TObject);
begin
  UnselectAll;
end;

procedure TGUIVCLTestRunner.actCompareExecute(Sender: TObject);
var
  ResultNode: TResultNode;
  FileExt: string;
  ExpFile: string;
  ActFile: string;
begin
  ResultNode := GetComparableResultsNode;
  if ResultNode <> nil then
  begin
    if ResultNode.Format = TDUnitXComparableFormatXml then
      FileExt := 'xml'
    else if ResultNode.Format = TDUnitXComparableFormatCsv then
      FileExt := 'csv'
    else FileExt := '';

    //Save files
    ExpFile := PathUtils.MakeTempFile(ResultNode.Expected, FileExt);
    ActFile := PathUtils.MakeTempFile(ResultNode.Actual, FileExt);
    try
      //Spawn BC
      PathUtils.Compare(ExpFile, ActFile);
    finally
      TFile.Delete(ExpFile);
      TFile.Delete(ActFile);
    end;
  end;
end;

procedure TGUIVCLTestRunner.actCopyActualExecute(Sender: TObject);
var
  ResultNode: TResultNode;
begin
  ResultNode := GetComparableResultsNode;
  if ResultNode <> nil then
    Clipboard.AsText := ResultNode.Actual;
end;

procedure TGUIVCLTestRunner.actCopyExpectedExecute(Sender: TObject);
var
  ResultNode: TResultNode;
begin
  ResultNode := GetComparableResultsNode;
  if ResultNode <> nil then
    Clipboard.AsText := ResultNode.Expected;
end;

procedure TGUIVCLTestRunner.actResultsCollapseAllExecute(Sender: TObject);
begin
  tvwResults.FullCollapse;
end;

procedure TGUIVCLTestRunner.actResultsExpandAllExecute(Sender: TObject);
begin
  tvwResults.FullExpand;
end;

procedure TGUIVCLTestRunner.FormDestroy(Sender: TObject);
begin
  FTestBookmarkList.Free;
end;

function TGUIVCLTestRunner.GetComparableResultsNode: TResultNode;
var
  Selected: TResultNode;
begin
  Result := nil;

  Selected := GetSelectedResultsNode;
  if Selected <> nil then
  begin
    if Selected.Level = 2 then
      Selected := Selected.Parent as TResultNode;

    if Selected.IsComparable then
      Result := Selected;
  end;
end;

function TGUIVCLTestRunner.GetNodeByTestFullName(const TestFullName: string): TTreeNode;
var
  TestNode: TTreeNode;
begin
  TestNode := nil;

  tvwTests.IterateAll
  (
    procedure(const Node: TTreeNode; var Stop: boolean)
    begin
      if Node.Text = TestFullName then
      begin
        TestNode := Node;
        Stop := True;
      end;
    end
  );

  Result := TestNode;

  if Result = nil then
    raise Exception.Create('Could not find node for test: ' + TestFullName);
end;

function TGUIVCLTestRunner.GetRootNode(const nodeType: TTestResultType): TTreeNode;
begin
  if FRootNodes[nodeType] = nil then
  begin
    if nodeType = TTestResultType.Pass then
      Result := tvwResults.Items.Add(nil, '')
    else Result := tvwResults.Items.AddFirst(nil, '');
    Result.ImageIndex := cTestResultImageMap[nodeType];
    Result.SelectedIndex := Result.ImageIndex;
    FRootNodes[nodeType] := Result;
  end;
  Result := FRootNodes[nodeType];
end;

function TGUIVCLTestRunner.GetSelectedResultsNode: TResultNode;
begin
  Result := nil;

  if tvwResults.Selected <> nil then
    Result := tvwResults.Selected as TResultNode;
end;

procedure TGUIVCLTestRunner.actRunExecute(Sender: TObject);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    tvwTests.IterateAll
    (
      procedure(const Node: TTreeNode; var Stop: boolean)
      begin
        if (not TTestNode(Node).IsFixtureNode) and
          (Node.StateIndex = cTestNodeStateChecked) then
          StrList.Add(Node.Text);
      end
    );
    TDUnitX.Filter := TNameFilter.Create(StrList);
    UpdateGUIForRun(StrList.Count);
  finally
    StrList.Free;
  end;

  RunExecute;
end;

procedure TGUIVCLTestRunner.actRunSelectedExecute(Sender: TObject);
begin
  if tvwTests.Selected <> nil then
  begin
    TDUnitX.Filter := TNameFilter.Create(tvwTests.Selected.Text);
    UpdateGUIForRun(1);

    RunExecute;
  end;
end;

procedure TGUIVCLTestRunner.actFindInTextExecute(Sender: TObject);
var
  LineNum: integer;
  Found: boolean;
begin
  Found := False;

  if tvwTests.Selected <> nil then
  begin
    pgeResults.ActivePage := tabText;
    if FTestBookmarkList.TryGetValue(tvwTests.Selected.Text, LineNum) then
    begin
      rchText.Perform(EM_LINESCROLL, 0, LineNum - rchText.Perform(EM_GETFIRSTVISIBLELINE, 0, 0));
      Found := True;
    end;
  end;

  if not Found then
    MessageDlg(SCouldNotFindResultsForTest, mtInformation, [mbOK], 0);
end;

procedure TGUIVCLTestRunner.actFindInStructuredExecute(Sender: TObject);
var
  Found: boolean;
begin
  Found := False;

  if tvwTests.Selected <> nil then
  begin
    pgeResults.ActivePage := tabStructured;

    tvwResults.IterateAll
    (
      procedure(const Node: TTreeNode; var Stop: boolean)
      var
        ResultNode: TResultNode;
      begin
        ResultNode := Node as TResultNode;
        if ResultNode.TestNode = tvwTests.Selected then
        begin
          tvwResults.Selected := ResultNode;
          tvwResults.Selected.Expand(True);
          Found := True;
          Stop := True;
        end;
      end
    );
  end;

  if not Found then
    MessageDlg(SCouldNotFindResultsForTest, mtInformation, [mbOK], 0);
end;

procedure TGUIVCLTestRunner.actTestsInvertExecute(Sender: TObject);
begin
  Invert;
end;

procedure TGUIVCLTestRunner.actTestsSelectAllExecute(Sender: TObject);
begin
  SelectAll;
end;

procedure TGUIVCLTestRunner.FormClose(Sender: TObject; var Action: TCloseAction);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    tvwTests.IterateAll(
      procedure(const Node: TTreeNode; var Stop: boolean)
      begin
        if TTestNode(Node).IsFixtureNode then Exit;
        IniFile.WriteBool(cTestSetup, Node.Text, Node.StateIndex = cTestNodeStateChecked);
      end
    );
  finally
    IniFile.Free;
  end;
end;

procedure TGUIVCLTestRunner.FormCreate(Sender: TObject);
begin
  Width := Screen.WorkAreaWidth;
  Height := Screen.WorkAreaHeight;

  FTestBookmarkList := TTestBookmarkList.Create;

  Self.Caption := Format(SApplicationName, [ExtractFileName(ParamStr(0))]);
  Application.Title := Self.Caption;

  txtStatus.Caption := SGUIStatusIdle;
  txtStatus.Visible := True;
  pnlSummary.Visible := False;

  pgeResults.ActivePageIndex := 0;
end;

procedure TGUIVCLTestRunner.Loaded;
begin
  inherited;
  PostMessage(Self.Handle, WM_LOAD_TESTS, 0, 0);
end;

function TGUIVCLTestRunner.NeedRunner: ITestRunner;
begin
  Result := TDUnitX.CreateRunner([Self, TDUnitXGUIVCLRichEditLogger.Create(rchText, FTestBookmarkList)]);
  Result.FailsOnNoAsserts := True;
end;

procedure TGUIVCLTestRunner.OnBeginTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  UpdateGUIForProgress(pgResults.Position + 1, Test.FullName);
  Application.ProcessMessages;
end;

procedure TGUIVCLTestRunner.OnEndSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnEndSetupTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnEndTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnEndTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnEndTest(const threadId: TThreadID; const Test: ITestResult);
begin
  ProcessTestResult(Test);
end;

procedure TGUIVCLTestRunner.OnEndTestFixture(const threadId: TThreadID; const results: IFixtureResult);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnExecuteTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnLog(const logType: TLogLevel; const msg: string);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnSetupFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnSetupTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnStartTestFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnTearDownFixture(const threadId: TThreadID; const fixture: ITestFixtureInfo);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnTeardownTest(const threadId: TThreadID; const Test: ITestInfo);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnTestError(const threadId: TThreadID; const Error: ITestError);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnTestFailure(const threadId: TThreadID; const Failure: ITestError);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnTestIgnored(const threadId: TThreadID; const Ignored: ITestResult);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnTestingEnds(const RunResults: IRunResults);
begin
  UpdateGUIForComplete(RunResults);
end;

procedure TGUIVCLTestRunner.OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount: Cardinal);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnTestMemoryLeak(const threadId: TThreadID; const Test: ITestResult);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.OnTestSuccess(const threadId: TThreadID; const Test: ITestResult);
begin
  //Empty
end;

procedure TGUIVCLTestRunner.ProcessMessagesForNode(const ResultNode: TResultNode; const Test: ITestResult);
var
  LogMessage: TLogMessage;
  LogMessageNode: TTreeNode;
  ParentNode: TTreeNode;
begin
  if Test.ResultType = TTestResultType.Pass then
    ParentNode := ResultNode
  else
  begin
    ParentNode := ResultNode.Owner.AddChild(ResultNode, ReplaceStr(Test.Message, #13#10, ' '));
    ParentNode.ImageIndex := ResultNode.ImageIndex;
    ParentNode.SelectedIndex := ParentNode.ImageIndex;
  end;

  if Test.LogMessages <> nil then
  begin
    for LogMessage in Test.LogMessages do
    begin
      LogMessageNode := ResultNode.Owner.AddChild(ParentNode, LogMessage.Msg);
      LogMessageNode.ImageIndex := cLogLevelImageMap[LogMessage.Level];
      LogMessageNode.SelectedIndex := LogMessageNode.ImageIndex;
    end;
  end;
end;

procedure TGUIVCLTestRunner.ProcessTestResult(const Test: ITestResult);
var
  Error: ITestError;
  ParentNode: TTreeNode;
  Node: TTreeNode;
  ResultNode: TResultNode;
  testResult : TTestResultType;
begin
  testResult := Test.ResultType;

  tvwResults.Items.BeginUpdate;
  try
    if Test.QueryInterface(ITestError, Error) = 0 then
    begin
      case testResult of
        TTestResultType.Failure:
        begin
          //Not sure if this is correct behavior - the no assertions is generated by an option to fail test.
          if Test.Message = SNoAssertions then
          begin
            ParentNode := GetRootNode(TTestResultType.Warning);
            testResult := TTestResultType.Warning;
          end
          else ParentNode := GetRootNode(TTestResultType.Failure);
        end;
        TTestResultType.Error     : ParentNode := GetRootNode(TTestResultType.Error);
        TTestResultType.MemoryLeak: ParentNode := GetRootNode(TTestResultType.MemoryLeak);
      else
        raise Exception.Create('Passing/ignored test implements ITestError!');
      end;

      Node := tvwResults.Items.AddChild(ParentNode, Error.Test.FullName);
      ResultNode := Node as TResultNode;
      ResultNode.ImageIndex := -1;

      if Error.IsComparable then
      begin
        ResultNode.IsComparable := True;
        ResultNode.Expected := Error.Expected;
        ResultNode.Actual := Error.Actual;
        ResultNode.Format := Error.Format;
        ResultNode.ImageIndex := 9;
        ResultNode.SelectedIndex := ResultNode.ImageIndex;
      end;
    end
    else
    begin
      case testResult of
        TTestResultType.Pass       : ParentNode := GetRootNode(TTestResultType.Pass);
        TTestResultType.Ignored    : ParentNode := GetRootNode(TTestResultType.Ignored);
        TTestResultType.MemoryLeak : ParentNode := GetRootNode(TTestResultType.MemoryLeak);
      else
        raise Exception.Create('Failing test does not implement ITestError!');
      end;

      Node := tvwResults.Items.AddChild(ParentNode, Test.Test.FullName);
      ResultNode := Node as TResultNode;
      ResultNode.ImageIndex := -1;
    end;

    ResultNode.TestNode := GetNodeByTestFullName(Test.Test.FullName);
    if ResultNode.ImageIndex < 0 then
    begin
      ResultNode.ImageIndex := ResultNode.Parent.ImageIndex;
      ResultNode.SelectedIndex := ResultNode.ImageIndex;
    end;

    ProcessMessagesForNode(ResultNode, Test);
    SetNodeTestResult(ResultNode.TestNode, ResultNode.ImageIndex);
    UpdateRootNodeCaption(testResult);
    ParentNode.Expand(False);
    ResultNode.Expand(True);

    if tvwResults.Items.Count > 0 then
      tvwResults.TopItem := tvwResults.Items[0];
  finally
    tvwResults.Items.EndUpdate;
  end;
  Application.ProcessMessages;
end;

procedure TGUIVCLTestRunner.LoadTests;
var
  Runner: ITestRunner;
begin
  Runner := NeedRunner;

  FFixtureList := Runner.BuildFixtures as ITestFixtureList;

  tvwTests.Items.BeginUpdate;
  try
    tvwTests.Items.Clear;
    FTestBookmarkList.Clear;
    FFixtureCount:=0;
    BuildTestTreeNode(FFixtureList,nil);

    if tvwTests.Items.Count > 0 then
    begin
      tvwTests.AlphaSort(True);
      tvwTests.Selected := tvwTests.Items[0];
      tvwTests.TopItem := tvwTests.Selected;
    end;
  finally
    tvwTests.Items.EndUpdate;
  end;

  stsMain.Panels[0].Text := Format(STestsFound, [tvwTests.Items.Count-FFixtureCount]);

  UpdateFormActions;
end;

procedure TGUIVCLTestRunner.RunExecute;
var
  Runner: ITestRunner;
begin
  FRunning := true;
  UpdateFormActions;
  Screen.Cursor := crHourGlass;
  try
    ClearResults;
    //Make sure UI is up-to-date before entering test run
    Application.ProcessMessages;

    Runner := NeedRunner;
    FLastResult := Runner.Execute;

    if tvwResults.Items.Count > 0 then
      tvwResults.TopItem := tvwResults.Items[0];

    tvwTests.Invalidate;
  finally
    Screen.Cursor := crDefault;
  end;

  FRunning := False;

  UpdateFormActions;
end;

procedure TGUIVCLTestRunner.SetNodeTestResult(const Node: TTreeNode; const ImageIndex: integer);

var
  NewWeight: integer;
begin
  Node.ImageIndex := ImageIndex;
  Node.SelectedIndex := ImageIndex;

  NewWeight:=TTestNode.ToWeight(ImageIndex);

  Node.IterateParents(
    procedure(const Node: TTreeNode; var Stop: boolean)
    var
      Weight: integer;
    begin
      Weight:=TTestNode.ToWeight(Node.ImageIndex);
      if Weight=NewWeight then
        Stop:=true
      else
      begin
        if NewWeight<Weight then
        begin
          Node.IterateChilds(
            procedure(const Node: TTreeNode; var Stop: boolean)
            var
              ChildWeight: integer;
            begin
              ChildWeight:=TTestNode.ToWeight(Node.ImageIndex);
              if NewWeight<ChildWeight then
                NewWeight:=ChildWeight;
            end);
          if Weight=NewWeight then
          begin
            Stop:=true;
            Exit;
          end;
        end;
        Node.ImageIndex:=TTestNode.ToImageIndex(NewWeight);
        Node.SelectedIndex:=Node.ImageIndex;
      end;
    end);
end;

procedure TGUIVCLTestRunner.BuildTestTreeNode(const FixtureList: ITestFixtureList;
  const ParentNode: TTreeNode);

  function HasFilteredTests(Fixture: ITestFixture): boolean;
  var
    Test : ITest;
    Filter: String;
  begin
    Result:=false;
    if Fixture.Tests=nil then Exit;
    filter:=edtFilter.Text;
    for Test in Fixture.Tests do
    begin
      if (Test.Enabled)and
        ((Filter = '') or ContainsText(Test.FullName, Filter)) then
        Exit(true);
    end;
  end;

  function HasFilteredChildTests(Fixture: ITestFixture): boolean;
  var
    Child: ITestFixture;
  begin
    Result:=false;
    if Fixture.Children=nil then
      Exit;
    for Child in Fixture.Children do
    begin
      if HasFilteredTests(Child) then
        Exit(true);
      if HasFilteredChildTests(Child) then
        Exit(true);
    end;
  end;

var
  Fixture: ITestFixture;
  Test: ITest;
  TestNode: TTreeNode;
  IniFile: TIniFile;
  FixtureNode: TTreeNode;
  HasTests: boolean;
  HasChildTests: boolean;

begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    for Fixture in FixtureList do
    begin
      HasTests:=HasFilteredTests(Fixture);
      HasChildTests:=HasFilteredChildTests(Fixture);
      if (not HasTests) and (not HasChildTests) then
        continue;
      FixtureNode:= tvwTests.Items.AddChild(ParentNode, Fixture.FullName);
      FixtureNode.StateIndex:=cTestNodeStateUnChecked;
      inc(FFixtureCount);
      TTestNode(FixtureNode).FIsFixtureNode:=true;

      if HasChildTests then
        BuildTestTreeNode(Fixture.Children,FixtureNode);

      for Test in Fixture.Tests do
      begin
        if Test.Enabled and
          ((edtFilter.Text = '') or ContainsText(Test.FullName, edtFilter.Text)) then
        begin
          TestNode := tvwTests.Items.AddChild(FixtureNode, Test.FullName);
          TestNode.StateIndex:=cTestNodeStateUnchecked;
          if IniFile.ReadBool(cTestSetup, Test.FullName, true) then
            TTestNode(TestNode).InvertState;
        end;
      end;
     end;
  finally
    IniFile.Free;
  end;
end;

procedure TGUIVCLTestRunner.edtFilterRightButtonClick(Sender: TObject);
begin
  edtFilter.Clear;
  LoadTests;
end;

procedure TGUIVCLTestRunner.ClearResults;
var
  i : TTestResultType;
begin
  for i := Low(TTestResultType) to High(TTestResultType) do
    FRootNodes[i] := nil;

  tvwResults.Items.Clear;
  pgeResults.ActivePageIndex := 0;
end;

procedure TGUIVCLTestRunner.edtFilterKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    LoadTests;
    Key := #0;
  end;
end;

procedure TGUIVCLTestRunner.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F9) and (Shift = []) then
    actRun.Execute;
end;

procedure TGUIVCLTestRunner.FormShow(Sender: TObject);
begin
  pnlResults.Width := (ClientWidth - splMain.Width) div 2;
end;

procedure TGUIVCLTestRunner.UpdateGUIForProgress(const CurrTestCount: integer; const CurrTestName: string);
begin
  pgResults.Position := CurrTestCount;
  txtStatus.Caption := Format(SGUIStatusRunning, [pgResults.Position, pgResults.Max]);
  if CurrTestName <> '' then
    txtStatus.Caption := txtStatus.Caption + ' - ' + CurrTestName;
end;

procedure TGUIVCLTestRunner.UpdateGUIForRun(const TestCount: integer);
begin
  txtStatus.Visible := True;
  pnlSummary.Visible := False;
  pgResults.Max := TestCount;
  UpdateGUIForProgress(0, '');
end;

procedure TGUIVCLTestRunner.Invert;
begin
  tvwTests.IterateAll(
    procedure(const Node: TTreeNode; var Stop: boolean)
    begin
      if not TTestNode(Node).IsFixtureNode then
        TTestNode(Node).InvertState;
    end);
end;

procedure TGUIVCLTestRunner.SelectAll;
begin
  tvwTests.IterateAll(
    procedure(const Node: TTreeNode; var Stop: boolean)
    begin
      Node.StateIndex:=cTestNodeStateChecked;
    end);
end;

procedure TGUIVCLTestRunner.WMTestPopUp(var message : TMessage);
begin
  popTests.Popup(message.WParam, message.LParam);
end;

procedure TGUIVCLTestRunner.tvwTestsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitTestRes: THitTests;
  HitNode: TTreeNode;
begin
  HitTestRes := tvwTests.GetHitTestInfoAt(X, Y);
  if htOnStateIcon in HitTestRes then
  begin
    HitNode := tvwTests.GetNodeAt(X, Y);
    if HitNode <> nil then
      TTestNode(HitNode).InvertState;
  end
  else if (Button=mbRight) and (THitTest.htOnItem in HitTestRes) then
  begin
    HitNode := tvwTests.GetNodeAt(X, Y);
    if HitNode<>nil then
    begin
      HitNode.Selected:=true;
      Application.ProcessMessages;
      PostMessage(Handle,WM_TESTPOPUP,Mouse.CursorPos.X,Mouse.CursorPos.Y);
    end
  end;
end;

procedure TGUIVCLTestRunner.tvwTestsChange(Sender: TObject; Node: TTreeNode);
begin
  self.UpdateFormActions;
end;

procedure TGUIVCLTestRunner.tvwTestsCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TTestNode;
end;

procedure TGUIVCLTestRunner.tvwTestsCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
 if TTestNode(Node).IsFixtureNode then begin
    Sender.Canvas.Font.Style:=Sender.Canvas.Font.Style + [fsBold];
    Sender.Canvas.Font.Color:=clWindowText;
  end;
end;

procedure TGUIVCLTestRunner.tvwTestsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ' ' then
  begin
    TTestNode(tvwTests.Selected).InvertState;
    Key := #0
  end;
end;

procedure TGUIVCLTestRunner.tvwResultsChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateFormActions;
end;

procedure TGUIVCLTestRunner.tvwResultsCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TResultNode;
end;

procedure TGUIVCLTestRunner.WMResultPopUp(var message : TMessage);
begin
  popResults.Popup(message.WParam, message.LParam);
end;

procedure TGUIVCLTestRunner.tvwResultsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  if Button = mbRight then
  begin
    Node := tvwResults.GetNodeAt(X, Y);
    if Node <> nil then
    begin
      Node.Selected := True;
      PostMessage(handle,WM_RESULTPOPUP,Mouse.CursorPos.X, Mouse.CursorPos.Y);
    end;
  end;
end;

procedure TGUIVCLTestRunner.UnselectAll;
begin
  tvwTests.IterateAll(
    procedure(const Node: TTreeNode; var Stop: boolean)
    begin
      Node.StateIndex:=cTestNodeStateUnchecked;
    end);
end;

procedure TGUIVCLTestRunner.UpdateFormActions;
begin
  actRun.Enabled := (not FRunning) and
   ((tvwTests.Items.Count - FFixtureCount) > 0);

  actRunSelected.Enabled := (not FRunning) and (tvwTests.Selected <> nil)
    and (not TTestNode(tvwTests.Selected).IsFixtureNode);

  actTestsSelectAll.Enabled := actRun.Enabled;
  actTestsUnselectAll.Enabled := actRun.Enabled;
  actTestsInvert.Enabled := actRun.Enabled;

  actResultsExpandAll.Enabled := tvwResults.Items.Count > 0;
  actResultsCollapseAll.Enabled := actResultsExpandAll.Enabled;

  actFindInStructured.Enabled := actRunSelected.Enabled and
    actResultsExpandAll.Enabled;

  actFindInText.Enabled := actFindInStructured.Enabled;

  actCompare.Enabled := GetComparableResultsNode <> nil;

  actCopyActual.Enabled := actCompare.Enabled;
  actCopyExpected.Enabled := actCompare.Enabled;

end;

procedure TGUIVCLTestRunner.UpdateGUIForComplete(const RunResults: IRunResults);
begin
  txtPassed.Caption := IntToStr(RunResults.PassCount);
  txtIgnored.Caption := IntToStr(RunResults.IgnoredCount);
  txtFailed.Caption := IntToStr(RunResults.FailureCount);
  txtErrored.Caption := IntToStr(RunResults.ErrorCount);
  txtMemLeak.Caption := IntToStr(RunResults.MemoryLeakCount);

  txtStatus.Visible := False;
  pnlSummary.Visible := True;
end;

procedure TGUIVCLTestRunner.UpdateRootNodeCaption(const nodeType: TTestResultType);
var
  node : TTreeNode;
begin
  node := FRootNodes[nodeType];

  if node = nil then
    exit;
  node.Text := Format(cRootNodeStrings[nodeType], [Node.Count]);
end;

procedure TGUIVCLTestRunner.WMLoadTests(var message: TMessage);
begin
  LoadTests;
end;

class procedure PathUtils.Compare(const ExpFile, ActFile: string);
var
  BCPath: string;
  CmdLine: string;
begin
  BCPath := FindBeyondCompare;
  if BCPath <> '' then
  begin
    CmdLine := Format('"%s" "%s" "%s" /lefttitle="%s" /righttitle="%s"', [BCPath, ExpFile, ActFile, 'Expected', 'Actual']);
    CreateProcessAndWait(CmdLine);
  end
  else MessageDlg('Could not find BeyondCompare in any known paths.', mtError, [mbOK], 0);
end;

class function PathUtils.CreateProcessAndWait(const CmdLine: string): boolean;

  procedure WaitFor(const ProcessHandle: THandle);
  var
    msg: TMsg;
    ret: DWORD;
    hdl: THandle;
  begin
    //Param can't be const, so assign to local var
    hdl := ProcessHandle;
    repeat
      ret := MsgWaitForMultipleObjects(1, hdl, False, INFINITE, QS_PAINT or QS_SENDMESSAGE);

      if ret = WAIT_FAILED then
        exit;

      if ret = (WAIT_OBJECT_0 + 1) Then
      begin
        while PeekMessage(msg, 0, WM_PAINT, WM_PAINT, PM_REMOVE) do
          DispatchMessage(msg);
      end;
    until ret = WAIT_OBJECT_0;
  end;

var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;

begin
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_SHOWNORMAL;
  if CreateProcess(nil, PChar(CmdLine), nil, nil, False, CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then
  begin
    Result := True;
    WaitFor(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end
  else Result := False;
end;

class function PathUtils.FindBeyondCompare: string;
const
  BCPathList: array [0..1] of string =
  (
    'Beyond Compare 4\BComp.exe',
    'Beyond Compare 3\BComp.exe'
  );
var
  BCPath: string;
  PFPathList: TStringArray;
  PFPath: string;
begin
  Result := '';

  PFPathList := PathUtils.GetProgramFiles;

  for PFPath in PFPathList do
    for BCPath in BCPathList do
      if TFile.Exists(PFPath + BCPath) then
      begin
        Result := PFPath + BCPath;
        break;
      end;
end;

class function PathUtils.GetProgramFiles: TStringArray;
var
  X86: string;
  X64: string;
begin
  X86 := GetProgramFilesX86;
  X64 := GetProgramFilesX64;
  if X64 <> '' then
  begin
    SetLength(Result, 2);
    Result[0] := X64;
    Result[1] := X86;
  end
  else
  begin
    SetLength(Result, 1);
    Result[0] := X86;
  end;
end;

class function PathUtils.GetProgramFilesX64: string;
begin
  if IsWow64 then
    Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('ProgramW6432'))
  else
    Result := '';
end;

class function PathUtils.GetProgramFilesX86: string;
var
  Ident: string;
begin
  Ident := IfThen(IsWow64, 'ProgramFiles(x86)', 'ProgramFiles');
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable(Ident));
end;

class function PathUtils.IsWow64: boolean;
type
  TIsWow64Process = function(hProcess: THandle; var bWow64Process: BOOL): BOOL; stdcall;
var
  IsWow64Process: TIsWow64Process;
  bWow64Process: BOOL;
begin
  Result := False;
  @IsWow64Process := GetProcAddress(GetModuleHandle('kernel32.dll'), 'IsWow64Process');
  if @IsWow64Process <> nil then
  begin
    IsWow64Process(GetCurrentProcess(), bWow64Process);
    Result := bWow64Process;
  end;
end;

class function PathUtils.MakeTempFile(const Contents: string; const FileExtNoDot: string): string;
var
  FileName: string;
begin
  FileName := TPath.GetTempFileName;
  if FileExtNoDot <> '' then
  begin
    Result := ChangeFileExt(FileName, '.' + FileExtNoDot);
    RenameFile(FileName, Result);
  end
  else Result := FileName;

  System.Assert(FileExists(Result));
  TFile.WriteAllText(Result, Contents);
end;

// TTreeViewHelper
//================
function TTreeViewHelper.IterateAll(const Proc: TTreeViewIterateProc): boolean;
var
  Node: TTreeNode;
begin
  System.Assert(Assigned(Proc));

  Result := False;
  for Node in Self.Items do
  begin
    Proc(Node, Result);
    if Result then
      break;
  end;
end;

// TTreeNodeHelper
//================
function TTreeNodeHelper.IterateAllChilds(const Proc: TTreeViewIterateProc): boolean;
var
  Child: TTreeNode;
begin
  System.Assert(Assigned(Proc));

  Result := False;
  Child:=self.getFirstChild;
  while Child<>nil do
  begin
    Proc(Child,Result);
    if Result then
      break;
    Result:=Child.IterateAllChilds(Proc);
    if Result then
      break;
    Child:=self.GetNextChild(Child);
  end;
end;

function TTreeNodeHelper.IterateChilds(const Proc: TTreeViewIterateProc): boolean;
var
  Child: TTreeNode;
begin
  System.Assert(Assigned(Proc));

  Result := False;
  Child:=self.getFirstChild;
  while Child<>nil do
  begin
    Proc(Child,Result);
    if Result then
      break;
    Child:=self.GetNextChild(Child);
  end;
end;

function TTreeNodeHelper.IterateParents(const Proc: TTreeViewIterateProc): boolean;
var
  Parent: TTreeNode;
begin
  System.Assert(Assigned(Proc));
  Result := False;
  Parent:=self.Parent;
  while Parent<>nil do
  begin
    Proc(Parent,Result);
    if Result then
      break;
    Parent:=Parent.Parent;
  end;
end;

procedure TResultNode.AfterConstruction;
begin
  inherited;
  IsComparable := False;
end;

// TTestNode
//==========
procedure TTestNode.InvertState;
var
  Checked: boolean;
  NewStateIndex: integer;
begin
  Checked:=not (StateIndex=cTestNodeStateChecked);
  if Checked then
    NewStateIndex:=cTestNodeStateChecked
  else
    NewStateIndex:=cTestNodeStateUnchecked;

  StateIndex:=NewStateIndex;

  self.IterateAllChilds(
    procedure(const Node: TTreeNode; var Stop: boolean)
    begin
      Node.StateIndex:=NewStateIndex;
    end);

  if Checked then
    self.IterateParents(
      procedure(const Node: TTreeNode; var Stop: boolean)
      begin
        if Node.StateIndex=cTestNodeStateChecked then
          Stop:=true
        else
          Node.StateIndex:=cTestNodeStateChecked;
      end)
  else
    self.IterateParents(
      procedure(const Node: TTreeNode; var Stop: boolean)
      begin
        if Node.IterateChilds(
          procedure(const Node: TTreeNode; var Stop: boolean)
          begin
            if Node.StateIndex=cTestNodeStateChecked then
              Stop:=true;
          end) then
          Stop:=true
        else
          Node.StateIndex:=cTestNodeStateUnchecked
      end);
end;

class function TTestNode.ToWeight(const ImageIndex: integer): integer;
begin
  case ImageIndex of
    0: Result:=0; // ignored
    1: Result:=1; // pass
    3: Result:=2; // warning
  else
    Result:=3; // Error
  end;
end;

class function TTestNode.ToImageIndex(const Weight: integer): integer;
begin
  case Weight of
    0: Result:=0; // ignored
    1: Result:=1; // pass
    2: Result:=3; // warning
  else
    Result:=2; // Error
  end;
end;

end.
