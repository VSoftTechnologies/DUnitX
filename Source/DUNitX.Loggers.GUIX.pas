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

unit DUNitX.Loggers.GUIX;

interface
{$I DUnitX.Inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.Layouts, FMX.TreeView, FMX.Edit,
  DUnitX.TestFramework, DUnitX.Extensibility, DUnitX.InternalInterfaces, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView, FMX.ListBox, System.Generics.Collections, FMX.Memo, FMX.ScrollBox, FMX.Controls.Presentation, System.IniFiles,
  FMX.ListView.Adapters.Base;

{$HPPEMIT '#if defined(USEPACKAGES)'}
{$HPPEMIT '# pragma comment(lib, "Fmx.bpi")'}
{$HPPEMIT '# pragma comment(lib, "DUnitXFMXRuntime.bpi")'}
{$HPPEMIT '#else'}
{$HPPEMIT '# pragma comment(lib, "Fmx")'}
{$HPPEMIT '# pragma comment(lib, "DUnitXFMXRuntime")'}
{$HPPEMIT '#endif'}

type
  TGUIXTestRunner = class(TForm, ITestLogger)
    TopPanel: TPanel;
    TestActions: TActionList;
    Run: TAction;
    ToolBar1: TToolBar;
    Button1: TButton;
    Panel1: TPanel;
    TestTree: TTreeView;
    GridPanelLayout1: TGridPanelLayout;
    FailList: TListView;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    SuccessTests: TLabel;
    TotalRuns: TLabel;
    Label5: TLabel;
    FailTests: TLabel;
    MemoryLeakedLabel: TLabel;
    MemoryLeaked: TLabel;
    TestRunnerProgress: TProgressBar;
    Splitter1: TSplitter;
    FailTestDetailPanel: TPanel;
    StackTrace: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    FailTestName: TLabel;
    FailTestStartTime: TLabel;
    Label7: TLabel;
    FailTestFinishTime: TLabel;
    Label6: TLabel;
    FailTestMessage: TMemo;
    Label8: TLabel;
    SelectFailedButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RunExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FailListItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure TestTreeChangeCheck(Sender: TObject);
    procedure SelectFailedButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FTestRunner: ITestRunner;
    FFixtureList: ITestFixtureList;
    FLastResult: IRunResults;
    FFailedTests: TDictionary<String, ITestResult>;
    function CreateNode(Owner: TComponent; Test: ITest): TTreeViewItem; overload;
    function CreateNode(Owner: TComponent; Text: String; TestFullName: String): TTreeViewItem; overload;
    function GetNode(FullName: String): TTreeViewItem;
    procedure BuildTree(parentNode: TTreeViewItem; const fixtureList: ITestFixtureList);
    procedure BuildEnabledTestList;
    procedure SaveConfiguration(AIniFile: TCustomIniFile);
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
    procedure OnTestMemoryLeak(const threadId: TThreadID; const AIgnored: ITestResult);
    procedure OnTestIgnored(const threadId: TThreadID; const Ignored: ITestResult);
    procedure OnTestingEnds(const RunResults: IRunResults);
    procedure OnTestingStarts(const threadId: TThreadID; testCount, testActiveCount: Cardinal);
  public
    { Public declarations }
  end;

var
  GUIXTestRunner: TGUIXTestRunner;

implementation

uses
  FMX.Objects;


type
  TTestNode = class(TTreeViewItem)
  strict private
    FFullName: String;
    FImage: TImage;
    FTest: ITest;
    FResultType: TTestResultType;
  public
    constructor Create(Owner: TComponent; Test: ITest; Text: String; TestFullName: String); reintroduce;
    destructor Destroy; override;
    property FullName: String read FFullName;
    property Test: ITest read FTest;
    property ResultType: TTestResultTYpe read FResultType write FResultType;
    procedure SetResultType(resultType: TTestResultType);
    procedure Reload;
  end;



{$R *.fmx}

const
  TEST_INI_FILE = 'dunitx.ini';

{ TGUIXTestRunner }

procedure TGUIXTestRunner.BuildEnabledTestList;
  procedure SetEnabled(Item: TTreeViewItem);
  var
    J: Integer;
  begin
    if Assigned(TTestNode(Item).Test) then
      TTestNode(Item).Test.Enabled := Item.IsChecked;
    for J := 0 to Item.Count - 1 do
      SetEnabled(Item.Items[J]);
  end;
var
  I: Integer;
begin
  for I := 0 to TestTree.Count - 1 do
    SetEnabled(TestTree.Items[I]);
end;

procedure TGUIXTestRunner.BuildTree(parentNode: TTreeViewItem;
  const fixtureList: ITestFixtureList);
const
  DisabledTests = 'DisabledTests';
var
  fixture : ITestFixture;
  test : ITest;
  fixtureNode : TTreeViewItem;
  testNode : TTreeViewItem;
  LIniFile : TCustomIniFile;
begin
  LIniFile := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + TEST_INI_FILE);
  try
    for fixture in fixtureList do
    begin
      fixtureNode := CreateNode(TestTree, fixture.Name, fixture.FullName);

      if Assigned(parentNode) then begin
        fixtureNode.Parent := parentNode;
      end
      else begin
        fixtureNode.Parent := TestTree;
      end;

      if fixture.HasChildFixtures then
        BuildTree(fixtureNode, fixture.Children);
      for test in fixture.Tests do
      begin
        testNode := CreateNode(TestTree, test);
        testNode.Parent := fixtureNode;
        testNode.IsChecked := LIniFile.ReadBool(DisabledTests, TTestNode(testNode).FullName, True);
      end;
      fixtureNode.ExpandAll;
    end;
  finally
    LIniFile.Free;
  end;
end;


procedure TGUIXTestRunner.SaveConfiguration(AIniFile: TCustomIniFile);
const
  DisabledTests = 'DisabledTests';

  procedure WriteValues(Item: TTreeViewItem);
  var
    J: Integer;
  begin
    if Assigned(TTestNode(Item).Test) and not Item.IsChecked then
      AIniFile.WriteBool(DisabledTests, TTestNode(Item).FullName, False);
    for J := 0 to Item.Count - 1 do
      WriteValues(Item.Items[J]);
  end;

var
  I: Integer;
begin
  if AIniFile.SectionExists(DisabledTests) then
    AIniFile.EraseSection(DisabledTests);

  for I := 0 to TestTree.Count - 1 do
    WriteValues(TestTree.Items[I]);
end;

procedure TGUIXTestRunner.SelectFailedButtonClick(Sender: TObject);
  procedure SetChecked(Item: TTreeViewItem);
  var
    J: Integer;
  begin
    if Assigned(TTestNode(Item).Test) then
      Item.IsChecked := (TTestNode(Item).ResultType = TTestResultType.Failure)
                        or (TTestNode(Item).ResultType = TTestResultType.Error)
                        or (TTestNode(Item).ResultType = TTestResultType.MemoryLeak);
    for J := 0 to Item.Count - 1 do
      SetChecked(Item.Items[J]);
  end;
var
  I: Integer;
begin
  for I := 0 to TestTree.Count - 1 do
    SetChecked(TestTree.Items[I]);
end;

function TGUIXTestRunner.CreateNode(Owner: TComponent; Test: ITest): TTreeViewItem;
begin
  Result := TTestNode.Create(Owner, Test, Test.Name, Test.Fixture.FullName + '.' + Test.Name);
end;

function TGUIXTestRunner.CreateNode(Owner: TComponent; Text: String; TestFullName: String): TTreeViewItem;
begin
  Result := TTestNode.Create(Owner, nil, Text, TestFullName);
end;

procedure TGUIXTestRunner.FailListItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  testResult: ITestResult;
begin
  testResult := FFailedTests[AItem.Detail];
  FailTestName.Text := testResult.Test.FullName;
  FailTestStartTime.Text := TimeToStr(testResult.StartTime);
  FailTestFinishTime.Text := TimeToStr(testResult.FinishTime);
  FailTestMessage.Text := testResult.Message;
  StackTrace.Text := testResult.StackTrace;
end;

procedure TGUIXTestRunner.FormClose(Sender: TObject; var Action: TCloseAction);
var
  LIniFile: TCustomIniFile;
begin
  LIniFile := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + TEST_INI_FILE);
  try
    SaveConfiguration(LIniFile);
    LIniFile.UpdateFile;
  finally
    LIniFile.Free;
  end;
end;

procedure TGUIXTestRunner.FormCreate(Sender: TObject);
begin
  FFailedTests := TDictionary<String, ITestResult>.Create;
  FTestRunner := TDUnitX.CreateRunner;
  FTestRunner.AddLogger(Self);
  FailTestName.Text := '';
  FailTestStartTime.Text := '';
  FailTestFinishTime.Text := '';
  FailTestMessage.Text := '';
  StackTrace.Text := '';
  TotalRuns.Text := '';
  FailTests.Text := '';
  SuccessTests.Text := '';
  MemoryLeaked.Text := '';
end;

procedure TGUIXTestRunner.FormDestroy(Sender: TObject);
begin
  FFailedTests.Free;
  FLastResult := nil;
  FFixtureList := nil;
  FTestRunner := nil;
end;

procedure TGUIXTestRunner.FormShow(Sender: TObject);
begin
  FFixtureList := FTestRunner.BuildFixtures as ITestFixtureList;
  BuildTree(nil, FFixtureList);
  TestRunnerProgress.Max := FFixtureList.Count;
end;

function TGUIXTestRunner.GetNode(FullName: String): TTreeViewItem;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  repeat begin
    if (TestTree.ItemByGlobalIndex(i) as TTestNode).FullName = FullName then
      Result := TestTree.ItemByGlobalIndex(i);
    Inc(i);
  end
  until Assigned(Result) or (i >= TestTree.GlobalCount);
end;

procedure TGUIXTestRunner.OnBeginTest(const threadId: TThreadID;
  const Test: ITestInfo);
begin

end;

procedure TGUIXTestRunner.OnEndSetupFixture(const threadId: TThreadID;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TGUIXTestRunner.OnEndSetupTest(const threadId: TThreadID;
  const Test: ITestInfo);
begin

end;

procedure TGUIXTestRunner.OnEndTearDownFixture(const threadId: TThreadID;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TGUIXTestRunner.OnEndTeardownTest(const threadId: TThreadID;
  const Test: ITestInfo);
begin

end;

procedure TGUIXTestRunner.OnEndTest(const threadId: TThreadID;
  const Test: ITestResult);
var
  failItem: TListViewItem;
  testNode: TTestNode;
begin
  if (Test.ResultType = TTestResultType.Failure)
    or (Test.ResultType = TTestResultType.Error)
    or (Test.ResultType = TTestResultType.MemoryLeak) then begin
    FFailedTests.Add(Test.Test.FullName, Test);
    failItem := FailList.Items.Add;
    failItem.Text := Test.Test.Name;
    failItem.Detail := Test.Test.FullName;
  end;

  testNode := GetNode(Test.Test.FullName) as TTestNode;

  if Assigned(testNode) then begin
    testNode.SetResultType(Test.ResultType);
  end;
end;

procedure TGUIXTestRunner.OnEndTestFixture(const threadId: TThreadID;
  const results: IFixtureResult);
var
  fixtureNode: TTestNode;
begin
  TestRunnerProgress.Value := TestRunnerProgress.Value + 1;
  fixtureNode := GetNode(results.Fixture.FullName) as TTestNode;
  // I shouldn't call this here !!!
  (results as IFixtureResultBuilder).RollUpResults;

  if Assigned(fixtureNode) then begin
    if results.HasFailures then begin
      fixtureNode.SetResultType(TTestResultType.Failure);
    end
    else if results.Errors.Count > 0 then begin
      fixtureNode.SetResultType(TTestResultType.Error);
    end
    else begin
      fixtureNode.SetResultType(TTestResultType.Pass);
    end;
  end;


end;

procedure TGUIXTestRunner.OnExecuteTest(const threadId: TThreadID;
  const Test: ITestInfo);
begin

end;

procedure TGUIXTestRunner.OnLog(const logType: TLogLevel; const msg: string);
begin

end;

procedure TGUIXTestRunner.OnSetupFixture(const threadId: TThreadID;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TGUIXTestRunner.OnSetupTest(const threadId: TThreadID;
  const Test: ITestInfo);
begin

end;

procedure TGUIXTestRunner.OnStartTestFixture(const threadId: TThreadID;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TGUIXTestRunner.OnTearDownFixture(const threadId: TThreadID;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TGUIXTestRunner.OnTeardownTest(const threadId: TThreadID;
  const Test: ITestInfo);
begin

end;

procedure TGUIXTestRunner.OnTestError(const threadId: TThreadID;
  const Error: ITestError);
begin

end;

procedure TGUIXTestRunner.OnTestFailure(const threadId: TThreadID;
  const Failure: ITestError);
begin

end;

procedure TGUIXTestRunner.OnTestIgnored(const threadId: TThreadID;
  const Ignored: ITestResult);
begin

end;

procedure TGUIXTestRunner.OnTestingEnds(const RunResults: IRunResults);
begin

end;

procedure TGUIXTestRunner.OnTestingStarts(const threadId: TThreadID; testCount,
  testActiveCount: Cardinal);
begin

end;

procedure TGUIXTestRunner.OnTestMemoryLeak(const threadId: TThreadID;
  const AIgnored: ITestResult);
begin

end;

procedure TGUIXTestRunner.OnTestSuccess(const threadId: TThreadID;
  const Test: ITestResult);
begin

end;

procedure TGUIXTestRunner.RunExecute(Sender: TObject);
begin
  TestRunnerProgress.Value := 0;
  FailTestName.Text := '';
  FailTestStartTime.Text := '';
  FailTestFinishTime.Text := '';
  FailTestMessage.Text := '';
  StackTrace.Text := '';
  TotalRuns.Text := '';
  FailTests.Text := '';
  SuccessTests.Text := '';
  MemoryLeaked.Text := '';
  FFailedTests.Clear;
  FailList.Selected := nil;
  FailList.Items.Clear;
  BuildEnabledTestList;
  FLastResult := FTestRunner.Execute;
  TotalRuns.Text := IntToStr(FLastResult.TestCount);
  FailTests.Text := IntToStr(FLastResult.FailureCount);
  SuccessTests.Text := IntToStr(FLastResult.PassCount);
  MemoryLeaked.Text := IntToStr(FLastResult.MemoryLeakCount);
end;


procedure TGUIXTestRunner.TestTreeChangeCheck(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to TTreeViewItem(Sender).Count - 1 do
    TTreeViewItem(Sender).Items[I].IsChecked := TTreeViewItem(Sender).IsChecked;
end;

{ TTestNode }

constructor TTestNode.Create(Owner: TComponent; Test: ITest; Text, TestFullName: String);
begin
  inherited Create(Owner);
  Self.Text := Text;
  FFullName := TestFullName;
  FTest := Test;
  FImage := TImage.Create(Owner);
  FImage.Parent := Self;
  FImage.Align := TAlignLayout.Right;

  FImage.Bitmap.Create(15, 15);
  FImage.Bitmap.Clear(TAlphaColorRec.Gray);
  FImage.SendToBack;
end;

destructor TTestNode.Destroy;
begin
  FImage.FreeOnRelease;
  inherited;
end;

procedure TTestNode.Reload;
begin
  FImage.Bitmap.Clear(TAlphaColorRec.Gray);
end;

procedure TTestNode.SetResultType(resultType: TTestResultType);
begin
 FResultType := resultType;
 case resultType of
   TTestResultType.Pass: begin
     FImage.Bitmap.Clear(TAlphaColorRec.Green);
   end;
   TTestResultType.Failure: begin
     FImage.Bitmap.Clear(TAlphaColorRec.DarkRed);
   end;
   TTestResultType.Error: begin
     FImage.Bitmap.Clear(TAlphaColorRec.Red);
   end;
   TTestResultType.Ignored: begin
     FImage.Bitmap.Clear(TAlphaColorRec.Lightgray);
   end;
   TTestResultType.MemoryLeak: begin
     FImage.Bitmap.Clear(TAlphaColorRec.Yellow);
   end;
 end;
end;

end.

