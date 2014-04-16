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

unit DUNitX.Loggers.GUIX;

interface
{$I DUnitX.Inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.Layouts, FMX.TreeView, FMX.Edit,
  DUnitX.TestFramework, DUnitX.Extensibility, DUnitX.InternalInterfaces, FMX.ListView.Types,
  FMX.ListView, FMX.ListBox, Generics.Collections, FMX.Memo;

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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RunExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FailListItemClick(const Sender: TObject;
      const AItem: TListViewItem);
  private
    { Private declarations }
    FTestRunner: ITestRunner;
    FFixtureList: ITestFixtureList;
    FLastResult: IRunResults;
    FFailedTests: TDictionary<String, ITestResult>;
    function CreateNode(Owner: TComponent; Text: String; TestFullName: String): TTreeViewItem;
    function GetNode(FullName: String): TTreeViewItem;
    procedure BuildTree(parentNode: TTreeViewItem; const fixtureList: ITestFixtureList);
  protected
    procedure OnBeginTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndSetupTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnEndTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnEndTest(const threadId: Cardinal; const Test: ITestResult);
    procedure OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);
    procedure OnExecuteTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnLog(const logType: TLogLevel; const msg: string);
    procedure OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnSetupTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
    procedure OnTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
    procedure OnTestError(const threadId: Cardinal; const Error: ITestError);
    procedure OnTestFailure(const threadId: Cardinal; const Failure: ITestError);
    procedure OnTestSuccess(const threadId: Cardinal; const Test: ITestResult);
    procedure OnTestMemoryLeak(const threadId : Cardinal; const AIgnored: ITestResult);
    procedure OnTestIgnored(const threadId: Cardinal; const Ignored: ITestResult);
    procedure OnTestingEnds(const RunResults: IRunResults);
    procedure OnTestingStarts(const threadId: Cardinal; const testCount: Cardinal; const testActiveCount: Cardinal);
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
  public
    constructor Create(Owner: TComponent; Text: String; TestFullName: String); reintroduce;
    destructor Destroy; override;
    property FullName: String read FFullName;
    procedure SetResultType(resultType: TTestResultType);
    procedure Reload;
  end;



{$R *.fmx}

{ TGUIXTestRunner }

procedure TGUIXTestRunner.BuildTree(parentNode: TTreeViewItem;
  const fixtureList: ITestFixtureList);
var
  fixture : ITestFixture;
  test : ITest;
  fixtureNode : TTreeViewItem;
  testNode : TTreeViewItem;
begin
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
      testNode := CreateNode(TestTree, test.Name, test.Fixture.FullName + '.' + test.Name);
      testNode.Parent := fixtureNode;
    end;
    fixtureNode.ExpandAll;
  end;
end;


function TGUIXTestRunner.CreateNode(Owner: TComponent; Text: String; TestFullName: String): TTreeViewItem;
begin
  Result := TTestNode.Create(Owner, Text, TestFullName);
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

procedure TGUIXTestRunner.OnBeginTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin

end;

procedure TGUIXTestRunner.OnEndSetupFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TGUIXTestRunner.OnEndSetupTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin

end;

procedure TGUIXTestRunner.OnEndTearDownFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TGUIXTestRunner.OnEndTeardownTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin

end;

procedure TGUIXTestRunner.OnEndTest(const threadId: Cardinal;
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

procedure TGUIXTestRunner.OnEndTestFixture(const threadId: Cardinal;
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

procedure TGUIXTestRunner.OnExecuteTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin

end;

procedure TGUIXTestRunner.OnLog(const logType: TLogLevel; const msg: string);
begin

end;

procedure TGUIXTestRunner.OnSetupFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TGUIXTestRunner.OnSetupTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin

end;

procedure TGUIXTestRunner.OnStartTestFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TGUIXTestRunner.OnTearDownFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TGUIXTestRunner.OnTeardownTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin

end;

procedure TGUIXTestRunner.OnTestError(const threadId: Cardinal;
  const Error: ITestError);
begin

end;

procedure TGUIXTestRunner.OnTestFailure(const threadId: Cardinal;
  const Failure: ITestError);
begin

end;

procedure TGUIXTestRunner.OnTestIgnored(const threadId: Cardinal;
  const Ignored: ITestResult);
begin

end;

procedure TGUIXTestRunner.OnTestingEnds(const RunResults: IRunResults);
begin

end;

procedure TGUIXTestRunner.OnTestingStarts(const threadId, testCount,
  testActiveCount: Cardinal);
begin

end;

procedure TGUIXTestRunner.OnTestMemoryLeak(const threadId: Cardinal;
  const AIgnored: ITestResult);
begin

end;

procedure TGUIXTestRunner.OnTestSuccess(const threadId: Cardinal;
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
  FailList.ClearItems;
  FLastResult := FTestRunner.Execute;
  TotalRuns.Text := IntToStr(FLastResult.TestCount);
  FailTests.Text := IntToStr(FLastResult.FailureCount);
  SuccessTests.Text := IntToStr(FLastResult.PassCount);
  MemoryLeaked.Text := IntToStr(FLastResult.MemoryLeakCount);
end;


{ TTestNode }

constructor TTestNode.Create(Owner: TComponent; Text, TestFullName: String);
begin
  inherited Create(Owner);
  Self.Text := Text;
  FFullName := TestFullName;
  FImage := TImage.Create(Owner);
  FImage.Parent := Self;
  {$IFDEF DELPHI_XE6_UP}
    FImage.Align := TAlignLayout.Right;
  {$ELSE}
    FImage.Align := TAlignLayout.alRight;
  {$ENDIF}

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

