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

unit DUnitX.Loggers.GUI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ActnList, StdActns, ActnCtrls,
  ToolWin, ActnMan, ActnMenus, ImgList,
  PlatformDefaultStyleActnCtrls, ExtCtrls, ComCtrls,
  DUnitX.TestFrameWork,
  DUnitX.Extensibility,
  DUnitX.InternalInterfaces,
  StdCtrls;

const
  WM_LOAD_TESTS = WM_USER + 123;

type
  TDUnitXGuiLoggerForm = class(TForm,ITestLogger)
    ActionManager1: TActionManager;
    ActionImages: TImageList;
    actRunSelected: TAction;
    actRunAll: TAction;
    Panel2: TPanel;
    Panel3: TPanel;
    Label2: TLabel;
    Edit1: TEdit;
    Splitter1: TSplitter;
    Panel4: TPanel;
    ToolBar1: TToolBar;
    StateImages: TImageList;
    RunImages: TImageList;
    Panel1: TPanel;
    TestTree: TTreeView;
    ToolButton1: TToolButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FRunner : ITestRunner;
    FFixtureList : ITestFixtureList;
    FApplicationName : string;
    procedure BuildTree(parentNode: TTreeNode; const fixtureList: ITestFixtureList);
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
    procedure OnTestMemoryLeak(const threadId : Cardinal; const Test: ITestResult);
    procedure OnTestIgnored(const threadId: Cardinal; const Ignored: ITestResult);
    procedure OnTestingEnds(const RunResults: IRunResults);
    procedure OnTestingStarts(const threadId: Cardinal; const testCount: Cardinal; const testActiveCount: Cardinal);

    procedure WMLoadTests(var message : TMessage); message WM_LOAD_TESTS;
    procedure Loaded; override;

  public
    { Public declarations }


  end;

var
  DUnitXGuiLoggerForm: TDUnitXGuiLoggerForm;

implementation

{$R *.dfm}

procedure TDUnitXGuiLoggerForm.FormCreate(Sender: TObject);
begin
  FRunner := TDUnitX.CreateRunner;
  FRunner.AddLogger(Self);
  FApplicationName := ExtractFileName(ParamStr(0));
  Self.Caption := Format('DUnitX - [%s]',[FApplicationName]);
end;

procedure TDUnitXGuiLoggerForm.Loaded;
begin
  inherited;
  PostMessage(Self.Handle,WM_LOAD_TESTS,0,0);
end;

procedure TDUnitXGuiLoggerForm.OnBeginTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXGuiLoggerForm.OnEndSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXGuiLoggerForm.OnEndSetupTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXGuiLoggerForm.OnEndTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXGuiLoggerForm.OnEndTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXGuiLoggerForm.OnEndTest(const threadId: Cardinal; const Test: ITestResult);
begin

end;

procedure TDUnitXGuiLoggerForm.OnEndTestFixture(const threadId: Cardinal; const results: IFixtureResult);
begin

end;

procedure TDUnitXGuiLoggerForm.OnExecuteTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXGuiLoggerForm.OnLog(const logType: TLogLevel; const msg: string);
begin

end;

procedure TDUnitXGuiLoggerForm.OnSetupFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXGuiLoggerForm.OnSetupTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXGuiLoggerForm.OnStartTestFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXGuiLoggerForm.OnTearDownFixture(const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin

end;

procedure TDUnitXGuiLoggerForm.OnTeardownTest(const threadId: Cardinal; const Test: ITestInfo);
begin

end;

procedure TDUnitXGuiLoggerForm.OnTestError(const threadId: Cardinal; const Error: ITestError);
begin

end;

procedure TDUnitXGuiLoggerForm.OnTestFailure(const threadId: Cardinal; const Failure: ITestError);
begin

end;

procedure TDUnitXGuiLoggerForm.OnTestIgnored(const threadId: Cardinal; const Ignored: ITestResult);
begin

end;

procedure TDUnitXGuiLoggerForm.OnTestingEnds(const RunResults: IRunResults);
begin

end;

procedure TDUnitXGuiLoggerForm.OnTestingStarts(const threadId, testCount, testActiveCount: Cardinal);
begin

end;

procedure TDUnitXGuiLoggerForm.OnTestMemoryLeak(const threadId: Cardinal; const Test: ITestResult);
begin

end;

procedure TDUnitXGuiLoggerForm.OnTestSuccess(const threadId: Cardinal; const Test: ITestResult);
begin

end;


procedure TDUnitXGuiLoggerForm.BuildTree(parentNode : TTreeNode; const fixtureList : ITestFixtureList);
var
  fixture : ITestFixture;
  test : ITest;
  fixtureNode : TTreeNode;
begin
  for fixture in fixtureList do
  begin
    fixtureNode := TestTree.Items.AddChild(parentNode,fixture.Name);
    if fixture.HasChildFixtures then
      BuildTree(fixtureNode,fixture.Children);
    for test in fixture.Tests do
    begin
      TestTree.Items.AddChild(fixtureNode,test.Name);
    end;
  end;
end;


procedure TDUnitXGuiLoggerForm.WMLoadTests(var message: TMessage);
var
  rootNode : TTreeNode;
begin
  FFixtureList := FRunner.BuildFixtures as ITestFixtureList;
  TestTree.Items.BeginUpdate;
  TestTree.Items.Clear;
  try
    BuildTree(nil,FFixtureList);
  finally
    rootNode := TestTree.Items.GetFirstNode;
    if rootNode <> nil then
      rootNode.Expand(true);
    TestTree.Items.EndUpdate;
  end;



end;

end.
