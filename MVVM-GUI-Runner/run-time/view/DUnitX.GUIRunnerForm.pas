unit DUnitX.GUIRunnerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DUnitX.BaseExecutive, AppEvnts, DUnitX.TestFramework,
  DUnitX.ViewModel_VCLForms, PlatformDefaultStyleActnCtrls, Menus, ActnPopup,
  ComCtrls, ImgList, ActnMan, ActnColorMaps, ActnList, ExtCtrls, StdCtrls,
  ToolWin, ActnCtrls, XPStyleActnCtrls, Generics.Collections, DUnitX.viewModel_LoggerContainer,
  System.Actions;

type
  TmfmGUIRunner = class(TForm)
    appevMain: TApplicationEvents;
    actmngrMain: TActionManager;
    tlbrMain: TActionToolBar;
    sbarMain: TStatusBar;
    memoLog: TRichEdit;
    pnlTestCases: TPanel;
    splTestCases: TSplitter;
    actRun: TAction;
    actAbort: TAction;
    actSelectAll: TAction;
    actSelectFailed: TAction;
    actClear: TAction;
    actToggleSelection: TAction;
    coloursXP: TXPColorMap;
    imglstGlyphs16x16: TImageList;
    pbarTests: TProgressBar;
    actAttachLogger: TAction;
    actDetachLogger: TAction;
    actEditLoggerProps: TAction;
    actPrimaryLvlInformation: TAction;
    actPrimaryLvlWarning: TAction;
    actPrimaryLvlError: TAction;
    actSecondaryLvlInformation: TAction;
    actSecondaryLvlWarning: TAction;
    actSecondaryLvlError: TAction;
    actHaltOnFirstFailure: TAction;
    actFailOnLeak: TAction;
    actNotYetDeveloped: TAction;
    popupLogMemo: TPopupActionBar;
    actAbout: TAction;
    procedure FormDestroy(Sender: TObject);
    procedure appevMainIdle(Sender: TObject; var Done: Boolean);
    procedure actRunExecute(Sender: TObject);
    procedure actRunUpdate(Sender: TObject);
    procedure actAttachLoggerExecute(Sender: TObject);

  private
    FViewModel: IViewModel_VCLForms;
    FSecondaryLoggersItem: TActionClientItem;

    procedure CreateDynamicMenuItems( SecondaryLoggers: TList<ILoggerContainerFactory>);
    function  CreateAttachLoggerAction( const Factory: ILoggerContainerFactory): TAction;
    function  CreateLoggerAction( Template: TAction; const Logger: ITestLogger; const Properties: IInterface; const DisplayName: string): TAction;

  protected
    procedure Loaded; override;

  public
    { Public declarations }
  end;

var
  mfmGUIRunner: TmfmGUIRunner;

implementation




{$R *.dfm}

type
TFactoryAction = class( TAction)
  public
    FFactory: ILoggerContainerFactory;
    constructor Create( AOwner: TComponent; const Factory: ILoggerContainerFactory);
  end;

TLoggerAction = class( TAction)
  public
    FLogger: ITestLogger;
    FProps: IInterface;
    constructor Create( AOwner: TComponent; const Logger: ITestLogger; const Props: IInterface);
  end;

TConcreteView = class( TViewModel_VCLForms)
  protected
    procedure OnTestingStarts(const threadId, testCount, testActiveCount : Cardinal);                 override;
    procedure OnStartTestFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);        override;
    procedure OnSetupFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);            override;
    procedure OnEndSetupFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);         override;
    procedure OnBeginTest(const threadId : Cardinal;const  Test: ITestInfo);                          override;
    procedure OnSetupTest(const threadId : Cardinal;const  Test: ITestInfo);                          override;
    procedure OnEndSetupTest(const threadId : Cardinal;const  Test: ITestInfo);                       override;
    procedure OnExecuteTest(const threadId : Cardinal;const  Test: ITestInfo);                        override;
    procedure OnTestSuccess(const threadId : Cardinal;const  Test: ITestResult);                      override;
    procedure OnTestError(const threadId : Cardinal;const Error: ITestError);                         override;
    procedure OnTestFailure(const threadId : Cardinal;const  Failure: ITestError);                    override;
    procedure OnTestIgnored(const threadId : Cardinal; const AIgnored: ITestResult);                  override;
    procedure OnTestMemoryLeak(const threadId : Cardinal; const AIgnored: ITestResult);               override;
    procedure OnLog(const logType : TLogLevel; const msg : string);                                   override;
    procedure OnTeardownTest(const threadId : Cardinal;const  Test: ITestInfo);                       override;
    procedure OnEndTeardownTest(const threadId : Cardinal; const Test: ITestInfo);                    override;
    procedure OnEndTest(const threadId : Cardinal;const  Test: ITestResult);                          override;
    procedure OnTearDownFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);         override;
    procedure OnEndTearDownFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);      override;
    procedure OnEndTestFixture(const threadId : Cardinal; const results : IFixtureResult);            override;
    procedure OnTestingEnds(const RunResults: IRunResults);                                           override;
    function TreeOwner: TComponent;         override;
    function TreeParent: TWinControl;       override;
    function TreeName: string;              override;
    procedure IntegrateSecondariesIntoMenus;              override;
    procedure AttachVisualTree;                           override;
 end;


procedure AssignAction( Destination, Source: TAction);
begin
Destination.OnExecute          := Source.OnExecute;
Destination.OnUpdate           := Source.OnUpdate;
Destination.AutoCheck          := Source.AutoCheck;
Destination.Caption            := Source.Caption;
Destination.Checked            := Source.Checked;
Destination.DisableIfNoHandler := Source.DisableIfNoHandler;
Destination.Enabled            := Source.Enabled;
Destination.GroupIndex         := Source.GroupIndex;
Destination.HelpContext        := Source.HelpContext;
Destination.HelpKeyword        := Source.HelpKeyword;
Destination.HelpType           := Source.HelpType;
Destination.Hint               := Source.Hint;
Destination.ImageIndex         := Source.ImageIndex;
Destination.ShortCut           := Source.ShortCut;
Destination.SecondaryShortCuts := Source.SecondaryShortCuts;
Destination.Visible            := Source.Visible;
Destination.OnHint             := Source.OnHint
end;



procedure TmfmGUIRunner.Loaded;
begin
inherited;
FViewModel := TConcreteView.Create;
FViewModel.FormLoaded( self)
end;


function TmfmGUIRunner.CreateLoggerAction( Template: TAction; const Logger: ITestLogger; const Properties: IInterface; const DisplayName: string): TAction;
begin
result := TLoggerAction.Create( Template.Owner, Logger, Properties);
result.ActionList := Template.ActionList;
AssignAction( result, Template);
if Pos( '%s', result.Caption) > 0 then
  result.Caption  := Format( result.Caption, [DisplayName]);
result.Category := '(dynamic)'

end;

procedure TmfmGUIRunner.actAttachLoggerExecute( Sender: TObject);
var
  Logger: ITestLogger;
  Properties: IInterface;
  Addend: TActionClientItem;
  DisplayName: string;
begin
  // For each attached secondary logger, we will also have ...
  //   - Logger properties
  //   - Detach
  //   - Secondary logger log level
  //    -- Information   (group 2+)
  //    -- Warning
  //    -- Error
with Sender as TFactoryAction do
  begin
  if not FFactory.CreateLogger( Logger, Properties, DisplayName) then exit;
  FViewModel.AttachSecondaryLogger( Logger);
  Addend := FSecondaryLoggersItem.Items.Add;
  Addend.Action := CreateLoggerAction( actEditLoggerProps, Logger, Properties, DisplayName);
  Addend := FSecondaryLoggersItem.Items.Add;
  Addend.Action := CreateLoggerAction( actDetachLogger, Logger, Properties, DisplayName);
  Addend := FSecondaryLoggersItem.Items.Add;
  Addend.Caption := 'Secondary logger log level';
  Addend.Items.Add.Action := CreateLoggerAction( actPrimaryLvlInformation, Logger, Properties, DisplayName);
  Addend.Items.Add.Action := CreateLoggerAction( actPrimaryLvlWarning    , Logger, Properties, DisplayName);
  Addend.Items.Add.Action := CreateLoggerAction( actPrimaryLvlError      , Logger, Properties, DisplayName)
  end;
end;

procedure TmfmGUIRunner.actRunExecute(Sender: TObject);
begin
FViewModel.Run
end;

procedure TmfmGUIRunner.actRunUpdate(Sender: TObject);
begin
//
end;

procedure TmfmGUIRunner.appevMainIdle( Sender: TObject; var Done: Boolean);
begin
appevMain.OnIdle := nil;
FViewModel.FirstIdleEvent
end;

procedure TmfmGUIRunner.CreateDynamicMenuItems( SecondaryLoggers: TList<ILoggerContainerFactory>);
var
  Options, Addend: TActionClientItem;
  Factory: ILoggerContainerFactory;
begin
Options := (tlbrMain.ActionClient as TActionBarItem).Items.Add;
Options.Caption := 'Options';
Addend := Options.Items.Add;
Addend.Action := actHaltOnFirstFailure;
Addend := Options.Items.Add;
Addend.Action := actFailOnLeak;
Addend := Options.Items.Add;
Addend.Caption := 'Primary logger';
Addend.Items.Add.Action := actPrimaryLvlInformation;
Addend.Items.Add.Action := actPrimaryLvlWarning;
Addend.Items.Add.Action := actPrimaryLvlError;
Addend := Options.Items.Add;
Addend.Caption := 'Secondary loggers';
FSecondaryLoggersItem := Addend;

  // For each attached secondary logger, we will also have ...
  //   - Logger properties
  //   - Detach
  //   - Secondary logger log level
  //    -- Information   (group 2+)
  //    -- Warning
  //    -- Error

for Factory in SecondaryLoggers do
  begin
  Addend := FSecondaryLoggersItem.Items.Add;
  Addend.Action := CreateAttachLoggerAction( Factory);
  end;
tlbrMain.RecreateControls;
end;

function TmfmGUIRunner.CreateAttachLoggerAction( const Factory: ILoggerContainerFactory): TAction;
begin
result := TFactoryAction.Create( actAttachLogger.Owner, Factory);
result.ActionList := actAttachLogger.ActionList;
AssignAction( result, actAttachLogger);
result.Caption  := Format( result.Caption, [Factory.ClassDescriptor]);
result.Category := '(dynamic)'
end;

procedure TmfmGUIRunner.FormDestroy( Sender: TObject);
begin
FViewModel.FormDestroyed
end;




procedure TConcreteView.AttachVisualTree;
begin
end;

procedure TConcreteView.IntegrateSecondariesIntoMenus;
begin
(FForm as TmfmGUIRunner).CreateDynamicMenuItems( FSecondaryLoggerContainerFactories)
end;

procedure TConcreteView.OnBeginTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin
  inherited;

end;

procedure TConcreteView.OnEndSetupFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin
  inherited;

end;

procedure TConcreteView.OnEndSetupTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin
  inherited;

end;

procedure TConcreteView.OnEndTearDownFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin
  inherited;

end;

procedure TConcreteView.OnEndTeardownTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin
  inherited;

end;

procedure TConcreteView.OnEndTest(const threadId: Cardinal;
  const Test: ITestResult);
begin
  inherited;

end;

procedure TConcreteView.OnEndTestFixture(const threadId: Cardinal;
  const results: IFixtureResult);
begin
  inherited;

end;

procedure TConcreteView.OnExecuteTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin
  inherited;

end;

procedure TConcreteView.OnLog(const logType: TLogLevel; const msg: string);
begin
  inherited;

end;

procedure TConcreteView.OnSetupFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin
  inherited;

end;

procedure TConcreteView.OnSetupTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin
  inherited;

end;

procedure TConcreteView.OnStartTestFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin
  inherited;

end;

procedure TConcreteView.OnTearDownFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin
  inherited;

end;

procedure TConcreteView.OnTeardownTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin
  inherited;

end;

procedure TConcreteView.OnTestError(const threadId: Cardinal;
  const Error: ITestError);
begin
  inherited;

end;

procedure TConcreteView.OnTestFailure(const threadId: Cardinal;
  const Failure: ITestError);
begin
  inherited;

end;

procedure TConcreteView.OnTestIgnored(const threadId: Cardinal;
  const AIgnored: ITestResult);
begin
  inherited;

end;

procedure TConcreteView.OnTestingEnds(const RunResults: IRunResults);
begin
  inherited;

end;

procedure TConcreteView.OnTestingStarts(const threadId, testCount,
  testActiveCount: Cardinal);
begin
  inherited;

end;

procedure TConcreteView.OnTestMemoryLeak(const threadId: Cardinal;
  const AIgnored: ITestResult);
begin
  inherited;

end;

procedure TConcreteView.OnTestSuccess(const threadId: Cardinal;
  const Test: ITestResult);
begin
  inherited;

end;

function TConcreteView.TreeName: string;
begin
result := 'treeTestSuite'
end;

function TConcreteView.TreeOwner: TComponent;
begin
result := FForm
end;

function TConcreteView.TreeParent: TWinControl;
begin
result :=(FForm as TmfmGUIRunner).pnlTestCases
end;

{ TFactoryAction }

constructor TFactoryAction.Create(
  AOwner: TComponent; const Factory: ILoggerContainerFactory);
begin
inherited Create( AOwner);
FFactory := Factory
end;

{ TLoggerAction }

constructor TLoggerAction.Create(AOwner: TComponent; const Logger: ITestLogger;
  const Props: IInterface);
begin
inherited Create( AOwner);
FLogger := Logger;
FProps  := Props
end;

end.
