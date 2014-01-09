unit DUnitX.GUIRunnerForm;

interface

{$if RTLVersion < 23.00}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DUnitX.BaseExecutive, AppEvnts, DUnitX.TestFramework,
  DUnitX.ViewModel_VCLForms, PlatformDefaultStyleActnCtrls, Menus, ActnPopup,
  ComCtrls, ImgList, ActnMan, ActnColorMaps, ActnList, ExtCtrls, StdCtrls,
  ToolWin, ActnCtrls, XPStyleActnCtrls, Generics.Collections, DUnitX.viewModel_LoggerContainer;
{$else}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DUnitX.BaseExecutive, AppEvnts, DUnitX.TestFramework,
  DUnitX.ViewModel_VCLForms, PlatformDefaultStyleActnCtrls, Menus, ActnPopup,
  ComCtrls, ImgList, ActnMan, ActnColorMaps, ActnList, ExtCtrls, StdCtrls,
  ToolWin, ActnCtrls, XPStyleActnCtrls, Generics.Collections, DUnitX.viewModel_LoggerContainer,
  System.Actions;
{$ifend}

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
    actHaltOnFirstFailure: TAction;
    actFailOnLeak: TAction;
    actNotYetDeveloped: TAction;
    popupLogMemo: TPopupActionBar;
    actAbout: TAction;
    actClearLog: TAction;
    miClearLog: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure appevMainIdle(Sender: TObject; var Done: Boolean);
    procedure actRunExecute(Sender: TObject);
    procedure actAttachLoggerExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    procedure actAbortExecute(Sender: TObject);
    procedure actAbortUpdate(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectFailedExecute(Sender: TObject);
    procedure actSelectFailedUpdate(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actClearUpdate(Sender: TObject);
    procedure actToggleSelectionExecute(Sender: TObject);
    procedure actEditLoggerPropsExecute(Sender: TObject);
    procedure actDetachLoggerExecute(Sender: TObject);

  private
    FViewModel: IViewModel_VCLForms;
    FSecondaryLoggersItem: TActionClientItem;

    procedure CreateDynamicMenuItems( SecondaryLoggers: TList<ILoggerContainerFactory>);
    function  CreateAttachLoggerAction( const Factory: ILoggerContainerFactory): TAction;
    function  CreateLoggerAction( Template: TAction; const Factory1: ILoggerContainerFactory; const Logger: ITestLogger; const Properties: IInterface; const DisplayName: string): TAction;

  protected
    procedure Loaded; override;
  end;

var
  mfmGUIRunner: TmfmGUIRunner;

implementation






uses DUnitX.InternalInterfaces;
{$R *.dfm}

type
TFactoryAction = class( TAction)
  public
    FFactory: ILoggerContainerFactory;
    constructor Create( AOwner: TComponent; const Factory: ILoggerContainerFactory);
  end;

TLoggerAction = class( TAction)
  public
    FFactory: ILoggerContainerFactory;
    FLogger: ITestLogger;
    FProps: IInterface;
    constructor Create( AOwner: TComponent; const Factory1: ILoggerContainerFactory; const Logger: ITestLogger; const Props: IInterface);
  end;

TConcreteView = class( TViewModel_VCLForms)
  protected
    procedure OnTestingStarts(const threadId, testCount, testActiveCount : Cardinal);                 override;
    procedure OnTestingEnds(const RunResults: IRunResults);                                           override;
    function TreeOwner: TComponent;         override;
    function TreeParent: TWinControl;       override;
    function TreeName: string;              override;
    procedure IntegrateSecondariesIntoMenus;              override;
    procedure AttachVisualTree;                           override;
    procedure ClearLog;                                   override;
    procedure Put( Level: TPutLevel; const Line: string); override;
    procedure SetDisplayState( Value: TSuiteRunnerState); override;
    procedure Breathe;                                    override;
    procedure InitiateView( TestCaseCount: integer);      override;
    procedure EnterOperation( isEntering: boolean);       override;

  private
    function  GetShowProgressBar: boolean;
    procedure SetShowProgressBar( Value: boolean);
    function  GetProgressPosition: integer;
    procedure SetProgressPosition( Value: integer);
    function  GetProgressMax: integer;
    procedure SetProgressMax( Value: integer);
    function  Form: TmfmGUIRunner;

    property ShowProgressBar: boolean     read GetShowProgressBar  write SetShowProgressBar;
    property ProgressPosition: integer    read GetProgressPosition write SetProgressPosition;
    property ProgressMax: integer         read GetProgressMax      write SetProgressMax;
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


function TmfmGUIRunner.CreateLoggerAction( Template: TAction; const Factory1: ILoggerContainerFactory; const Logger: ITestLogger; const Properties: IInterface; const DisplayName: string): TAction;
begin
result := TLoggerAction.Create( Template.Owner, Factory1, Logger, Properties);
result.ActionList := Template.ActionList;
AssignAction( result, Template);
if Pos( '%s', result.Caption) > 0 then
  result.Caption  := Format( result.Caption, [DisplayName]);
result.Category := '(dynamic)'

end;

procedure TmfmGUIRunner.actAbortExecute(Sender: TObject);
begin
// Not implemented yet.
end;

procedure TmfmGUIRunner.actAbortUpdate(Sender: TObject);
begin
// Not implemented yet.
end;

procedure TmfmGUIRunner.actAttachLoggerExecute( Sender: TObject);
var
  Logger: ITestLogger;
  Properties: IInterface;
  Addend: TActionClientItem;
  DisplayName: string;
  Secondary: TActionClientItem;
begin
  // For each attached secondary logger, we will also have ...
  //   - Logger properties
  //   - Detach
with Sender as TFactoryAction do
  begin
  if not FFactory.CreateLogger( Logger, Properties, DisplayName) then exit;
  FViewModel.AttachSecondaryLogger( Logger);
  Secondary := FSecondaryLoggersItem.Items.Add;
  Secondary.Caption := DisplayName;
  Addend := Secondary.Items.Add;
  Addend.Action := CreateLoggerAction( actEditLoggerProps, FFactory, Logger, Properties, DisplayName);
  Addend := Secondary.Items.Add;
  Addend.Action := CreateLoggerAction( actDetachLogger, FFactory, Logger, Properties, DisplayName)
  end;
end;

procedure TmfmGUIRunner.actClearExecute(Sender: TObject);
begin
FViewModel.ClearSelections
end;

procedure TmfmGUIRunner.actClearLogExecute( Sender: TObject);
begin
FViewModel.ClearLog
end;

procedure TmfmGUIRunner.actClearUpdate( Sender: TObject);
begin
(Sender as TAction).Enabled := FViewModel.CanClearSelections
end;

procedure TmfmGUIRunner.actDetachLoggerExecute(Sender: TObject);
begin
ShowMessage( 'Not implemented yet.')
end;

procedure TmfmGUIRunner.actEditLoggerPropsExecute( Sender: TObject);
begin
with Sender as TLoggerAction do
  FFactory.DisplayProperties( FLogger, FProps)
end;

procedure TmfmGUIRunner.actRunExecute( Sender: TObject);
begin
FViewModel.Run
end;

procedure TmfmGUIRunner.actSelectAllExecute( Sender: TObject);
begin
FViewModel.SelectAll
end;

procedure TmfmGUIRunner.actSelectFailedExecute( Sender: TObject);
begin
FViewModel.SelectFailed
end;

procedure TmfmGUIRunner.actSelectFailedUpdate( Sender: TObject);
begin
(Sender as TAction).Enabled := FViewModel.CanSelectFailed
end;

procedure TmfmGUIRunner.actToggleSelectionExecute( Sender: TObject);
begin
FViewModel.ToggleSelections
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

procedure TmfmGUIRunner.FormCreate(Sender: TObject);
begin
memoLog.Clear;
Caption := 'DUnitX Suite Runner: ' + '<variable part inserted here>'
end;

procedure TmfmGUIRunner.FormDestroy( Sender: TObject);
begin
FViewModel.FormDestroyed
end;




procedure TConcreteView.AttachVisualTree;
begin
end;

procedure TConcreteView.Breathe;
begin
Application.ProcessMessages
end;


procedure TConcreteView.ClearLog;
begin
Form.memoLog.Clear
end;

procedure TConcreteView.EnterOperation( isEntering: boolean);
begin
inherited;
if isEntering then
    Form.actmngrMain.State := asSuspended
  else
    Form.actmngrMain.State := asNormal
end;

function TConcreteView.Form: TmfmGUIRunner;
begin
result := FForm as TmfmGUIRunner
end;

function TConcreteView.GetProgressMax: integer;
begin
result := Form.pbarTests.Max
end;

function TConcreteView.GetProgressPosition: integer;
begin
result := Form.pbarTests.Position
end;

function TConcreteView.GetShowProgressBar: boolean;
begin
result := Form.pbarTests.Visible
end;

procedure TConcreteView.InitiateView( TestCaseCount: integer);
begin
inherited;
ShowProgressBar := False;
ProgressPosition := 0;
ProgressMax := TestCaseCount
end;

procedure TConcreteView.IntegrateSecondariesIntoMenus;
begin
Form.CreateDynamicMenuItems( FSecondaryLoggerContainerFactories)
end;







procedure TConcreteView.OnTestingStarts(
  const threadId, testCount, testActiveCount: Cardinal);
begin // STEP 1
ProgressPosition := 0;
ProgressMax      := testCount;
inherited
end;

procedure TConcreteView.OnTestingEnds( const RunResults: IRunResults);
begin
  // Do nothing
end;

procedure TConcreteView.Put( Level: TPutLevel; const Line: string);
var
  Size1: integer;
  Style1: TFontStyles;
  Colour1: TColor;
begin
case Level of
  lvDebug:      begin
                Size1   := 8;
                Style1  := [];
                Colour1 := clDkGray
                end;

  lvNormal:     begin
                Size1   := 10;
                Style1  := [];
                Colour1 := clBlack
                end;

  lvHighLight:  begin
                Size1   := 12;
                Style1  := [fsBold];
                Colour1 := clRed
                end;
  end;
with Form.memoLog do
  begin
  Form.memoLog.GetTextLen;
  SelAttributes.Size  := Size1;
  SelAttributes.Style := Style1;
  SelAttributes.Color := Colour1;
  SelText := Line + #13#10
  end
end;

procedure TConcreteView.SetDisplayState( Value: TSuiteRunnerState);
const
  StateStrings: array[ TSuiteRunnerState] of string = (
  // rsIdle, rsSettingUp, rsExecuting, rsTearingDown, rsBetweenTests
     'Idle', 'Setting up', 'Executing', 'Tearing down', 'In between');
begin
inherited SetDisplayState( Value);
Form.sbarMain.Panels[0].Text := StateStrings[ Value];
ShowProgressBar := Value <> rsIdle
end;

procedure TConcreteView.SetProgressMax( Value: integer);
begin
Form.pbarTests.Max := Value;
Form.sbarMain.Panels[2].Text := Format( 'of %d', [Value])
end;

procedure TConcreteView.SetProgressPosition( Value: integer);
begin
Form.pbarTests.Position := Value;
Form.sbarMain.Panels[1].Text := Format( 'Test cases = %d', [Value])
end;

procedure TConcreteView.SetShowProgressBar( Value: boolean);
begin
Form.pbarTests.Visible := Value
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

constructor TLoggerAction.Create(AOwner: TComponent; const Factory1: ILoggerContainerFactory; const Logger: ITestLogger;
  const Props: IInterface);
begin
inherited Create( AOwner);
FFactory := Factory1;
FLogger  := Logger;
FProps   := Props
end;

end.
