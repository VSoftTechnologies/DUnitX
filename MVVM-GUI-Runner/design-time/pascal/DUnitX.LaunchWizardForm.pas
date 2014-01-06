unit DUnitX.LaunchWizardForm;
interface
uses DUnitX.WizardPageIntf, DUnitX.IoC;


type
TTreeViewChoice = (tvDUnitXVTree,    // The DUnitX bundled TVirtualStringTree
                   tvUserVTree,      // The users's pre-installed TVirtualStringTree
                   tvWinTreeView);   // Windows TTreeView

TPlatform = (pWin32, pWin64);
TViewParadigm = (vMVVM_GUI, vConsole);


///<summary>
///  Launch the form wizard and enquire parameters from the user.
///  For parameter definitions see DUnitX.WizardStates
///</summary>
function LaunchWizardForm(
    var   isNewProjectGroup: boolean;      // In and out
    const CurrentProjectName: string;      // In
    const CurrentProjectPath: string;      // In
    var   CurrentPlatform: TPlatform;      // In and out
    const BDSProjectPath: string;          // In
    var   LibraryAbsolutePath: string;     // In and out
    var   UnitTestingProjectName: string;  // Out
    var   UnitTestingLocation: string;     // Out
    var   ApplicationTitle: string;        // Out
    var   Tree: TTreeViewChoice;           // Out
    var   View: TViewParadigm;             // Out
    var   LibraryRelativePath: string)     // Out
    : boolean;  // True if accepted. False if user cancelled.

type
ILaunchWizardFormService = interface
  ['{2D69AF4A-8B28-496E-8FA4-E7161285E73F}']
    function LaunchWizardForm(
        var   isNewProjectGroup: boolean;      // In and out
        const CurrentProjectName: string;      // In
        const CurrentProjectPath: string;      // In
        var   CurrentPlatform: TPlatform;      // In and out
        const BDSProjectPath: string;          // In
        var   LibraryAbsolutePath: string;     // In and out
        var   UnitTestingProjectName: string;  // Out
        var   UnitTestingLocation: string;     // Out
        var   ApplicationTitle: string;        // Out
        var   Tree: TTreeViewChoice;           // Out
        var   View: TViewParadigm;             // Out
        var   LibraryRelativePath: string)     // Out
        : boolean;  // True if accepted. False if user cancelled.
  end;


procedure RegisterStockLaunchWizardService( const ServiceContainer: TDUnitXIoC);

implementation












uses
 {$if CompilerVersion >= 23}
    // XE2+
    Vcl.Controls,
  {$else}
    // D2010, XE
    Controls,
  {$ifend}
    DUnitX.fmProjectWizard, DUnitX.Generics, StrUtils,
    DUnitX.frmMainDetailsPage, SysUtils, DUnitX.frmPathPage,
    DUnitX.frmMultipleChoicePage, DUnitX.frmReadMemoPage,
    DUnitX.WizardStates;


type
TDUnitXWizardPageGang = class( TDUnitXList<IWizardPageFactory>, IWizardPageFactoryGang);

TStockLauncher = class( TInterfacedObject, ILaunchWizardFormService)
  private
    function LaunchWizardForm(
        var   isNewProjectGroup: boolean;
        const CurrentProjectName: string;
        const CurrentProjectPath: string;
        var   CurrentPlatform: TPlatform;
        const BDSProjectPath: string;
        var   LibraryAbsolutePath: string;
        var   UnitTestingProjectName: string;
        var   UnitTestingLocation: string;
        var   ApplicationTitle: string;
        var   Tree: TTreeViewChoice;
        var   View: TViewParadigm;
        var   LibraryRelativePath: string)
        : boolean;
  end;

procedure RegisterStockLaunchWizardService( const ServiceContainer: TDUnitXIoC);
begin
if not assigned( ServiceContainer) then exit;
ServiceContainer.RegisterType<ILaunchWizardFormService>( function: ILaunchWizardFormService
  begin
  result := TStockLauncher.Create
  end)
end;


function ComputeNextPage( const PreviousPage: string): string;
const
  // Constants to tweak during development. Take them out when finished.
  DoIncludeParadigm: Boolean = True;
var
  Visible: boolean;
begin
result := PreviousPage;
repeat
  if result = '' then
    result := 'main-details'           // Page 1
  else if result = 'main-details' then
    result := 'tree'                   // Page 2
  else if result = 'tree' then
    result := 'platform'               // Page 3
  else if result = 'platform' then
    result := 'paradigm'               // Page 4
  else if result = 'paradigm' then
    result := 'library'                // Page 5
  else if result = 'library' then
    result := 'read-me'                // Page 6
  else if result = 'read-me' then
    result := 'summary'                // Page 7
  else
    result := '';
  Visible := result <> '';

  // Now compute conditional page skips.
  {$IF CompilerVersion < 23.0}
    // D2010 and XE skip the platform page.
    if result = 'platform' then
      Visible := False;
  {$IFEND}

  if (not DoIncludeParadigm) and (result = 'paradigm') then
    Visible := False;

until Visible or (result = '')
end;


function BoolToString( doCreateNewProjectGroup: boolean): string;
begin
result := IfThen( doCreateNewProjectGroup, 'yes', 'no')
end;

function TreeViewChoiceToString( Tree: TTreeViewChoice): string;
const TreeStrs: array[ TTreeViewChoice] of string = (
  'The DUnitX bundled TVirtualStringTree',
  'Your pre-installed TVirtualStringTree',
  'Windows TTreeView');
begin
result := TreeStrs[ Tree]
end;


function PlatformToString( Platform1: TPlatform): string;
const PlatformStrs: array[ TPlatform] of string = (
  'Win32',
  'Win64');
begin
result := PlatformStrs[ Platform1]
end;


function ViewToString( View: TViewParadigm): string;
const ParadigmStrs: array[ TViewParadigm] of string = (
  'MV-V-M GUI',
  'Console');
begin
result := ParadigmStrs[ View]
end;



function LaunchWizardForm(
    var   isNewProjectGroup: boolean;
    const CurrentProjectName: string;
    const CurrentProjectPath: string;
    var   CurrentPlatform: TPlatform;
    const BDSProjectPath: string;
    var   LibraryAbsolutePath: string;
    var   UnitTestingProjectName: string;
    var   UnitTestingLocation: string;
    var   ApplicationTitle: string;
    var   Tree: TTreeViewChoice;
    var   View: TViewParadigm;
    var   LibraryRelativePath: string)
    : boolean;
var
  Environment: RWizardInitialState;
  UserData: RWizardFinalState;
  Wiz: TfmProjectWizard;
  Services: TDUnitXIoC;
begin
Environment.Create( isNewProjectGroup, CurrentProjectName, CurrentProjectPath,
                    CurrentPlatform, BDSProjectPath, LibraryAbsolutePath);
Wiz := TfmProjectWizard.Create( nil);
Services := TDUnitXIoC.Create;
try
  Services.RegisterType<IWizardPageFactoryGang>(
    function: IWizardPageFactoryGang
    begin
      // Page 1
      result := TDUnitXWizardPageGang.Create;
      result.Add( TfrmMainDetailsPage.TFactory.Create);

      // Page 2
      result.Add( TfrmMultipleChoice.TFactory.Create(
        {PageId = } 'tree',
        {NextId= } ComputeNextPage('tree'),
        {Title= } 'Select the tree view class',
        {InstructionText= } 'You can only select the pre-installed version of TVirtualStringTree if you'#13#10 +
                            'already have a copy installed. For more information see http://www.soft-gems.net',
        {GroupCaption= } 'Tree class',
        {Items= } '|The DUnitX bundled TVirtualStringTree' +
                  '|Your pre-installed TVirtualStringTree' +
                  '|Boring old TTreeView',
        {InitItemSelectionProc = } procedure( const PassInState: IInterface; var SelectedItem: integer)
          var
            State: IWizardIntermediateState;
          begin
          if Supports( PassInState, IWizardIntermediateState, State) then
              SelectedItem := Ord( State.State.FFinl.FTree)
          end,

        {ParseItemSelectionProc= } procedure( const PassInState: IInterface; SelectedItem: integer; var PassOutState: IInterface)
          var
            State: IWizardIntermediateState;
          begin
          if Supports( PassInState, IWizardIntermediateState, State) then
              begin
              State.SetTree( TTreeViewChoice( SelectedItem));
              PassOutState := State.Clone
              end
            else
              PassOutState := nil
          end
        ));

      // Page 3
      result.Add( TfrmMultipleChoice.TFactory.Create(
        {PageId = } 'platform',
        {NextId= } ComputeNextPage('platform'),
        {Title= } 'Select the initial project platform',
        {InstructionText= } 'OS X, iOS and Android are not yet supported.',
        {GroupCaption= } 'Target platform',
        {Items= } '|Win32' +
                  '|Win64',
        {InitItemSelectionProc = } procedure( const PassInState: IInterface; var SelectedItem: integer)
          var
            State: IWizardIntermediateState;
          begin
          if Supports( PassInState, IWizardIntermediateState, State) then
              SelectedItem := Ord( State.State.FFinl.FPlatform)
          end,

        {ParseItemSelectionProc= } procedure( const PassInState: IInterface; SelectedItem: integer; var PassOutState: IInterface)
          var
            State: IWizardIntermediateState;
          begin
          if Supports( PassInState, IWizardIntermediateState, State) then
              begin
              State.SetPlatform( TPlatform( SelectedItem));
              PassOutState := State.Clone
              end
            else
              PassOutState := nil
          end
        ));

      // Page 4
      result.Add( TfrmMultipleChoice.TFactory.Create(
        {PageId = } 'paradigm',
        {NextId= } ComputeNextPage('paradigm'),
        {Title= } 'Select the view paradigm',
        {InstructionText= } 'This page is a place-marker-only.'#13#10 +
                            'At the moment, we only support Model-View View Model GUI paradigm.',
        {GroupCaption= } 'View paradigm',
        {Items= } '|MVVM GUI' +
                  '|Console',
        {InitItemSelectionProc = } procedure( const PassInState: IInterface; var SelectedItem: integer)
          var
            State: IWizardIntermediateState;
          begin
          if Supports( PassInState, IWizardIntermediateState, State) then
              SelectedItem := Ord( State.State.FFinl.FView)
          end,

        {ParseItemSelectionProc= } procedure( const PassInState: IInterface; SelectedItem: integer; var PassOutState: IInterface)
          var
            State: IWizardIntermediateState;
          begin
          if Supports( PassInState, IWizardIntermediateState, State) then
              begin
              State.SetView( TViewParadigm( SelectedItem));
              PassOutState := State.Clone
              end
            else
              PassOutState := nil
          end
        ));

      // Page 5
      result.Add( TfrmPathPage.TFactory.Create);

      // Page 6: ReadMe (about what to do next)
      result.Add( TfrmReadMemo.TFactory.Create( 'read-me', 'summary', procedure( const PassInState: IInterface; var MemoText: string)
          var
            State: IWizardIntermediateState;
          begin
          if Supports( PassInState, IWizardIntermediateState, State) then
              begin
              MemoText := 'Read-me to be developed';
              end
          end));

      // Page 7: Summary of all options
      result.Add( TfrmReadMemo.TFactory.Create( 'summary', '', procedure( const PassInState: IInterface; var MemoText: string)
          var
            State: IWizardIntermediateState;
          begin
          if Supports( PassInState, IWizardIntermediateState, State) then
            with State.FinalState.Data do
              MemoText := Format(
                'Summary'#13#10 +
                '--------'#13#10 +
                #13#10 +
                'A DUnitX unit testing application will be generated with the following parameters:'#13#10 +
                'Create project in new project group = %0:s'#13#10 + // %0:s == FdoCreateNewProjectGroup
                'Project name = %1:s'#13#10 +                        // %1:s == FUnitTestingProjectName
                'Project location = %2:s'#13#10 +                    // %2:s == FUnitTestingLocation
                'Application title = %3:s'#13#10 +                   // %3:s = FApplicationTitle
                'Tree class = %4:s'#13#10 +                          // %4:s == FTree
                'Platform = %5:s'#13#10 +                            // %5:s == FPlatform
                'View paradigm = %6:s'#13#10 +                       // %6:s == FView
                'DUnitX library path (absolute) = %7:s'#13#10 +      // %7:s == FLibraryAbsolutePath
                'DUnitX library path (relative) = %8:s'#13#10,       // %8:s == FLibraryRelativePath
                [
                 {%0:s} BoolToString( FdoCreateNewProjectGroup),
                 {%1:s} FUnitTestingProjectName,
                 {%2:s} FUnitTestingLocation,
                 {%3:s} FApplicationTitle,
                 {%4:s} TreeViewChoiceToString( FTree),
                 {%5:s} PlatformToString( FPlatform),
                 {%6:s} ViewToString( FView),
                 {%7:s} FLibraryAbsolutePath,
                 {%8:s} FLibraryRelativePath
                ])
          end));
    end,
    '');
  Wiz.FServices  := Services;
  Wiz.FStartPage := ComputeNextPage('');
  Wiz.FEnvironment := CreateWizardInitialState( Environment);
  result := Wiz.ShowModal = mrOk;
  if result then
    UserData := Wiz.FUserData.Data
finally
  Services.Free;
  Wiz.Free
  end;
if result then
  begin
  isNewProjectGroup      := UserData.FdoCreateNewProjectGroup;
  CurrentPlatform        := UserData.FPlatform;
  LibraryAbsolutePath    := UserData.FLibraryAbsolutePath;
  UnitTestingProjectName := UserData.FUnitTestingProjectName;
  UnitTestingLocation    := UserData.FUnitTestingLocation;
  ApplicationTitle       := UserData.FApplicationTitle;
  Tree                   := UserData.FTree;
  View                   := UserData.FView;
  LibraryRelativePath    := UserData.FLibraryRelativePath
  end
end;


function TStockLauncher.LaunchWizardForm(var isNewProjectGroup: boolean;
  const CurrentProjectName, CurrentProjectPath: string;
  var CurrentPlatform: TPlatform; const BDSProjectPath: string;
  var LibraryAbsolutePath, UnitTestingProjectName, UnitTestingLocation,
  ApplicationTitle: string; var Tree: TTreeViewChoice; var View: TViewParadigm;
  var LibraryRelativePath: string): boolean;
begin
// The stock launcher is stateless and just defaults to the static function.
result := DUnitX.LaunchWizardForm.LaunchWizardForm(
  isNewProjectGroup, CurrentProjectName, CurrentProjectPath,
  CurrentPlatform, BDSProjectPath, LibraryAbsolutePath, UnitTestingProjectName,
  UnitTestingLocation, ApplicationTitle, Tree, View, LibraryRelativePath)
end;

end.
