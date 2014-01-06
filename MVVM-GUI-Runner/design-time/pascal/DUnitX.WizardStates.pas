unit DUnitX.WizardStates;
interface
uses DUnitX.LaunchWizardForm;

const
  LitmusFile = 'DUnitX.TestFramework.pas';

type
///<summary>
///  Input to data to the Wizard dialog forms.
///</summary>
RWizardInitialState = record
    ///<summary>
    ///  True if there is no pre-existing Project Group and we must create one.
    ///  False if there is a pre-existing Project Group. The user decides if we
    ///   will use this one, or create a new one.
    ///</summary>
    FisNewProjectGroup: boolean;

    ///<summary>
    ///  If there was a pre-existing project, this is the unqualified name.
    ///  Otherwise CurrentProjectName is empty.
    ///</summary>
    FCurrentProjectName: string;

    ///<summary>
    ///  If there was a pre-existing project, this is the absolute file path to the project head.
    ///  Otherwise CurrentProjectName is empty. No trailing path delimiter.
    ///</summary>
    FCurrentProjectPath: string;

    ///<summary>
    ///  If there was a pre-existing project, this the active platform selection.
    ///  Otherwise it is Win32. For D2010, it is always win32.
    ///  Only supported DUnitX platforms count. For the moment, this is Win32 and Win64.
    ///  We hope to add support for unit testing on other platforms.
    ///</summary>
    FCurrentPlatform: TPlatform;

    ///<summary>
    ///  Resolution of $(BDSPROJECTSDIR).
    ///</summary>
    FBDSProjectPath: string;

    ///<summary>
    ///  Absolute path to the DUnitX base directory. The client of the wizard
    ///  forms should determine this by scanning the IDE library path list
    ///  for the active platform of the current project, and look for the first path
    ///  that contains the file 'DUnitX.TestFramework.pas'.
    ///  No trailing path delimiter.
    ///</summary>
    FLibraryAbsolutePath: string;

    constructor Create( isNewProjectGroup1: boolean;
                        const CurrentProjectName1, CurrentProjectPath1: string;
                        CurrentPlatform1: TPlatform;
                        const BDSProjectPath1, LibraryAbsolutePath: string);
  end;

IWizardInitialState = interface
  ['{D994D7C5-F2BA-459A-BADE-0F01C098D2B1}']
    function Data: RWizardInitialState;
    function Clone: IWizardInitialState;
  end;
function CreateWizardInitialState( const Data: RWizardInitialState):  IWizardInitialState;

type
///<summary>
///  Output from the Wizard dialog forms and enough data to generate our
///  DUnitX unit testing application.
///</summary>
RWizardFinalState = record
    ///<summary>
    ///  If True, we should create a new Project Group.
    ///  If False, we will add to the pre-existing Project Group.
    ///</summary>
    FdoCreateNewProjectGroup: boolean;

    ///<summary>
    ///  The project name to be.
    ///</summary>
    FUnitTestingProjectName: string;

    ///<summary>
    ///  The absolute fully qualified file path of the project head to be generated.
    ///  No trailing path delimiter.
    ///</summary>
    FUnitTestingLocation: string;

    ///<summary>
    ///  The Application Title of the project to be generated.
    ///</summary>
    FApplicationTitle: string;

    ///<summary>
    ///  The class of tree to be used by the GUI view.
    ///</summary>
    FTree: TTreeViewChoice;

    ///<summary>
    ///  The active and only supported platform of the project to be generated.
    ///  At the moment, this is constrained to Win32 and Win64.
    ///</summary>
    FPlatform: TPlatform;

    ///<summary>
    ///  The view paradigm of the generated project. At the moment, this
    ///   is constrained to be GUI.
    ///</summary>
    FView: TViewParadigm;

    ///<summary>
    ///  Absolute file path to the DUnitX base directory, as set by the user.
    ///  The default value is RWizardInitialState.FLibraryAbsolutePath.
    ///</summary>
    FLibraryAbsolutePath: string;

    ///<summary>
    ///  Relative file path to the DUnitX base directory.
    ///  Relative path is computed from Absolute path.
    ///  No trailing path delimiter.
    ///</summary>
    FLibraryRelativePath: string;


    procedure CreateDefault;
  end;

IWizardFinalState = interface
  ['{A579E012-C99E-43BB-8392-1A56CB179A47}']
    function Data: RWizardFinalState;
    function Clone: IWizardFinalState;
  end;

type
///<summary>
///  Common intermediate state between pages.
///</summary>
RWizardIntermediateState = record
    FInit: RWizardInitialState;
    FFinl: RWizardFinalState;
  end;
IWizardIntermediateState = interface
  ['{B17260C2-B91D-4661-81B7-488165221C04}']
    function  State: RWizardIntermediateState;
    function  FinalState: IWizardFinalState;
    function  Clone: IWizardIntermediateState;
    procedure SetDoCreateNewProjectGroup( Value: boolean);
    procedure SetUnitTestingProjectName( const Value: string);
    procedure SetUnitTestingLocation( const Value: string);
    procedure SetApplicationTitle( const Value: string);
    procedure SetTree( Value: TTreeViewChoice);
    procedure SetPlatform( Value: TPlatform);
    procedure SetView( Value: TViewParadigm);
    procedure SetLibraryAbsolutePath( const Value: string);  // Also sets relative path
  end;
function CreateWizardIntermediateState( const InitialState: RWizardInitialState): IWizardIntermediateState;
function AbsPathToRelPath( const AbsPath, BasePath: string): string;


implementation




uses SysUtils, IOUtils, ShLwApi, Windows;

type
TWizardInitialState = class( TInterfacedObject, IWizardInitialState)
  private
    FData: RWizardInitialState;
    function Data: RWizardInitialState;
    function Clone: IWizardInitialState;
    constructor Create( const Data1: RWizardInitialState);
  end;

TWizardFinalState = class( TInterfacedObject, IWizardFinalState)
  private
    FData: RWizardFinalState;
    function Data: RWizardFinalState;
    function Clone: IWizardFinalState;
    constructor Create( const Data1: RWizardFinalState);
  end;

TWizardIntermediateState = class( TInterfacedObject, IWizardIntermediateState)
  private
    FState: RWizardIntermediateState;
    function  State: RWizardIntermediateState;
    function  FinalState: IWizardFinalState;
    function  Clone: IWizardIntermediateState;
    procedure SetDoCreateNewProjectGroup( Value: boolean);
    procedure SetUnitTestingProjectName( const Value: string);
    procedure SetUnitTestingLocation( const Value: string);
    procedure SetApplicationTitle( const Value: string);
    procedure SetTree( Value: TTreeViewChoice);
    procedure SetPlatform( Value: TPlatform);
    procedure SetView( Value: TViewParadigm);
    procedure SetLibraryAbsolutePath( const Value: string);
    procedure UpdateRelativePath;
    constructor Create( const InitialState1: RWizardInitialState);
  end;

function CreateWizardInitialState( const Data: RWizardInitialState):  IWizardInitialState;
begin
result := TWizardInitialState.Create( Data)
end;

function CreateWizardIntermediateState( const InitialState: RWizardInitialState): IWizardIntermediateState;
begin
result := TWizardIntermediateState.Create( InitialState)
end;




constructor RWizardInitialState.Create(
  isNewProjectGroup1: boolean;
  const CurrentProjectName1, CurrentProjectPath1: string;
  CurrentPlatform1: TPlatform; const BDSProjectPath1,
  LibraryAbsolutePath: string);
begin
FisNewProjectGroup   := isNewProjectGroup1;
FCurrentProjectName  := CurrentProjectName1;
FCurrentProjectPath  := ExcludeTrailingPathDelimiter( CurrentProjectPath1);
FCurrentPlatform     := CurrentPlatform1;
FBDSProjectPath      := BDSProjectPath1;
FLibraryAbsolutePath := ExcludeTrailingPathDelimiter( LibraryAbsolutePath)
end;


function TWizardInitialState.Clone: IWizardInitialState;
var
  Newbie: TWizardInitialState;
begin
Newbie := inherited Create;
Newbie.FData := FData;
result := Newbie
end;

constructor TWizardInitialState.Create( const Data1: RWizardInitialState);
begin
FData := Data1
end;

function TWizardInitialState.Data: RWizardInitialState;
begin
result := FData
end;


function TWizardFinalState.Clone: IWizardFinalState;
var
  Newbie: TWizardFinalState;
begin
Newbie := inherited Create;
Newbie.FData := FData;
result := Newbie
end;

constructor TWizardFinalState.Create( const Data1: RWizardFinalState);
begin
FData := Data1
end;

function TWizardFinalState.Data: RWizardFinalState;
begin
result := FData
end;


function TWizardIntermediateState.Clone: IWizardIntermediateState;
var
  Newbie: TWizardIntermediateState;
begin
Newbie := inherited Create;
Newbie.FState := FState;
result := Newbie
end;

constructor TWizardIntermediateState.Create(
  const InitialState1: RWizardInitialState);
begin
FState.FInit := InitialState1;
FState.FFinl.CreateDefault;
FState.FFinl.FdoCreateNewProjectGroup := InitialState1.FisNewProjectGroup;
FState.FFinl.FLibraryAbsolutePath     := FState.FInit.FLibraryAbsolutePath;
UpdateRelativePath
end;

function TWizardIntermediateState.FinalState: IWizardFinalState;
begin
result := TWizardFinalState.Create( FState.FFinl)
end;

procedure TWizardIntermediateState.SetApplicationTitle( const Value: string);
begin
FState.FFinl.FApplicationTitle := Value
end;

procedure TWizardIntermediateState.SetDoCreateNewProjectGroup( Value: boolean);
begin
FState.FFinl.FdoCreateNewProjectGroup := Value
end;

procedure TWizardIntermediateState.SetLibraryAbsolutePath( const Value: string);
begin
FState.FFinl.FLibraryAbsolutePath := ExcludeTrailingPathDelimiter( Value);
UpdateRelativePath
end;

procedure TWizardIntermediateState.SetPlatform( Value: TPlatform);
begin
FState.FFinl.FPlatform := Value
end;

procedure TWizardIntermediateState.SetTree( Value: TTreeViewChoice);
begin
FState.FFinl.FTree := Value
end;

procedure TWizardIntermediateState.SetUnitTestingLocation( const Value: string);
begin
FState.FFinl.FUnitTestingLocation := ExcludeTrailingPathDelimiter( Value);
UpdateRelativePath
end;

procedure TWizardIntermediateState.SetUnitTestingProjectName( const Value: string);
begin
FState.FFinl.FUnitTestingProjectName := Value
end;

procedure TWizardIntermediateState.SetView( Value: TViewParadigm);
begin
FState.FFinl.FView := Value
end;

function TWizardIntermediateState.State: RWizardIntermediateState;
begin
result := FState
end;


function AbsPathToRelPath( const AbsPath, BasePath: string): string;
// Copied from Andreas Rejbrand (http://stackoverflow.com/users/282848/andreas-rejbrand)
// http://stackoverflow.com/questions/5329472
// TODO: Find solution for non-Windows platforms.
var
  Path: array[0..MAX_PATH-1] of char;
begin
if AbsPath = BasePath then
    result := ''  // Same place. Use empty rather than '.' .
  else if AbsPath = '' then
    result := ''  // Path is unknown or not yet intialized.
  else if BasePath = '' then
    result := AbsPath // No base. So Absolute is the best we can get
  else if PathRelativePathTo( @Path[0], PChar( BasePath), FILE_ATTRIBUTE_DIRECTORY, PChar( AbsPath), 0) then
    result := ExcludeTrailingPathDelimiter( Path)
  else
    result := AbsPath; // The computation bombed for some reason. So Absolute is the best we can get.
if Copy( result, 1, 2) = '.\' then
  System.Delete( result, 1, 2)
end;

procedure TWizardIntermediateState.UpdateRelativePath;
// Either FFinl.FLibraryAbsolutePath or FFinl.FUnitTestingLocation has changed,
//  so maintain the invariant relationship with FLibraryRelativePath.
begin
FState.FFinl.FLibraryRelativePath := AbsPathToRelPath( FState.FFinl.FLibraryAbsolutePath, FState.FFinl.FUnitTestingLocation)
end;

procedure RWizardFinalState.CreateDefault;
begin
FdoCreateNewProjectGroup := True;
FUnitTestingProjectName  := 'UnitTests';
FUnitTestingLocation     := ExcludeTrailingPathDelimiter( TDirectory.GetCurrentDirectory);
FApplicationTitle        := 'Unit tests';
FTree                    := tvDUnitXVTree;
FPlatform                := pWin32;
FView                    := vMVVM_GUI;
FLibraryAbsolutePath     := '';
FLibraryRelativePath     := ''
end;

end.
