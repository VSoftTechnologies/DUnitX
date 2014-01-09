unit DUnitX.Wizard;
interface
uses
  ToolsAPI, DUnitX.IoC, DUnitX.IDE_API;

type
TProjectWizard = class( TNotifierObject,
                        IOTAWizard, IOTARepositoryWizard, IOTARepositoryWizard60,
                        IOTARepositoryWizard80,
                        IOTAProjectWizard, IOTAProjectWizard100)
  protected
    function  GetIDString: string;
    function  GetName: string;
    function  GetState: TWizardState;
    procedure Execute;                             virtual;   // Rarely override
    function  GetMenuText: string;
    function  GetAuthor: string;                   virtual;   // Frequently override
    function  GetComment: string;                  virtual;   // Frequently override
    function  GetPage: string;
    function  GetGlyph: Cardinal;
    function  GetDesigner: string;
    function  GetGalleryCategory: IOTAGalleryCategory;    virtual; // Rarely override
    function  GetPersonality: string;
    function  IsVisible( Project: IOTAProject): boolean;
    function  CurrentProject: IOTAProject;
    function  GetProjectGroup: IOTAProjectGroup;
    function  CreateModule( const TemplateFN, StylesheetFN: string; const Owner: IOTAModule): IOTAModule;

  private
    procedure FindLibrary( const sActivePlatform: string);

  protected
    function  GetGalleryCategoryStringId: string;        virtual;  // Frequently override
    procedure AdjustUnitNamespaces;

  private
    FServices: TDUnitXIoC;
    FIDE: IIDE_API;
    FLibraryAbsolutePath: string;
    FUnitTestingLocation, FUnitTestingProjectName: string;
    FTreeId: string;

  public
    constructor Create( Services1: TDUnitXIoC);
    destructor Destroy; override;
  end;

implementation

















uses
  {$if CompilerVersion >= 23}
    // XE2+
    DCCStrs,
  {$else}
    // D2010, XE
  {$ifend}
    SysUtils, HTTPProd, Classes, Windows, DUnitX.LaunchWizardForm, IOUtils,
    DUnitX.Utils.XML2, XmlIntf, DUnitX.WizardStates;


type

TIOTACreator = class( TNotifierObject, IInterface, IOTACreator,
    IOTAProjectCreator, IOTAProjectCreator50, IOTAProjectCreator80,
    IOTAModuleCreator)
  private
    FStyleSheet: TStyleSheet;
    FDUnitXLibraryPath: string; // Path to the DUnitX base directory.
    FTemplateFileName: string; // like 'GUIRunner.dpr.template'
    FStyleSheetFileName: string; // like 'IOTAConstruct.xsl'
    FProjectLocation: string; // Path to where the generated dpr will be located.
    FIDE: IIDE_API;
    FDoc: IXMLDocument;
    FUnitTestingProjectName: string;
    FOwner: IOTAModule;
    FTreeId: string;

  protected
    function QueryInterface( const IID: TGUID; out Obj): HResult; stdcall;

  private
    function  GetCreatorType: string;
    function  GetExisting: Boolean;
    function  GetFileSystem: string;
    function  GetOwner: IOTAModule;
    function  GetUnnamed: Boolean;
    function  GetFileName: string;
    function  GetOptionFileName: string;
    function  GetShowSource: boolean;
    procedure NewDefaultModule; deprecated;
    function  NewOptionSource( const ProjectName: string): IOTAFile;
    procedure NewProjectResource( const Project: IOTAProject);
    function  NewProjectSource( const ProjectName: string): IOTAFile;
    procedure NewDefaultProjectModule( const Project: IOTAProject);
    function  GetProjectPersonality: string;
    function  GetAncestorName: string;
    function  GetImplFileName: string;
    function  GetIntfFileName: string;
    function  GetFormName: string;
    function  GetMainForm: boolean;
    function  GetShowForm: boolean;
    function  NewFormFile( const FormIdent, AncestorIdent: string): IOTAFile;
    function  NewImplSource( const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function  NewIntfSource( const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated( const FormEditor: IOTAFormEditor);

    function PredicateToSupportCompiler: string;
    function SupportsPathExpression( const sInterfaceName: string): string;
    function SupportsProjectCreator: boolean;
    function SupportsModuleCreator: boolean;
    function TemplateDocNode: IXMLNode;
    function TemplateNodeExists( const XPathExpression: string): boolean;
    function Transform( const SourceType, ModuleName: string; var Output: string): boolean;
    function ComputeSource( const FileName, SourceType, ModuleName: string): IOTAFile;
    function LookUpCreatorString( const PathFromt_IOTACreator: string): string;

 public
   constructor Create(
          const IDE1: IIDE_API;
          const DUnitXLibraryPath1, TemplateFileName1, UnitTestingLocation1,
                UnitTestingProjectName1, StyleSheetName1: string;
          const TreeId1: string;
          const Owner1: IOTAModule);
   destructor  Destroy; override;

 public
   function CreateModules: IOTAModule;

 private type
   TOTASavedFile = class( TInterfacedObject, IOTAFile)
     private
       FSource: string;
       FAge: TDateTime;
       function GetSource: string; virtual;
       function GetAge: TDateTime; virtual;
     public
       constructor Create( const FileName, Source: string);
     end;
 end;


function TProjectWizard.GetProjectGroup: IOTAProjectGroup;
var
  i: Integer;
  Svc: IOTAModuleServices;
begin
Svc := FIDE.IOTAModuleServices;
if assigned( Svc) then
    begin
    result := Svc.MainProjectGroup;
    if not assigned( result) then
      for i := 0 to Svc.ModuleCount - 1 do
        if Supports( Svc.Modules[I], IOTAProjectGroup, result) then
          break
    end
  else
    result := nil
end;

constructor TProjectWizard.Create( Services1: TDUnitXIoC);
begin
FServices := Services1;
FIDE := FServices.Resolve<IIDE_API>()
end;

function TProjectWizard.CurrentProject: IOTAProject;
begin
result := FIDE.GetActiveProject
end;

destructor TProjectWizard.Destroy;
begin
FServices.Free; // Ownership was transferred, so need to destroy it.
inherited
end;

procedure TProjectWizard.FindLibrary( const sActivePlatform: string);
var
  Paths: TStrings;
  Path: string;
begin;
if FLibraryAbsolutePath <> '' then exit;
Paths := TStringList.Create;
try
  FIDE.GetLibraryPaths( sActivePlatform, Paths);
  for Path in Paths do
    begin
    if not TFile.Exists( Path + '\' + LitmusFile) then continue;
    FLibraryAbsolutePath := Path;
    break
    end
finally
  Paths.Free
  end;
end;

procedure TProjectWizard.Execute;
var
  Wiz: ILaunchWizardFormService;
  Ok : boolean;
  isNewProjectGroup: boolean;
  CurrentProjectName: string;
  CurrentProjectPath: string;
  CurrentPlatform: TPlatform;
  BDSProjectPath: string;
  ApplicationTitle: string;
  Tree: TTreeViewChoice;
  View: TViewParadigm;
  LibraryRelativePath: string;
  Proj: IOTAProject;
  sActivePlatform: string;
  {$if CompilerVersion >= 23}
    O: IOTAProjectOptionsConfigurations;
  {$ifend}
  Creator: TIOTACreator;
  Group: IOTAProjectGroup;
  BaseConfig: IOTABuildConfiguration;
begin
isNewProjectGroup  := GetProjectGroup <> nil;
Proj := CurrentProject;
sActivePlatform := 'Win32';
if assigned( Proj) then
    begin
    {$if CompilerVersion >= 24}
      // Defined in XE3+
      if Supports( Proj.ProjectOptions, IOTAProjectOptionsConfigurations, O) then
          sActivePlatform := O.ActivePlatformName
        else
          sActivePlatform := Proj.CurrentPlatform;
    {$else}
      // Not defined in D2010, XE. Unknown in XE2.
    {$ifend}
    CurrentProjectName := TPath.GetFileNameWithoutExtension( Proj.FileName);
    CurrentProjectPath := TPath.GetDirectoryName( Proj.FileName);
    end
  else
    begin
    CurrentProjectName := '';
    CurrentProjectPath := '';
    end;
if SameText( sActivePlatform, 'Win32') then
    CurrentPlatform := pWin32
  else if SameText( sActivePlatform, 'Win64') then
    CurrentPlatform := pWin64
  else
    CurrentPlatform := pWin32;
FindLibrary( sActivePlatform);
Wiz := FServices.Resolve<ILaunchWizardFormService>;
if assigned( Wiz) then
    Ok := Wiz.LaunchWizardForm(
        isNewProjectGroup,
        CurrentProjectName,
        CurrentProjectPath,
        CurrentPlatform,
        BDSProjectPath,
        FLibraryAbsolutePath,
        FUnitTestingProjectName,
        FUnitTestingLocation,
        ApplicationTitle,
        Tree,
        View,
        LibraryRelativePath)
  else
    Ok := LaunchWizardForm(
        isNewProjectGroup,
        CurrentProjectName,
        CurrentProjectPath,
        CurrentPlatform,
        BDSProjectPath,
        FLibraryAbsolutePath,
        FUnitTestingProjectName,
        FUnitTestingLocation,
        ApplicationTitle,
        Tree,
        View,
        LibraryRelativePath);
if not Ok then exit;
if isNewProjectGroup then
    begin
    Group := nil;
    (BorlandIDEServices as IOTAModuleServices).CloseAll
    end
  else
    Group := GetProjectGroup;
case Tree of
  tvDUnitXVTree: FTreeId := 'DxVTree';    // The DUnitX bundled TVirtualStringTree
  tvUserVTree  : FTreeId := 'VTree';      // The users's pre-installed TVirtualStringTree
  tvWinTreeView: FTreeId := 'WinTree';    // TreeView
  end;
CreateModule( 'GUIRunner.dpr.template'        , 'IOTAConstruct.xsl', Group as IOTAModule);
CreateModule( 'DUnitX.uExecutive.pas.template', 'IOTAConstruct.xsl', CurrentProject as IOTAModule);
AdjustUnitNamespaces;
// TODO:
//  Research. There doesn't appear to be a way in ToolsAPI to add a platform.
//   I will post a question, either on StackOverflow or Embarcadero forums.
end;

procedure TProjectWizard.AdjustUnitNamespaces;
// TODO:
//   Acquire the Namespace from the template, so we can support Fmx.
const
  sFrameworkNamespace = 'Vcl'; // This to become dynamic.
var
  sNamespaceList: string;
begin
{$if CompilerVersion >= 23} // XE2+
//    This bit not finished yet. Our goal is to add Vcl. to the project namespaces,
//      if not already on the list.
if Supports( CurrentProject.ProjectOptions, IOTAProjectOptionsConfigurations, O) then
   begin
   BaseConfig := O.BaseConfiguration;
   sNamespaceList := BaseConfig.GetValue( sNamespace);
   if Pos(';' + sFrameworkNamespace + ';', ';' + sNamespaceList + ';') = -1 then
     BaseConfig.SetValue( sNamespace, sNamespaceList + ';' + sFrameworkNamespace)
   end
{$ifend}
end;

function TProjectWizard.CreateModule( const TemplateFN, StylesheetFN: string; const Owner: IOTAModule): IOTAModule;
var
  Creator: TIOTACreator;
begin
Creator := TIOTACreator.Create( FIDE, FLibraryAbsolutePath, TemplateFN,
     FUnitTestingLocation, FUnitTestingProjectName, StylesheetFN, FTreeId, Owner);
result  := Creator.CreateModules as IOTAModule;
end;

function TProjectWizard.GetAuthor: string;
begin
result := 'Sean B. Durkin'
end;

function TProjectWizard.GetComment: string;
begin
result := 'This wizard will generate a project to unit test your application.'
end;

function TProjectWizard.GetDesigner: string;
begin
result := dAny
end;

function TProjectWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
result := FIDE.IOTAGalleryCategoryManager.FindCategory( GetGalleryCategoryStringId)
end;

function TProjectWizard.GetGalleryCategoryStringId: string;
begin
// File | New | Other | Unit Test | DUnitX Project Wizard
result := sCategoryNewUnitTest
end;

function TProjectWizard.GetGlyph: Cardinal;
// Requires an icon with a 32x32 and a 16x16
begin
result := LoadIcon( hInstance, 'DUNITXWIZARD')
end;

function TProjectWizard.GetIDString: string;
begin
result := '[{482AC3BE-AAE9-4F3C-AD39-5E03F0D991E1}]'
end;

function TProjectWizard.GetMenuText: string;
begin
result := ''
end;

function TProjectWizard.GetName: string;
begin
result := 'DUnitX Project Wizard'
end;

function TProjectWizard.GetPage: string;
begin
// This function is ignored because we implement GetGalleryCategory().
result := ''
end;

function TProjectWizard.GetPersonality: string;
begin
result := sDelphiPersonality
end;

function TProjectWizard.GetState: TWizardState;
begin
// Only used by menu wizards. Setting it to [wsEnabled] is just wrong.
result := []
end;

function TProjectWizard.IsVisible( Project: IOTAProject): boolean;
// Don't make the form available if we don't have a project open.
begin
result := True
end;


constructor TIOTACreator.Create(
          const IDE1: IIDE_API;
          const DUnitXLibraryPath1, TemplateFileName1, UnitTestingLocation1,
                UnitTestingProjectName1, StyleSheetName1: string;
          const TreeId1: string;
          const Owner1: IOTAModule);
begin
FIDE := IDE1;
FStyleSheetFileName := StyleSheetName1;
FStyleSheet := TStyleSheet.Create( nil);
FDUnitXLibraryPath := DUnitXLibraryPath1;
FTemplateFileName  := TemplateFileName1;
FProjectLocation := UnitTestingLocation1;
FUnitTestingProjectName := UnitTestingProjectName1;
FTreeId := TreeId1;
FStyleSheet.Options := [oSimpleXSLT1];
FStyleSheet.URI := 'https://github.com/SeanBDurkin/DUnitX/template';
FStyleSheet.XMLVer := xml10;
FStyleSheet.XSDVer := xsd11;
FStyleSheet.WorkingDirectory := TPath.GetTempPath;
FStyleSheet.Content.LoadFromFile( FDUnitXLibraryPath + '\MVVM-GUI-Runner\design-time\xslt-stylesheets\' + FStyleSheetFileName);
end;


function ReadUTF8File( const FN: string): string;
// IOUtils.TFile.ReadAllText( FN, TEncoding.UTF8) is broken for files without a BOM,
//  so use this custom code.
var
  Lines: TStrings;
begin
Lines := TStringList.Create;
try
  Lines.LoadFromFile( FN);
  result := Lines.Text
finally
  Lines.Free
  end
end;

function TIOTACreator.Transform( const SourceType, ModuleName: string; var Output: string): boolean;
var
  sOutFN: string;
  Params: IStyleSheetParameterSet;
  DistinctRelPaths: TStrings;
  sPathTranslations, sDUnitXRelativePath: string;
  sError: string;
  sRelPath: string;
  RelPathNode: IXMLNode;
begin
try
  sOutFN := TPath.GetTempFileName;
  SetLength( sOutFN, StrLen( PChar( sOutFN)));
  Params := FStyleSheet.Parameters;
  Params.Param( '', 'CompilerVersion').ParameterValue := Format( '%.1f', [CompilerVersion]);
  Params.Param( '', 'SourceType').ParameterValue := SourceType;
  Params.Param( '', 'module-name').ParameterValue := ModuleName;

  DistinctRelPaths := TStringList.Create;
  sPathTranslations := '';
  for RelPathNode in TXPath.Select( TemplateDocNode,
    't:IOTACreation/t:IOTACreator' + PredicateToSupportCompiler + '/t:IOTAFile/t:stream//t:DUnitX-path/@plus') do
      begin
      sRelPath := StringValue( RelPathNode);
      if DistinctRelPaths.IndexOf( sRelPath) <> -1 then continue;
      DistinctRelPaths.Add( sRelPath);
      sPathTranslations := sPathTranslations + '|' + sRelPath +
        '=' + AbsPathToRelPath( FDUnitXLibraryPath + '\' + sRelPath, FProjectLocation)
      end;
  DistinctRelPaths.Free;
  Params.Param( '', 'tree').ParameterValue := FTreeId;
  Params.Param( '', 'path-translations').ParameterValue := sPathTranslations;
  FStyleSheet.LoadedAtRunTime;
  result := FStyleSheet.Transform(
    FDUnitXLibraryPath + '\MVVM-GUI-Runner\design-time\templates\' + FTemplateFileName,
    sOutFN, sError, Params);
  if result then
      Output := ReadUTF8File( sOutFN)
    else
      Output := sError;
  TFile.Delete( sOutFN)
except on E: Exception do
    begin
    result := False;
    Output := E.Message
    end;
  end
end;

function TIOTACreator.CreateModules: IOTAModule;
begin
result := FIDE.IOTAModuleServices.CreateModule( self as IOTACreator)
end;

destructor TIOTACreator.Destroy;
begin
FStyleSheet.Free;
inherited
end;

procedure TIOTACreator.FormCreated( const FormEditor: IOTAFormEditor);
// IOTAModuleCreator
begin
end;

function TIOTACreator.GetAncestorName: string;
// IOTAModuleCreator
begin
result := ''
end;

function TIOTACreator.LookUpCreatorString( const PathFromt_IOTACreator: string): string;
begin
result := TXPath.SelectedString( TemplateDocNode, 't:IOTACreation/t:IOTACreator' + PathFromt_IOTACreator)
end;

function TIOTACreator.GetCreatorType: string;
// IOTACreator
begin
result := LookUpCreatorString( '/@CreatorType')
end;

function TIOTACreator.GetExisting: boolean;
// IOTACreator
begin
result := False
end;

function TIOTACreator.GetFileName: string;
// IOTAProjectCreator
begin
result := FProjectLocation + '\' + FUnitTestingProjectName + '.dpr';
// This is equal to ...
//  FProjectLocation + '\' + LookUpCreatorString(
 //         PredicateToSupportCompiler +
 //         '/t:IOTAFile[t:ProjectSource/@val=''true'']/@filename')
end;

function TIOTACreator.GetFileSystem: string;
// IOTACreator
begin
result := ''
end;

function TIOTACreator.GetFormName: string;
// IOTAModuleCreator
begin
result := ''
end;

function TIOTACreator.GetImplFileName: string;
// IOTAModuleCreator
begin
result := FProjectLocation + '\' + LookUpCreatorString(
  PredicateToSupportCompiler +
  '/t:IOTAFile[t:ImplSource/@val=''true'']/@filename');
end;

function TIOTACreator.GetIntfFileName: string;
// IOTAModuleCreator
begin
result := '';
end;

function TIOTACreator.GetMainForm: boolean;
// IOTAModuleCreator
begin
result := False
end;

function TIOTACreator.GetShowForm: boolean;
begin
result := True
end;

function TIOTACreator.GetShowSource: boolean;
// IOTAProjectCreator
begin
result := True
end;

function TIOTACreator.GetUnnamed: boolean;
// IOTACreator
begin
result := True
end;

function TIOTACreator.NewFormFile( const FormIdent,
  AncestorIdent: string): IOTAFile;
// IOTAModuleCreator
begin
result := nil
end;

function TIOTACreator.NewImplSource( const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
// IOTAModuleCreator
begin
result := ComputeSource( GetImplFileName, 'ImplSource', 'DUnitX.uExecutive')
end;

function TIOTACreator.NewIntfSource( const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
// IOTAModuleCreator
begin
result := nil
end;

function TIOTACreator.NewOptionSource( const ProjectName: string): IOTAFile;
// IOTAProjectCreator
begin
result := nil
end;

function TIOTACreator.PredicateToSupportCompiler: string;
begin
result :=  Format(
        '[t:CompilerVersion/@include=''*'' or ' +
         't:CompilerVersion/@include=%.1f]',[ CompilerVersion])
end;

function TIOTACreator.SupportsPathExpression( const sInterfaceName: string): string;
begin
result :=  Format(
    't:IOTACreation/t:IOTACreator' + // It must have the <IOTACreator> element.
        '[t:Supports=''%s'']' +      // It must explicitly support interface
        PredicateToSupportCompiler,  // And the compiler version has to be right.
        [sInterfaceName])
end;

function TIOTACreator.SupportsModuleCreator: boolean;
begin
result := TemplateNodeExists(  SupportsPathExpression( 'IOTAModuleCreator'))
end;

function TIOTACreator.SupportsProjectCreator: boolean;
begin
result := TemplateNodeExists(  SupportsPathExpression( 'IOTAProjectCreator'))
end;

function TIOTACreator.TemplateDocNode: IXMLNode;
var
  sFN: string;
begin
try
  if not assigned( FDoc) then
    begin
    sFN := FDUnitXLibraryPath + '\MVVM-GUI-Runner\design-time\templates\' + FTemplateFileName;
    FDoc := TXMLDoc.FromFile( sFN);
    TXMLDoc.DeclareSelectionNamespaces( FDoc, 'xmlns:t="https://github.com/SeanBDurkin/DUnitX/template"')
    end;
  result := FDoc.Node;
except
  result := nil
  end
end;

function TIOTACreator.TemplateNodeExists( const XPathExpression: string): boolean;
var
  SomeNode: IXMLNode;
begin
result := TXPath.SelectFirst( TemplateDocNode, XPathExpression, SomeNode)
end;



procedure TIOTACreator.NewProjectResource( const Project: IOTAProject);
// IOTAProjectCreator
begin
end;

function TIOTACreator.GetOptionFileName: string;
// IOTAProjectCreator
begin
result := ''
end;

function TIOTACreator.GetOwner: IOTAModule;
// IOTACreator
begin
result := FOwner
end;

function TIOTACreator.GetProjectPersonality: string;
// IOTAProjectCreator
begin
result := sDelphiPersonality
end;

procedure TIOTACreator.NewDefaultModule;  // deprecated
// IOTAProjectCreator
begin
end;

procedure TIOTACreator.NewDefaultProjectModule( const Project: IOTAProject);
// IOTAProjectCreator
// Called to create a new default module(s) for the given project.
begin
end;


function TIOTACreator.ComputeSource( const FileName, SourceType, ModuleName: string): IOTAFile;
var
  sStringResult: string;
begin
try
  Transform( SourceType, ModuleName, sStringResult)
except on E: Exception do
  sStringResult := E.Message
  end;
if FileName <> '' then
    result := TOTASavedFile.Create( FileName, sStringResult)
  else
    result := StringToIOTAFile( sStringResult);
end;


function TIOTACreator.NewProjectSource( const ProjectName: string): IOTAFile;
// IOTAProjectCreator
begin
result := ComputeSource( GetFileName, 'ProjectSource', FUnitTestingProjectName)
end;

function TIOTACreator.QueryInterface( const IID: TGUID; out Obj): HResult;
// IInterface/IUnknown
const
  E_NOINTERFACE = HRESULT( $80004002);
var
  doInherited: boolean;
begin
if IsEqualGuid( IID, IOTAProjectCreator) or
   IsEqualGuid( IID, IOTAProjectCreator50) or
   IsEqualGuid( IID, IOTAProjectCreator80) then
     doInherited := SupportsProjectCreator
  else if IsEqualGuid( IID, IOTAModuleCreator) then
     doInherited := SupportsModuleCreator
  else // including IOTACreator
     doInherited := True;
if doInherited then
    result := inherited QueryInterface( IID, Obj)
  else
    result := E_NOINTERFACE
end;



constructor TIOTACreator.TOTASavedFile.Create( const FileName, Source: string);
var
  Lines: TStrings;
begin
FSource := Source;
try
  Lines := TStringList.Create;
  try
    Lines.SaveToFile( FileName)
  finally
    Lines.Free
    end;
  FAge := Now
except // It's ok to fail.
  FAge := -1  // Mark it as unsaved
  end
end;

function TIOTACreator.TOTASavedFile.GetAge: TDateTime;
begin
result := FAge
end;

function TIOTACreator.TOTASavedFile.GetSource: string;
begin
result := FSource
end;


//    TO DO List for DUnitX Wizard
//    ==============================
//     1. For XE2+, add Win64 platform, if it is chosen.
//     2. Implement TTestCaseNode.GetDoneCycleCount()
//     3. Implement TBaseExecutionDecorator.MeasureProgress()
//     4. Implement TRepeatDecorator.MeasureProgress()
//     5. TTreeView impl
//     6. Console impl
//     7. Create Package Heads for XE2 and XE4. Some code adjustments
//          may also be necessary to support these compilers.
//     8. Implement the dialogs for the 2 stock secondary factories.
//     9. Implement message acceptance filter on the main form.

end.
