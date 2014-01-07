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

  protected
    function  GetGalleryCategoryStringId: string;        virtual;  // Frequently override

  private
    FServices: TDUnitXIoC;
    FIDE: IIDE_API;

  public
    constructor Create( Services1: TDUnitXIoC);
    destructor Destroy; override;
  end;

implementation

















uses
  {$if CompilerVersion >= 23}
    // XE2+
    Vcl.Dialogs,
  {$else}
    // D2010, XE
    Dialogs,
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
    FPlugUnits: string; // Like 'DUnitX.VirtualTrees DUnitX.VTAccessibility DUnitX.VTAccessibilityFactory'
    FPluginPath: string; // Like '..\..\Extenal-libraries\VirtualTreeView\Source'. Path relative to ProjectLocation
    FIDE: IIDE_API;
    FDoc: IXMLDocument;
    FUnitTestingProjectName: string;
    FGroup: IOTAProjectGroup;

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
    function Transform( const SourceType, ModuleName, ProjectDirectories, PlugInUnits, PlugInPath: string; var Output: string): boolean;

 public
   constructor Create(
          const IDE1: IIDE_API;
          const DUnitXLibraryPath1, TemplateFileName1, UnitTestingLocation1,
                UnitTestingProjectName1, StyleSheetName1: string;
          const Group1: IOTAProjectGroup);
   destructor  Destroy; override;

 public
   function CreateModules: IInterface;
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


procedure TProjectWizard.Execute;
var
  Wiz: ILaunchWizardFormService;
  Ok : boolean;
  isNewProjectGroup: boolean;
  CurrentProjectName: string;
  CurrentProjectPath: string;
  CurrentPlatform: TPlatform;
  BDSProjectPath: string;
  LibraryAbsolutePath: string;
  UnitTestingProjectName: string;
  UnitTestingLocation: string;
  ApplicationTitle: string;
  Tree: TTreeViewChoice;
  View: TViewParadigm;
  LibraryRelativePath: string;
  Proj: IOTAProject;
  sActivePlatform: string;
  {$if CompilerVersion >= 24}
    O: IOTAProjectOptionsConfigurations;
  {$ifend}
  Creator: TIOTACreator;
  Group: IOTAProjectGroup;
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
Wiz := FServices.Resolve<ILaunchWizardFormService>;
if assigned( Wiz) then
    Ok := Wiz.LaunchWizardForm(
        isNewProjectGroup,
        CurrentProjectName,
        CurrentProjectPath,
        CurrentPlatform,
        BDSProjectPath,
        LibraryAbsolutePath,
        UnitTestingProjectName,
        UnitTestingLocation,
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
        LibraryAbsolutePath,
        UnitTestingProjectName,
        UnitTestingLocation,
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

Creator := TIOTACreator.Create( FIDE, LibraryAbsolutePath, 'GUIRunner.dpr.template',
                                UnitTestingLocation, UnitTestingProjectName, 'IOTAConstruct.xsl', Group);
Creator.CreateModules;

// TODO: Uncomment this when it is working ...
//Creator := TIOTACreator.Create( FIDE, LibraryAbsolutePath, 'DUnitX.uExecutive.pas.template',
//                                UnitTestingLocation, UnitTestingProjectName, 'IOTAConstruct.xsl', Proj);
//Creator.CreateModules
end;

function TProjectWizard.GetAuthor: string;
begin
// Override as required.
result := 'Sean B. Durkin'
end;

function TProjectWizard.GetComment: string;
begin
// Override as required.
result := 'This wizard is based off the DUnitX.TWizard class.'#13#10 +
          'Please override these comments.'
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
// Override as required.
begin
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
// Only used by menu wizards.
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
          const Group1: IOTAProjectGroup);
begin
FIDE := IDE1;
FStyleSheetFileName := StyleSheetName1;
FStyleSheet := TStyleSheet.Create( nil);
FDUnitXLibraryPath := DUnitXLibraryPath1;
FTemplateFileName  := TemplateFileName1;
FProjectLocation := UnitTestingLocation1;
FUnitTestingProjectName := UnitTestingProjectName1;
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

function TIOTACreator.Transform( const SourceType, ModuleName, ProjectDirectories, PlugInUnits, PlugInPath: string; var Output: string): boolean;
var
  sOutFN: string;
  Params: IStyleSheetParameterSet;
  DUnitXProjectDirs: TStrings;
  sPathTranslations, sDUnitXRelativePath: string;
  sError: string;
begin
try
  sOutFN := TPath.GetTempFileName;
  SetLength( sOutFN, StrLen( PChar( sOutFN)));
  Params := FStyleSheet.Parameters;
  Params.Param( '', 'CompilerVersion').ParameterValue := Format( '%.1f', [CompilerVersion]);
  Params.Param( '', 'SourceType').ParameterValue := SourceType;
  Params.Param( '', 'module-name').ParameterValue := ModuleName;
  DUnitXProjectDirs := TStringList.Create;
  DUnitXProjectDirs.StrictDelimiter := True;
  DUnitXProjectDirs.Delimiter := '|';
  DUnitXProjectDirs.DelimitedText := '|' + ProjectDirectories;
  sPathTranslations := '';
  for sDUnitXRelativePath in DUnitXProjectDirs do
    sPathTranslations := sPathTranslations + '|' + sDUnitXRelativePath + '=' +
      AbsPathToRelPath( FDUnitXLibraryPath + '\' + sDUnitXRelativePath, FProjectLocation);
  DUnitXProjectDirs.Free;
  // TODO: Add paramters for t:plugin-view-units and t:plugin-view-registration
  Params.Param( '', 'path-translations').ParameterValue := sPathTranslations;
  Params.Param( '', 'plugin-units').ParameterValue := PlugInUnits;
  if PlugInUnits <> '' then
      Params.Param( '', 'plugin-unit-path').ParameterValue := AbsPathToRelPath( FDUnitXLibraryPath + '\' + PlugInPath, FProjectLocation)
    else
      Params.Param( '', 'plugin-unit-path').ParameterValue := '';
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

function TIOTACreator.CreateModules: IInterface;
begin
result := self;
FIDE.IOTAModuleServices.CreateModule( result as IOTACreator)
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

function TIOTACreator.GetCreatorType: string;
// IOTACreator
begin
result := TXPath.SelectedString( TemplateDocNode, 't:IOTACreation/t:IOTACreator/@CreatorType')
end;

function TIOTACreator.GetExisting: boolean;
// IOTACreator
begin
result := False
end;

function TIOTACreator.GetFileName: string;
// IOTAProjectCreator
begin
if FUnitTestingProjectName = '' then
  FUnitTestingProjectName := TXPath.SelectedString( TemplateDocNode,
        't:IOTACreation/t:IOTACreator' + PredicateToSupportCompiler +
        '/t:IOTAFile[t:ProjectSource/@val=''true'']/@filename');
result := FProjectLocation + '\' + FUnitTestingProjectName + '.dpr';
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
// Deferred TODO:
result := ''
end;

function TIOTACreator.GetIntfFileName: string;
// IOTAModuleCreator
begin
result := ''
end;

function TIOTACreator.GetMainForm: boolean;
// IOTAModuleCreator
begin
// Deferred TODO:
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
var
  sStringResult: string;
begin
self.Transform( 'Unit', 'DUnitX.uExecutive', '', sStringResult);
   // TODO: Add parameters for t:plugin-view-units etc.
result := StringToIOTAFile( sStringResult)
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
  else
     doInherited := True;
if doInherited then
    result := inherited QueryInterface( IID, Obj)
  else
    result := E_NOINTERFACE
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
result := FGroup as IOTAModule
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


function TIOTACreator.NewProjectSource( const ProjectName: string): IOTAFile;
// IOTAProjectCreator
var
  sStringResult: string;
begin
self.Transform( 'ProjectSource', FUnitTestingProjectName,
  '|MVVM-GUI-Runner\run-time\view' +
  '|MVVM-GUI-Runner\run-time\backplane' +
  '|MVVM-GUI-Runner\run-time\view-model' +
  '|MVVM-GUI-Runner\run-time\view\plug-ins\trees' +
  '|MVVM-GUI-Runner\run-time\view\plug-ins\trees\DUnitX.VirtualStringTree',
  'DUnitX.VirtualTrees DUnitX.VTAccessibility DUnitX.VTAccessibilityFactory',
  'MVVM-GUI-Runner\Extenal-libraries\VirtualTreeView\Source',
   sStringResult);
result := StringToIOTAFile( sStringResult)
end;

//    TO DO List for DUnitX Wizard
//    ==============================
//     1. Impement the dynamic generate of the unit DUnitX.uExecutive from TIOTACreator.
//         This unit has dynamic parts
//     2. Create DUnitX.uTestSuiteVirtualTree version for bundled VTrees
//     3. Implement TTestCaseNode.GetDoneCycleCount()
//     4. Implement procedure TTestSuiteVirtualTreeObj.TChangeContext.Delete/Insert();
//     5. Implement TBaseExecutionDecorator.MeasureProgress()
//     6. Implement TRepeatDecorator.MeasureProgress()
//     7. Select directory dialog component
//     8. implement a design-time component library to support dclUnitX. So there will be
//         1 run-time and 2 design-time packages within the grou.
//     9. TTreeView impl
//     10. Console impl
//     11. Create Package Heads for XE2, XE3 and XE4. Some code adjustments
//          may also be necessary to support these compilers.

end.
