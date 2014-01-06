unit DUnitX.mfmWizardTester;

interface

uses
  {$if CompilerVersion >= 23}
    // XE2+
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  {$else}
    // D2010, XE
    Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$ifend}
    Windows, Messages, SysUtils, Variants, Classes;

type
  TmfmWizardTester = class(TForm)
    btnLaunchWizard: TButton;
    procedure btnLaunchWizardClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mfmWizardTester: TmfmWizardTester;

implementation




uses DUnitX.LaunchWizardForm, DUnitX.WizardPageIntf, DUnitX.WizardStates;
{$R *.dfm}

procedure TmfmWizardTester.btnLaunchWizardClick(Sender: TObject);
var
  isNewProjectGroup: boolean;      // In and out
  CurrentProjectName: string;      // In
  CurrentProjectPath: string;      // In
  CurrentPlatform: TPlatform;      // In and out
  BDSProjectPath: string;          // In
  LibraryAbsolutePath: string;     // In and out
  UnitTestingProjectName: string;  // Out
  UnitTestingLocation: string;     // Out
  ApplicationTitle: string;        // Out
  Tree: TTreeViewChoice;           // Out
  View: TViewParadigm;             // Out
  LibraryRelativePath: string;     // Out
begin
isNewProjectGroup   := True;
CurrentProjectName  := 'SomeProject';
CurrentProjectPath  := 'C:\Temp\Projects\SomeProj';
CurrentPlatform     := pWin32;
BDSProjectPath      := 'C:\TEMP\Projects';
LibraryAbsolutePath := 'C:\TEMP\Projects\Third Party\Samarkand\DUnitX';
if LaunchWizardForm(
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
    LibraryRelativePath) then
    begin
    ShowMessage( 'Accepted');

    end
  else
    ShowMessage( 'Cancel')
end;

end.
