unit DUnitX.frmMainDetailsPage;

interface

uses
  {$if CompilerVersion >= 23}
    // XE2+
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnMan, Vcl.ImgList,
    Vcl.StdCtrls, Vcl.FileCtrl, Vcl.ExtCtrls, Vcl.Grids, Vcl.Outline,
    Vcl.Samples.DirOutln,
  {$else}
    // D2010, XE
    Graphics, Controls, Forms, Dialogs, ActnMan, ImgList,
    StdCtrls, FileCtrl, ExtCtrls, Grids, Outline, DirOutln,
  {$ifend}
    Windows, Messages, SysUtils, Variants, Classes, DUnitX.WizardStates,
    DUnitX.WizardPageIntf;

type
  TfrmMainDetailsPage = class( TFrame, IWizardPage)
    edtProjectName: TLabeledEdit;
    edtLocation: TButtonedEdit;
    lblLocation: TLabel;
    chkbxAddToProject: TCheckBox;
    edtAppTitle: TLabeledEdit;
    imglstButtons16x16: TImageList;
    lblTitle: TStaticText;
    lblInstruction: TStaticText;
    lstbxSelectDirectory: TDirectoryOutline;
    procedure edtLocationRightButtonClick(Sender: TObject);
    procedure lstbxSelectDirectoryExit(Sender: TObject);
  private
    FState: IWizardIntermediateState;
    function  Frame: TFrame;
    function  Can_Back( const PageId: string): boolean;
    function  IsValid( const PageId: string; Level: TValidationLevel; const State: IInterface): boolean;
    function  isFinal( const PageId: string): boolean;
    function  Post( const PageId: string; var State: IInterface; var NextPageId: string): boolean;

  public const
    Id = 'main-details';
    NextPage = 'tree';

  public type
    TFactory = class( TInterfacedObject, IWizardPageFactory)
      private
        function  handlesPageId( const PageId: string): boolean;
        function  GetPage( const PageId: string; AOwner: TComponent; AParent: TWinControl; AColours: TCustomActionBarColorMap; const State: IInterface; isForward: boolean): IWizardPage;
        procedure ReleasePage( const PageId: string; var Page: IWizardPage);
      end;

  {$REGION 'property accessors'}
  private
    function  GetProjectName : string;
    procedure SetProjectName( const Value: string);
    function  GetLocation    : string;
    procedure SetLocation( const Value: string);
    function  GetAddToProject: boolean;
    procedure SetAddToProject( Value: Boolean);
    function  GetAppTitle    : string;
    procedure SetAppTitle( const Value: string);
  {$ENDREGION}

  public
    property ProjectName: string   read GetProjectName  write SetProjectName;
    property Location: string      read GetLocation     write SetLocation;
    property AddToProject: boolean read GetAddToProject write SetAddToProject;
    property AppTitle: string      read GetAppTitle     write SetAppTitle;
  end;


implementation


















uses IOUtils;

{$R *.dfm}


function TfrmMainDetailsPage.TFactory.GetPage( const PageId: string;
  AOwner: TComponent; AParent: TWinControl; AColours: TCustomActionBarColorMap;
  const State: IInterface; isForward: boolean): IWizardPage;
var
  Construct: TfrmMainDetailsPage;
  Init: IWizardInitialState;
  Imtm: IWizardIntermediateState;
begin
Construct := TfrmMainDetailsPage.Create( AOwner);
result    := Construct as IWizardPage;
if Construct.Parent <> AParent then
  Construct.Parent := AParent;
Construct.Align := alClient;
if Supports( State, IWizardIntermediateState, Imtm) then
    Construct.FState := Imtm.Clone
  else if Supports( State, IWizardInitialState, Init) then
    Construct.FState := CreateWizardIntermediateState( Init.Data)
  else
    Construct.FState := nil;
Construct.AddToProject              := not Construct.FState.State.FInit.FisNewProjectGroup;
Construct.chkbxAddToProject.Enabled := not Construct.AddToProject;
Construct.ProjectName :=  Construct.FState.State.FInit.FCurrentProjectName;
if Construct.ProjectName = '' then
    Construct.ProjectName := 'UnitTests'
  else
    Construct.ProjectName := Construct.ProjectName + 'Test';
Construct.Location := Construct.FState.State.FInit.FCurrentProjectPath;
if Construct.Location = '' then
  Construct.Location := Construct.FState.State.FInit.FBDSProjectPath
end;


function TfrmMainDetailsPage.TFactory.handlesPageId(
  const PageId: string): boolean;
begin
result := PageId = Id
end;

procedure TfrmMainDetailsPage.TFactory.ReleasePage(
  const PageId: string; var Page: IWizardPage);
begin
Page.Frame.Free;
Page := nil
end;

function TfrmMainDetailsPage.Can_Back( const PageId: string): boolean;
begin
result := True
end;

procedure TfrmMainDetailsPage.edtLocationRightButtonClick(Sender: TObject);
begin
try
  lstbxSelectDirectory.Directory := Location;
  lstbxSelectDirectory.Visible   := True
except
  lstbxSelectDirectory.Directory := TDirectory.GetCurrentDirectory;
  lstbxSelectDirectory.Visible   := True
  end
end;

function TfrmMainDetailsPage.Frame: TFrame;
begin
result := self
end;

function TfrmMainDetailsPage.GetAddToProject: boolean;
begin
result := chkbxAddToProject.Checked
end;

function TfrmMainDetailsPage.GetAppTitle: string;
begin
result := edtAppTitle.Text
end;

function TfrmMainDetailsPage.GetLocation: string;
begin
result := ExcludeTrailingPathDelimiter( edtLocation.Text)
end;

function TfrmMainDetailsPage.GetProjectName: string;
begin
result := edtProjectName.Text
end;

function TfrmMainDetailsPage.isFinal( const PageId: string): boolean;
begin
result := NextPage = ''
end;

function TfrmMainDetailsPage.IsValid(
  const PageId: string; Level: TValidationLevel; const State: IInterface): boolean;
begin
// TODO:
result := not lstbxSelectDirectory.Visible
end;

procedure TfrmMainDetailsPage.lstbxSelectDirectoryExit(Sender: TObject);
begin
Location := lstbxSelectDirectory.Directory;
lstbxSelectDirectory.Visible := False
end;

function TfrmMainDetailsPage.Post(
  const PageId: string; var State: IInterface; var NextPageId: string): boolean;
begin
result := True;
FState.SetUnitTestingProjectName( ProjectName);
FState.SetUnitTestingLocation( Location);
FState.SetDoCreateNewProjectGroup( AddToProject);
FState.SetApplicationTitle( AppTitle);
State := FState.Clone;
NextPageId := NextPage
end;


procedure TfrmMainDetailsPage.SetAddToProject( Value: Boolean);
begin
chkbxAddToProject.Checked := Value
end;

procedure TfrmMainDetailsPage.SetAppTitle( const Value: string);
begin
edtAppTitle.Text := Value
end;

procedure TfrmMainDetailsPage.SetLocation( const Value: string);
begin
edtLocation.Text := Value
end;

procedure TfrmMainDetailsPage.SetProjectName( const Value: string);
begin
edtProjectName.Text := Value
end;


end.
