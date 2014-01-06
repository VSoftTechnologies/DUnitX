unit DUnitX.frmPathPage;

interface

uses
 {$if CompilerVersion >= 23}
    // XE2+
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnMan, Vcl.Grids,
    Vcl.Outline,  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ImgList, Vcl.Samples.DirOutln,
  {$else}
    // D2010, XE
    Graphics, Controls, Forms, Dialogs, ActnMan, Grids,
    Outline,  StdCtrls, ExtCtrls, ImgList, DirOutln,
  {$ifend}
    Windows, Messages, SysUtils, Variants, Classes,
    DUnitX.WizardPageIntf, DUnitX.WizardStates;

type
  TfrmPathPage = class( TFrame, IWizardPage)
    lblLocation: TLabel;
    edtLocation: TButtonedEdit;
    lstbxSelectDirectory: TDirectoryOutline;
    imglstButtons16x16: TImageList;
    lblTitle: TStaticText;
    lblInstruction: TStaticText;
    procedure edtLocationRightButtonClick(Sender: TObject);
    procedure lstbxSelectDirectoryExit(Sender: TObject);
  public const
    PageId = 'library';
    NextPage = 'read-me';

  private
    function  Frame: TFrame;
    function  Can_Back( const PageId: string): boolean;
    function  IsValid( const PageId: string; Level: TValidationLevel; const State: IInterface): boolean;
    function  isFinal( const PageId: string): boolean;
    function  Post( const PageId: string; var State: IInterface; var NextPageId: string): boolean;

  public type
    TFactory = class( TInterfacedObject, IWizardPageFactory)
      private
        function  handlesPageId( const PageId: string): boolean;
        function  GetPage( const PageId: string; AOwner: TComponent; AParent: TWinControl; AColours: TCustomActionBarColorMap; const State: IInterface; isForward: boolean): IWizardPage;
        procedure ReleasePage( const PageId: string; var Page: IWizardPage);
      end;

  private
    FState: IWizardIntermediateState;
    function GetPath: string;
    procedure SetPath( const Path1: string);

  public
    property Path: string  read GetPath write SetPath;
  end;

implementation






















uses IOUtils;

{$R *.dfm}


function TfrmPathPage.Can_Back( const PageId: string): boolean;
begin
result := True
end;

procedure TfrmPathPage.edtLocationRightButtonClick(Sender: TObject);
begin
try
  lstbxSelectDirectory.Directory := Path;
  lstbxSelectDirectory.Visible   := True
except
  lstbxSelectDirectory.Directory := TDirectory.GetCurrentDirectory;
  lstbxSelectDirectory.Visible   := True
  end
end;

function TfrmPathPage.Frame: TFrame;
begin
result := self
end;

function TfrmPathPage.GetPath: string;
begin
result := ExcludeTrailingPathDelimiter( edtLocation.Text)
end;

function TfrmPathPage.isFinal( const PageId: string): boolean;
begin
result := NextPage = ''
end;

function TfrmPathPage.IsValid(
  const PageId: string; Level: TValidationLevel; const State: IInterface): boolean;
begin
result := (Path <> '') and TFile.Exists( Path + '\' + LitmusFile)
end;

procedure TfrmPathPage.lstbxSelectDirectoryExit(Sender: TObject);
begin
Path := lstbxSelectDirectory.Directory;
lstbxSelectDirectory.Visible := False
end;

function TfrmPathPage.Post(
  const PageId: string; var State: IInterface; var NextPageId: string): boolean;
begin
result := True;
FState.SetLibraryAbsolutePath( Path);
State := FState.Clone;
NextPageId := NextPage
end;

procedure TfrmPathPage.SetPath( const Path1: string);
begin
edtLocation.Text := Path1
end;


function TfrmPathPage.TFactory.GetPage(
  const PageId: string; AOwner: TComponent;
  AParent: TWinControl; AColours: TCustomActionBarColorMap;
  const State: IInterface; isForward: boolean): IWizardPage;
var
  Construct: TfrmPathPage;
  Init: IWizardInitialState;
  Imtm: IWizardIntermediateState;
begin
Construct := TfrmPathPage.Create( AOwner);
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
Construct.Path := Construct.FState.State.FFinl.FLibraryAbsolutePath
end;

function TfrmPathPage.TFactory.handlesPageId( const PageId: string): boolean;
begin
result := PageId = TfrmPathPage.PageId
end;

procedure TfrmPathPage.TFactory.ReleasePage(
  const PageId: string; var Page: IWizardPage);
var
  Obj: TObject;
begin
if assigned( Page) then
    Obj := Page.Frame
  else
    Obj := nil;
Page := nil;
Obj.Free
end;

end.
