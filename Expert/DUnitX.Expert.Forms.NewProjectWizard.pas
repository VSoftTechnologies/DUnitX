unit DUnitX.Expert.Forms.NewProjectWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  TfrmDunitXNewProject = class(TForm)
    gbTestUnitOpt: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkCreateSetupTearDown: TCheckBox;
    chkCreateSampleMethods: TCheckBox;
    chkCreateTestUnit: TCheckBox;
    chkAddToProjectGroup: TCheckBox;
    edtClassName: TEdit;
    lblClassName: TLabel;
    procedure chkCreateTestUnitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function GetAddToProjectGroup: boolean;
    function GetCreateSampleMethods: boolean;
    function GetCreateSetupTearDownMethods: boolean;
    function GetCreateTestUnit: boolean;
    function GetTestFixtureClassName: string;
  public
    { Public declarations }
    // Read Only Properties to extract values without having to know control values.
    property TestFixtureClasaName : string read GetTestFixtureClassName;
    property CreateSetupTearDownMethods : boolean read GetCreateSetupTearDownMethods;
    property CreateSampleMethods : boolean read GetCreateSampleMethods;
    property CreateTestUnit : boolean read GetCreateTestUnit;
    property AddToProjectGroup : boolean read GetAddToProjectGroup;
  end;

var
  frmDunitXNewProject: TfrmDunitXNewProject;

implementation
uses
  DUnitX.Expert.CodeGen.Templates;

{$R *.dfm}

procedure TfrmDunitXNewProject.chkCreateTestUnitClick(Sender: TObject);
begin
  gbTestUnitOpt.Enabled := chkCreateTestUnit.Checked;
  chkCreateSetupTearDown.Enabled := chkCreateTestUnit.Checked;
  chkCreateSampleMethods.Enabled := chkCreateTestUnit.Checked;
  edtClassName.Enabled := chkCreateTestUnit.Checked;
end;

procedure TfrmDunitXNewProject.FormCreate(Sender: TObject);
begin
  edtClassName.TextHint := SDefaultClassName;
end;

function TfrmDunitXNewProject.GetAddToProjectGroup: boolean;
begin
  result := chkAddToProjectGroup.Checked;
end;

function TfrmDunitXNewProject.GetCreateSampleMethods: boolean;
begin
  result := chkCreateSampleMethods.Checked;
end;

function TfrmDunitXNewProject.GetCreateSetupTearDownMethods: boolean;
begin
  result := chkCreateSetupTearDown.Checked;
end;

function TfrmDunitXNewProject.GetCreateTestUnit: boolean;
begin
  result := chkCreateTestUnit.Checked;
end;

function TfrmDunitXNewProject.GetTestFixtureClassName: string;
begin
  if Trim(edtClassName.Text) = '' then
    result := SDefaultClassName
  else
    result := Trim(edtClassName.Text);
end;

end.
