unit DUnitX.Expert.Forms.NewUnitWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  TfrmDunitXNewUnit = class(TForm)
    GroupBox1: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblClassName: TLabel;
    edtClassName: TEdit;
    chkCreateSetupTearDown: TCheckBox;
    chkCreateSampleMethods: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
    function GetCreateSampleMethods: boolean;
    function GetCreateSetupTearDownMethods: boolean;
    function GetTestFixtureClassName: string;
    { Private declarations }
  public
    { Public declarations }
    property TestFixtureClasaName : string read GetTestFixtureClassName;
    property CreateSetupTearDownMethods : boolean read GetCreateSetupTearDownMethods;
    property CreateSampleMethods : boolean read GetCreateSampleMethods;

  end;

var
  frmDunitXNewUnit: TfrmDunitXNewUnit;

implementation
uses
  DUnitX.Expert.CodeGen.Templates;

{$R *.dfm}

{ TfrmDunitXNewUnit }

procedure TfrmDunitXNewUnit.FormCreate(Sender: TObject);
begin
  edtClassName.TextHint := SDefaultClassName;
end;

function TfrmDunitXNewUnit.GetCreateSampleMethods: boolean;
begin
  result := chkCreateSampleMethods.Checked;
end;

function TfrmDunitXNewUnit.GetCreateSetupTearDownMethods: boolean;
begin
  result := chkCreateSetupTearDown.Checked;
end;

function TfrmDunitXNewUnit.GetTestFixtureClassName: string;
begin
  if Trim(edtClassName.Text) = '' then
    result := SDefaultClassName
  else
    result := Trim(edtClassName.Text);
end;

end.
