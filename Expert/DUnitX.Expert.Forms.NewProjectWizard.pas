{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2015 Vincent Parrett & Contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit DUnitX.Expert.Forms.NewProjectWizard;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  WinAPI.Windows,
  WinAPI.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.StdCtrls,
  Vcl.ExtCtrls,
  {$ELSE}
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  {$ENDIF}
  DUnitX.Expert.CodeGen.NewTestProject;

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
    rgReportLeakOptions: TRadioGroup;
    procedure chkCreateTestUnitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function GetAddToProjectGroup: boolean;
    function GetCreateSampleMethods: boolean;
    function GetCreateSetupTearDownMethods: boolean;
    function GetCreateTestUnit: boolean;
    function GetTestFixtureClassName: string;
    function GetReportLeakOption: TReportLeakOptions;
  public
    { Public declarations }
    // Read Only Properties to extract values without having to know control values.
    property TestFixtureClasaName : string read GetTestFixtureClassName;
    property CreateSetupTearDownMethods : boolean read GetCreateSetupTearDownMethods;
    property CreateSampleMethods : boolean read GetCreateSampleMethods;
    property CreateTestUnit : boolean read GetCreateTestUnit;
    property AddToProjectGroup : boolean read GetAddToProjectGroup;
    property ReportLeakOption: TReportLeakOptions read GetReportLeakOption;
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

function TfrmDunitXNewProject.GetReportLeakOption: TReportLeakOptions;
begin
  Result := TReportLeakOptions(rgReportLeakOptions.ItemIndex);
end;

function TfrmDunitXNewProject.GetTestFixtureClassName: string;
begin
  if Trim(edtClassName.Text) = '' then
    result := SDefaultClassName
  else
    result := Trim(edtClassName.Text);
end;

end.
