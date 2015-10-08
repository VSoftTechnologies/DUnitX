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

unit DUnitX.Expert.Forms.NewUnitWizard;

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
  VCL.StdCtrls;
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
  StdCtrls;
  {$ENDIF}


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
