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

unit DUnitX.Expert.ProjectWizard;

interface

{$I DUnitX.inc}

uses
  ToolsApi,
  VCL.Graphics;


type
   TDUnitXNewProjectWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard, IOTARepositoryWizard80, IOTAProjectWizard)
   protected
     FIcon : TIcon;
   public
    // IOTARepositoryWizard80
     function GetGalleryCategory: IOTAGalleryCategory;
     function GetPersonality: string;
    // IOTARepositoryWizard60
    function GetDesigner: string;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;


    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    { Launch the AddIn }
    procedure Execute;
    constructor Create;
    destructor Destroy; override;
   end;


implementation

uses
  DUnitX.Expert.Forms.NewProjectWizard,
  DUnitX.Expert.CodeGen.NewTestProject,
  DUnitX.Expert.CodeGen.NewTestUnit,
  DccStrs,
  Controls,
  Forms,
  SysUtils,
  Windows;

{ TNewBatchJobWizard }

constructor TDUnitXNewProjectWizard.Create;
begin
  FIcon := TIcon.create;
  FIcon.LoadFromResourceName(HInstance,'DUnitXNewProjectIcon');
end;

destructor TDUnitXNewProjectWizard.Destroy;
begin
  FIcon.Free;
  inherited;
end;

procedure TDUnitXNewProjectWizard.Execute;
var
  WizardForm     : TfrmDunitXNewProject;
  ModuleServices : IOTAModuleServices;
  Project        : IOTAProject;
  Configs        : IOTAProjectOptionsConfigurations;
  TestUnit       : IOTAModule;
begin
  WizardForm := TfrmDunitXNewProject.Create(Application);
  try
    if WizardForm.ShowModal = mrOk then
    begin
      ModuleServices := (BorlandIDEServices as IOTAModuleServices);
      if not WizardForm.AddToProjectGroup then
      begin
        // CloseAll returns False when the user cancels its save prompt: stop here rather than
        // add the new project to the group the user meant to close.
        if not ModuleServices.CloseAll then
          Exit;
      end;
      // Create Project Source
      ModuleServices.CreateModule(TTestProjectFile.Create(WizardForm.ReportLeakOption));
      Project :=  GetActiveProject;
      if (Project <> nil) and Supports(Project.ProjectOptions, IOTAProjectOptionsConfigurations, Configs) then
        Configs.BaseConfiguration.SetValue(sUnitSearchPath,'$(DUnitX)');
      // Create Test Unit
      if WizardForm.CreateTestUnit then
      begin
         TestUnit := ModuleServices.CreateModule(
                       TNewTestUnit.Create(WizardForm.CreateSetupTearDownMethods,
                                           WizardForm.CreateSampleMethods,
                                           WizardForm.TestFixtureClasaName ));
         if Project <> nil then
           Project.AddFile(TestUnit.FileName, true);
      end;


    end;
  finally
    WizardForm.Free;
  end;
end;

function TDUnitXNewProjectWizard.GetAuthor: string;
begin
  result := 'DUnitX Team - https://github.com/VSoftTechnologies/DUnitX';
end;

function TDUnitXNewProjectWizard.GetComment: string;
begin
  result := 'Create New DUnitX Test Project';
end;

function TDUnitXNewProjectWizard.GetDesigner: string;
begin
  result := dAny;
end;

function TDUnitXNewProjectWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  result :=  (BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory(sCategoryDelphiNew);
end;

function TDUnitXNewProjectWizard.GetGlyph: Cardinal;
begin
  result := CopyIcon(FIcon.Handle);
end;

function TDUnitXNewProjectWizard.GetIDString: string;
begin
 result := 'DunitX.Wizard.NewProjectWizard';
end;

function TDUnitXNewProjectWizard.GetName: string;
begin
  result := 'DUnitX Project';
end;

function TDUnitXNewProjectWizard.GetPage: string;
begin
 // Results not used if IOTARepositoryWizard80.GetGalleryCategory implemented
  result := 'Delphi Project';
end;

function TDUnitXNewProjectWizard.GetPersonality: string;
begin
 result := sDelphiPersonality;
end;

function TDUnitXNewProjectWizard.GetState: TWizardState;
begin
 result := [wsEnabled];
end;

end.
