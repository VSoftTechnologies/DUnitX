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

unit DUnitX.Expert.ProjectWizardEx;

interface

{$I DUnitX.inc}

uses
  ToolsApi,
  {$IFDEF USE_NS}
  VCL.Graphics,
  {$ELSE}
  Graphics,
  {$ENDIF}
  PlatformAPI;

type
  TDUnitXNewProjectWizard = class
  public
    class procedure RegisterDUnitXProjectWizard(const APersonality: string);
  end;

implementation

uses
  DccStrs,
  {$IFDEF USE_NS}
  Vcl.Controls,
  Vcl.Forms,
  WinApi.Windows,
  System.SysUtils,
  {$ELSE}
  Controls,
  Forms,
  Windows,
  SysUtils,
  {$ENDIF}
  DUnitX.Expert.Forms.NewProjectWizard,
  DUnitX.Expert.CodeGen.NewTestProject,
  DUnitX.Expert.CodeGen.NewTestUnit,
  ExpertsRepository;

resourcestring
  sNewTestProjectCaption = 'DUnitX Project';
  sNewTestProjectHint = 'Create New DUnitX Test Project';

{ TDUnitXNewProjectWizard }

class procedure TDUnitXNewProjectWizard.RegisterDUnitXProjectWizard(const APersonality: string);
begin
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(APersonality,
    sNewTestProjectHint, sNewTestProjectCaption, 'DunitX.Wizard.NewProjectWizard',  // do not localize
    'DUnitX', 'DUnitX Team - https://github.com/VSoftTechnologies/DUnitX', // do not localize
    procedure
    var
      WizardForm     : TfrmDunitXNewProject;
      ModuleServices : IOTAModuleServices;
      Project        : IOTAProject;
      Config         : IOTABuildConfiguration;
      TestUnit       : IOTAModule;
    begin
      WizardForm := TfrmDunitXNewProject.Create(Application);
      try
        if WizardForm.ShowModal = mrOk then
        begin
          if not WizardForm.AddToProjectGroup then
          begin
            (BorlandIDEServices as IOTAModuleServices).CloseAll;
          end;
          ModuleServices := (BorlandIDEServices as IOTAModuleServices);
          // Create Project Source
          ModuleServices.CreateModule(TTestProjectFile.Create(APersonality));
          Project :=  GetActiveProject;
          Config := (Project.ProjectOptions as IOTAProjectOptionsConfigurations).BaseConfiguration;
          Config.SetValue(sUnitSearchPath,'$(DUnitX)');
          // Create Test Unit
          if WizardForm.CreateTestUnit then
          begin
             TestUnit := ModuleServices.CreateModule(
                           TNewTestUnit.Create(WizardForm.CreateSetupTearDownMethods,
                                               WizardForm.CreateSampleMethods,
                                               WizardForm.TestFixtureClasaName,
                                               APersonality));
             if Project <> nil then
               Project.AddFile(TestUnit.FileName,true);
          end;
        end;
      finally
        WizardForm.Free;
      end;
    end,
    function: Cardinal
    begin
      Result := LoadIcon(HInstance,'DUnitXNewProjectIcon');
    end,
    TArray<string>.Create(cWin32Platform, cWin64Platform, cOSX32Platform, cAndroidPlatform, ciOSSimulatorPlatform, ciOSDevice32Platform, ciOSDevice64Platform),
    nil));
end;

end.
