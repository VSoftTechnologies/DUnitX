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
  VCL.Graphics,
{$IFDEF DELPHI_XE103_UP}
  PlatformConst
{$ELSE}
  PlatformAPI
{$ENDIF}
  ;

type
  TDUnitXNewProjectWizard = class
  public
    class procedure RegisterDUnitXProjectWizard(const APersonality : string);
  end;

implementation

uses
  DccStrs,
  Vcl.Controls,
  Vcl.Forms,
  WinApi.Windows,
  System.SysUtils,
  DUnitX.Expert.Forms.NewProjectWizard,
  DUnitX.Expert.CodeGen.NewTestProject,
  DUnitX.Expert.CodeGen.NewTestUnit,
  ExpertsRepository;

resourcestring
  sNewTestProjectCaption = 'DUnitX Project';
  sNewTestProjectHint = 'Create New DUnitX Test Project';

  { TDUnitXNewProjectWizard }

class procedure TDUnitXNewProjectWizard.RegisterDUnitXProjectWizard(const APersonality : string);
begin
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(APersonality,
      sNewTestProjectHint, sNewTestProjectCaption, 'DunitX.Wizard.NewProjectWizard', // do not localize
      'DUnitX', 'DUnitX Team - https://github.com/VSoftTechnologies/DUnitX', // do not localize
      procedure
      var
        WizardForm : TfrmDunitXNewProject;
        ModuleServices : IOTAModuleServices;
        Project : IOTAProject;
        Configs : IOTAProjectOptionsConfigurations;
        TestUnit : IOTAModule;
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
            ModuleServices.CreateModule(TTestProjectFile.Create(APersonality, WizardForm.ReportLeakOption));
            Project := GetActiveProject;
            if (Project <> nil) and Supports(Project.ProjectOptions, IOTAProjectOptionsConfigurations, Configs) then
              Configs.BaseConfiguration.SetValue(sUnitSearchPath, '$(DUnitX)');
            // Create Test Unit
            if WizardForm.CreateTestUnit then
            begin
              TestUnit := ModuleServices.CreateModule(
                TNewTestUnit.Create(WizardForm.CreateSetupTearDownMethods,
                  WizardForm.CreateSampleMethods,
                  WizardForm.TestFixtureClasaName,
                  APersonality));
              if Project <> nil then
                Project.AddFile(TestUnit.FileName, true);
            end;
          end;
        finally
          WizardForm.Free;
        end;
      end,
      // The glyph is an HICON: Delphi 13 declares this callback as TFunc<UInt64> on Win64 and
      // TFunc<Cardinal> on Win32. NativeUInt matches both.
      function : NativeUInt
      begin
        Result := LoadIcon(HInstance, 'DUnitXNewProjectIcon');
      end,
{$IFDEF DELPHI_XE103_UP}
      GetAllPlatforms,
{$ELSE}
      TArray<string>.Create(cWin32Platform, cWin64Platform, cOSX32Platform, cAndroidPlatform, ciOSSimulatorPlatform, ciOSDevice32Platform,
        ciOSDevice64Platform),
{$ENDIF}
      nil));
end;

end.

