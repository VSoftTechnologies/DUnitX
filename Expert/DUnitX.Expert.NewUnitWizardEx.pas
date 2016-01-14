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

unit DUnitX.Expert.NewUnitWizardEx;

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
  TDUnitXNewUnitWizard = class
  public
    class procedure RegisterDUnitXNewUnitWizard(const APersonality: string);
  end;

implementation


uses
  DUnitX.Expert.Forms.NewUnitWizard,
  DUnitX.Expert.CodeGen.NewTestUnit,
  Controls,
  Forms,
  Windows,
  ExpertsRepository;

resourcestring
 sNewTestUnitCaption = 'DUnitX Unit';
 sNewTestProjectHint = 'Create New DUnitX Test Unit';

{ TDUnitXNewUnitWizard }

class procedure TDUnitXNewUnitWizard.RegisterDUnitXNewUnitWizard(const APersonality: string);
begin
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(APersonality,
    sNewTestProjectHint, sNewTestUnitCaption, 'DunitX.Wizard.NewUnitWizard',  // do not localize
    'DUnitX', 'DUnitX Team - https://github.com/VSoftTechnologies/DUnitX', // do not localize
    procedure
    var
      WizardForm     : TfrmDunitXNewUnit;
      ModuleServices : IOTAModuleServices;
      Project        : IOTAProject;
      TestUnit       : IOTAModule;
    begin
      WizardForm := TfrmDunitXNewUnit.Create(Application);
      try
        if WizardForm.ShowModal = mrOk then
        begin
          ModuleServices := (BorlandIDEServices as IOTAModuleServices);
          Project :=  GetActiveProject;
          TestUnit := ModuleServices.CreateModule(
                           TNewTestUnit.Create(WizardForm.CreateSetupTearDownMethods,
                                               WizardForm.CreateSampleMethods,
                                               WizardForm.TestFixtureClasaName,
                                               APersonality));
          if Project <> nil then
            Project.AddFile(TestUnit.FileName,true);
        end;
      finally
        WizardForm.Free;
      end;
    end,
    function: Cardinal
    begin
      Result := LoadIcon(HInstance,'DUnitXNewUnitIcon');
    end,
    TArray<string>.Create(cWin32Platform, cWin64Platform, cOSX32Platform, cAndroidPlatform, ciOSSimulatorPlatform, ciOSDevice32Platform, ciOSDevice64Platform),
    nil));
 end;

end.
