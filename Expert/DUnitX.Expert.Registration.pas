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

unit DUnitX.Expert.Registration;

interface

{$I DUnitX.inc}


//Note: "Register" method name is case senstive.
procedure Register;

implementation
uses
  ToolsApi,
  {$IFDEF USE_NS}
  Vcl.Dialogs,
  {$ELSE}
  Dialogs,
  {$ENDIF}
  {$IFDEF DELPHIX_SEATTLE_UP}
  DUnitX.Expert.ProjectWizardEx,
  DUnitX.Expert.NewUnitWizardEx;
  {$ELSE}
  DUnitX.Expert.ProjectWizard,
  DUnitX.Expert.NewUnitWizard;
  {$ENDIF}

procedure Register;
begin
  {$IFDEF DELPHIX_SEATTLE_UP}
  TDUnitXNewProjectWizard.RegisterDUnitXProjectWizard(sDelphiPersonality);
  TDUnitXNewProjectWizard.RegisterDUnitXProjectWizard(sCBuilderPersonality);

  TDUnitXNewUnitWizard.RegisterDUnitXNewUnitWizard(sDelphiPersonality);
  TDUnitXNewUnitWizard.RegisterDUnitXNewUnitWizard(sCBuilderPersonality);
  {$ELSE}
  RegisterPackageWizard(TDUnitXNewProjectWizard.Create);
  RegisterPackageWizard(TDUnitXNewUnitWizard.Create);
  {$ENDIF}
end;

end.
