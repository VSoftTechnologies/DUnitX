{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2013 Vincent Parrett                              }
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

unit DUnitX.Expert.CodeGen.NewProject;
// This is done to Warnings that I can't control, as Embarcadero has
// deprecated the functions, but due to design you are still required to
// to implement.
{$WARN SYMBOL_DEPRECATED OFF}
interface

{$I DUnitX.inc}

uses
{$IFDEF DELPHI_XE103_UP}
  PlatformConst,
{$ENDIF}
  PlatformAPI,
  ToolsAPI;

type
  TNewProject = class abstract(TNotifierObject, IOTACreator, IOTAProjectCreator, IOTAProjectCreator80)
  protected
    //IOTACreator
    function GetCreatorType : string; virtual;
    function GetExisting : Boolean;
    function GetFileSystem : string;
    function GetOwner : IOTAModule;
    function GetUnnamed : Boolean;
    // IOTAProjectCreator
    function GetFileName : string;
    function GetOptionFileName : string; deprecated;
    function GetShowSource : Boolean;
    procedure NewDefaultModule; deprecated;
    function NewOptionSource(const ProjectName : string) : IOTAFile; deprecated;
    procedure NewProjectResource(const Project : IOTAProject);
    function NewProjectSource(const ProjectName : string) : IOTAFile; virtual; abstract; // MUST OVERRIDE!
    // IOTAProjectCreator80
    function GetProjectPersonality : string; virtual;
    procedure NewDefaultProjectModule(const Project : IOTAProject);
  private
    procedure SetFileName(const Value : string);
  protected
    FFileName : string;
  public
    property FileName : string read GetFileName write SetFileName;
  end;

{$IFDEF DELPHIX_SEATTLE_UP}
  TNewProjectEx = class(TNewProject, IOTAProjectCreator160)
  private
    FPersonality : string;
  protected
    function GetProjectPersonality : string; override;

    // IOTAProjectCreator160
    function GetPlatforms : TArray<string>;
    function GetFrameworkType : string;
    function GetPreferredPlatform : string;
    procedure SetInitialOptions(const NewProject : IOTAProject);
  public
    property Personality : string read FPersonality write FPersonality;
  end;
{$ENDIF}

implementation

uses
  System.SysUtils;

{ TNewProject }

function TNewProject.GetCreatorType : string;
begin
  result := sConsole;                   // May want to change this in the future, at least making method virtual
end;

function TNewProject.GetExisting : Boolean;
begin
  result := false;
end;

function TNewProject.GetFileName : string;
begin
  result := FFileName;
end;

function TNewProject.GetFileSystem : string;
begin
  result := '';
end;

function TNewProject.GetOptionFileName : string;
begin
  result := '';
end;

function TNewProject.GetOwner : IOTAModule;
begin
  result := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
end;

function TNewProject.GetProjectPersonality : string;
begin
  result := sDelphiPersonality;
end;

function TNewProject.GetShowSource : Boolean;
begin
  result := false;
end;

function TNewProject.GetUnnamed : Boolean;
begin
  result := true;
end;

procedure TNewProject.NewDefaultModule;
begin
end;

procedure TNewProject.NewDefaultProjectModule(const Project : IOTAProject);
begin
end;

function TNewProject.NewOptionSource(const ProjectName : string) : IOTAFile;
begin
  result := nil;
end;

procedure TNewProject.NewProjectResource(const Project : IOTAProject);
begin
end;

procedure TNewProject.SetFileName(const Value : string);
begin
  FFileName := Value;
end;

{$IFDEF DELPHIX_SEATTLE_UP}
function TNewProjectEx.GetFrameworkType : string;
begin
  Result := '';
end;

function TNewProjectEx.GetPlatforms : TArray<string>;
var
  Candidates : TArray<string>;
  PlatformServices : IOTAPlatformServices;
  PlatformName : string;
begin
{$IFDEF DELPHI_XE103_UP}
  // Every platform the IDE knows, as the gallery registration uses; the filter below keeps the ones it has.
  Candidates := GetAllPlatforms;
{$ELSE}
  if SameText(GetProjectPersonality, sDelphiPersonality) then
    Candidates := TArray<string>.Create(cWin32Platform, cWin64Platform, cOSX32Platform, cAndroidPlatform,
      ciOSSimulatorPlatform{$IFDEF DELPHI_XE8_UP}, ciOSDevice32Platform, ciOSDevice64Platform{$ENDIF})
  else
    Candidates := TArray<string>.Create(cWin32Platform, cWin64Platform, cOSX32Platform, cAndroidPlatform{$IFDEF DELPHI_XE8_UP},
      ciOSDevice32Platform, ciOSDevice64Platform{$ENDIF});
{$ENDIF}
  // Offer only the platforms this IDE has for the project's personality.
  if not Supports(BorlandIDEServices, IOTAPlatformServices, PlatformServices) then
    Exit(Candidates);
  Result := nil;
  for PlatformName in Candidates do
    if PlatformServices.PlatformAvailable(PlatformName, GetProjectPersonality) then
      Result := Result + [PlatformName];
end;

function TNewProjectEx.GetPreferredPlatform : string;
begin
  Result := '';
end;

function TNewProjectEx.GetProjectPersonality : string;
begin
  if FPersonality.IsEmpty then
    result := sDelphiPersonality
  else
    result := FPersonality;
end;

procedure TNewProjectEx.SetInitialOptions(const NewProject : IOTAProject);
var
  LBuildConf : IOTAProjectOptionsConfigurations;
  PlatformName : string;
  PlatformConf : IOTABuildConfiguration;
begin
  // Both are C++Builder options: a Delphi project gets neither.
  if (FPersonality = sCBuilderPersonality) and
    Supports(NewProject.ProjectOptions, IOTAProjectOptionsConfigurations, LBuildConf) then
  begin
    LBuildConf.BaseConfiguration.AsBoolean['UsingDelphiRTL'] := True;
    // Only the project's own platforms are indexed: one it does not have may have no configuration.
    for PlatformName in LBuildConf.BaseConfiguration.Platforms do
      if SameText(PlatformName, cAndroidPlatform) or SameText(PlatformName, ciOSDevice32Platform) or
        SameText(PlatformName, ciOSDevice64Platform) then
      begin
        PlatformConf := LBuildConf.BaseConfiguration.PlatformConfiguration[PlatformName];
        if PlatformConf <> nil then
          PlatformConf.AsBoolean['ILINK_LinkwithDUnitXRuntime'] := True;
      end;
  end;
end;
{$ENDIF}

end.

