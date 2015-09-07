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

unit DUnitX.Extensibility.PluginManager;

interface

{$I DUnitX.inc}

uses
  DUnitX.Generics,
  DUnitX.Extensibility;

type
  IPluginManager = interface
    ['{0AD83588-CF0F-4185-B5F8-093893150BB3}']
    procedure Init;
    procedure CreateFixtures;
  end;

  TCreateFixtureProc = function(const AInstance : TObject; const AFixtureClass: TClass; const AName: string; const ACategory : string): ITestFixture of object;

  TPluginManager = class(TInterfacedObject,IPluginManager,IPluginLoadContext,IFixtureProviderContext)
  private
    FFixtureProviders : IList<IFixtureProvider>;
    FCreateFixtureProc : TCreateFixtureProc;
    FUseRtti : boolean;
  protected
    //IPluginLoader
    procedure Init;
    procedure CreateFixtures;

    //IPluginLoadContext
    procedure RegisterFixtureProvider(const provider: IFixtureProvider);

    //IFixtureProviderContext
    function CreateFixture(const AFixtureClass: TClass; const AName: string; const ACategory : string): ITestFixture;overload;
    function CreateFixture(const AInstance : TObject; const AName: string; const ACategory : string): ITestFixture;overload;
    function GetUseRtti: Boolean;

  public
    constructor Create(const ACreateFixtureProc : TCreateFixtureProc; const AUseRtti : boolean);
    destructor Destroy;override;


  end;

implementation

uses
  DUnitX.TestFramework;

{ TPluginManager }

constructor TPluginManager.Create(const ACreateFixtureProc : TCreateFixtureProc; const AUseRtti : boolean);
begin
  FFixtureProviders := TDUnitXList<IFixtureProvider>.Create;
  FCreateFixtureProc := ACreateFixtureProc;
  FUseRtti := AUseRtti;

end;

function TPluginManager.CreateFixture(const AFixtureClass: TClass; const AName: string; const ACategory : string): ITestFixture;
begin
  //delegate back to the runner so it can own the fixture.
  result := FCreateFixtureProc(nil,AFixtureClass,AName,ACategory);
end;

function TPluginManager.CreateFixture(const AInstance: TObject; const AName: string; const ACategory : string): ITestFixture;
begin
  //delegate back to the runner so it can own the fixture.
  result := FCreateFixtureProc(AInstance,AInstance.ClassType,AName,ACategory);

end;

procedure TPluginManager.CreateFixtures;
var
  provider : IFixtureProvider;
begin
  for provider in FFixtureProviders do
  begin
    provider.Execute(Self);
  end;
end;

destructor TPluginManager.Destroy;
begin
  FFixtureProviders := nil;//not needed, just aids debugging;
  inherited;
end;

function TPluginManager.GetUseRtti: Boolean;
begin
  result := FUseRtti;
end;

procedure TPluginManager.Init;
var
  plugin : IPlugin;
begin

  for plugin in TDUnitX.RegisteredPlugins do
  begin
    plugin.GetPluginFeatures(Self);
  end;
end;


procedure TPluginManager.RegisterFixtureProvider(const provider: IFixtureProvider);
begin
  FFixtureProviders.Add(provider);
end;

end.
