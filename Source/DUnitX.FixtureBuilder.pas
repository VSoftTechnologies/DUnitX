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

unit DUnitX.FixtureBuilder;

interface

uses
  DUnitX.Extensibility;

type
  TDUnitXFixtureBuilder = class(TInterfacedObject, IFixtureBuilder, IFixtureProviderContext)
  private
    FUseRtti : boolean;
    FFixtureList : ITestFixtureList;
  protected
    function GetUseRtti : boolean;
    function CreateFixture(const AFixtureClass : TClass; const AName : string; const ACategory : string) : ITestFixture;overload;
    function BuildFixtureList : ITestFixtureList;

  public
    constructor Create(useRtti : boolean);
  end;

implementation

uses
  DUnitX.Exceptions,
  DUnitX.ServiceLocator,
  DUnitX.TestFixture,
  DUnitX.ResStrs;

{ TDUnitXFixtureBuilder }

function TDUnitXFixtureBuilder.BuildFixtureList: ITestFixtureList;
var
  provider : IFixtureProvider;
begin
  result := FFixtureList;

  provider := TDUnitXServiceLocator.DefaultContainer.Resolve<IFixtureProvider>();
  if provider = nil then
    raise ETestFrameworkException.Create(SNoFixtureProvider);

  provider.Execute(Self);

end;

constructor TDUnitXFixtureBuilder.Create(useRtti: boolean);
begin
  FUseRtti := useRtti;
  FFixtureList := TTestFixtureList.Create;
end;


function TDUnitXFixtureBuilder.CreateFixture(const AFixtureClass: TClass; const AName, ACategory: string): ITestFixture;
begin
  result := TDUnitXTestFixture.Create(AName, ACategory, AFixtureClass, AFixtureClass.UnitName);
  FFixtureList.Add(Result);
end;

function TDUnitXFixtureBuilder.GetUseRtti: boolean;
begin
  result := FUseRtti;
end;
end.
