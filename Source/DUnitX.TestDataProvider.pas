{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2017 Vincent Parrett & Contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{           This unit authored by Uwe Rupprecht                             }
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

unit DUnitX.TestDataProvider;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Classes,
  System.Generics.Collections,
  {$ELSE}
  Classes,
  Generics.Collections,
  {$ENDIF}
  DUnitX.Types,
  DUnitX.InternalDataProvider;

type
  TestDataProviderManager = class
  private
    class var FList : TDictionary<string, TClass>;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure RegisterProvider(const name : string; const AClass : TTestDataProviderClass);
    class procedure UnregisterProvider(const name : string);

    class function GetProvider(const name : string) : ITestDataProvider;overload;
    class function GetProvider(const AClass:TTestDataProviderClass) : ITestDataProvider;overload;
  end;

implementation

{ TestDataProviderManager }

class constructor TestDataProviderManager.Create;
begin
  FList := TDictionary<string, TClass>.Create;
end;

class Destructor TestDataProviderManager.Destroy;
begin
  FList.Free;
end;

class function TestDataProviderManager.GetProvider(const AClass: TTestDataProviderClass) : ITestDataProvider;
var
  key : string;
begin
  result := nil;
  if (FList.ContainsValue(AClass)) then
  begin
    for key in flist.keys do
    begin
      if (flist[key] = AClass) then
      begin
        result := TTestDataProviderClass(flist[key]).Create;
        break;
      end;
    end;
  end;
end;

class function TestDataProviderManager.GetProvider(const name : string) : ITestDataProvider;
begin
  result := nil;
  if (FList.ContainsKey(name)) then
    result := TTestDataProviderClass(FList[name]).Create;
end;

class procedure TestDataProviderManager.RegisterProvider(const name: string; const AClass: TTestDataProviderClass);
begin
  if (not FList.ContainsKey(name)) then
    FList.add(name,AClass);
end;

class procedure TestDataProviderManager.UnregisterProvider(const name: string);
begin
 if (FList.ContainsKey(name)) then
    FList.Remove(Name);
end;

end.
