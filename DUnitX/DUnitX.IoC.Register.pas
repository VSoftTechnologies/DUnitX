{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2012 Vincent Parrett                              }
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


unit DUnitX.IoC.Register;

///
///  Simple IoC container. Note that this is used internally in DUnitX, but can
///  also be used in unit tests.
///

interface

uses
  SysUtils;

type
  TActivatorDelegate<T: class> = reference to function: T;

  TDUnitXIoCRegister = class
  public
    class procedure RegisterType<TInterface:IInterface;TImplementation:class>(const name : string = '');overload;
    class procedure RegisterType<TInterface:IInterface;TImplementation:class>(const delegate : TActivatorDelegate<TImplementation>; const name : string = '');overload;
    class procedure RegisterSingletonType<TInterface:IInterface;TImplementation:class>(const name : string = '');overload;
    class procedure RegisterSingletonType<TInterface:IInterface;TImplementation:class>(const delegate : TActivatorDelegate<TImplementation>; const name : string = '');overload;
  end;



implementation

uses
  TypInfo,
  DUnitX.IoC.Internal;

{ TDUnitXIoCRegister }

class procedure TDUnitXIoCRegister.RegisterSingletonType<TInterface, TImplementation>(const name: string);
begin

end;

class procedure TDUnitXIoCRegister.RegisterSingletonType<TInterface, TImplementation>(const delegate: TActivatorDelegate<TImplementation>; const name: string);
begin

end;

class procedure TDUnitXIoCRegister.RegisterType<TInterface, TImplementation>(const name: string);
var
  key   : string;
  pInfo : PTypeInfo;
  rego  : TIoCRegistration;
begin
  pInfo := TypeInfo(TInterface);
  if name = '' then
    key := pInfo.Name
  else
    key := pInfo.Name + '_' + name;

  if not IocContainerInfo.TryGetValue(key,rego) then
  begin
    rego.IInterface := pInfo;
    rego.Name := name;
    rego.Activator := nil;
    rego.ImplClass := TImplementation;
    rego.IsSingleton := False;
  end
  else
    raise EIoCException.Create(Format('An implementation for type %s with name %s is already registered with IoC',[pInfo.Name, name]));
end;

class procedure TDUnitXIoCRegister.RegisterType<TInterface, TImplementation>(const delegate: TActivatorDelegate<TImplementation>; const name: string);
var
  key   : string;
  pInfo : PTypeInfo;
  rego  : TIoCRegistration;
begin
  pInfo := TypeInfo(TInterface);
  if name = '' then
    key := pInfo.Name
  else
    key := pInfo.Name + '_' + name;

  if not IocContainerInfo.TryGetValue(key,rego) then
  begin
    rego.IInterface := pInfo;
    rego.Name := name;
    rego.Activator := Pointer(delegate);
    rego.ImplClass := nil;
    rego.IsSingleton := False;
  end
  else
    raise EIoCException.Create(Format('An implementation for type %s with name %s is already registered with IoC',[pInfo.Name, name]));
end;

end.
