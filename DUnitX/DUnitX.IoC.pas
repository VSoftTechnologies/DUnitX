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

unit DUnitX.IoC;

///
///  A Simple IoC container. This is used internally by DUnitX, not intended
///  for use by tests. Does not do DI.

interface

uses
  SysUtils;

type
  TActivatorDelegate<T : IInterface> = reference to function: T;

  TDUnitXIoC = class
  public
    //Registration
    class procedure RegisterType<TInterface:IInterface;TImplementation:class>(const name : string = '');overload;
    class procedure RegisterType<TInterface:IInterface;TImplementation:class>(const singleton : boolean;const name : string = '');overload;

    class procedure RegisterType<TInterface:IInterface>(const delegate : TActivatorDelegate<TInterface>; const name : string = '');overload;
    class procedure RegisterType<TInterface:IInterface>(const singleton : boolean;const delegate : TActivatorDelegate<TInterface>; const name : string = '');overload;

    //Resolution
    class function Resolve<T : IInterface>(const name: string = ''): T;
  end;

  EIoCException = class(Exception);
  EIoCRegistrationException = class(EIoCException);
  EIoCResolutionException = class(EIoCException);


implementation

uses
  TypInfo,
  Rtti,
  Generics.Collections,
  DUnitX.IoC.Internal;



class procedure TDUnitXIoC.RegisterType<TInterface, TImplementation>(const name: string);
begin
  TDUnitXIoC.RegisterType<TInterface, TImplementation>(false,name);
end;



class procedure TDUnitXIoC.RegisterType<TInterface, TImplementation>(const singleton: boolean; const name: string);
var
  key   : string;
  pInfo : PTypeInfo;
  rego  : TIoCRegistration<TInterface>;
  o     : TObject;
begin
  pInfo := TypeInfo(TInterface);
  if name = '' then
    key := string(pInfo.Name)
  else
    key := string(pInfo.Name) + '_' + name;
  key := LowerCase(key);

  if not IocContainerInfo.TryGetValue(key,o) then
  begin
    rego := TIoCRegistration<TInterface>.Create;
    rego.IInterface := pInfo;
    rego.ActivatorDelegate := nil;
    rego.ImplClass := TImplementation;
    rego.IsSingleton := singleton;
    IocContainerInfo.Add(key,rego);
  end
  else
    raise EIoCException.Create(Format('An implementation for type %s with name %s is already registered with IoC',[pInfo.Name, name]));
end;


class procedure TDUnitXIoC.RegisterType<TInterface>(const delegate: TActivatorDelegate<TInterface>; const name: string);
begin
  TDUnitXIoC.RegisterType<TInterface>(false,delegate,name);
end;


class procedure TDUnitXIoC.RegisterType<TInterface>(const singleton: boolean; const delegate: TActivatorDelegate<TInterface>; const name: string);
var
  key   : string;
  pInfo : PTypeInfo;
  rego  : TIoCRegistration<TInterface>;
  o     : TObject;
begin
  pInfo := TypeInfo(TInterface);
  if name = '' then
    key := string(pInfo.Name)
  else
    key := string(pInfo.Name) + '_' + name;
  key := LowerCase(key);

  if not IocContainerInfo.TryGetValue(key,o) then
  begin
    rego := TIoCRegistration<TInterface>.Create;
    rego.IInterface := pInfo;
    rego.ActivatorDelegate := delegate;
    rego.ImplClass := nil;
    rego.IsSingleton := singleton;
    IocContainerInfo.Add(key,rego);
  end
  else
    raise EIoCException.Create(Format('An implementation for type %s with name %s is already registered with IoC',[pInfo.Name, name]));
end;



class function TDUnitXIoC.Resolve<T>(const name: string): T;
var
  key   : string;
  pInfo : PTypeInfo;
  rego  : TIoCRegistration<T>;
  lcontainer : TDictionary<string,TObject>;
  res : IInterface;
  activator : TActivatorDelegate<T>;
  o : TObject;
begin
  result := Default(T);
  pInfo := TypeInfo(T);
  if name = '' then
    key := string(pInfo.Name)
  else
    key := string(pInfo.Name) + '_' + name;
  key := LowerCase(key);

  lcontainer := IocContainerInfo;

  if not lcontainer.TryGetValue(key,o) then
    raise EIoCResolutionException.Create(Format('No implementation registered for type %s',[pInfo.Name]));

  rego := TIoCRegistration<T>(o);

  if rego.IsSingleton then
  begin
    //If a singleton was registered with this interface then check if it's already been instanciated.
    if rego.Instance <> nil then
    begin
      //get the result as T
      if rego.Instance.QueryInterface(GetTypeData(pInfo).Guid,result) <> 0 then
        //should never happen?
        raise EIoCResolutionException.Create(Format('The Implementation registered for type %s does not actually implement %s' ,[pInfo.Name,pInfo.Name]));
    end
    else
    begin
      MonitorEnter(lcontainer);
      try
        if rego.ImplClass <> nil then
          rego.Instance := TClassActivator.CreateInstance(rego.ImplClass)
        else if rego.ActivatorDelegate <> nil then
        begin
          rego.Instance := rego.ActivatorDelegate();
          if rego.Instance = nil then
            raise EIoCResolutionException.Create(Format('The Activator Delegate registered for type %s did not instantiate an instance' ,[pInfo.Name]));
        end;
        //get the result as T
        if rego.Instance.QueryInterface(GetTypeData(pInfo).Guid,result) <> 0 then
        //should never happen?
          raise EIoCResolutionException.Create(Format('The Implementation registered for type %s does not actually implement %s' ,[pInfo.Name,pInfo.Name]));

        lcontainer.AddOrSetValue(key,rego);

      finally
        MonitorExit(lcontainer);
      end;
    end;
  end
  else
  begin
    MonitorEnter(lcontainer);
    try
      if rego.ImplClass <> nil then
      begin
        res := TClassActivator.CreateInstance(rego.ImplClass);
        if res.QueryInterface(GetTypeData(pInfo).Guid,result) <> 0 then
        //should never happen?
          raise EIoCResolutionException.Create(Format('The Implementation registered for type %s does not actually implement %s' ,[pInfo.Name,pInfo.Name]));
      end
      else if rego.ActivatorDelegate <> nil then
      begin
        activator := TActivatorDelegate<T>(rego.ActivatorDelegate);
        res := activator();
        if res = nil then
          raise EIoCResolutionException.Create(Format('The Activator Delegate registered for type %s did not instantiate an instance' ,[pInfo.Name]));
        if res.QueryInterface(GetTypeData(pInfo).Guid,result) <> 0 then
        //should never happen?
          raise EIoCResolutionException.Create(Format('The Implementation registered for type %s does not actually implement %s' ,[pInfo.Name,pInfo.Name]));
      end;

    finally
      MonitorExit(lcontainer);
    end;
  end;


end;

end.
