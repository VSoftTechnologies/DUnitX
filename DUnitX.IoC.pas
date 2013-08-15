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

{$I DUnitX.inc}

///
///  A Simple IoC container. This is used internally by DUnitX, not intended
///  for use by tests. Does not do DI.

interface

uses
  Generics.Collections,
  TypInfo,
  SysUtils;

type
  TActivatorDelegate<TInterface: IInterface> = reference to function: TInterface;



  TDUnitXIoC = class
  private
    type
      TIoCRegistration<T : IInterface> = class
      IInterface        : PTypeInfo;
      ImplClass         : TClass;
      ActivatorDelegate : TActivatorDelegate<T>;
      IsSingleton       : boolean;
      Instance          : IInterface;
    end;

    private class var
      FDefault : TDUnitXIoC;

    FContainerInfo : TDictionary<string,TObject>;

    private class destructor ClassDestroy;

  public
    constructor Create;
    destructor Destroy;override;
    {$IFDEF DELPHI_XE_UP}
    //Exe's compiled with D2010 will crash when these are used.
    //NOTES: The issue is due to the two generics included in the functions. The constaints also seem to be an issue.
    procedure RegisterType<TInterface: IInterface; TImplementation: class>;overload;
    procedure RegisterType<TInterface: IInterface; TImplementation: class>(const name : string);overload;
    procedure RegisterType<TInterface: IInterface; TImplementation: class>(const singleton : boolean);overload;
    procedure RegisterType<TInterface: IInterface; TImplementation: class>(const singleton : boolean;const name : string);overload;
    {$ENDIF}
    procedure RegisterType<TInterface: IInterface>(const delegate : TActivatorDelegate<TInterface>);overload;
    procedure RegisterType<TInterface: IInterface>(const delegate : TActivatorDelegate<TInterface>; const name : string );overload;
    procedure RegisterType<TInterface: IInterface>(const singleton : boolean;const delegate : TActivatorDelegate<TInterface>);overload;
    procedure RegisterType<TInterface: IInterface>(const singleton : boolean;const delegate : TActivatorDelegate<TInterface>; const name : string);overload;

    //Resolution
    function Resolve<TInterface: IInterface>(const name: string = ''): TInterface; overload;

    //Default Container - used internally by DUnitX
    class function DefaultContainer : TDUnitXIoC;
  end;

  EIoCException = class(Exception);
  EIoCRegistrationException = class(EIoCException);
  EIoCResolutionException = class(EIoCException);


implementation

uses
  Rtti,
  DUnitX.IoC.Internal;

{$IFDEF DELPHI_XE_UP}

procedure TDUnitXIoC.RegisterType<TInterface, TImplementation>();
begin
  Self.RegisterType<TInterface, TImplementation>(false,'');
end;

procedure TDUnitXIoC.RegisterType<TInterface, TImplementation>(const name: string);
var
  newName : string;
begin
  newName := name;

  Self.RegisterType<TInterface, TImplementation>(false, newName);
end;

procedure TDUnitXIoC.RegisterType<TInterface, TImplementation>(const singleton: boolean);
var
  newSingleton : boolean;
begin
  newSingleton := singleton;

  Self.RegisterType<TInterface, TImplementation>(newSingleton,'');
end;

procedure TDUnitXIoC.RegisterType<TInterface, TImplementation>(const singleton: boolean; const name: string);
var
  key   : string;
  pInfo : PTypeInfo;
  rego  : TIoCRegistration<TInterface>;
  o     : TObject;
  newName : string;
  newSingleton : boolean;
begin
  newSingleton := singleton;
  newName := name;

  pInfo := TypeInfo(TInterface);
  if newName = '' then
    key := string(pInfo.Name)
  else
    key := string(pInfo.Name) + '_' + newName;
  key := LowerCase(key);

  if not FContainerInfo.TryGetValue(key,o) then
  begin
    rego := TIoCRegistration<TInterface>.Create;
    rego.IInterface := pInfo;
    rego.ActivatorDelegate := nil;
    rego.ImplClass := TImplementation;
    rego.IsSingleton := newSingleton;
    FContainerInfo.Add(key,rego);
  end
  else
    raise EIoCException.Create(Format('An implementation for type %s with name %s is already registered with IoC',[pInfo.Name, newName]));
end;
{$ENDIF}

procedure TDUnitXIoC.RegisterType<TInterface>(const delegate: TActivatorDelegate<TInterface>);
begin
  Self.RegisterType<TInterface>(false, delegate, '');
end;


procedure TDUnitXIoC.RegisterType<TInterface>(const delegate: TActivatorDelegate<TInterface>; const name: string);
begin
  Self.RegisterType<TInterface>(false, delegate, name);
end;

procedure TDUnitXIoC.RegisterType<TInterface>(const singleton: boolean; const delegate: TActivatorDelegate<TInterface>);
begin
  Self.RegisterType<TInterface>(false,delegate,'');
end;

class destructor TDUnitXIoC.ClassDestroy;
begin
  FDefault.Free;
end;

constructor TDUnitXIoC.Create;
begin
  FContainerInfo := TDictionary<string,TObject>.Create;
end;

class function TDUnitXIoC.DefaultContainer: TDUnitXIoC;
begin
  if FDefault = nil then
    FDefault := TDUnitXIoC.Create;

  result := FDefault;
end;

destructor TDUnitXIoC.Destroy;
var
  o : TObject;
begin
  for o in FContainerInfo.Values do
    o.Free;

  FContainerInfo.Free;
  inherited;
end;


procedure TDUnitXIoC.RegisterType<TInterface>(const singleton: boolean; const delegate: TActivatorDelegate<TInterface>; const name: string);
var
  key   : string;
  pInfo : PTypeInfo;
  rego  : TIoCRegistration<TInterface>;
  o     : TObject;
  newName : string;
begin
  newName := name;

  pInfo := TypeInfo(TInterface);
  if newName = '' then
    key := string(pInfo.Name)
  else
    key := string(pInfo.Name) + '_' + newName;
  key := LowerCase(key);

  if not FContainerInfo.TryGetValue(key,o) then
  begin
    rego := TIoCRegistration<TInterface>.Create;
    rego.IInterface := pInfo;
    rego.ActivatorDelegate := delegate;
    rego.ImplClass := nil;
    rego.IsSingleton := singleton;
    FContainerInfo.Add(key,rego);
  end
  else
    raise EIoCException.Create(Format('An implementation for type %s with name %s is already registered with IoC',[pInfo.Name, newName]));
end;





function TDUnitXIoC.Resolve<TInterface>(const name: string): TInterface;
var
  key   : string;
  pInfo : PTypeInfo;
  rego  : TIoCRegistration<TInterface>;
  lcontainer : TDictionary<string,TObject>;
  res : IInterface;
  activator : TActivatorDelegate<TInterface>;
  o : TObject;
  newName : string;
begin
  newName := name;

  result := Default(TInterface);
  pInfo := TypeInfo(TInterface);
  if newName = '' then
    key := string(pInfo.Name)
  else
    key := string(pInfo.Name) + '_' + newName;
  key := LowerCase(key);

  lcontainer := FContainerInfo;

  if not lcontainer.TryGetValue(key,o) then
  begin
    exit(nil);
  end;
//    raise EIoCResolutionException.Create(Format('No implementation registered for type %s',[pInfo.Name]));

  rego := TIoCRegistration<TInterface>(o);

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
        if rego.Instance.QueryInterface( GetTypeData(pInfo).Guid,result) <> 0 then
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
        activator := TActivatorDelegate<TInterface>(rego.ActivatorDelegate);
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
