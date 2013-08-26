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
  TResolveResult = (Unknown, Success, InterfaceNotRegistered, ImplNotRegistered, DeletegateFailedCreate);

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

    function GetInterfaceKey<TInterface>(const AName: string = ''): string;
    function InternalResolve<TInterface: IInterface>(out AInterface: TInterface; const AName: string = ''): TResolveResult;

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
    function Resolve<TInterface: IInterface>(const name: string = ''; const AThrowOnFail : boolean = false): TInterface;

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
  Self.RegisterType<TInterface>(singleton, delegate, '');
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


function TDUnitXIoC.GetInterfaceKey<TInterface>(const AName: string): string;
var
  pInfo : PTypeInfo;
begin
  //By default the key is the interface name unless otherwise found.
  pInfo := TypeInfo(TInterface);
  result := string(pInfo.Name);

  if (AName <> '') then
    result := result + '_' + AName;

  //All keys are stored in lower case form.
  result := LowerCase(result);
end;

function TDUnitXIoC.InternalResolve<TInterface>(out AInterface: TInterface; const AName: string): TResolveResult;
var
  key : string;
  errorMsg : string;
  
  container : TDictionary<string,TObject>;
  registrationObj : TObject;
  registration  : TIoCRegistration<TInterface>;
  resolvedInf : IInterface;
  resolvedObj : TInterface;
  bIsSingleton: Boolean;
  bInstanciate: Boolean;
begin
  AInterface := Default(TInterface);
  Result := TResolveResult.Unknown;

  //Get the key for the interace we are resolving and locate the container for that key.
  key := GetInterfaceKey<TInterface>(AName);
  container := FContainerInfo;

  if not container.TryGetValue(key, registrationObj) then
  begin
    result := TResolveResult.InterfaceNotRegistered;
    Exit;
  end;

  //Get the interface registration class correctly.
  registration := TIoCRegistration<TInterface>(registrationObj);
  bIsSingleton := registration.IsSingleton;

  bInstanciate := true;

  if bIsSingleton then
  begin
    //If a singleton was registered with this interface then check if it's already been instanciated.
    if registration.Instance <> nil then
    begin
      //Get AInterface as TInterface
      if registration.Instance.QueryInterface(GetTypeData(TypeInfo(TInterface)).Guid, AInterface) <> 0 then
      begin
        result  := TResolveResult.ImplNotRegistered;
        Exit;
      end;

      bInstanciate := False;
    end;
  end;

  if bInstanciate then
  begin
    //If the instance hasn't been instanciated then we need to lock and instanciate
    MonitorEnter(container);
    try
      //If we have a implementing class then used this to activate.
      if registration.ImplClass <> nil then
        resolvedInf := TClassActivator.CreateInstance(registration.ImplClass)
      //Otherwise if there is a activate delegate use this to activate.
      else if registration.ActivatorDelegate <> nil then
      begin
        resolvedInf := registration.ActivatorDelegate();

        if resolvedInf = nil then
        begin
          result  := TResolveResult.DeletegateFailedCreate;
          Exit;
        end;
      end;

      //Get AInterface as TInterface
      if resolvedInf.QueryInterface(GetTypeData(TypeInfo(TInterface)).Guid, resolvedObj) <> 0 then
      begin
        result  := TResolveResult.ImplNotRegistered;
        Exit;
      end;

      AInterface := resolvedObj;

      if bIsSingleton then
      begin
        registration.Instance := resolvedObj;

        //Reset the registration to show the instance which was created.
        container.AddOrSetValue(key, registration);
      end;
    finally
      MonitorExit(container);
    end;
  end;
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

function TDUnitXIoC.Resolve<TInterface>(const name: string = ''; const AThrowOnFail : boolean = false): TInterface;
var
  resolveResult: TResolveResult;
  errorMsg : string;
  pInfo : PTypeInfo;
begin
  pInfo := TypeInfo(TInterface);
  resolveResult := InternalResolve<TInterface>(result, name);

  //If we don't have a resolution and the caller wants an exception then throw one.
  if (result = nil) and (AThrowOnFail) then
    begin
    case resolveResult of
      TResolveResult.Success : ;
      TResolveResult.InterfaceNotRegistered : errorMsg := Format('No implementation registered for type %s', [pInfo.Name]);
      TResolveResult.ImplNotRegistered : errorMsg := Format('The Implementation registered for type %s does not actually implement %s', [pInfo.Name, pInfo.Name]);
      TResolveResult.DeletegateFailedCreate : errorMsg := Format('The Implementation registered for type %s does not actually implement %s', [pInfo.Name, pInfo.Name]);
    else
      //All other error types are treated as unknown until defined here.
      errorMsg := Format('An Unknown Error has occurred for the resolution of the interface %s %s. This is either because a new error type isn''t being handled, ' +
          'or it''s an bug.', [pInfo.Name, name]);
    end;

    raise EIoCResolutionException.Create(errorMsg);
  end;
end;

end.
