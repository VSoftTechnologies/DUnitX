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
///  A Simple IoC container. This is used internally by DUnitX
///  DUnitX used the default container, if you need to use this
///  for your tests, create your own container instance.
///  NOTE: Does not do dependency Injection, if you need that then
///  use the Spring for Delphi Framework

interface

uses
  Generics.Collections,
  TypInfo,
  Rtti,
  SysUtils;

type
  TResolveResult = (Unknown, Success, InterfaceNotRegistered, ImplNotRegistered, DeletegateFailedCreate);

  TActivatorDelegate<TInterface: IInterface> = reference to function: TInterface;



  TDUnitXIoC = class
  private
    FRaiseIfNotFound : boolean;
    FContainerInfo : TDictionary<string,TObject>;
    type
      TIoCRegistration<T : IInterface> = class
      IInterface        : PTypeInfo;
      ImplClass         : TClass;
      ActivatorDelegate : TActivatorDelegate<T>;
      IsSingleton       : boolean;
      Instance          : IInterface;
    end;

    private
      class var FDefault : TDUnitXIoC;

  protected
    function GetInterfaceKey<TInterface>(const AName: string = ''): string;
    function InternalResolve<TInterface: IInterface>(out AInterface: TInterface; const AName: string = ''): TResolveResult;
    procedure InternalRegisterType<TInterface : IInterface>(const singleton : boolean; const AImplementation : TClass; const delegate : TActivatorDelegate<TInterface>; const name : string = '');
  public
    constructor Create;
    destructor Destroy;override;
    class destructor ClassDestroy;
    //Default Container - used internally by DUnitX
    class function DefaultContainer : TDUnitXIoC;
    {$IFDEF DELPHI_XE_UP}
    //Exe's compiled with D2010 will crash when these are used.
    //NOTES: The issue is due to the two generics included in the functions. The constaints also seem to be an issue.
    procedure RegisterType<TInterface: IInterface; TImplementation: class>(const name : string = '');overload;
    procedure RegisterType<TInterface: IInterface; TImplementation: class>(const singleton : boolean;const name : string = '');overload;
    {$ENDIF}
    procedure RegisterType<TInterface: IInterface>(const delegate : TActivatorDelegate<TInterface>; const name : string = '' );overload;
    procedure RegisterType<TInterface: IInterface>(const singleton : boolean;const delegate : TActivatorDelegate<TInterface>; const name : string = '');overload;

    //Register an instance as a signleton. If there is more than one instance that implements the interface
    //then use the name parameter
    procedure RegisterSingleton<TInterface :IInterface>(const instance : TInterface; const name : string = '');

    //Resolution
    function Resolve<TInterface: IInterface>(const name: string = ''): TInterface;

    //Returns true if we have such a service.
    function HasService<T: IInterface> : boolean;

    //Empty the Container.. usefull for testing only!
    procedure Clear;

    property RaiseIfNotFound : boolean read FRaiseIfNotFound write FRaiseIfNotFound;



  end;

  EIoCException = class(Exception);
  EIoCRegistrationException = class(EIoCException);
  EIoCResolutionException = class(EIoCException);


  //Makes sure virtual constructors are called correctly. Just using a class reference will not call the overriden constructor!
  //See http://stackoverflow.com/questions/791069/how-can-i-create-an-delphi-object-from-a-class-reference-and-ensure-constructor
  TClassActivator = class
  private
    class var
      FRttiCtx : TRttiContext;
      class constructor Create;
  public
    class function CreateInstance(const AClass : TClass) : IInterface;
  end;


implementation


{ TActivator }

class constructor TClassActivator.Create;
begin
  TClassActivator.FRttiCtx := TRttiContext.Create;
end;

class function TClassActivator.CreateInstance(const AClass : TClass): IInterface;
var
  rType : TRttiType;
  method: TRttiMethod;
begin
  result := nil;

  rType := FRttiCtx.GetType(AClass);
  if not (rType is TRttiInstanceType) then
    exit;

  for method in TRttiInstanceType(rType).GetMethods do
  begin
    if method.IsConstructor and (Length(method.GetParameters) = 0) then
    begin
      Result := method.Invoke(TRttiInstanceType(rtype).MetaclassType, []).AsInterface;
      Break;
    end;
  end;

end;




function TDUnitXIoC.HasService<T>: boolean;
begin
  result := Self.Resolve<T> <> nil;
end;

{$IFDEF DELPHI_XE_UP}

procedure TDUnitXIoC.RegisterType<TInterface, TImplementation>(const name: string);
begin
  InternalRegisterType<TInterface>(false,TImplementation,nil,name);
end;

procedure TDUnitXIoC.RegisterType<TInterface, TImplementation>(const singleton: boolean; const name: string);
begin
  Self.InternalRegisterType<TInterface>(singleton,TImplementation,nil,name);
end;
{$ENDIF}

procedure TDUnitXIoC.InternalRegisterType<TInterface>(const singleton : boolean; const AImplementation : TClass; const delegate : TActivatorDelegate<TInterface>; const name : string = '');
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
    rego.ActivatorDelegate := delegate;
    rego.ImplClass := AImplementation;
    rego.IsSingleton := newSingleton;
    FContainerInfo.Add(key,rego);
  end
  else
  begin
    rego := TIoCRegistration<TInterface>(o);
    //cannot replace a singleton that has already been instanciated.
    if rego.IsSingleton and (rego.Instance <> nil)  then
      raise EIoCException.Create(Format('An implementation for type %s with name %s is already registered with IoC',[pInfo.Name, newName]));
    rego.IInterface := pInfo;
    rego.ActivatorDelegate := delegate;
    rego.ImplClass := AImplementation;
    rego.IsSingleton := newSingleton;
    FContainerInfo.AddOrSetValue(key,rego);
  end;
end;


procedure TDUnitXIoC.RegisterType<TInterface>(const delegate: TActivatorDelegate<TInterface>; const name: string);
begin
  Self.InternalRegisterType<TInterface>(false, nil,delegate, name);
end;


class destructor TDUnitXIoC.ClassDestroy;
begin
  if FDefault <> nil then
    FDefault.Free;
end;

procedure TDUnitXIoC.Clear;
begin
  FContainerInfo.Clear;
end;

constructor TDUnitXIoC.Create;
begin
  FContainerInfo := TDictionary<string,TObject>.Create;
  FRaiseIfNotFound := false;
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
  if FContainerInfo <> nil then
  begin
    for o in FContainerInfo.Values do
      if o <> nil then
        o.Free;

    FContainerInfo.Free;
  end;
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

procedure TDUnitXIoC.RegisterSingleton<TInterface>(const instance: TInterface; const name: string);
var
  key   : string;
  pInfo : PTypeInfo;
  rego  : TIoCRegistration<TInterface>;
  o     : TObject;
begin
  pInfo := TypeInfo(TInterface);
  key := GetInterfaceKey<TInterface>(name);

  if not FContainerInfo.TryGetValue(key,o) then
  begin
    rego := TIoCRegistration<TInterface>.Create;
    rego.IInterface := pInfo;
    rego.ActivatorDelegate := nil;
    rego.ImplClass := nil;
    rego.IsSingleton := true;
    rego.Instance := instance;
    FContainerInfo.Add(key,rego);
  end
  else
    raise EIoCException.Create(Format('An implementation for type %s with name %s is already registered with IoC',[pInfo.Name, name]));
end;

procedure TDUnitXIoC.RegisterType<TInterface>(const singleton: boolean; const delegate: TActivatorDelegate<TInterface>; const name: string);
begin
  Self.InternalRegisterType<TInterface>(singleton,nil,delegate,name);
end;

function TDUnitXIoC.Resolve<TInterface>(const name: string = ''): TInterface;
var
  resolveResult: TResolveResult;
  errorMsg : string;
  pInfo : PTypeInfo;
begin
  pInfo := TypeInfo(TInterface);
  resolveResult := InternalResolve<TInterface>(result, name);

  //If we don't have a resolution and the caller wants an exception then throw one.
  if (result = nil) and (FRaiseIfNotFound) then
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
