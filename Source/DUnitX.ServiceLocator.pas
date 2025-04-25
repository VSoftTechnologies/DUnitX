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

unit DUnitX.ServiceLocator;

{$I DUnitX.inc}

///  A Simple IoC container. This is used internally by DUnitX
///  DUnitX used the default container, if you need to use this
///  for your tests, create your own container instance.
///  NOTE: Does not do dependency Injection, if you need that then
///  use the Spring for Delphi Framework

interface

uses
  {$IFDEF USE_NS}
  System.Generics.Collections,
  System.TypInfo,
  System.Rtti,
  System.SysUtils;
  {$ELSE}
  Generics.Collections,
  TypInfo,
  Rtti,
  SysUtils;
  {$ENDIF}

type
  TActivatorDelegate<TInterface: IInterface> = reference to function: TInterface;
  TActivatorDelegate = reference to function: IInterface;

  TDUnitXServiceLocator = class
  private
    type
      TIoCRegistration = class
        ActivatorDelegate : TActivatorDelegate;
        Instance          : IInterface;
        function CreateSingletonActivator(const delegate: TActivatorDelegate): TActivatorDelegate;
        procedure Initialize(const delegate: TActivatorDelegate; singleton: Boolean);
      end;
  private
    FRaiseIfNotFound : boolean;
    FContainerInfo : TDictionary<string,TIoCRegistration>;
    class var FDefault : TDUnitXServiceLocator;
  protected
    function GetInterfaceKey(const typeInfo: PTypeInfo; const AName: string = ''): string;
    function InternalResolve(const typeInfo: PTypeInfo; const AName: string = ''): IInterface;
    procedure InternalRegisterType(const typeInfo: PTypeInfo; const singleton : boolean; const delegate : TActivatorDelegate; const name : string = '');
  public
    constructor Create;
    destructor Destroy;override;
    class destructor ClassDestroy;
    //Default Container - used internally by DUnitX
    class function DefaultContainer : TDUnitXServiceLocator;

    procedure RegisterType<TInterface: IInterface; TImplementation: class>(const name : string = '');overload;
    procedure RegisterType<TInterface: IInterface; TImplementation: class>(const singleton : boolean;const name : string = '');overload;

    procedure RegisterType<TInterface: IInterface>(const delegate : TActivatorDelegate<TInterface>; const name : string = '' );overload;
    procedure RegisterType<TInterface: IInterface>(const singleton : boolean;const delegate : TActivatorDelegate<TInterface>; const name : string = '');overload;

    //Register an instance as a singleton. If there is more than one instance that implements the interface
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

  EServiceLocatorException = class(Exception);
  EServiceLocatorRegistrationException = class(EServiceLocatorException);
  EServiceLocatorResolutionException = class(EServiceLocatorException);


  //Makes sure virtual constructors are called correctly. Just using a class reference will not call the overriden constructor!
  //See http://stackoverflow.com/questions/791069/how-can-i-create-an-delphi-object-from-a-class-reference-and-ensure-constructor

  TClassActivator = class
  private
    class var
      FRttiCtx : TRttiContext;
      class constructor Create;
  public
    class function CreateInstance(const AClass : TClass) : IInterface;
    class function CreateActivatorDelegate(const AClass : TClass; typeInfo : PTypeInfo; raiseOnError : Boolean) : TActivatorDelegate;
  end;

implementation

uses
  DUnitX.ResStrs;

{ TActivator }

class constructor TClassActivator.Create;
begin
  TClassActivator.FRttiCtx := TRttiContext.Create;
end;

class function TClassActivator.CreateInstance(const AClass : TClass): IInterface;
var
  delegate : TActivatorDelegate;
begin
  Result := nil;

  delegate := CreateActivatorDelegate(AClass, TypeInfo(IInterface), False);
  if Assigned(delegate) then
    Result := delegate();
end;

class function TClassActivator.CreateActivatorDelegate(
  const AClass : TClass; typeInfo : PTypeInfo; raiseOnError : Boolean): TActivatorDelegate;
var
  rType : TRttiType;
  method : TRttiMethod;
  guid : TGUID;
  ctor : function(InstanceOrVMT: Pointer; Alloc: ShortInt = 1): Pointer; // constructor signature
begin
  Result := nil;

  rType := FRttiCtx.GetType(AClass);
  if rType is TRttiInstanceType then
    for method in TRttiInstanceType(rType).GetMethods do
      if method.IsConstructor and (Length(method.GetParameters) = 0) then
      begin
        guid := GetTypeData(typeInfo).Guid;
        ctor := method.CodeAddress;
        Result :=
          function : IInterface
          var
            obj : TObject;
          begin
            obj := ctor(AClass);
            if not Supports(obj, guid, Result) and raiseOnError then
              raise EServiceLocatorResolutionException.CreateFmt(SRegisteredImplementationError, [AClass.ClassName, typeInfo.Name]);
          end;
        Exit;
      end;
end;

{ TDUnitXServiceLocator }

function TDUnitXServiceLocator.HasService<T>: boolean;
begin
  Result := FContainerInfo.ContainsKey(GetInterfaceKey(TypeInfo(T)));
end;

procedure TDUnitXServiceLocator.RegisterType<TInterface, TImplementation>(const name: string);
begin
  InternalRegisterType(TypeInfo(TInterface), False,
    TClassActivator.CreateActivatorDelegate(TImplementation, TypeInfo(TInterface), FRaiseIfNotFound), name);
end;

procedure TDUnitXServiceLocator.RegisterType<TInterface, TImplementation>(const singleton: boolean; const name: string);
begin
  InternalRegisterType(TypeInfo(TInterface), singleton,
    TClassActivator.CreateActivatorDelegate(TImplementation, TypeInfo(TInterface), FRaiseIfNotFound), name);
end;

procedure TDUnitXServiceLocator.InternalRegisterType(const typeInfo: PTypeInfo; const singleton : boolean; const delegate : TActivatorDelegate; const name : string = '');
var
  key : string;
  rego : TIoCRegistration;
begin
  key := GetInterfaceKey(typeInfo, name);

  if not FContainerInfo.TryGetValue(key,rego) then
  begin
    rego := TIoCRegistration.Create;
    rego.Initialize(delegate, singleton);
    FContainerInfo.Add(key, rego);
  end
  else
  begin
    //cannot replace a singleton that has already been instanciated (Instance property is only used by singletons)
    if rego.Instance <> nil then
      raise EServiceLocatorRegistrationException.Create(Format(SImplementationAlreadyRegistered, [typeInfo.Name, name]));
    rego.Initialize(delegate, singleton);
  end;
end;

procedure TDUnitXServiceLocator.RegisterType<TInterface>(const delegate: TActivatorDelegate<TInterface>; const name: string);
var
  internalDelegate: TActivatorDelegate;
begin
  TActivatorDelegate<TInterface>(internalDelegate) := delegate;
  InternalRegisterType(TypeInfo(TInterface), False, internalDelegate, name);
end;

class destructor TDUnitXServiceLocator.ClassDestroy;
begin
  FDefault.Free;
end;

procedure TDUnitXServiceLocator.Clear;
begin
  FContainerInfo.Clear;
end;

constructor TDUnitXServiceLocator.Create;
begin
  FContainerInfo := TObjectDictionary<string,TIoCRegistration>.Create([doOwnsValues]);
  FRaiseIfNotFound := False;
end;

class function TDUnitXServiceLocator.DefaultContainer: TDUnitXServiceLocator;
begin
  if FDefault = nil then
    FDefault := TDUnitXServiceLocator.Create;

  Result := FDefault;
end;

destructor TDUnitXServiceLocator.Destroy;
begin
  FContainerInfo.Free;
  inherited;
end;

function TDUnitXServiceLocator.GetInterfaceKey(const typeInfo: PTypeInfo; const AName: string): string;
begin
  //By default the key is the interface name unless otherwise found.
  Result := GetTypeName(typeInfo);

  if AName <> '' then
    Result := Result + '_' + AName;

  //All keys are stored in lower case form.
  Result := LowerCase(Result);
end;

function TDUnitXServiceLocator.InternalResolve(const typeInfo: PTypeInfo; const AName: string): IInterface;
var
  key : string;
  registration : TIoCRegistration;
begin
  Result := nil;

  key := GetInterfaceKey(typeInfo, AName);

  if not FContainerInfo.TryGetValue(key, registration) then
  begin
    if FRaiseIfNotFound then
      raise EServiceLocatorResolutionException.CreateFmt(SNoImplementationRegistered, [typeInfo.Name])
    //If we are not meant to raise exceptions, then handle the registration not being set.
    else
      Exit;
  end;

  Result := registration.ActivatorDelegate();
  if Result = nil then
    if FRaiseIfNotFound then
      raise EServiceLocatorResolutionException.CreateFmt(SNoInstance, [typeInfo.Name]);
end;

procedure TDUnitXServiceLocator.RegisterSingleton<TInterface>(const instance: TInterface; const name: string);
begin
  InternalRegisterType(TypeInfo(TInterface), True,
    function: IInterface
    begin
      Result := instance;
    end, name);
end;

procedure TDUnitXServiceLocator.RegisterType<TInterface>(const singleton: boolean; const delegate: TActivatorDelegate<TInterface>; const name: string);
var
  internalDelegate: TActivatorDelegate;
begin
  TActivatorDelegate<TInterface>(internalDelegate) := delegate;
  InternalRegisterType(TypeInfo(TInterface), singleton, internalDelegate, name);
end;

function TDUnitXServiceLocator.Resolve<TInterface>(const name: string = ''): TInterface;
begin
  IInterface(Result) := InternalResolve(TypeInfo(TInterface), name);
end;

{ TDUnitXServiceLocator.TIoCRegistration }

procedure TDUnitXServiceLocator.TIoCRegistration.Initialize(
  const delegate: TActivatorDelegate; singleton: Boolean);
begin
  inherited Create;
  if Assigned(delegate) and singleton then
    ActivatorDelegate := CreateSingletonActivator(delegate)
  else
    ActivatorDelegate := delegate;
end;

function TDUnitXServiceLocator.TIoCRegistration.CreateSingletonActivator(
  const delegate: TActivatorDelegate): TActivatorDelegate;
begin
  Result :=
    function: IInterface
    begin
      if not Assigned(Instance) then
      begin
        MonitorEnter(Self);
        try
          if not Assigned(Instance) then
            Instance := delegate();
        finally
          MonitorExit(Self);
        end;
      end;
      Result := Instance;
    end;
end;

end.
