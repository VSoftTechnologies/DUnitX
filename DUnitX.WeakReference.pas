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

unit DUnitX.WeakReference;

(*

The idea behind this unit is provide a similar lifecycle to reference counted objects
in delphi as WeakReference does in .NET.

Reference counted objects in delphi have some limitations when it comes to circular references,
where for example TParent references it's children (via IChild), and TChild references it's parent
(via IParent). If we remove any external references to our IParent and IChild instances without first
getting the child to remove it's reference to IParent, we would end up with orphaned objects. This
is because our IChild and IParent instances are holding references to each other, and thus they never
get released.

This unit was borrowed from FinalBuilder 7(with permission), it has been extensively used with
Delphi 2010 and has so far proven to be very reliable.

*)

interface

{$I DUnitX.inc}
uses
  System.Generics.Collections;

type
  /// Implemented by our weak referenced object base class
  IWeakReferenceableObject = interface
    ['{3D7F9CB5-27F2-41BF-8C5F-F6195C578755}']
    procedure AddWeakRef(value : Pointer);
    procedure RemoveWeakRef(value : Pointer);
    function GetRefCount : integer;
  end;

  ///  This is our base class for any object that can have a weak reference to
  ///  it. It implements IInterface so the object can also be used just like
  ///  any normal reference counted objects in Delphi.
  TWeakReferencedObject = class(TObject, IInterface, IWeakReferenceableObject)
  protected
    FRefCount: Integer;
    FWeakReferences : TList<Pointer>;
    FDestroying : boolean;
    FDestroyed : boolean;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddWeakRef(value : Pointer);
    procedure RemoveWeakRef(value : Pointer);
    function GetRefCount : integer;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    {$IFDEF NEXTGEN}[Result: Unsafe]{$ENDIF} class function NewInstance: TObject; override;
    destructor Destroy; override;
    property RefCount: Integer read FRefCount;
  end;

  // This is our generic WeakReference interface
  IWeakReference<T : IInterface> = interface
    function IsAlive : boolean;
    function Data : T;
  end;

  //The actual WeakReference implementation.
  TWeakReference<T: IInterface> = class(TInterfacedObject, IWeakReference<T>)
  private
    FData : TObject;
  protected
    function IsAlive : boolean;
    function Data : T;
  public
    constructor Create(const data : T);
    destructor Destroy;override;
  end;

//only here to work around compiler limitation.
function SafeMonitorTryEnter(const AObject: TObject): Boolean;

implementation

uses
  DUnitX.ResStrs,
  {$IFDEF USE_NS}
  System.TypInfo,
  System.Classes,
  System.Sysutils,
  System.SyncObjs;
  {$ELSE}
  TypInfo,
  classes,
  SysUtils,
  SyncObjs;
  {$ENDIF}

{$IFNDEF DELPHI_XE2_UP}
type
  TInterlocked = class
  public
    class function Increment(var Target: Integer): Integer; static; inline;
    class function Decrement(var Target: Integer): Integer; static; inline;
    class function Add(var Target: Integer; Increment: Integer): Integer;static;
  end;

class function TInterlocked.Decrement(var Target: Integer): Integer;
begin
    result := Add(Target,-1);
end;

class function TInterlocked.Increment(var Target: Integer): Integer;
begin
  result := Add(Target,1);
end;

class function TInterlocked.Add(var Target: Integer; Increment: Integer): Integer;
{$IFNDEF CPUX86}
asm
  .NOFRAME
  MOV  EAX,EDX
  LOCK XADD [RCX].Integer,EAX
  ADD  EAX,EDX
end;
{$ELSE CPUX86}
asm
  MOV  ECX,EDX
  XCHG EAX,EDX
  LOCK XADD [EDX],EAX
  ADD  EAX,ECX
end;
{$ENDIF}
{$ENDIF DELPHI_XE2_UPE2}
//MonitorTryEnter doesn't do a nil check!
function SafeMonitorTryEnter(const AObject: TObject): Boolean;
begin
  if AObject <> nil then
    Result := TMonitor.TryEnter(AObject)
  else
    result := False;
end;


constructor TWeakReference<T>.Create(const data: T);
var
  target : IWeakReferenceableObject;
  d : IInterface;
  pInfo : PTypeInfo;
begin
  inherited Create;
  pInfo := TypeInfo(T);

  if data = nil then
    raise Exception.Create(format('[%s] passed to TWeakReference was nill', [pInfo.Name]));

  d := IInterface(data);
  if Supports(d,IWeakReferenceableObject,target) then
  begin
    FData := target as TObject;
    target.AddWeakRef(@FData);
  end
  else
    raise Exception.Create(SWeakReferenceError);
end;

function TWeakReference<T>.Data: T;
begin
  result := Default(T); /// can't assign nil to T
  if FData <> nil then
  begin
    //Make sure that the object supports the interface which is our generic type if we
    //simply pass in the interface base type, the method table doesn't work correctly
    if Supports(FData, GetTypeData(TypeInfo(T))^.Guid, result) then
    //if Supports(FData, IInterface, result) then
      result := T(result);
  end;
end;

destructor TWeakReference<T>.Destroy;
var
  target : IWeakReferenceableObject;
begin
  if FData <> nil then
  begin
    if SafeMonitorTryEnter(FData) then //FData could become nil
    begin
      //get a strong reference to the target
      if Supports(FData,IWeakReferenceableObject,target) then
      begin
        target.RemoveWeakRef(@FData);
        target := nil; //release the reference asap.
      end;
      MonitorExit(FData);
    end;
    FData := nil;
  end;
  inherited;
end;

function TWeakReference<T>.IsAlive: boolean;
begin
  result := FData <> nil;
end;

{ TWeakReferencedObject }

procedure TWeakReferencedObject.AddWeakRef(value: Pointer);
begin
  MonitorEnter(Self);
  try
    if FWeakReferences = nil then
      FWeakReferences := TList<Pointer>.Create;
    FWeakReferences.Add(value);
  finally
    MonitorExit(Self);
  end;
end;

procedure TWeakReferencedObject.RemoveWeakRef(value: Pointer);
begin
  MonitorEnter(Self);
  try
    if FWeakReferences = nil then // should never happen
      {$IFDEF DEBUG}
      raise Exception.Create('FWeakReferences = nil');
      {$ELSE}
      exit;
      {$ENDIF}
    FWeakReferences.Remove(value);
    if FWeakReferences.Count = 0 then
      FreeAndNil(FWeakReferences);
  finally
    MonitorExit(Self);
  end;
end;

procedure TWeakReferencedObject.AfterConstruction;
begin
  // Release the constructor's implicit refcount
  TInterlocked.Decrement(FRefCount);
  //spin for short period of time before
  TMonitor.SetSpinCount(Self, 200); //default is 1000
end;

procedure TWeakReferencedObject.BeforeDestruction;
var
  value : pointer;
  i: Integer;
begin
  if FRefCount <> 0 then
    System.Error(reInvalidPtr);
  MonitorEnter(Self);
  try
    if FWeakReferences <> nil then
    begin
      for i := 0 to FWeakReferences.Count -1 do
      begin
        value := FWeakReferences.Items[i];
        TObject(value^) := nil;
      end;
        FreeAndNil(FWeakReferences);
    end;
  finally
    MonitorExit(Self);
  end;
  inherited;
end;

destructor TWeakReferencedObject.Destroy;
begin
  if FDestroyed then
    raise Exception.Create('Destroy called when Object already destroyed!');
  FDestroyed := true;
  inherited;
end;

function TWeakReferencedObject.GetRefCount: integer;
begin
  result := FRefCount;
end;

class function TWeakReferencedObject.NewInstance: TObject;
begin
  // Set an implicit refcount so that refcounting
  // during construction won't destroy the object.
  Result := inherited NewInstance;
  TWeakReferencedObject(Result).FRefCount := 1;
end;

function TWeakReferencedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TWeakReferencedObject._AddRef: Integer;
begin
  if FDestroyed then
    raise Exception.Create('_Addref called when Object already destroyed!');
  Result := TInterlocked.Increment(FRefCount);
end;

function TWeakReferencedObject._Release: Integer;
begin
  if FDestroyed then
    raise Exception.Create('_Release called when Object already destroyed!');
  Result := TInterlocked.Decrement(FRefCount);
  if (Result = 0) and (not FDestroying) then
  begin
    FDestroying := True;
    Destroy;
  end;
end;


end.
