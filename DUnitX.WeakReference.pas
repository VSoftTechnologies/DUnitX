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

{$I DUnitX.inc}

interface

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
    FWeakReferences : Array of Pointer;
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure AddWeakRef(value : Pointer);
    procedure RemoveWeakRef(value : Pointer);
    function GetRefCount : integer;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

  // This is our generic WeakReference interface
  IWeakReference<T : IInterface> = interface
    ['{A6B88944-15A2-4FFD-B755-1B17960401BE}']
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


implementation

uses
  TypInfo,
  classes,
  sysutils;

{$IFDEF CPUX86}

function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer;
asm
      MOV   ECX,EAX
      MOV   EAX,EDX
 LOCK XADD  [ECX],EAX
      ADD   EAX,EDX
end;


function InterlockedDecrement(var Addend: Integer): Integer;
asm
      MOV   EDX,-1
      JMP   InterlockedAdd
end;

function InterlockedIncrement(var Addend: Integer): Integer;
asm
      MOV   EDX,1
      JMP   InterlockedAdd
end;


{$ELSE}
function InterlockedDecrement(var Addend: LongInt): LongInt;
asm
      .NOFRAME
      MOV   EAX,-1
 LOCK XADD  [RCX].Integer,EAX
      DEC   EAX
end;

function InterlockedIncrement(var Addend: LongInt): LongInt;
asm
      MOV   EAX,1
 LOCK XADD  [RCX].Integer,EAX
      INC   EAX
end;


{$ENDIF}

constructor TWeakReference<T>.Create(const data: T);
var
  weakRef : IWeakReferenceableObject;
  d : IInterface;
begin
  d := IInterface(data);
  if Supports(d,IWeakReferenceableObject,weakRef) then
  begin
    FData := d as TObject;
    weakRef.AddWeakRef(@FData);
  end
  else
    raise Exception.Create('TWeakReference can only be used with objects derived from TWeakReferencedObject');
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
  weakRef : IWeakReferenceableObject;
begin
  if IsAlive then
  begin
    if Supports(FData,IWeakReferenceableObject,weakRef) then
    begin
      weakRef.RemoveWeakRef(@FData);
      weakRef := nil;
    end;
  end;
  FData := nil;
  inherited;
end;

function TWeakReference<T>.IsAlive: boolean;
begin
  result := FData <> nil;
end;

{ TWeakReferencedObject }

procedure TWeakReferencedObject.AddWeakRef(value: Pointer);
var
  l : integer;
begin
  MonitorEnter(Self);
  try
    l := Length(FWeakReferences);
    Inc(l);
    SetLength(FWeakReferences,l);
    FWeakReferences[l-1] := Value;
  finally
    MonitorExit(Self);
  end;
end;

procedure TWeakReferencedObject.RemoveWeakRef(value: Pointer);
var
  l : integer;
  i : integer;
  idx : integer;
begin
  MonitorEnter(Self);
  try
    l := Length(FWeakReferences);
    if l > 0 then
    begin
      idx := -1;
      for i := 0 to l - 1 do
      begin
        if idx <> -1 then
        begin
          FWeakReferences[i -1] := FWeakReferences[i];
        end;
        if FWeakReferences[i] = Value then
        begin
          FWeakReferences[i] := nil;
          idx := i;
        end;
      end;
      Dec(l);
      SetLength(FWeakReferences,l);
    end;
  finally
    MonitorExit(Self);
  end;
end;


procedure TWeakReferencedObject.AfterConstruction;
begin
  // Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TWeakReferencedObject.BeforeDestruction;
var
  i: Integer;
begin
  if RefCount <> 0 then
    System.Error(reInvalidPtr);
  MonitorEnter(Self);
  try
    for i := 0 to Length(FWeakReferences) - 1 do
       TObject(FWeakReferences[i]^) := nil;
    SetLength(FWeakReferences,0);
  finally
    MonitorExit(Self);
  end;
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
  Result := InterlockedIncrement(FRefCount);
end;

function TWeakReferencedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0  then
    Destroy;
end;


end.
