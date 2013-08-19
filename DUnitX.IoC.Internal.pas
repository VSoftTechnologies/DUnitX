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

unit DUnitX.IoC.Internal;

{$I DUnitX.inc}

///
///  Internals of the DUnitX IoC container;
///

interface

uses
  TypInfo,
  RTTI,
  Generics.Collections,
  DUnitX.IoC;

type

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



//function IocContainerInfo : TDictionary<string,TObject>;



implementation


//Borrowed from XE2 - in earlier versions of delphi these were from the windows unit.

{$IFDEF CPUX86}
{
function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer;
asm
      MOV   ECX,EAX
      MOV   EAX,EDX
 LOCK XADD  [ECX],EAX
      ADD   EAX,EDX
end;
}

function InterlockedCompareExchange(var Target: Integer; Exchange: Integer; Comparand: Integer): Integer;
asm
      XCHG    EAX,ECX
 LOCK CMPXCHG [ECX],EDX
end;

function InterlockedCompareExchangePointer(var Target: Pointer; Exchange: Pointer; Comparand: Pointer): Pointer;
asm
      JMP InterlockedCompareExchange
end;

{$ELSE}
function InterlockedCompareExchangePointer(var Destination: Pointer; Exchange: Pointer; Comparand: Pointer): Pointer;
asm
      .NOFRAME
      MOV     RAX,R8
 LOCK CMPXCHG [RCX],RDX
end;
{$ENDIF}

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

end.
