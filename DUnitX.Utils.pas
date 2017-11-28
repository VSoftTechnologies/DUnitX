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
{                                                                           }
{ Portions of the file also fall under the following license                }
{ as they were taken from the DSharp Project                                }
{ https://bitbucket.org/sglienke/dsharp                                     }
{                                                                           }
(*
  Copyright (c) 2011-2012, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)


unit DUnitX.Utils;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Generics.Collections,
  System.TimeSpan,
  System.Rtti,
  System.SysUtils,
  System.Types,
  System.TypInfo;
  {$ELSE}
  Generics.Collections,
  TimeSpan,
  Rtti,
  SysUtils,
  Types,
  TypInfo;
  {$ENDIF}



type
  TCustomAttributeClass = class of TCustomAttribute;

  TAttributeUtils = class
  public
    class function ContainsAttribute(const attributes : TArray<TCustomAttribute>; const AttributeClass : TCustomAttributeClass) : boolean;
    class function FindAttribute(const attributes : TArray<TCustomAttribute>; const AttributeClass : TCustomAttributeClass) : TCustomAttribute;overload;
    class function FindAttribute(const attributes : TArray<TCustomAttribute>; const AttributeClass : TCustomAttributeClass; var attribute  : TCustomAttribute; const startIndex : integer = 0) : integer;overload;
    class function FindAttributes(const attributes : TArray<TCustomAttribute>; const AttributeClass : TCustomAttributeClass) :  TArray<TCustomAttribute>;
  end;

{$IFDEF DELPHI_2010}
const
  TicksPerMillisecond = 10000;

type
  TTimeSpanHelper = record helper for TTimeSpan
  public
    class function Subtract(const D1, D2: TDateTime): TTimeSpan; static;
  end;
{$ENDIF}

type
  TStrUtils = class
    class function PadString(const s: string; const totalLength: integer; const padLeft: boolean = True; padChr: Char = ' '): string;
    class function SplitString(const S, Delimiters: string): TArray<string>;
    class function Join(const values : TArray<string>; const delim : string) : string;overload;
    class function EncodeWhitespace(const S: string): string;
  end;

  TListStringUtils = class
    class function ToArray(const values : TList<string>) : TArray<string>;
  end;

//  function GetElapsedTime(const ALastTick : Cardinal) : Cardinal;

type
  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="System.TObject">TObject</see> for easier RTTI use.
  ///	</summary>
  {$ENDREGION}
  TObjectHelper = class helper for TObject
  public
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns a list of all fields of the object.
    ///	</summary>
    {$ENDREGION}
    function GetFields: TArray<TRttiField>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the field with the given name; <b>nil</b> if nothing is found.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the field to find
    ///	</param>
    {$ENDREGION}
    function GetField(const AName: string): TRttiField;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the member with the given name; <b>nil</b> if nothing is found.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the member to find
    ///	</param>
    {$ENDREGION}
    function GetMember(const AName: string): TRttiMember;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns a list of all methods of the object.
    ///	</summary>
    {$ENDREGION}
    function GetMethods: TArray<TRttiMethod>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the method at the given code address; <b>nil</b> if nothing
    ///	  is found.
    ///	</summary>
    ///	<param name="ACodeAddress">
    ///	  Code address of the method to find.
    ///	</param>
    {$ENDREGION}
    function GetMethod(ACodeAddress: Pointer): TRttiMethod; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the method with the given name; <b>nil</b> if nothing is
    ///	  found.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the method to find
    ///	</param>
    {$ENDREGION}
    function GetMethod(const AName: string): TRttiMethod; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns a list of all properties of the object.
    ///	</summary>
    {$ENDREGION}
    function GetProperties: TArray<TRttiProperty>;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the property with the given name; <b>nil</b> if nothing is
    ///	  found.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the property to find
    ///	</param>
    {$ENDREGION}
    function GetProperty(const AName: string): TRttiProperty;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the type of the object; nil if nothing is found.
    ///	</summary>
    {$ENDREGION}
    function GetType: TRttiType;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns if the object contains a field with the given name.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the field to find
    ///	</param>
    {$ENDREGION}
    function HasField(const AName: string): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns if the object contains a method with the given name.
    ///	</summary>
    {$ENDREGION}
    function HasMethod(const AName: string): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns if the object contains a property with the given name.
    ///	</summary>
    {$ENDREGION}
    function HasProperty(const AName: string): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the field to find
    ///	</param>
    ///	<param name="AField">
    ///	  Field that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetField(const AName: string; out AField: TRttiField): Boolean;

    {$REGION 'Documentation'}
    ///	<param name="AName">
    ///	  Name of the member to find
    ///	</param>
    ///	<param name="AMember">
    ///	  Member that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMember(const AName: string; out AMember: TRttiMember): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given code address and returns if this
    ///	  was successful.
    ///	</summary>
    ///	<param name="ACodeAddress">
    ///	  Code address of the method to find
    ///	</param>
    ///	<param name="AMethod">
    ///	  Method that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMethod(ACodeAddress: Pointer; out AMethod: TRttiMethod): Boolean; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the method to find
    ///	</param>
    ///	<param name="AMethod">
    ///	  Method that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMethod(const AName: string; out AMethod: TRttiMethod): Boolean; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the property with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the property to find
    ///	</param>
    ///	<param name="AProperty">
    ///	  Property that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetProperty(const AName: string; out AProperty: TRttiProperty): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the type of the object and returns if this was successful.
    ///	</summary>
    ///	<param name="AType">
    ///	  Type of the object when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetType(out AType: TRttiType): Boolean;

{$IF CompilerVersion < 23}
    class function QualifiedClassName: string;
{$IFEND}
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="System.Rtti.TRttiField">TRttiField</see> for easier RTTI
  ///	  use.
  ///	</summary>
  {$ENDREGION}
  TRttiFieldHelper = class helper for TRttiField
  public
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the value of the field and returns if this was successful.
    ///	</summary>
    ///	<param name="Instance">
    ///	  Pointer to the instance of the field
    ///	</param>
    ///	<param name="Value">
    ///	  Value of the field when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetValue(Instance: Pointer; out Value: TValue): Boolean;
  end;

{$IF CompilerVersion < 23}

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="Rtti.TRttiInstanceTypeHelper">TRttiInstanceTypeHelper</see>
  ///	  for easier RTTI use.
  ///	</summary>
  {$ENDREGION}
  TRttiInstanceTypeHelper = class helper for TRttiInstanceType
  public
    function GetDeclaredImplementedInterfaces: TArray<TRttiInterfaceType>;
    function GetImplementedInterfaces: TArray<TRttiInterfaceType>;
  end;
{$IFEND}

{$IF DELPHI_XE2_UP}

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="System.Rtti.TRttiInvokableType">TRttiInvokableType</see>
  ///	  for easier RTTI use.
  ///	</summary>
  {$ENDREGION}
  TRttiInvokableTypeHelper = class helper for TRttiInvokableType
  private
    function GetParameterCount: Integer;
  public
    property ParameterCount: Integer read GetParameterCount;
  end;
{$IFEND}

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="System.Rtti.TRttiMember">TRttiMember</see> for easier RTTI
  ///	  use.
  ///	</summary>
  {$ENDREGION}
  TRttiMemberHelper = class helper for TRttiMember
  private
    function GetMemberIsReadable: Boolean;
    function GetMemberIsWritable: Boolean;
    function GetMemberRttiType: TRttiType;
  public
    property IsReadable: Boolean read GetMemberIsReadable;
    property IsWritable: Boolean read GetMemberIsWritable;
    property RttiType: TRttiType read GetMemberRttiType;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="System.Rtti.TRttiMethod">TRttiMethod</see> for easier RTTI
  ///	  use.
  ///	</summary>
  {$ENDREGION}
  TRttiMethodHelper = class helper for TRttiMethod
  private
    function GetParameterCount: Integer;
  public
    function Format(const Args: array of TValue; SkipSelf: Boolean = True): string;
    property ParameterCount: Integer read GetParameterCount;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="System.Rtti.TRttiObject">TRttiObject</see> for easier RTTI
  ///	  use.
  ///	</summary>
  {$ENDREGION}
  TRttiObjectHelper = class helper for TRttiObject
  public
    function GetAttributeOfType<T: TCustomAttribute>: T;
    function GetAttributesOfType<T: TCustomAttribute>: TArray<T>;

    function HasAttributeOfType<T: TCustomAttribute>: Boolean;

    function TryGetAttributeOfType<T: TCustomAttribute>(out AAttribute: T): Boolean;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="System.Rtti.TRttiParameter">TRttiParameter</see> for easier
  ///	  RTTI use.
  ///	</summary>
  {$ENDREGION}
  TRttiParameterHelper = class helper for TRttiParameter
  public
    class function Equals(const Left, Right: TArray<TRttiParameter>): Boolean; //overload;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="System.Rtti.TRttiProperty">TRttiProperty</see> for easier
  ///	  RTTI use.
  ///	</summary>
  {$ENDREGION}
  TRttiPropertyHelper = class helper for TRttiProperty
  public
    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the value of the property and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="Instance">
    ///	  Pointer to the instance of the field
    ///	</param>
    ///	<param name="Value">
    ///	  Value of the field when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetValue(Instance: Pointer; out Value: TValue): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Sets the value of the property and returns if this was successful.
    ///	</summary>
    ///	<param name="Instance">
    ///	  Pointer to the instance of the field
    ///	</param>
    ///	<param name="Value">
    ///	  Value the field should be set to
    ///	</param>
    {$ENDREGION}
    function TrySetValue(Instance: Pointer; Value: TValue): Boolean;
  end;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="System.Rtti.TRttiType">TRttiType</see> for easier RTTI use.
  ///	</summary>
  {$ENDREGION}
  TRttiTypeHelper = class helper for TRttiType
  private
    function GetAsInterface: TRttiInterfaceType;
    function GetIsInterface: Boolean;
    function GetMethodCount: Integer;
    function InheritsFrom(OtherType: PTypeInfo): Boolean;
  public
    function GetAttributesOfType<T: TCustomAttribute>: TArray<T>;
    function GetGenericArguments: TArray<TRttiType>;
    function GetGenericTypeDefinition(const AIncludeUnitName: Boolean = True): string;

    function GetMember(const AName: string): TRttiMember;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Returns the method at the given code address; <b>nil</b> if nothing
    ///	  is found.
    ///	</summary>
    ///	<param name="ACodeAddress">
    ///	  Code address of the method to find
    ///	</param>
    {$ENDREGION}
    function GetMethod(ACodeAddress: Pointer): TRttiMethod; overload;
    function GetProperty(const AName: string): TRttiProperty;

    function GetStandardConstructor: TRttiMethod;

    function IsCovariantTo(OtherClass: TClass): Boolean; overload;
    function IsCovariantTo(OtherType: PTypeInfo): Boolean; overload;
    function IsGenericTypeDefinition: Boolean;
    function IsGenericTypeOf(const BaseTypeName: string): Boolean;
    function IsInheritedFrom(OtherType: TRttiType): Boolean; overload;
    function IsInheritedFrom(const OtherTypeName: string): Boolean; overload;
    function MakeGenericType(const TypeArguments: array of PTypeInfo): TRttiType;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the field to find
    ///	</param>
    ///	<param name="AField">
    ///	  Field that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetField(const AName: string; out AField: TRttiField): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the member with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the member to find
    ///	</param>
    ///	<param name="AMember">
    ///	  Member that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMember(const AName: string; out AMember: TRttiMember): Boolean;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given code address and returns if this
    ///	  was successful.
    ///	</summary>
    ///	<param name="ACodeAddress">
    ///	  Code address of the method to find
    ///	</param>
    ///	<param name="AMethod">
    ///	  Method that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMethod(ACodeAddress: Pointer; out AMethod: TRttiMethod): Boolean; overload;

    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the method with the given code address and returns if this
    ///	  was successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the method to find
    ///	</param>
    ///	<param name="AMethod">
    ///	  Method that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetMethod(const AName: string; out AMethod: TRttiMethod): Boolean; overload;

    //will get the first declated constructor it finds
    function TryGetConstructor(out AMethod : TRttiMethod) : boolean;

    function TryGetDestructor(out AMethod : TRttiMethod) : boolean;


    {$REGION 'Documentation'}
    ///	<summary>
    ///	  Retrieves the property with the given name and returns if this was
    ///	  successful.
    ///	</summary>
    ///	<param name="AName">
    ///	  Name of the property to find
    ///	</param>
    ///	<param name="AProperty">
    ///	  Property that was found when Result is <b>True</b>
    ///	</param>
    {$ENDREGION}
    function TryGetProperty(const AName: string; out AProperty: TRttiProperty): Boolean;

    function TryGetStandardConstructor(out AMethod: TRttiMethod): Boolean;

    property AsInterface: TRttiInterfaceType read GetAsInterface;
    property IsInterface: Boolean read GetIsInterface;
    property MethodCount: Integer read GetMethodCount;
  end;

  TValue = {$IFDEF USE_NS}System.{$ENDIF}Rtti.TValue;

  {$REGION 'Documentation'}
  ///	<summary>
  ///	  Extends <see cref="System.Rtti.TValue">TValue</see> for easier RTTI use.
  ///	</summary>
  {$ENDREGION}
  TValueHelper = record helper for TValue
  private
    function GetRttiType: TRttiType;
    class function FromFloat(ATypeInfo: PTypeInfo; AValue: Extended): TValue; static;
  public
    function IsFloat: Boolean;
    function IsNumeric: Boolean;
    function IsPointer: Boolean;
    function IsString: Boolean;

    function IsInstance: Boolean;
    function IsInterface: Boolean;

    // conversion for almost all standard types
    function TryConvert(ATypeInfo: PTypeInfo; out AResult: TValue): Boolean; overload;
    function TryConvert<T>(out AResult: TValue): Boolean; overload;

    function AsByte: Byte;
    function AsCardinal: Cardinal;
    function AsCurrency: Currency;
    function AsDate: TDate;
    function AsDateTime: TDateTime;
    function AsDouble: Double;
    function AsFloat: Extended;
    function AsPointer: Pointer;
    function AsShortInt: ShortInt;
    function AsSingle: Single;
    function AsSmallInt: SmallInt;
    function AsTime: TTime;
    function AsUInt64: UInt64;
    function AsWord: Word;

    function ToObject: TObject;
    function ToVarRec: TVarRec;

    class function ToString(const Value: TValue): string; overload; static;
    class function ToString(const Values: array of TValue): string; overload; static;
    class function ToVarRecs(const Values: array of TValue): TArray<TVarRec>; static;
    class function Equals(const Left, Right: TArray<TValue>): Boolean; overload; static;
    class function Equals<T>(const Left, Right: T): Boolean; overload; static;

    class function From(ABuffer: Pointer; ATypeInfo: PTypeInfo): TValue; overload; static;
    class function From(AValue: NativeInt; ATypeInfo: PTypeInfo): TValue; overload; static;
    class function From(AObject: TObject; AClass: TClass): TValue; overload; static;
    class function FromBoolean(const Value: Boolean): TValue; static;
    class function FromString(const Value: string): TValue; static;
    class function FromVarRec(const Value: TVarRec): TValue; static;

    function IsBoolean: Boolean;
    function IsByte: Boolean;
    function IsCardinal: Boolean;
    function IsCurrency: Boolean;
    function IsDate: Boolean;
    function IsDateTime: Boolean;
    function IsDouble: Boolean;
    function IsInteger: Boolean;
    function IsInt64: Boolean;
    function IsShortInt: Boolean;
    function IsSingle: Boolean;
    function IsSmallInt: Boolean;
    function IsTime: Boolean;
    function IsUInt64: Boolean;
    function IsVariant: Boolean;
    function IsWord: Boolean;

    property RttiType: TRttiType read GetRttiType;
  end;

  {$IFDEF DELPHI_XE3_UP}
  PPropInfoExt = ^TPropInfoExt;
  TPropInfoExt = record
    PropType: PPTypeInfo;
    GetProc: Pointer;
    SetProc: Pointer;
    StoredProc: Pointer;
    Index: Integer;
    Default: Integer;
    NameIndex: SmallInt;
    NameLength : Byte;
    NameData : array[0..255] of Byte;
    function NameFld: TTypeInfoFieldAccessor; inline;
    function Tail: PPropInfoExt; inline;
  end;
  {$ENDIF}
 
  TRttiPropertyExtension = class(TRttiInstanceProperty)
  private
    {$IFDEF DELPHI_XE3_UP}
    FPropInfo: TPropInfoExt;
    {$ELSE}
    FPropInfo: TPropInfo;
    {$ENDIF}
    FGetter: TFunc<Pointer, TValue>;
    FSetter: TProc<Pointer, TValue>;
    class var
      FRegister: TDictionary<TPair<PTypeInfo, string>, TRttiPropertyExtension>;
      FPatchedClasses: TDictionary<TClass, TClass>;
    function GetIsReadableStub: Boolean; //override;
    function GetIsWritableStub: Boolean; //override;
    function DoGetValueStub(Instance: Pointer): TValue; //override;
    procedure DoSetValueStub(Instance: Pointer; const AValue: TValue); //override;
    function GetPropInfoStub: PPropInfo; // override;
  protected
    class procedure InitVirtualMethodTable;

    function GetIsReadable: Boolean; virtual;
    function GetIsWritable: Boolean; virtual;
    function DoGetValue(Instance: Pointer): TValue; virtual;
    procedure DoSetValue(Instance: Pointer; const AValue: TValue); virtual;
    function GetPropInfo: PPropInfo; virtual;
  public
    class constructor Create;
    class destructor Destroy;

    constructor Create(Parent: PTypeInfo; const Name: string; PropertyType: PTypeInfo);

    class function FindByName(Parent: TRttiType;
      const PropertyName: string): TRttiPropertyExtension; overload;
    class function FindByName(const FullPropertyName: string): TRttiPropertyExtension; overload;

    property Getter: TFunc<Pointer, TValue> read FGetter write FGetter;
    property Setter: TProc<Pointer, TValue> read FSetter write FSetter;
  end;


  TArrayHelper = class
  public
    class function Concat<T>(const Arrays: array of TArray<T>): TArray<T>; static;
    class function Create<T>(const a : T; const b : T) : TArray<T>;static;
{$IF DELPHI_2010}
    class function ToArray<T>(Enumerable: TEnumerable<T>; Count: Integer): TArray<T>; static;
{$IFEND}
  end;

  PObject = ^TObject;

function FindType(const AName: string; out AType: TRttiType): Boolean; overload;
function FindType(const AGuid: TGUID; out AType: TRttiType): Boolean; overload;

{$REGION 'Documentation'}
///	<summary>
///	  Returns the RTTI type of the given TClass.
///	</summary>
{$ENDREGION}
function GetRttiType(AClass: TClass): TRttiType; overload;

{$REGION 'Documentation'}
///	<summary>
///	  Returns the RTTI type of the given TypeInfo.
///	</summary>
{$ENDREGION}
function GetRttiType(ATypeInfo: PTypeInfo): TRttiType; overload;
function GetRttiTypes: TArray<TRttiType>;
function IsClassCovariantTo(ThisClass, OtherClass: TClass): Boolean;
function IsTypeCovariantTo(ThisType, OtherType: PTypeInfo): Boolean;
function TryGetRttiType(AClass: TClass; out AType: TRttiType): Boolean; overload;
function TryGetRttiType(ATypeInfo: PTypeInfo; out AType: TRttiType): Boolean; overload;

function CompareValue(const Left, Right: TValue): Integer;
function SameValue(const Left, Right: TValue): Boolean;

function StripUnitName(const s: string): string;

{$IFDEF DELPHI_2010}
function SplitString(const S: string; const Delimiters: string): TStringDynArray;
{$ENDIF}

function Supports(const Instance: TValue; const IID: TGUID; out Intf): Boolean; overload;

const
  ObjCastGUID: TGUID = '{CEDF24DE-80A4-447D-8C75-EB871DC121FD}';


implementation

uses
  DUnitX.Helpers,
  {$IFDEF USE_NS}
  System.Classes,
  System.Generics.Defaults,
  System.Math,
  System.StrUtils,
  System.SysConst;
  {$ELSE}
  Classes,
  Generics.Defaults,
  Math,
  StrUtils,
  SysConst;
  {$ENDIF}

var
  Context: TRttiContext;
  Enumerations: TDictionary<PTypeInfo, TStrings>;


{ TAttributeUtils }

class function TAttributeUtils.ContainsAttribute(const attributes: TArray<TCustomAttribute>; const AttributeClass: TCustomAttributeClass): boolean;
begin
  result := FindAttribute(attributes,AttributeClass) <> nil;
end;

class function TAttributeUtils.FindAttribute(const attributes: TArray<TCustomAttribute>; const AttributeClass: TCustomAttributeClass): TCustomAttribute;
var
  attribute : TCustomAttribute;
begin
  result := nil;
  for attribute in attributes do
  begin
    if attribute.ClassType = AttributeClass then
      Exit(attribute);
  end;
end;


class function TAttributeUtils.FindAttribute(const attributes: TArray<TCustomAttribute>; const AttributeClass: TCustomAttributeClass;
                                       var attribute: TCustomAttribute; const startIndex: integer): integer;
var
  i : integer;
begin
  result := -1;
  attribute := nil;
  for i := startIndex to Length(attributes) -1 do
  begin
    if attributes[i].ClassType = AttributeClass then
    begin
      attribute := attributes[i];
      Exit(i);
    end;
  end;
end;

class function TAttributeUtils.FindAttributes( const attributes: TArray<TCustomAttribute>; const AttributeClass: TCustomAttributeClass): TArray<TCustomAttribute>;
var
  i : integer;
  attribute : TCustomAttribute;
begin
  i := 0;
  SetLength(result,0);
  for attribute in attributes do
  begin
    if attribute.ClassType = AttributeClass then
    begin
      SetLength(result,i + 1);
      result[i] := attribute;
      Inc(i);
    end;
  end;
end;

class function TStrUtils.EncodeWhitespace(const S: string): string;
const
  DELIMITER: array[boolean] of string = (#39, '');
var
  index: integer;
  lastWhitespace: boolean;
begin
  Result := '';
  lastWhitespace := true;
  for index := 1 to Length(S) do
  begin
    if S[index] < #32 then
    begin
      Result := Result + DELIMITER[lastWhitespace] + '#' + IntToStr(Ord(S[index]));
      lastWhitespace := true;
    end
    else begin
      Result := Result + DELIMITER[not lastWhitespace] + S[index];
      lastWhitespace := false;
    end;
  end;
  Result := Result + DELIMITER[lastWhitespace];
end;

class function TStrUtils.Join(const values : TArray<string>; const delim: string): string;
var
  v : string;
begin
  result := '';
  for v in values do
  begin
    if result <> '' then
      result := result + delim;
    result := result + v;
  end;
end;

class function TStrUtils.PadString(const s: string; const totalLength: integer; const padLeft: boolean = True; padChr: Char = ' '): string;
begin
  Result := s;
  while Length(result) < totalLength do
  begin
    if padLeft then
      Result := padChr + Result
    else
      Result := Result + padChr;
  end;
end;


{$REGION 'Conversion functions'}
type
  TConvertFunc = function(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;

function ConvFail(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  Result := False;
end;

{$IFDEF DELPHI_XE3_UP}
function ConvStr2DynArray(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  s: string;
  values: TStringDynArray;
  i: Integer;
  p: Pointer;
  v1, v2: TValue;
  elType: PTypeInfo;
begin
  s := ASource.AsString;
  if StartsStr('[', s) and EndsStr(']', s) then
    s := Copy(s, 2, Length(s) - 2);
  values := SplitString(s, ',');
  i := Length(values);
  p := nil;
  DynArraySetLength(p, ATarget, 1, @i);
  TValue.MakeWithoutCopy(@p, ATarget, AResult);
  elType := ATarget.TypeData.DynArrElType^;
  for i := 0 to High(values) do
  begin
    v1 := TValue.FromString(values[i]);
    if not v1.TryConvert(elType, v2) then
      Exit(False);
    AResult.SetArrayElement(i, v2);
  end;
  Result := True;
end;
{$ENDIF}

function ConvAny2Nullable(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LType: TRttiType;
  LValue: TValue;
  LBuffer: array of Byte;
begin
  Result := TryGetRttiType(ATarget, LType) and LType.IsGenericTypeOf('Nullable')
    and ASource.TryConvert(LType.GetGenericArguments[0].Handle, LValue);
  if Result then
  begin
    SetLength(LBuffer, LType.TypeSize);
    Move(LValue.GetReferenceToRawData^, LBuffer[0], LType.TypeSize - SizeOf(string));
    PString(@LBuffer[LType.TypeSize - SizeOf(string)])^ := DefaultTrueBoolStr;
    TValue.Make(LBuffer, LType.Handle, AResult);
    PString(@LBuffer[LType.TypeSize - SizeOf(string)])^ := '';
  end
end;

function ConvClass2Class(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  Result := ASource.TryCast(ATarget, AResult);
  if not Result and IsTypeCovariantTo(ASource.TypeInfo, ATarget) then
  begin
    AResult := TValue.From(ASource.AsObject, GetTypeData(ATarget).ClassType);
    Result := True;
  end;
end;

function ConvClass2Enum(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  Result := ATarget = TypeInfo(Boolean);
  if Result then
    AResult := ASource.AsObject <> nil;
end;

function ConvEnum2Class(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LType: TRttiType;
  LStrings: TStrings;
  i: Integer;
begin
  Result := TryGetRttiType(ATarget, LType)
    and LType.AsInstance.MetaclassType.InheritsFrom(TStrings);
  if Result then
  begin
    if not Enumerations.TryGetValue(ASource.TypeInfo, LStrings) then
    begin
      LStrings := TStringList.Create;
      with TRttiEnumerationType(ASource.RttiType) do
      begin
        for i := MinValue to MaxValue do
        begin
          LStrings.Add(GetEnumName(Handle, i));
        end;
      end;
      Enumerations.Add(ASource.TypeInfo, LStrings);
    end;
    AResult := TValue.From(LStrings, TStrings);
    Result := True;
  end;
end;

function ConvFloat2Ord(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  Result := Frac(ASource.AsExtended) = 0;
  if Result then
    AResult := TValue.FromOrdinal(ATarget, Trunc(ASource.AsExtended));
end;

function ConvFloat2Str(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LValue: TValue;
begin
  if ASource.TypeInfo = TypeInfo(TDate) then
    LValue := DateToStr(ASource.AsExtended)
  else if ASource.TypeInfo = TypeInfo(TDateTime) then
    LValue := DateTimeToStr(ASource.AsExtended)
  else if ASource.TypeInfo = TypeInfo(TTime) then
    LValue := TimeToStr(ASource.AsExtended)
  else
    LValue := FloatToStr(ASource.AsExtended);
  Result := LValue.TryCast(ATarget, AResult);
end;

function ConvIntf2Class(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  Result := ConvClass2Class(ASource.AsInterface as TObject, ATarget, AResult);
end;

function ConvIntf2Intf(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LType: TRttiType;
  LMethod: TRttiMethod;
  LInterface: IInterface;
begin
  Result := ASource.TryCast(ATarget, AResult);
  if not Result then
  begin
    if IsTypeCovariantTo(ASource.TypeInfo, ATarget) then
    begin
      AResult := TValue.From(ASource.GetReferenceToRawData, ATarget);
      Result := True;
    end else
    if TryGetRttiType(ASource.TypeInfo, LType) and (GetTypeName(ATarget) = 'IList')
      and LType.IsGenericTypeOf('IList') and LType.TryGetMethod('AsList', LMethod) then
    begin
      LInterface := LMethod.Invoke(ASource, []).AsInterface;
      AResult := TValue.From(@LInterface, ATarget);
      Result := True;
    end;
  end;
end;

function ConvNullable2Any(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LType: TRttiType;
  LValue: TValue;
begin
  Result := TryGetRttiType(ASource.TypeInfo, LType)
    and LType.IsGenericTypeOf('Nullable');
  if Result then
  begin
    LValue := TValue.From(ASource.GetReferenceToRawData, LType.GetGenericArguments[0].Handle);
    Result := LValue.TryConvert(ATarget, AResult);
  end
end;

function ConvOrd2Float(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  AResult := TValue.FromFloat(ATarget, ASource.AsOrdinal);
  Result := True;
end;

function ConvOrd2Ord(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  AResult := TValue.FromOrdinal(ATarget, ASource.AsOrdinal);
  Result := True;
end;

function ConvOrd2Str(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LValue: TValue;
begin
  LValue := ASource.ToString;
  Result := LValue.TryCast(ATarget, AResult);
end;

function ConvRec2Meth(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  if ASource.TypeInfo = TypeInfo(TMethod) then
  begin
    AResult := TValue.From(ASource.GetReferenceToRawData, ATarget);
    Result := True;
  end
  else
  begin
    Result := ConvNullable2Any(ASource, ATarget, AResult);
  end;
end;

function ConvSet2Class(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  LType: TRttiType;
  LTypeData: PTypeData;
  LStrings: TStrings;
  i: Integer;
begin
  Result := TryGetRttiType(ATarget, LType)
    and LType.AsInstance.MetaclassType.InheritsFrom(TStrings);
  if Result then
  begin
    LTypeData := GetTypeData(ASource.TypeInfo);
    if not Enumerations.TryGetValue(LTypeData.CompType^, LStrings) then
    begin
      LStrings := TStringList.Create;
      with TRttiEnumerationType(TRttiSetType(ASource.RttiType).ElementType) do
      begin
        for i := MinValue to MaxValue do
        begin
          LStrings.Add(GetEnumName(Handle, i));
        end;
      end;
      Enumerations.Add(LTypeData.CompType^, LStrings);
    end;
    AResult := TValue.From(LStrings, TStrings);
  end
end;

function ConvStr2Enum(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  AResult := TValue.FromOrdinal(ATarget, GetEnumValue(ATarget, ASource.AsString));
  Result := True;
end;

function ConvStr2Float(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
var
  lFormatSettings : TFormatSettings;
  lValue : string;
begin
  lFormatSettings.DecimalSeparator := '.';
  lValue := StringReplace(ASource.AsString, ',', '.', [rfReplaceAll]);

  if ATarget = TypeInfo(TDate) then
    AResult := TValue.From<TDate>(StrToDateDef(lValue, 0))
  else if ATarget = TypeInfo(TDateTime) then
    AResult := TValue.From<TDateTime>(StrToDateTimeDef(lValue, 0))
  else if ATarget = TypeInfo(TTime) then
    AResult := TValue.From<TTime>(StrToTimeDef(lValue, 0))
  else
    AResult := TValue.FromFloat(ATarget, StrToFloatDef(lValue, 0, lFormatSettings));
  Result := True;
end;

function ConvStr2Ord(const ASource: TValue; ATarget: PTypeInfo; out AResult: TValue): Boolean;
begin
  AResult := TValue.FromOrdinal(ATarget, StrToInt64Def(ASource.AsString, 0));
  Result := True;
end;

{$ENDREGION}

{$REGION 'Conversions'}
const
  Conversions: array[TTypeKind, TTypeKind] of TConvertFunc = (
    // tkUnknown
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkInteger
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvOrd2Str,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvOrd2Str, ConvFail, ConvFail, ConvFail
    ),
    // tkChar
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkEnumeration
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvEnum2Class, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvOrd2Str, ConvFail, ConvFail, ConvFail
    ),
    // tkFloat
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFloat2Ord, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFloat2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFloat2Str, ConvFail, ConvFail, ConvFail
    ),
    // tkString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkSet
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvSet2Class, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkClass
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvClass2Enum, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvClass2Class, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkMethod
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkWChar
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkLString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkWString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkVariant
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkRecord
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvRec2Meth, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkInterface
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvIntf2Class, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvIntf2Intf, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkInt64
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Ord, ConvOrd2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvOrd2Ord, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvOrd2Str, ConvFail, ConvFail, ConvFail
    ),
    // tkDynArray
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkUString
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvStr2Ord, ConvFail, ConvStr2Enum, ConvStr2Float, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvStr2Ord, {$IFDEF DELPHI_XE3_UP}ConvStr2DynArray{$ELSE}ConvFail{$ENDIF},
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkClassRef
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkPointer
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    ),
    // tkProcedure
    (
      // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray
      ConvFail, ConvFail, ConvFail, ConvFail, ConvFail, ConvFail,
      // tkUString, tkClassRef, tkPointer, tkProcedure
      ConvFail, ConvFail, ConvFail, ConvFail
    )
  );
{$ENDREGION}

function ExtractGenericArguments(ATypeInfo: PTypeInfo): string;
var
  i: Integer;
  s: string;
begin
  s := UTF8ToString(ATypeInfo.Name);
  {$IFNDEF NEXTGEN}
  i := Pos('<', s);
  if i > 0 then
  begin
    Result := Copy(s, Succ(i), Length(s) - Succ(i));
  end
  else
  begin
    Result := ''
  end;
  {$ELSE}
  i := s.IndexOf('<');
  if i > -1 then
    Result := s.SubString(Succ(i), s.Length - (Succ(i) + 1))
  else
    Result := string.Empty; 
  {$ENDIF}
end;

function FindType(const AName: string; out AType: TRttiType): Boolean;
var
  LType: TRttiType;
begin
  AType := Context.FindType(AName);
  if not Assigned(AType) then
  begin
    for LType in Context.GetTypes do
    begin
      if SameText(LType.Name, AName) then
      begin
        AType := LType;
        Break;
      end;
    end;
  end;
  Result := Assigned(AType);
end;

function FindType(const AGuid: TGUID; out AType: TRttiType): Boolean;
var
  LType: TRttiType;
begin
  AType := nil;
  for LType in Context.GetTypes do
  begin
    if (LType is TRttiInterfaceType)
      and IsEqualGUID(TRttiInterfaceType(LType).GUID, AGuid) then
    begin
      AType := LType;
      Break;
    end;
  end;
  Result := Assigned(AType);
end;

function GetRttiType(AClass: TClass): TRttiType;
begin
  Result := Context.GetType(AClass);
end;

function GetRttiType(ATypeInfo: PTypeInfo): TRttiType;
begin
  Result := Context.GetType(ATypeInfo);
end;

function GetRttiTypes: TArray<TRttiType>;
begin
  Result := Context.GetTypes();
end;

function IsClassCovariantTo(ThisClass, OtherClass: TClass): Boolean;
var
  LType: TRttiType;
begin
  LType := Context.GetType(ThisClass);
  Result := Assigned(LType) and LType.IsCovariantTo(OtherClass.ClassInfo);
end;

function IsTypeCovariantTo(ThisType, OtherType: PTypeInfo): Boolean;
var
  LType: TRttiType;
begin
  LType := Context.GetType(ThisType);
  Result := Assigned(LType) and LType.IsCovariantTo(OtherType);
end;

function MergeStrings(Values: TStringDynArray; const Delimiter: string): string;
var
  i: Integer;
begin
  result := '';
  for i := Low(Values) to High(Values) do
  begin
    if i = 0 then
      Result := Values[i]
    else
      Result := Result + Delimiter + Values[i];
  end;
end;

function TryGetRttiType(AClass: TClass; out AType: TRttiType): Boolean; overload;
begin
  AType := Context.GetType(AClass);
  Result := Assigned(AType);
end;

function TryGetRttiType(ATypeInfo: PTypeInfo; out AType: TRttiType): Boolean; overload;
begin
  AType := Context.GetType(ATypeInfo);
  Result := Assigned(AType);
end;

function StripUnitName(const s: string): string;
begin
  Result := ReplaceText(s, 'System.', '');
end;

{$IFDEF DELPHI_2010}
function SplitString(const S: string; const Delimiters: string): TStringDynArray;
var
  StartIdx: Integer;
  FoundIdx: Integer;
  SplitPoints: Integer;
  CurrentSplit: Integer;
  i: Integer;
begin
  Result := nil;

  if S <> '' then
  begin
    SplitPoints := 0;
    for i := 1 to Length(S) do
      if IsDelimiter(Delimiters, S, i) then
        Inc(SplitPoints);

    SetLength(Result, SplitPoints + 1);

    StartIdx := 1;
    CurrentSplit := 0;
    repeat
      FoundIdx := FindDelimiter(Delimiters, S, StartIdx);
      if FoundIdx <> 0 then
      begin
        Result[CurrentSplit] := Copy(S, StartIdx, FoundIdx - StartIdx);
        Inc(CurrentSplit);
        StartIdx := FoundIdx + 1;
      end;
    until CurrentSplit = SplitPoints;

    Result[SplitPoints] := Copy(S, StartIdx, Length(S) - StartIdx + 1);
  end;
end;
{$ENDIF}

function Supports(const Instance: TValue; const IID: TGUID; out Intf): Boolean; overload;
begin
  if Instance.Kind in [tkClass, tkInterface] then
  begin
    Result := Supports(Instance.ToObject, IID, Intf);
  end
  else
  begin
    Result := False;
  end;
end;

function CompareValue(const Left, Right: TValue): Integer;
begin
  if Left.IsOrdinal and Right.IsOrdinal then
  begin
    Result := {$IFDEF USE_NS}System.Math.{$ENDIF}CompareValue(Left.AsOrdinal, Right.AsOrdinal);
  end else
  if Left.IsFloat and Right.IsFloat then
  begin
    Result := {$IFDEF USE_NS}System.Math.{$ENDIF}CompareValue(Left.AsFloat, Right.AsFloat);
  end else
  if Left.IsString and Right.IsString then
  begin
    Result := {$IFDEF USE_NS}System.SysUtils.{$ENDIF}CompareStr(Left.AsString, Right.AsString);
  end else
  begin
    Result := 0;
  end;
end;

function SameValue(const Left, Right: TValue): Boolean;
begin
  if Left.IsNumeric and Right.IsNumeric then
  begin
    if Left.IsOrdinal then
    begin
      if Right.IsOrdinal then
      begin
        Result := Left.AsOrdinal = Right.AsOrdinal;
      end else
      if Right.IsSingle then
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsOrdinal, Right.AsSingle);
      end else
      if Right.IsDouble then
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsOrdinal, Right.AsDouble);
      end
      else
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsOrdinal, Right.AsExtended);
      end;
    end else
    if Left.IsSingle then
    begin
      if Right.IsOrdinal then
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsSingle, Right.AsOrdinal);
      end else
      if Right.IsSingle then
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsSingle, Right.AsSingle);
      end else
      if Right.IsDouble then
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsSingle, Right.AsDouble);
      end
      else
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsSingle, Right.AsExtended);
      end;
    end else
    if Left.IsDouble then
    begin
      if Right.IsOrdinal then
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsDouble, Right.AsOrdinal);
      end else
      if Right.IsSingle then
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsDouble, Right.AsSingle);
      end else
      if Right.IsDouble then
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsDouble, Right.AsDouble);
      end
      else
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsDouble, Right.AsExtended);
      end;
    end
    else
    begin
      if Right.IsOrdinal then
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsExtended, Right.AsOrdinal);
      end else
      if Right.IsSingle then
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsExtended, Right.AsSingle);
      end else
      if Right.IsDouble then
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsExtended, Right.AsDouble);
      end
      else
      begin
        Result := {$IFDEF USE_NS}System.Math.{$ENDIF}SameValue(Left.AsExtended, Right.AsExtended);
      end;
    end;
  end else
  if Left.IsString and Right.IsString then
  begin
    Result := Left.AsString = Right.AsString;
  end else
  if Left.IsClass and Right.IsClass then
  begin
    Result := Left.AsClass = Right.AsClass;
  end else
  if Left.IsObject and Right.IsObject then
  begin
    Result := Left.AsObject = Right.AsObject;
  end else
  if Left.IsPointer and Right.IsPointer then
  begin
    Result := Left.AsPointer = Right.AsPointer;
  end else
  if Left.IsVariant and Right.IsVariant then
  begin
    Result := Left.AsVariant = Right.AsVariant;
  end else
  if Left.TypeInfo = Right.TypeInfo then
  begin
    Result := Left.AsPointer = Right.AsPointer;
  end else
  begin
    Result := False;
  end;
end;

{ TArrayHelper }

class function TArrayHelper.Concat<T>(
  const Arrays: array of TArray<T>): TArray<T>;
var
  i, k, LIndex, LLength: Integer;
begin
  LLength := 0;
  for i := 0 to High(Arrays) do
    Inc(LLength, Length(Arrays[i]));
  SetLength(Result, LLength);
  LIndex := 0;
  for i := 0 to High(Arrays) do
  begin
    for k := 0 to High(Arrays[i]) do
    begin
      Result[LIndex] := Arrays[i][k];
      Inc(LIndex);
    end;
  end;
end;

{$IF DELPHI_2010}
class function TArrayHelper.ToArray<T>(Enumerable: TEnumerable<T>; Count: Integer): TArray<T>;
var
  LItem: T;
begin
  SetLength(Result, Count);
  Count := 0;
  for LItem in Enumerable do
  begin
    Result[Count] := LItem;
    Inc(Count);
  end;
end;
{$IFEND}

class function TArrayHelper.Create<T>(const a, b: T): TArray<T>;
begin
  SetLength(result,2);
  result[0] := a;
  result[1] := b;
end;

{ TObjectHelper }

function TObjectHelper.GetField(const AName: string): TRttiField;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetField(AName);
end;

function TObjectHelper.GetFields: TArray<TRttiField>;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetFields();
end;

function TObjectHelper.GetMember(const AName: string): TRttiMember;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetMember(AName);
end;

function TObjectHelper.GetMethod(const AName: string): TRttiMethod;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
  try
    Result := LType.GetMethod(AName);
  except
    Result := nil;
  end;
end;

function TObjectHelper.GetMethod(ACodeAddress: Pointer): TRttiMethod;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetMethod(ACodeAddress);
end;

function TObjectHelper.GetMethods: TArray<TRttiMethod>;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetMethods();
end;

function TObjectHelper.GetProperties: TArray<TRttiProperty>;
var
  LType: TRttiType;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetProperties();
end;

function TObjectHelper.GetProperty(const AName: string): TRttiProperty;
var
  LType: TRttiType;
//  LParent: TObject;
begin
  Result := nil;
  if TryGetType(LType) then
    Result := LType.GetProperty(AName);
 {
  if not Assigned(Result) and (Self is TComponent) then
  begin
    LParent := TFramework.GetParent(TComponent(Self));
    if Assigned(LParent) then
    begin
      Result := LParent.GetProperty(AName);
    end;
  end;
  }
end;

function TObjectHelper.GetType: TRttiType;
begin
  TryGetType(Result);
end;

function TObjectHelper.HasField(const AName: string): Boolean;
begin
  Result := GetField(AName) <> nil;
end;

function TObjectHelper.HasMethod(const AName: string): Boolean;
begin
  Result := GetMethod(AName) <> nil;
end;

function TObjectHelper.HasProperty(const AName: string): Boolean;
begin
  Result := GetProperty(AName) <> nil;
end;

{$IF CompilerVersion < 23}
class function TObjectHelper.QualifiedClassName: string;
var
  LUnitName: string;
begin
  LUnitName := UnitName;
  if LUnitName = '' then
    Result := ClassName
  else
    Result := LUnitName + '.' + ClassName;
end;
{$IFEND}

function TObjectHelper.TryGetField(const AName: string;
  out AField: TRttiField): Boolean;
begin
  AField := GetField(AName);
  Result := Assigned(AField);
end;

function TObjectHelper.TryGetMember(const AName: string;
  out AMember: TRttiMember): Boolean;
begin
  AMember := GetMember(AName);
  Result := Assigned(AMember);
end;

function TObjectHelper.TryGetMethod(ACodeAddress: Pointer;
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetMethod(ACodeAddress);
  Result := Assigned(AMethod);
end;

function TObjectHelper.TryGetMethod(const AName: string;
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetMethod(AName);
  Result := Assigned(AMethod);
end;

function TObjectHelper.TryGetProperty(const AName: string;
  out AProperty: TRttiProperty): Boolean;
begin
  AProperty := GetProperty(AName);
  Result := Assigned(AProperty);
end;

function TObjectHelper.TryGetType(out AType: TRttiType): Boolean;
begin
  Result := False;
  if Assigned(Self) then
  begin
    AType := Context.GetType(ClassInfo);
    Result := Assigned(AType);
  end;
end;

{ TRttiFieldHelper }

function TRttiFieldHelper.TryGetValue(Instance: Pointer;
  out Value: TValue): Boolean;
begin
  try
    Value := GetValue(Instance);
    Result := True;
  except
    Value := TValue.Empty;
    Result := False;
  end;
end;

{ TRttiInstanceTypeHelper }

{$IF CompilerVersion < 23}
function TRttiInstanceTypeHelper.GetDeclaredImplementedInterfaces: TArray<TRttiInterfaceType>;
var
  LInterfaceTable: PInterfaceTable;
  p: PPointer;
  i: Integer;
  LTypeInfo: PTypeInfo;
begin
  LInterfaceTable := PPointer(PByte(MetaclassType) + vmtIntfTable)^;

  if Assigned(LInterfaceTable) then
  begin
    p := @LInterfaceTable.Entries[LInterfaceTable.EntryCount];
    SetLength(Result, LInterfaceTable.EntryCount);

    for i := 0 to LInterfaceTable.EntryCount - 1 do
    begin
      LTypeInfo := PPTypeInfo(p^)^;
      Result[i] := GetRttiType(LTypeInfo) as TRttiInterfaceType;
      Inc(p);
    end;
  end;
end;

function TRttiInstanceTypeHelper.GetImplementedInterfaces: TArray<TRttiInterfaceType>;
var
  LCount: Integer;
  LInterfaces: TArray<TArray<TRttiInterfaceType>>;
  LType: TRttiInstanceType;
begin
  LCount := 0;
  LType := Self;
  repeat
    Inc(LCount);
    LType := LType.BaseType.AsInstance;
  until not Assigned(LType);

  SetLength(LInterfaces, LCount);
  LCount := 0;
  LType := Self;
  repeat
    LInterfaces[LCount] := LType.GetDeclaredImplementedInterfaces;
    Inc(LCount);
    LType := LType.BaseType.AsInstance;
  until not Assigned(LType);

  Result := TArrayHelper.Concat<TRttiInterfaceType>(LInterfaces);
end;
{$IFEND}

{ TRttiInvokableTypeHelper }

{$IF DELPHI_XE2_UP}
function TRttiInvokableTypeHelper.GetParameterCount: Integer;
begin
  Result := Length(GetParameters());
end;
{$IFEND}

{ TRttiMemberHelper }

function TRttiMemberHelper.GetMemberIsReadable: Boolean;
begin
  Result := True;
  if Self is TRttiField then
  begin
    Result := True;
  end else
  if Self is TRttiProperty then
  begin
    Result := TRttiProperty(Self).IsReadable;
  end else
  if Self is TRttiMethod then
  begin
    Result := True;
  end;
end;

function TRttiMemberHelper.GetMemberIsWritable: Boolean;
begin
  Result := False;
  if Self is TRttiField then
  begin
    Result := True;
  end else
  if Self is TRttiProperty then
  begin
    Result := TRttiProperty(Self).IsWritable;
  end;
end;

function TRttiMemberHelper.GetMemberRttiType: TRttiType;
begin
  Result := nil;
  if Self is TRttiField then
  begin
    Result := TRttiField(Self).FieldType;
  end else
  if Self is TRttiProperty then
  begin
    Result := TRttiProperty(Self).PropertyType;
  end else
  if Self is TRttiMethod then
  begin
    Result := TRttiMethod(Self).ReturnType;
  end;
end;

{ TRttiMethodHelper }

function TRttiMethodHelper.Format(const Args: array of TValue;
  SkipSelf: Boolean): string;
begin
  Result := StripUnitName(Parent.Name) + '.' + Name + '(';
  if SkipSelf then
  begin
    if Length(Args) > 1 then
      Result := Result + TValue.ToString(Args[1]);
  end
  else
    Result := Result + TValue.ToString(Args[0]);
  Result := Result + ')';
end;

function TRttiMethodHelper.GetParameterCount: Integer;
begin
  Result := Length(GetParameters());
end;

{ TRttiObjectHelper }

function TRttiObjectHelper.GetAttributeOfType<T>: T;
var
  LAttribute: TCustomAttribute;
begin
  Result := Default(T);
  for LAttribute in GetAttributes do
  begin
    if LAttribute.InheritsFrom(T) then
    begin
      Result := T(LAttribute);
      Break;
    end;
  end;
end;

function TRttiObjectHelper.GetAttributesOfType<T>: TArray<T>;
var
  LAttribute: TCustomAttribute;
begin
  SetLength(Result, 0);
  for LAttribute in GetAttributes do
  begin
    if LAttribute.InheritsFrom(T) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := T(LAttribute);
    end;
  end;
end;

function TRttiObjectHelper.HasAttributeOfType<T>: Boolean;
begin
  Result := GetAttributeOfType<T> <> nil;
end;

function TRttiObjectHelper.TryGetAttributeOfType<T>(out AAttribute: T): Boolean;
begin
  AAttribute := GetAttributeOfType<T>;
  Result := Assigned(AAttribute);
end;

{ TRttiParameterHelper }

class function TRttiParameterHelper.Equals(const Left,
  Right: TArray<TRttiParameter>): Boolean;
var
  i: Integer;
begin
  Result := Length(Left) = Length(Right);
  if Result then
  begin
    for i := Low(Left) to High(Left) do
    begin
      if Left[i].ParamType <> Right[i].ParamType then
      begin
        Result := False;
        Break;
      end;
    end
  end;
end;

{ TRttiPropertyHelper }

function TRttiPropertyHelper.TryGetValue(Instance: Pointer;
  out Value: TValue): Boolean;
begin
  try
    if IsReadable then
    begin
      Value := GetValue(Instance);
      Result := True;
    end
    else
    begin
      Result := False;
    end;
  except
    Value := TValue.Empty;
    Result := False;
  end;
end;

function TRttiPropertyHelper.TrySetValue(Instance: Pointer;
  Value: TValue): Boolean;
var
  LValue: TValue;
begin
  Result := Value.TryConvert(PropertyType.Handle, LValue);
  if Result then
  begin
    SetValue(Instance, LValue);
  end;
end;

{ TRttiTypeHelper }

function TRttiTypeHelper.GetAsInterface: TRttiInterfaceType;
begin
  Result := Self as TRttiInterfaceType;
end;

function TRttiTypeHelper.GetAttributesOfType<T>: TArray<T>;
var
  LAttribute: TCustomAttribute;
  LAttributes: TArray<T>;
  i: Integer;
begin
  SetLength(Result, 0);
  for LAttribute in GetAttributes do
  begin
    if LAttribute.InheritsFrom(T) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := T(LAttribute);
    end;
  end;

  if Assigned(BaseType) then
  begin
    for LAttribute in BaseType.GetAttributesOfType<T> do
    begin
      if LAttribute.InheritsFrom(T) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := T(LAttribute);
      end;
    end;
  end;
end;

function TRttiTypeHelper.GetGenericArguments: TArray<TRttiType>;
var
  i: Integer;
  args: TStringDynArray;
begin
  args := SplitString(ExtractGenericArguments(Handle), ',');
  SetLength(Result, Length(args));
  for i := 0 to Pred(Length(args)) do
    FindType(args[i], Result[i]);
end;

function TRttiTypeHelper.GetGenericTypeDefinition(
  const AIncludeUnitName: Boolean = True): string;
var
  i: Integer;
  args: TStringDynArray;
  s: string;
begin
  args := SplitString(ExtractGenericArguments(Handle), ',');
  for i := Low(args) to High(args) do
  begin
    // naive implementation - but will work in most cases
    if (i = 0) and (Length(args) = 1) then
      args[i] := 'T'
    else
      args[i] := 'T' + IntToStr(Succ(i));
  end;
  if IsPublicType and AIncludeUnitName then
    s := QualifiedName
  else
    s := Name;
  {$IFNDEF NEXTGEN}
  Result := Copy(s, 1, Pos('<', s)) + MergeStrings(args, ',') + '>';
  {$ELSE}
  Result := s.SubString(0, s.IndexOf('<') + 1) + MergeStrings(args, ',') + '>';
  {$ENDIF}
end;

function TRttiTypeHelper.GetIsInterface: Boolean;
begin
  Result := Self is TRttiInterfaceType;
end;

function TRttiTypeHelper.GetMember(const AName: string): TRttiMember;
var
  LProperty: TRttiProperty;
  LField: TRttiField;
  LMethod: TRttiMethod;
begin
  if TryGetProperty(AName, LProperty) then
  begin
    Result := LProperty;
  end else
  if TryGetField(AName, LField) then
  begin
    Result := LField;
  end else
  if TryGetMethod(AName, LMethod) then
  begin
    Result := LMethod;
  end else
  begin
    Result := nil;
  end;
end;

function TRttiTypeHelper.GetMethod(ACodeAddress: Pointer): TRttiMethod;
var
  LMethod: TRttiMethod;
begin
  Result := nil;
  for LMethod in GetMethods() do
  begin
    if LMethod.CodeAddress = ACodeAddress then
    begin
      Result := LMethod;
      Break;
    end;
  end;
end;

function TRttiTypeHelper.GetMethodCount: Integer;
begin
  Result := Length(GetMethods);
end;

function TRttiTypeHelper.GetProperty(const AName: string): TRttiProperty;
begin
  Result := inherited GetProperty(AName);

  if not Assigned(Result) then
  begin
    Result := TRttiPropertyExtension.FindByName(Self, AName);
  end;
end;

function TRttiTypeHelper.GetStandardConstructor: TRttiMethod;
var
  LMethod: TRttiMethod;
begin
  Result := nil;
  for LMethod in GetMethods do
  begin
    if LMethod.IsConstructor and (LMethod.ParameterCount = 0) then
    begin
      Result := LMethod;
      Break;
    end;
  end;
end;

function TRttiTypeHelper.InheritsFrom(OtherType: PTypeInfo): Boolean;
var
  LType: TRttiType;
begin
  Result := Handle = OtherType;

  if not Result then
  begin
    LType := BaseType;
    while Assigned(LType) and not Result do
    begin
      Result := LType.Handle = OtherType;
      LType := LType.BaseType;
    end;
  end;
end;

function TRttiTypeHelper.IsCovariantTo(OtherType: PTypeInfo): Boolean;
var
  t: TRttiType;
  args, otherArgs: TArray<TRttiType>;
  i: Integer;
begin
  Result := False;
  t := Context.GetType(OtherType);
  if Assigned(t) and IsGenericTypeDefinition then
  begin
    if SameText(GetGenericTypeDefinition, t.GetGenericTypeDefinition)
      or SameText(GetGenericTypeDefinition(False), t.GetGenericTypeDefinition(False)) then
    begin
      Result := True;
      args := GetGenericArguments;
      otherArgs := t.GetGenericArguments;
      for i := Low(args) to High(args) do
      begin
        if args[i].IsInterface and args[i].IsInterface
          and args[i].InheritsFrom(otherArgs[i].Handle) then
        begin
          Continue;
        end;

        if args[i].IsInstance and otherArgs[i].IsInstance
          and args[i].InheritsFrom(otherArgs[i].Handle) then
        begin
          Continue;
        end;

        Result := False;
        Break;
      end;
    end
    else
    begin
      if Assigned(BaseType) then
      begin
        Result := BaseType.IsCovariantTo(OtherType);
      end;
    end;
  end
  else
  begin
    Result := InheritsFrom(OtherType);
  end;
end;

function TRttiTypeHelper.IsCovariantTo(OtherClass: TClass): Boolean;
begin
  Result := Assigned(OtherClass) and IsCovariantTo(OtherClass.ClassInfo);
end;

function TRttiTypeHelper.IsGenericTypeDefinition: Boolean;
begin
  Result := Length(GetGenericArguments) > 0;
  if not Result and Assigned(BaseType) then
  begin
    Result := BaseType.IsGenericTypeDefinition;
  end;
end;

function TRttiTypeHelper.IsGenericTypeOf(const BaseTypeName: string): Boolean;
var
  s: string;
begin
  s := Name;
  {$IFNDEF NEXTGEN}
  Result := (Copy(s, 1, Succ(Length(BaseTypeName))) = (BaseTypeName + '<')) and (Copy(s, Length(s), 1) = '>');
  {$ELSE}
  Result := (s.SubString(0, Succ(BaseTypeName.Length)) = (BaseTypeName + '<')) and (s.SubString(s.Length-1, 1) = '>');
  {$ENDIF}
end;

function TRttiTypeHelper.IsInheritedFrom(const OtherTypeName: string): Boolean;
var
  LType: TRttiType;
begin
  Result := SameText(Name, OtherTypeName)
    or (IsPublicType and SameText(QualifiedName, OtherTypeName));

  if not Result then
  begin
    LType := BaseType;
    while Assigned(LType) and not Result do
    begin
      Result := SameText(LType.Name, OtherTypeName)
        or (LType.IsPublicType and SameText(LType.QualifiedName, OtherTypeName));
      LType := LType.BaseType;
    end;
  end;
end;

function TRttiTypeHelper.IsInheritedFrom(OtherType: TRttiType): Boolean;
var
  LType: TRttiType;
begin
  Result := Self.Handle = OtherType.Handle;

  if not Result then
  begin
    LType := BaseType;
    while Assigned(LType) and not Result do
    begin
      Result := LType.Handle = OtherType.Handle;
      LType := LType.BaseType;
    end;
  end;
end;

function TRttiTypeHelper.MakeGenericType(const TypeArguments: array of PTypeInfo): TRttiType;
var
  i: Integer;
  args: TStringDynArray;
  s: string;
begin
  if IsPublicType then
  begin
    args := SplitString(ExtractGenericArguments(Handle), ',');
    for i := Low(args) to High(args) do
      args[i] := Context.GetType(TypeArguments[i]).QualifiedName;
    {$IFNDEF NEXTGEN}
    s := Copy(QualifiedName, 1, Pos('<', QualifiedName)) + MergeStrings(args, ',') + '>';
    {$ELSE}
    s := QualifiedName.SubString(0, QualifiedName.IndexOf('<') + 1) + MergeStrings(args, ',') + '>';
    {$ENDIF}
    Result := Context.FindType(s);
  end
  else
    Result := nil;
end;

function TRttiTypeHelper.TryGetConstructor(out AMethod: TRttiMethod): boolean;
var
  methods : TArray<TRttiMethod>;
  method : TRttiMethod;
begin
  result := False;
  methods := GetDeclaredMethods;
  for method in methods do
  begin
    if method.IsConstructor and (Length(method.GetParameters) = 0) then
    begin
      AMethod := method;
      Exit(true);
    end;
  end;
end;

function TRttiTypeHelper.TryGetDestructor(out AMethod: TRttiMethod): boolean;
var
  methods : TArray<TRttiMethod>;
  method : TRttiMethod;
begin
  result := False;
  methods := GetDeclaredMethods;
  for method in methods do
  begin
    if method.IsDestructor then
    begin
      AMethod := method;
      Exit(true);
    end;
  end;
end;

function TRttiTypeHelper.TryGetField(const AName: string;
  out AField: TRttiField): Boolean;
begin
  AField := GetField(AName);
  Result := Assigned(AField);
end;

function TRttiTypeHelper.TryGetMethod(ACodeAddress: Pointer;
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetMethod(ACodeAddress);
  Result := Assigned(AMethod);
end;

function TRttiTypeHelper.TryGetMember(const AName: string;
  out AMember: TRttiMember): Boolean;
begin
  AMember := GetMember(AName);
  Result := Assigned(AMember);
end;

function TRttiTypeHelper.TryGetMethod(const AName: string;
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetMethod(AName);
  Result := Assigned(AMethod);
end;

function TRttiTypeHelper.TryGetProperty(const AName: string;
  out AProperty: TRttiProperty): Boolean;
begin
  AProperty := GetProperty(AName);
  Result := Assigned(AProperty);
end;

function TRttiTypeHelper.TryGetStandardConstructor(
  out AMethod: TRttiMethod): Boolean;
begin
  AMethod := GetStandardConstructor();
  Result := Assigned(AMethod);
end;

{ TValueHelper }

function TValueHelper.AsByte: Byte;
begin
  Result := AsType<Byte>;
end;

function TValueHelper.AsCardinal: Cardinal;
begin
  Result := AsType<Cardinal>;
end;

function TValueHelper.AsCurrency: Currency;
begin
  Result := AsType<Currency>;
end;

function TValueHelper.AsDate: TDate;
begin
  Result := AsType<TDate>;
end;

function TValueHelper.AsDateTime: TDateTime;
begin
  Result := AsType<TDateTime>;
end;

function TValueHelper.AsDouble: Double;
begin
  Result := AsType<Double>;
end;

function TValueHelper.AsFloat: Extended;
begin
  Result := AsType<Extended>;
end;

function TValueHelper.AsPointer: Pointer;
begin
  if Kind in [tkClass, tkInterface] then
    Result := ToObject
  else
    Result := GetReferenceToRawData;
end;

function TValueHelper.AsShortInt: ShortInt;
begin
  Result := AsType<ShortInt>;
end;

function TValueHelper.AsSingle: Single;
begin
  Result := AsType<Single>;
end;

function TValueHelper.AsSmallInt: SmallInt;
begin
  Result := AsType<SmallInt>;
end;

function TValueHelper.AsTime: TTime;
begin
  Result := AsType<TTime>;
end;

function TValueHelper.AsUInt64: UInt64;
begin
  Result := AsType<UInt64>;
end;

function TValueHelper.AsWord: Word;
begin
  Result := AsType<Word>;
end;

class function TValueHelper.Equals(const Left, Right: TArray<TValue>): Boolean;
var
  i: Integer;
begin
  Result := Length(Left) = Length(Right);
  if Result then
  begin
    for i := Low(Left) to High(Left) do
    begin
      if not SameValue(Left[i], Right[i]) then
      begin
        Result := False;
        Break;
      end;
    end
  end;
end;

class function TValueHelper.Equals<T>(const Left, Right: T): Boolean;
begin
  Result := TEqualityComparer<T>.Default.Equals(Left, Right);
end;

class function TValueHelper.From(ABuffer: Pointer;
  ATypeInfo: PTypeInfo): TValue;
begin
  TValue.Make(ABuffer, ATypeInfo, Result);
end;

class function TValueHelper.From(AValue: NativeInt;
  ATypeInfo: PTypeInfo): TValue;
begin
  TValue.Make(AValue, ATypeInfo, Result);
end;

class function TValueHelper.From(AObject: TObject; AClass: TClass): TValue;
begin
  TValue.Make(NativeInt(AObject), AClass.ClassInfo, Result);
end;

class function TValueHelper.FromBoolean(const Value: Boolean): TValue;
begin
  Result := TValue.From<Boolean>(Value);
end;

class function TValueHelper.FromFloat(ATypeInfo: PTypeInfo;
  AValue: Extended): TValue;
begin
  case GetTypeData(ATypeInfo).FloatType of
    ftSingle: Result := TValue.From<Single>(AValue);
    ftDouble: Result := TValue.From<Double>(AValue);
    ftExtended: Result := TValue.From<Extended>(AValue);
    ftComp: Result := TValue.From<Comp>(AValue);
    ftCurr: Result := TValue.From<Currency>(AValue);
  end;
end;

class function TValueHelper.FromString(const Value: string): TValue;
begin
  Result := TValue.From<string>(Value);
end;

class function TValueHelper.FromVarRec(const Value: TVarRec): TValue;
begin
  case Value.VType of
    vtInteger: Result := Value.VInteger;
    vtBoolean: Result := Value.VBoolean;
{$IFNDEF NEXTGEN}
    vtChar: Result := string(Value.VChar);
{$ENDIF}
    vtExtended: Result := Value.VExtended^;
{$IFNDEF NEXTGEN}
    vtString: Result := string(Value.VString^);
{$ENDIF}
    vtPointer: Result := TValue.From<Pointer>(Value.VPointer);
{$IFNDEF NEXTGEN}
    vtPChar: Result := string(Value.VPChar);
{$ENDIF}
    vtObject: Result := Value.VObject;
    vtClass: Result := Value.VClass;
    vtWideChar: Result := string(Value.VWideChar);
    vtPWideChar: Result := string(Value.VPWideChar);
{$IFNDEF NEXTGEN}
    vtAnsiString: Result := string(AnsiString(Value.VAnsiString));
{$ENDIF}
    vtCurrency: Result := Value.VCurrency^;
    vtVariant: Result := TValue.FromVariant(Value.VVariant^);
    vtInterface: Result := TValue.From<IInterface>(IInterface(Value.VInterface));
    vtWideString:
{$IFNDEF NEXTGEN}
      Result := WideString(Value.VWideString);
{$ELSE}
      Result := string(Value.VWideString);
{$ENDIF}
    vtInt64: Result := Value.VInt64^;
    vtUnicodeString: Result := string(Value.VUnicodeString);
  end;
end;

function TValueHelper.GetRttiType: TRttiType;
begin
  Result := Context.GetType(TypeInfo);
end;

function TValueHelper.IsBoolean: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Boolean);
end;

function TValueHelper.IsByte: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Byte);
end;

function TValueHelper.IsCardinal: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Cardinal);
{$IFNDEF CPUX64}
  Result := Result or (TypeInfo = System.TypeInfo(NativeUInt));
{$ENDIF}
end;

function TValueHelper.IsCurrency: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Currency);
end;

function TValueHelper.IsDate: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TDate);
end;

function TValueHelper.IsDateTime: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TDateTime);
end;

function TValueHelper.IsDouble: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Double);
end;

function TValueHelper.IsFloat: Boolean;
begin
  Result := Kind = tkFloat;
end;

function TValueHelper.IsInstance: Boolean;
begin
  Result := Kind in [tkClass, tkInterface];
end;

function TValueHelper.IsInt64: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Int64);
{$IFDEF CPUX64}
  Result := Result or (TypeInfo = System.TypeInfo(NativeInt));
{$ENDIF}
end;

function TValueHelper.IsInteger: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Integer);
{$IFNDEF CPUX64}
  Result := Result or (TypeInfo = System.TypeInfo(NativeInt));
{$ENDIF}
end;

function TValueHelper.IsInterface: Boolean;
begin
  Result := Assigned(TypeInfo) and (TypeInfo.Kind = tkInterface);
end;

function TValueHelper.IsNumeric: Boolean;
begin
  Result := Kind in [tkInteger, tkChar, tkEnumeration, tkFloat, tkWChar, tkInt64];
end;

function TValueHelper.IsPointer: Boolean;
begin
  Result := Kind = tkPointer;
end;

function TValueHelper.IsShortInt: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(ShortInt);
end;

function TValueHelper.IsSingle: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Single);
end;

function TValueHelper.IsSmallInt: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(SmallInt);
end;

function TValueHelper.IsString: Boolean;
begin
  Result := Kind in [tkChar, tkString, tkWChar, tkLString, tkWString, tkUString];
end;

function TValueHelper.IsTime: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TTime);
end;

function TValueHelper.IsUInt64: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(UInt64);
{$IFDEF CPUX64}
  Result := Result or (TypeInfo = System.TypeInfo(NativeInt));
{$ENDIF}
end;

function TValueHelper.IsVariant: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Variant);
end;

function TValueHelper.IsWord: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Word);
end;

function TValueHelper.ToObject: TObject;
begin
  if IsInterface then
    Result := AsInterface as TObject
  else
    Result := AsObject;
end;

class function TValueHelper.ToString(const Values: array of TValue): string;
var
  i: Integer;
begin
  Result := '';
  for i := Low(Values) to High(Values) do
  begin
    if i > Low(Values) then
      Result := Result + ', ';

    if Values[i].IsString then
      Result := Result + '''' + TValue.ToString(Values[i]) + ''''
    else
      Result := Result + TValue.ToString(Values[i]);
  end;
end;

function TValueHelper.ToVarRec: TVarRec;
begin
  case Kind of
    tkInteger:
    begin
      Result.VType := vtInteger;
      Result.VInteger := AsInteger;
    end;
{$IFNDEF NEXTGEN}
    tkChar:
    begin
      Result.VType := vtChar;
      Result.VChar := AsType<AnsiChar>;
    end;
{$ENDIF}
    tkEnumeration:
    begin
      if IsBoolean then
      begin
        Result.VType := vtBoolean;
        Result.VBoolean := AsBoolean;
      end
      else
      begin
        Result.VType := vtInteger;
        Result.VInteger := AsInteger;
      end;
    end;
    tkFloat:
    begin
      if IsCurrency then
      begin
        Result.VType := vtCurrency;
        Result.VCurrency := GetReferenceToRawData;
      end
      else
      begin
        Result.VType := vtExtended;
        Result.VExtended := GetReferenceToRawData;
      end;
    end;
    tkString, tkUString:
    begin
      Result.VType := vtUnicodeString;
      Result.VUnicodeString := Pointer(AsString);
    end;
    tkClass, tkInterface:
    begin
      Result.VType := vtUnicodeString;
      Result.VUnicodeString := Pointer(ToObject.ToString);
    end;
  end;
end;

class function TValueHelper.ToVarRecs(
  const Values: array of TValue): TArray<TVarRec>;
var
  i: Integer;
begin
  SetLength(Result, Length(Values));
  for i := Low(Values) to High(Values) do
    Result[i] := Values[i].ToVarRec;
end;

class function TValueHelper.ToString(const Value: TValue): string;
var
  LInterface: IInterface;
  LObject: TObject;
begin
  case Value.Kind of
    tkFloat:
    begin
      if Value.IsDate then
      begin
        Result := DateToStr(Value.AsDate);
      end else
      if Value.IsDateTime then
      begin
        Result := DateTimeToStr(Value.AsDateTime);
      end else
      if Value.IsTime then
      begin
        Result := TimeToStr(Value.AsTime);
      end else
      begin
        Result := Value.ToString;
      end;
    end;
    tkClass:
    begin
      LObject := Value.AsObject;
      Result := Format('%s($%x)', [StripUnitName(LObject.ClassName),
        NativeInt(LObject)]);
    end;
    tkInterface:
    begin
      LInterface := Value.AsInterface;
      LObject := LInterface as TObject;
      Result := Format('%s($%x) as %s', [StripUnitName(LObject.ClassName),
        NativeInt(LInterface), StripUnitName(GetTypeName(Value.TypeInfo))]);
    end
  else
    Result := Value.ToString;
  end;
end;

function TValueHelper.TryConvert(ATypeInfo: PTypeInfo;
  out AResult: TValue): Boolean;
begin
  Result := False;

  if ATypeInfo = System.TypeInfo(TValue) then
  begin
    AResult:= Self;
    Exit(True);
  end;

  if Assigned(ATypeInfo) then
  begin
    Result := Conversions[Kind, ATypeInfo.Kind](Self, ATypeInfo, AResult);

    if not Result then
    begin
      case Kind of
        tkRecord: Result := ConvNullable2Any(Self, ATypeInfo, AResult);
{$IFDEF VER210}
        // workaround for bug in RTTI.pas (fixed in XE)
        tkUnknown:
        begin
          case ATypeInfo.Kind of
            tkInteger, tkEnumeration, tkChar, tkWChar, tkInt64:
            begin
              AResult := TValue.FromOrdinal(ATypeInfo, 0);
              Result := True;
            end;
            tkFloat:
            begin
              AResult := TValue.From<Extended>(0);
              Result := True;
            end;
            tkUString:
            begin
              AResult := TValue.FromString('');
              Result := True;
            end;
          end;
        end;
{$ENDIF}
      end;
      case ATypeInfo.Kind of
        tkRecord: Result := ConvAny2Nullable(Self, ATypeInfo, AResult);
      end
    end;
    if not Result then
    begin
      Result := TryCast(ATypeInfo, AResult);
    end;
  end;
end;

function TValueHelper.TryConvert<T>(out AResult: TValue): Boolean;
begin
  Result := TryConvert(System.TypeInfo(T), AResult);
end;

type
 // Declare compatible members of TRttiObject in System.Rtti.pas
  TRttiObjectFieldRef = class abstract
  public
    FHandle: Pointer;
    FRttiDataSize: Integer;
    FPackage: Pointer{TRttiPackage};
    FParent: Pointer{TRttiObject};
    FAttributeGetter: Pointer{TFunc<TArray<TCustomAttribute>>};
  end;

  TRttiObjectAccess = class helper for TRttiObject
  public
    procedure Init(Parent: TRttiType; PropInfo: {$IFDEF DELPHI_XE3_UP}PPropInfoExt{$ELSE}PPropInfo {$ENDIF});
  end;


procedure TRttiObjectAccess.Init(Parent: TRttiType; PropInfo: {$IFDEF DELPHI_XE3_UP}PPropInfoExt{$ELSE}PPropInfo {$ENDIF});
const
{$IFDEF AUTOREFCOUNT}
  FHANDLE_OFFSET = (SizeOf(Pointer) * 2);
  FPARENT_OFFSET = (SizeOf(Pointer) * 3);
{$ELSE}
  FHANDLE_OFFSET = (SizeOf(Pointer) * 1);
  FPARENT_OFFSET = (SizeOf(Pointer) * 2);
{$ENDIF}
begin
//if TRttiObject.InstanceSize <> TRttiObjectFieldRef.InstanceSize then
//  assert;
  TRttiObjectFieldRef(Self).FParent := Parent;
  TRttiObjectFieldRef(Self).FHandle := PropInfo;
end;

{$IFDEF DELPHI_XE3_UP}
{ TPropInfoExt }

function TPropInfoExt.NameFld: TTypeInfoFieldAccessor;
begin
  Result.SetData(@NameLength);
end;

function TPropInfoExt.Tail: PPropInfoExt;
begin
  Result := PPropInfoExt(NameFld.Tail);
end;
{$ENDIF}
{ TRttiPropertyExtension }

class constructor TRttiPropertyExtension.Create;
begin
  FRegister := TObjectDictionary<TPair<PTypeInfo, string>, TRttiPropertyExtension>.Create([doOwnsValues]);
  FPatchedClasses := TDictionary<TClass, TClass>.Create;

  TRttiPropertyExtension.InitVirtualMethodTable;
end;

class destructor TRttiPropertyExtension.Destroy;
var
  LClass: TClass;
  LPointer: Pointer;
begin
  for LClass in FPatchedClasses.Values do
  begin
    LPointer := PByte(LClass) + vmtSelfPtr;
    FreeMem(LPointer);
  end;

  FPatchedClasses.Free;
  FRegister.Free;
end;

constructor TRttiPropertyExtension.Create(Parent: PTypeInfo; const Name: string; PropertyType: PTypeInfo);
{$IFDEF DELPHI_XE3_UP}
var
  M: TMarshaller;
{$ENDIF}
begin
  inherited Create;
  FPropInfo.PropType := Pointer(NativeInt(PropertyType) - SizeOf(PTypeInfo));
  {$IFNDEF DELPHI_XE3_UP}
  FPropInfo.Name := ShortString(Name);
  {$ELSE}
  if Name.Length > 255 then
    FPropInfo.NameLength := 255
  else
    FPropInfo.NameLength := Name.Length;
  Move(M.AsAnsi(Name).ToPointer^, FPropInfo.NameData[0], FPropInfo.NameLength);
  {$ENDIF}
  Init(GetRttiType(Parent), @FPropInfo);

  FRegister.Add(TPair<PTypeInfo, string>.Create(Parent, Name), Self);

  PPointer(Self)^ := FPatchedClasses[Self.ClassType];
end;

function TRttiPropertyExtension.DoGetValue(Instance: Pointer): TValue;
begin
  Result := FGetter(Instance);
end;

function TRttiPropertyExtension.DoGetValueStub(Instance: Pointer): TValue;
begin
  Result := DoGetValue(Instance);
end;

procedure TRttiPropertyExtension.DoSetValue(Instance: Pointer;
  const AValue: TValue);
begin
  FSetter(Instance, AValue);
end;

procedure TRttiPropertyExtension.DoSetValueStub(Instance: Pointer;
  const AValue: TValue);
begin
  DoSetValue(Instance, AValue);
end;

class function TRttiPropertyExtension.FindByName(Parent: TRttiType;
  const PropertyName: string): TRttiPropertyExtension;
var
  LPropertyExtension: TRttiPropertyExtension;
begin
  for LPropertyExtension in FRegister.Values do
  begin
    if (LPropertyExtension.Parent = Parent) and SameText(LPropertyExtension.Name, PropertyName) then
    begin
      Result := LPropertyExtension;
      Exit;
    end;
  end;

  if Assigned(Parent.BaseType) then
    Result := FindByName(Parent.BaseType, PropertyName)
  else
    Result := nil
end;

class function TRttiPropertyExtension.FindByName(
  const FullPropertyName: string): TRttiPropertyExtension;
var
  LScope: string;
  LName: string;
  LProp: TRttiPropertyExtension;
begin
  Result := nil;
  {$IFNDEF NEXTGEN}
  LScope := Copy(FullPropertyName, 1, LastDelimiter('.', FullPropertyName) - 1);
  LName := Copy(FullPropertyName, LastDelimiter('.', FullPropertyName) + 1);
  for LProp in FRegister.Values do
  begin
    if SameText(LProp.Name, LName)
      and EndsText(LScope, LProp.Parent.AsInstance.MetaclassType.QualifiedClassName) then
    begin
      Result := LProp;
      Break;
    end;
  end;
  {$ELSE}
  LScope := FullPropertyName.SubString(0, FullPropertyName.LastDelimiter('.'));
  LName := FullPropertyName.SubString(FullPropertyName.LastDelimiter('.') + 1);
  for LProp in FRegister.Values do
  begin
    if (string.Compare(LProp.Name, LName, [coIgnoreCase]) = 0)
      and LProp.Parent.AsInstance.MetaclassType.QualifiedClassName.EndsWith(LScope, True) then
    begin
      Result := LProp;
      Break;
    end;
  end;  
  {$ENDIF}
end;

function TRttiPropertyExtension.GetIsReadable: Boolean;
begin
  Result := Assigned(FGetter);
end;

function TRttiPropertyExtension.GetIsReadableStub: Boolean;
begin
  Result := GetIsReadable;
end;

function TRttiPropertyExtension.GetIsWritable: Boolean;
begin
  Result := Assigned(FSetter);
end;

function TRttiPropertyExtension.GetIsWritableStub: Boolean;
begin
  Result := GetIsWritable;
end;

function TRttiPropertyExtension.GetPropInfo: PPropInfo;
begin
  Result := Handle;
end;

function TRttiPropertyExtension.GetPropInfoStub: PPropInfo;
begin
  Result := GetPropInfo;
end;

class procedure TRttiPropertyExtension.InitVirtualMethodTable;
const
  MaxIndex = 17;  // TRttiInstanceProperty.GetPropInfo
{$POINTERMATH ON}
type
  PVtable = ^Pointer;
{$POINTERMATH OFF}
var
  LSize: Integer;
  LData: Pointer;
  LPatchedClass: TClass;
begin
  LSize := SizeOf(Pointer) * (1 + MaxIndex - (vmtSelfPtr div SizeOf(Pointer)));
  LData := AllocMem(LSize);
  LPatchedClass := TClass(PByte(LData) - vmtSelfPtr);
  FPatchedClasses.Add(Self, LPatchedClass);
  Move((PByte(Self) + vmtSelfPtr)^, LData^, LSize);

  PVtable(LPatchedClass)[5] := @TRttiPropertyExtension.GetIsReadableStub;
  PVtable(LPatchedClass)[6] := @TRttiPropertyExtension.GetIsWritableStub;
  PVtable(LPatchedClass)[7] := @TRttiPropertyExtension.DoGetValueStub;
  PVtable(LPatchedClass)[8] := @TRttiPropertyExtension.DoSetValueStub;
  PVtable(LPatchedClass)[12] := @TRttiPropertyExtension.GetPropInfoStub;
end;


{TTimeSpanHelper}
{$IFDEF DELPHI_2010}
class function TTimeSpanHelper.Subtract(const D1, D2: TDateTime): TTimeSpan;
begin
  Result := TTimeSpan.Create(Trunc(TimeStampToMSecs(DateTimeToTimeStamp(D1)) - TimeStampToMSecs(DateTimeToTimeStamp(D2))) * TicksPerMillisecond);
end;
{$ENDIF}


class function TStrUtils.SplitString(const S, Delimiters: string): TArray<string>;
var
  StartIdx: Integer;
  FoundIdx: Integer;
  SplitPoints: Integer;
  CurrentSplit: Integer;
  i: Integer;
begin
  Result := nil;

  {$IFNDEF NEXTGEN}
  if S <> '' then
  begin
    { Determine the length of the resulting array }
    SplitPoints := 0;
    for i := 1 to Length(S) do
      if IsDelimiter(Delimiters, S, i) then
        Inc(SplitPoints);

    SetLength(Result, SplitPoints + 1);

    { Split the string and fill the resulting array }
    StartIdx := 1;
    CurrentSplit := 0;
    repeat
      FoundIdx := FindDelimiter(Delimiters, S, StartIdx);
      if FoundIdx <> 0 then
      begin
        Result[CurrentSplit] := Copy(S, StartIdx, FoundIdx - StartIdx);
        Inc(CurrentSplit);
        StartIdx := FoundIdx + 1;
      end;
    until CurrentSplit = SplitPoints;

    // copy the remaining part in case the string does not end in a delimiter
    Result[SplitPoints] := Copy(S, StartIdx, Length(S) - StartIdx + 1);
  end;
  {$ELSE}
  if S <> string.Empty then
  begin
    { Determine the length of the resulting array }
    SplitPoints := 0;
    for I := 0 to S.Length - 1 do
      if S.IsDelimiter(Delimiters, i) then
        Inc(SplitPoints);

    SetLength(Result, SplitPoints + 1);

    { Split the string and fill the resulting array }
    StartIdx := 0;
    CurrentSplit := 0;
    repeat
      FoundIdx := S.IndexOfAny(Delimiters.ToCharArray, StartIdx);
      if FoundIdx <> -1 then
      begin
        Result[CurrentSplit] := S.SubString(StartIdx, FoundIdx - StartIdx);
        Inc(CurrentSplit);
        StartIdx := FoundIdx + 1;
      end;
    until CurrentSplit = SplitPoints;

    // copy the remaining part in case the string does not end in a delimiter
    Result[SplitPoints] := S.SubString(StartIdx, S.Length - StartIdx + 1);
  end;
  {$ENDIF}

end;

{ TListStringUtils }

class function TListStringUtils.ToArray(const values: TList<string>): TArray<string>;
var
  i : integer;
begin
  SetLength(result,values.Count);
  for i := 0 to values.Count - 1 do
    result[i] := values[i];
end;


initialization
  Enumerations := TObjectDictionary<PTypeInfo, TStrings>.Create([doOwnsValues]);

finalization
  Enumerations.Free;


end.

