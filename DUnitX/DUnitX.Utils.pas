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

unit DUnitX.Utils;

interface

uses
  Rtti;

{$I DUnitX.inc}


type
  TCustomAttributeClass = class of TCustomAttribute;

  TAttributeUtils = class
    class function ContainsAttribute(const attributes : TArray<TCustomAttribute>; const AttributeClass : TCustomAttributeClass) : boolean;
    class function FindAttribute(const attributes : TArray<TCustomAttribute>; const AttributeClass : TCustomAttributeClass) : TCustomAttribute;overload;
    class function FindAttribute(const attributes : TArray<TCustomAttribute>; const AttributeClass : TCustomAttributeClass; var attribute  : TCustomAttribute; const startIndex : integer = 0) : integer;overload;
    class function FindAttributes(const attributes : TArray<TCustomAttribute>; const AttributeClass : TCustomAttributeClass) :  TArray<TCustomAttribute>;
  end;

  TValueHelper = record helper for TValue
    //TValue's AsString/AsType is pretty much useless.. so many conversions could have been handled
    //but for some reason they chose not to. Note this needs more work.. copy some stuff from delphi mocks.
    function AsStringEx : string;
  end;

  TStrUtils = class
    class function PadString(const s: string; const totalLength: integer; const padLeft: boolean = True; padChr: Char = ' '): string;
  end;

implementation

uses
  TypInfo,
  SysUtils;

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

{ TValueHelper }

function TValueHelper.AsStringEx: string;
begin
  result := '';
  case Self.Kind of
    tkUnknown: ;
    tkInteger: ;
    tkChar: ;
    tkEnumeration: ;
    tkFloat: result := FloatToStr(Self.AsExtended);
    tkString: result := Self.AsString;
    tkSet: ;
    tkClass: ;
    tkMethod: ;
    tkWChar: ;
    tkLString: ;
    tkWString: ;
    tkVariant: ;
    tkArray: ;
    tkRecord: ;
    tkInterface: ;
    tkInt64: ;
    tkDynArray: ;
    tkUString: ;
    tkClassRef: ;
    tkPointer: ;
    tkProcedure: ;
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


end.
