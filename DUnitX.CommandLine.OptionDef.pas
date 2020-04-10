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

unit DUnitX.CommandLine.OptionDef;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Classes,
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  {$ELSE}
  Classes,
  SysUtils,
  Rtti,
  TypInfo,
  Generics.Collections,
  {$ENDIF}

  DUnitX.CommandLine.Options;

type
  IOptionDefInvoke = interface
    ['{580B5B40-CD7B-41B8-AE53-2C6890141FF0}']
    procedure Invoke(const value : string);
    function WasFound : boolean;
    function GetTypeInfo : PTypeInfo;
  end;

  TOptionDefinition<T> = class(TInterfacedObject,IOptionDefintion,IOptionDefInvoke)
  private
    FLongName : string;
    FShortName : string;
    FHelpText       : string;
    FHasValue       : boolean;
    FRequired       : boolean;
    FValueRequired  : boolean;
    FIsOptionFile   : boolean;
    FAllowMultiple  : boolean;
    FProc           : TProc<T>;
    FWasFound       : boolean;
    FTypeInfo       : PTypeInfo;
    FDefault        : T;
    FHidden         : boolean;
  protected
    function GetAllowMultiple: Boolean;
    function GetHasValue: Boolean;
    function GetHelpText: string;
    function GetLongName: string;
    function GetRequired: Boolean;
    function GetShortName: string;
    function GetValueRequired: Boolean;
    function GetIsOptionFile: Boolean;
    function GetIsUnnamed: Boolean;
    function GetIsHidden: Boolean;

    procedure SetIsHidden(const value: Boolean);
    procedure SetIsOptionFile(const value: Boolean);
    procedure SetAllowMultiple(const value: Boolean);
    procedure SetHasValue(const value: Boolean);
    procedure SetHelpText(const value: string);
    procedure SetLongName(const value: string);
    procedure SetRequired(const value: Boolean);
    procedure SetShortName(const value: string);
    procedure SetValueRequired(const value: Boolean);
    procedure Invoke(const value : string);
    function WasFound : boolean;
    function GetTypeInfo : PTypeInfo;
    procedure InitDefault;
  public
    constructor Create(const longName : string; const shortName : string; const proc : TProc<T>);overload;
    constructor Create(const longName : string; const shortName : string; const helpText : string; const proc : TProc<T>);overload;

  end;

//in interface so we can unit test them
const
  trueStrings:  array[0..10] of string = ('True' ,'T','+','Yes','Y','On' ,'Enable', 'Enabled' , '1','-1', '');
  falseStrings: array[0..8] of string = ('False','F','-','No' ,'N','Off','Disable','Disabled', '0');

function StringToBoolean(const value: string): boolean;

implementation

uses
  DUnitX.ResStrs,
  {$IFDEF USE_NS}
  System.StrUtils;
  {$ELSE}
  StrUtils;
  {$ENDIF}

{ TOptionDefinition<T> }



function StringToBoolean(const value: string): boolean;
begin
  if MatchText(value, trueStrings) then
    result := true
  else if MatchText(value,falseStrings) then
    result := false
  else
    raise Exception.Create(SInvalidValueBool);
end;



constructor TOptionDefinition<T>.Create(const longName, shortName: string; const proc: TProc<T>);
begin
  FTypeInfo := TypeInfo(T);
  if not (FTypeInfo.Kind in [tkInteger,tkEnumeration,tkFloat,tkString,tkSet,tkLString,tkWString,tkInt64,tkUString]) then
    raise Exception.Create(SInvalidOptionType);

  FLongName := longName;
  FShortName := shortName;
  FHasValue := true;
  FProc := proc;
  InitDefault;
end;

constructor TOptionDefinition<T>.Create(const longName, shortName, helpText: string; const proc: TProc<T>);
begin
  Self.Create(longName,shortName,proc);
  FHelpText := helpText;
end;

function TOptionDefinition<T>.GetAllowMultiple: Boolean;
begin
  result := FAllowMultiple;
end;

function TOptionDefinition<T>.GetHasValue: Boolean;
begin
  result := FHasValue;
end;

function TOptionDefinition<T>.GetHelpText: string;
begin
  result := FHelpText;
end;

function TOptionDefinition<T>.GetIsHidden: Boolean;
begin
  result := FHidden;
end;

function TOptionDefinition<T>.GetIsOptionFile: Boolean;
begin
  Result := FIsOptionFile;
end;

function TOptionDefinition<T>.GetIsUnnamed: Boolean;
begin
  result := FLongName = '';
end;

function TOptionDefinition<T>.GetLongName: string;
begin
  result := FLongName;
end;

function TOptionDefinition<T>.GetRequired: Boolean;
begin
  result := FRequired;
end;

function TOptionDefinition<T>.GetShortName: string;
begin
  result := FShortName;
end;

function TOptionDefinition<T>.GetTypeInfo: PTypeInfo;
begin
  result := FTypeInfo;
end;

function TOptionDefinition<T>.GetValueRequired: Boolean;
begin
  result := FValueRequired;
end;

function TOptionDefinition<T>.WasFound: boolean;
begin
  result := FWasFound;
end;

procedure TOptionDefinition<T>.InitDefault;
begin
  FDefault := Default(T);
  if not FHasValue and (GetTypeName(FTypeInfo) = 'Boolean') then
      FDefault := TValue.FromVariant(true).AsType<T>;

end;


//Note : Using TValue.FromVariant as TValue.From<T>
procedure TOptionDefinition<T>.Invoke(const value: string);
var
  v : TValue;
  intVal : integer;
  int64Val : Int64;
  floatVal : Double;
  PTemp: Pointer;
begin
  FWasFound := True;
  if Assigned(FProc) then
  begin
    if value <> '' then
    begin
      //there must be a cleaner way to do this, TValue still fails at the most basic conversions.
      case FTypeInfo.Kind of
        tkInteger :
        begin
          intVal := StrToInt(value);
          v := TValue.From<Integer>(intVal) ;
        end;
        tkInt64 :
        begin
          int64Val := StrToInt64(value);
          v := TValue.From<Int64>(int64Val) ;
        end;
        tkString, tkLString,tkWString,tkUString :
        begin
          v := TValue.From<string>(value);
        end;
        tkSet :
        begin
          intVal := StringToSet(FTypeInfo, value);
          PTemp := @intVal;
          v := TValue.From<T>(T(PTemp^));
        end;
        tkEnumeration :
        begin
          if GetTypeName(FTypeInfo) = 'Boolean' then
          begin
            v := TValue.From<Boolean>(StringToBoolean(value));
          end
          else
          begin
            intVal := GetEnumValue(FTypeInfo,value);
            if intVal < 0 then
              raise Exception.Create(SInvalidEnum + value);

            v := TValue.FromOrdinal(FTypeInfo,intVal);
          end;
        end;
        tkFloat :
        begin
           floatVal := StrToFloat(value);
           v := TValue.From<Double>(floatVal);
        end;
      else
        raise Exception.Create(SInvalidOpt);
        //what?
      end;
      FProc(v.AsType<T>);
    end
    else
    begin
      FProc(FDefault);
    end;
  end;
end;

procedure TOptionDefinition<T>.SetAllowMultiple(const value: Boolean);
begin
  FAllowMultiple := value;
end;

procedure TOptionDefinition<T>.SetHasValue(const value: Boolean);
begin
  FHasValue := value;
  InitDefault;
end;

procedure TOptionDefinition<T>.SetHelpText(const value: string);
begin
  FHelpText := value;
end;

procedure TOptionDefinition<T>.SetIsHidden(const value: Boolean);
begin
  FHidden := value;
end;

procedure TOptionDefinition<T>.SetIsOptionFile(const value: Boolean);
begin
  FIsOptionFile := value;
end;

procedure TOptionDefinition<T>.SetLongName(const value: string);
begin
  FLongName := value;
end;

procedure TOptionDefinition<T>.SetRequired(const value: Boolean);
begin
  FRequired := value;
end;

procedure TOptionDefinition<T>.SetShortName(const value: string);
begin
  FShortName := value;
end;

procedure TOptionDefinition<T>.SetValueRequired(const value: Boolean);
begin
  FValueRequired := value;
end;

end.
