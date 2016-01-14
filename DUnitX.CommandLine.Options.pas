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

unit DUnitX.CommandLine.Options;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
  System.Classes;
  {$ELSE}
  Generics.Collections,
  Rtti,
  TypInfo,
  SysUtils,
  Classes;
  {$ENDIF}

type
  ICommandLineParseResult = interface
    ['{1715B9FF-8A34-47C9-843E-619C5AEA3F32}']
    function GetHasErrors : boolean;
    function GetErrorText : string;
    property HasErrors : boolean read GetHasErrors;
    property ErrorText : string read GetErrorText;
  end;

  ICommandLineParser = interface
    ['{6F970026-D1EE-4A3E-8A99-300AD3EE9C33}']
    //parses the command line
    function Parse : ICommandLineParseResult;overload;
    //parses the passed in string - makes testing easier.
    function Parse(const values : TStrings) : ICommandLineParseResult;overload;
  end;


  IOptionDefintion = interface
  ['{1EAA06BA-8FBF-43F8-86D7-9F5DE26C4E86}']
    function GetLongName : string;
    function GetShortName : string;
    function GetHasValue : boolean;
    procedure SetHasValue(const value : boolean);
    function GetHelpText : string;
    procedure SetHelpText(const value : string);
    function GetRequired : boolean;
    procedure SetRequired(const value : boolean);
    function GetValueRequired : boolean;
    procedure SetValueRequired(const value : boolean);
    function GetAllowMultiple : boolean;
    procedure SetAllowMultiple(const value : boolean);
    function GetIsOptionFile : boolean;
    procedure SetIsOptionFile(const value : boolean);
    function GetIsHidden : boolean;
    procedure SetIsHidden(const value : boolean);

    function GetIsUnnamed : boolean;
    property LongName       : string read GetLongName;
    property ShortName      : string read GetShortName;
    property HasValue       : boolean read GetHasValue write SetHasValue;
    property HelpText       : string read GetHelpText write SetHelpText;
    property Required       : boolean read GetRequired write SetRequired;
    property ValueRequired  : boolean read GetValueRequired write SetValueRequired;
    property AllowMultiple  : boolean read GetAllowMultiple write SetAllowMultiple;
    property IsOptionFile   : boolean read GetIsOptionFile write SetIsOptionFile;
    property IsUnnamed      : boolean read GetIsUnnamed;
    property Hidden         : boolean read GetIsHidden write SetIsHidden;
  end;


  TOptionsRegistry = class
  private
    class var
      //used for fast lookup of option by name
      FOptionsLookup : TDictionary<string,IOptionDefintion>;
      //can't put unnamed options in dictionary, so we keep a list
      FUnnamedOptions : TList<IOptionDefintion>;
      //all registered options.
      FRegisteredOptions : TList<IOptionDefintion>;
  protected
    class constructor Create;
    class destructor Destroy;
  public
    class function RegisterOption<T>(const longName: string; const shortName : string; const Action : TProc<T>) : IOptionDefintion;overload;
    class function RegisterOption<T>(const longName: string; const shortName : string; const helpText : string; const Action : TProc<T>) : IOptionDefintion;overload;
    class function RegisterUnNamedOption<T>(const helpText : string; const Action : TProc<T>) : IOptionDefintion;overload;
    class function AllRegisteredOptions : TList<IOptionDefintion>;
    class function Parse: ICommandLineParseResult;overload;
    class function Parse(const values : TStrings) : ICommandLineParseResult;overload;
    class property RegisteredOptions : TDictionary<string,IOptionDefintion> read FOptionsLookup;
    class property RegisteredUnamedOptions : TList<IOptionDefintion> read FUnnamedOptions;
    class procedure PrintUsage(const proc : TProc<string>; const pad : integer = 30);
  end;

implementation

uses
  DUnitX.Utils,
  DUnitX.CommandLine.Parser,
  DUnitX.Commandline.OptionDef,
  DUnitX.ResStrs;


{ TOptionsRegistry }

class function TOptionsRegistry.RegisterOption<T>(const longName, shortName: string; const Action: TProc<T>): IOptionDefintion;
begin
  if longName = '' then
    raise Exception.Create(SNameRequired);

  if FOptionsLookup.ContainsKey(LowerCase(longName)) then
    raise Exception.Create(Format(SOptionAlreadyRegistered, [longName]));

  if FOptionsLookup.ContainsKey(LowerCase(shortName)) then
    raise Exception.Create(Format(SOptionAlreadyRegistered, [shortName]));

  result := TOptionDefinition<T>.Create(longName,shortName,Action);

  FOptionsLookup.Add(LowerCase(longName),Result);
  FRegisteredOptions.Add(Result);

  if shortName <> '' then
    FOptionsLookup.Add(LowerCase(shortName),Result);
end;

class function TOptionsRegistry.Parse: ICommandLineParseResult;
var
  parser : ICommandLineParser;
begin
  parser := TCommandLineParser.Create;
  result := parser.Parse;
end;

class function TOptionsRegistry.AllRegisteredOptions: TList<IOptionDefintion>;
begin
  result := FRegisteredOptions;
end;

class constructor TOptionsRegistry.Create;
begin
  FOptionsLookup := TDictionary<string,IOptionDefintion>.Create;
  FUnnamedOptions := TList<IOptionDefintion>.Create;
  FRegisteredOptions := TList<IOptionDefintion>.Create;

end;

class destructor TOptionsRegistry.Destroy;
begin
  FOptionsLookup.Free;
  FUnnamedOptions.Free;
  FRegisteredOptions.Free;
end;

class function TOptionsRegistry.Parse(const values: TStrings): ICommandLineParseResult;
var
  parser : ICommandLineParser;
begin
  parser := TCommandLineParser.Create;
  result := parser.Parse(values);
end;

class procedure TOptionsRegistry.PrintUsage(const proc: TProc<string>; const pad : integer);
var
  option : IOptionDefintion;
  helpString : string;
begin
  for option in AllRegisteredOptions do
  begin
    if option.Hidden then
      continue;
    helpString := '--' + option.LongName;
    if option.HasValue then
      helpString := helpString + ':value';
    if option.ShortName <> '' then
    begin
      helpString := helpString + ' or -' + option.ShortName ;
      if option.HasValue then
        helpString := helpString + ':value';
    end;
    helpString := TStrUtils.PadString(helpString,pad,false);

    if option.HelpText <> '' then
      helpString := helpString + ' - ' + option.HelpText;
    proc(helpString)
  end;  
end;

class function TOptionsRegistry.RegisterOption<T>(const longName, shortName, helpText: string; const Action: TProc<T>): IOptionDefintion;
begin
    result := RegisterOption<T>(longName,shortName,Action);
  result.HelpText := helpText;
end;

class function TOptionsRegistry.RegisterUnNamedOption<T>(const helpText: string; const Action: TProc<T>): IOptionDefintion;
begin
  result := TOptionDefinition<T>.Create('','',helptext,Action);
  result.HasValue := false;
  FUnNamedOptions.Add(Result);
  FRegisteredOptions.Add(Result);
end;

end.
