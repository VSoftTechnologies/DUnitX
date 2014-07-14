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

unit DUnitX.CommandLine;

interface

{$I DUnitX.inc}

uses
  Generics.Collections,
  DUnitX.TestFramework;

  {$IFDEF DOGFOODING}
  function NewCommandLine : ICommandLine;
  {$ENDIF}
  function CommandLine : ICommandLine;

  procedure RegisterCommandLineOption(const optionName: string; const separator : string; const hasValue : boolean);

implementation

uses
  System.SysUtils,
  classes,
  DUnitX.Options,
  DUnitX.Utils;

var
  _commandLine : ICommandLine;

type
  TCommandLineOptions = class(TInterfacedObject, ICommandLineOptions, IOptionsBase)
  private
    FParamList : TStringList;
    FHideBanner : boolean;
    FFixturesToRun : TStrings;
    FDirty : Boolean;
  protected
    procedure SetHideBanner(const AValue : boolean); deprecated 'Will not be settable but external forces.';

    //ICommandLineOptions
    function GetHideBanner : boolean;
    function GetFixtures : TStrings;

    //IOptionsBase
    function HasOption(const optionName : string) : boolean;
    function GetOptions : TStringList;
    function GetOptionValue(const optionName : string) : string;
    function GetIsDirty : boolean;

    procedure ProcessCommandLine(const AParams : TStrings);
  public
    constructor Create;
    destructor Destroy;override;
  end;

  TCommandLine = class(TInterfacedObject, ICommandLine, IOptionsProvider)
  private
    FParamList : TStringList;
    FOptions : ICommandLineOptions;
  protected
    //IOptionsProvider
    function GetHideBanner : boolean; deprecated 'Will be replaced by options object.';
    procedure SetHideBanner(const AValue : boolean); deprecated 'Will be replaced by options object.';
    function GetOptionsInterface : IOptionsBase;
    function GetName : string;
    function GetOptions : ICommandLineOptions;
  public
    constructor Create;
    destructor Destroy;override;
  end;

{ TDUniXCommandLine }

{$IFDEF DOGFOODING}
function NewCommandLine : ICommandLine;
begin
  //Used only when DUnitX is testing itself. We don't want to use the singleton command
  //line which currently in use by the test runner when we are testing ourselves.
  result := TComandLine.Create;
end;
{$ENDIF}

function CommandLine: ICommandLine;
begin
  if _commandLine = nil then
    _commandLine := TCommandLine.Create;
  result := _commandLine;
end;

{ TCommandLine }

constructor TCommandLine.Create;
var
  count : integer;
  i     : integer;
begin
  FOptions := TCommandLineOptions.Create;

  FParamList := TStringList.Create;
  //this is intended to make it eaiser to test ProcessCommandLine
  count := System.ParamCount;
  for i := 0 to count do
    FParamList.Add(ParamStr(i));

  (FOptions as TCommandLineOptions).ProcessCommandLine(FParamList);

  //Register ourselves as a provider of the commandline options.
  Options.RegisterCategory(self);
end;

function TCommandLine.GetHideBanner: boolean;
begin
  Result := FOptions.HideBanner;
end;

function TCommandLine.GetName: string;
begin
  Result := 'Command Line';
end;

function TCommandLine.GetOptions: ICommandLineOptions;
begin
  Result := FOptions;
end;

function TCommandLine.GetOptionsInterface: IOptionsBase;
begin
  Result := FOptions;
end;

procedure TCommandLine.SetHideBanner(const AValue: boolean);
begin
  (FOptions as TCommandLineOptions).SetHideBanner(AValue);
end;

destructor TCommandLine.Destroy;
begin
  //Remove ourselves as a provider of the command line options.
  Options.UnregisterCategory(self);

  FOptions := nil;

  FParamList.Free;
  inherited;
end;

procedure RegisterCommandLineOption(const optionName: string; const separator : string; const hasValue : boolean);
begin
  //  {$Message 'TODO: RegisterCommandLineOption is to be used for registration of extension parameters.'}
  //  {$Message 'TODO: RegisterCommandLineOption needs to pass a notify method when the option is supplied.'}
  //  {$Message 'TODO: ProcessCommandLine needs to test for registered command line options. Shouldn''t allow calling RegisterCommandLineOption after ProcessCommandLine.'}
end;

{ TCommandLineOptions }

constructor TCommandLineOptions.Create;
begin
  FFixturesToRun := TStringList.Create;
  FFixturesToRun.Delimiter := ',';

  FHideBanner := False;

  FDirty := False;
end;

destructor TCommandLineOptions.Destroy;
begin
  FreeAndNil(FFixturesToRun);

  inherited;
end;

function TCommandLineOptions.GetFixtures: TStrings;
begin
  Result := FFixturesToRun;
end;

function TCommandLineOptions.GetHideBanner: boolean;
begin
  Result := FHideBanner;
end;

function TCommandLineOptions.GetIsDirty: boolean;
begin
  Result := FDirty;
end;

function TCommandLineOptions.GetOptions: TStringList;
begin
  Result := TStringList.Create;

  Result.Add('/fixtures');

  //TODO: Add banner option
  //TODO: Add log level option
end;

function TCommandLineOptions.GetOptionValue(const optionName: string): string;
begin
  Result := '';

  if AnsiLowerCase(optionName) = 'fixtures' then
    Result := FFixturesToRun.DelimitedText;
end;

function TCommandLineOptions.HasOption(const optionName: string): boolean;
begin
  Result := False;

  if AnsiLowerCase(optionName) = 'fixtures' then
    Result := FFixturesToRun.Count > 0;
end;

procedure TCommandLineOptions.ProcessCommandLine(const AParams: TStrings);
var
  iParam: Integer;
  sCommand: string;
  iFixtureStart: Integer;
  slFixtures: TStringList;
  sParam: string;
  sFixturesToAdd: string;
begin
  iParam := 0;
  while iParam < AParams.Count do
  begin
    //Replace - with / as they are synonymous
    sParam := StringReplace(AParams[iParam], '-', '/', [rfReplaceAll]);

    //Extract the command for this param, it should be in the form /command:valueA,valueB
    sCommand := StrBetweenChar(sParam, '/', ':');

    //Deal with seeing the fixtures parameter
    if AnsiLowerCase(sCommand) = 'fixtures' then
    begin
      slFixtures := TStringList.Create;
      try
        iFixtureStart := Pos(':', AParams[iParam]);

        sFixturesToAdd := Copy(AParams[iParam], iFixtureStart + 1, Length(AParams[iParam]) - iFixtureStart);

        if Trim(sFixturesToAdd) = '' then
          Exit;

        //Each fixture to test should be seperated with a comma. Therefore when adding
        //another fixture param we need to append a comma.
        FFixturesToRun.Add(sFixturesToAdd);
      finally
        FreeAndNil(slFixtures);
      end;
    end;

    {$Message 'TODO: ProcessCommandLine needs to raise an issue when there are invalid parameters detected. It might need to move out of the create of TCommandLine to do this.'}

    Inc(iParam);
  end;
end;

procedure TCommandLineOptions.SetHideBanner(const AValue: boolean);
begin
  FHideBanner := AValue;
end;

initialization
  {$Message 'TODO: Need to fix how the command line is initialised'}
  CommandLine;

end.
