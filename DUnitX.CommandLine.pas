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

  function CommandLine : ICommandLine;

  procedure RegisterCommandLineOption(const optionName: string; const separator : string; const hasValue : boolean);

implementation

uses
  classes;

var
  _commandLine : ICommandLine;


type
  TCommandLine = class(TInterfacedObject,ICommandLine)
  private
    FParamList : TStringList;
    FLogLevel : TLogLevel;
    FHideBanner : boolean;
  protected

    procedure ProcessCommandLine(const sList : TStrings);

    //ICommandLine;
    function GetLogLevel: TLogLevel;
    function GetHideBanner: Boolean;
    procedure SetHideBanner(const value : boolean);
    function HasOption(const optionName: string): Boolean;
    function GetOptionValue(const optionName : string) : string;
  public
    constructor Create;
    destructor Destroy;override;
  end;


{ TDUniXCommandLine }

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
  FLogLevel := TLogLevel.ltInformation;

  FParamList := TStringList.Create;
  //this is intended to make it eaiser to test ProcessCommandLine
  count := System.ParamCount;
  for i := 0 to count do
    FParamList.Add(ParamStr(i));
  ProcessCommandLine(FParamList);
end;

function TCommandLine.GetLogLevel: TLogLevel;
begin
  result := FLogLevel;
end;

function TCommandLine.GetOptionValue(const optionName: string): string;
begin

end;

destructor TCommandLine.Destroy;
begin
  FParamList.Free;
  inherited;
end;

function TCommandLine.GetHideBanner: Boolean;
begin
  result := FHideBanner;
end;

function TCommandLine.HasOption(const optionName: string): Boolean;
begin
  result := False;
end;

procedure TCommandLine.ProcessCommandLine(const sList : TStrings);
begin

end;

procedure TCommandLine.SetHideBanner(const value: boolean);
begin
  FHideBanner := true;
end;

procedure RegisterCommandLineOption(const optionName: string; const separator : string; const hasValue : boolean);
begin

end;

end.
