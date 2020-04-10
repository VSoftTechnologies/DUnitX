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

unit DUnitX.ConsoleWriter.Base;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  System.Classes;
  {$ELSE}
  Classes;
  {$ENDIF}



type
  TConsoleColour = (ccDefault, ccBrightRed, ccDarkRed,
                    ccBrightBlue, ccDarkBlue,
                    ccBrightGreen, ccDarkGreen,
                    ccBrightYellow, ccDarkYellow,
                    ccBrightAqua, ccDarkAqua,
                    ccBrightPurple, ccDarkPurple,
                    ccGrey, ccBlack,
                    ccBrightWhite,
                    ccWhite); // the normal colour of text on the console
  {$M+}
  IDUnitXConsoleWriter = interface
    ['{EFE59EB8-0C0B-4790-A964-D8126A2728A9}']
    function GetIndent : Integer;
    procedure SetIndent(const count: Integer);
    procedure SetColour(const foreground: TConsoleColour; const background: TConsoleColour = ccDefault);
    procedure WriteLn(const s: String);overload;
    procedure WriteLn;overload;
    procedure Write(const s : string);
    procedure Indent(const value : integer = 1);
    procedure Outdent(const value : integer = 1);
    property CurrentIndentLevel : Integer read GetIndent write SetIndent;

  end;

  TDUnitXConsoleWriterBase = class(TInterfacedObject, IDUnitXConsoleWriter)
  private
    FIndent : integer;
    FConsoleWidth : integer;
    FRedirectedStdOut : boolean;
  protected
    function GetIndent : Integer;
    procedure SetIndent(const count: Integer);virtual;
    function InternalBreakupMessage(const s : string): TStringList;
    procedure InternalWriteLn(const s : string); virtual;abstract;
    procedure InternalWrite(const s : string);virtual;abstract;
    procedure Indent(const value : integer = 1);
    procedure Outdent(const value : integer = 1);
    property ConsoleWidth : integer read FConsoleWidth write FConsoleWidth;
    property RedirectedStdOut : boolean read FRedirectedStdOut write FRedirectedStdOut;
  public
    constructor Create;virtual;
    procedure SetColour(const foreground: TConsoleColour; const background: TConsoleColour = ccDefault); virtual;abstract;
    procedure WriteLn(const s: String);overload;virtual;
    procedure WriteLn;overload;virtual;
    procedure Write(const s : string);virtual;
    property CurrentIndentLevel : Integer read GetIndent write SetIndent;
  end;


implementation

{ TDUnitXConsoleWriterBase }

uses
  {$IFDEF USE_NS}
  System.Types,
  System.StrUtils,
  System.SysUtils;
  {$ELSE}
  Types,
  StrUtils,
  SysUtils;
  {$ENDIF}

const
  DEFAULT_CONSOLE_WIDTH = 80;
  MINIMUM_CONSOLE_WIDTH = 2;

constructor TDUnitXConsoleWriterBase.Create;
begin
  FConsoleWidth := DEFAULT_CONSOLE_WIDTH;
  FIndent := 0;
end;

function TDUnitXConsoleWriterBase.GetIndent: Integer;
begin
  result := FIndent;
end;

procedure TDUnitXConsoleWriterBase.Indent(const value: integer);
begin
  SetIndent(FIndent + value);
end;

function TDUnitXConsoleWriterBase.InternalBreakupMessage(const s: string): TStringList;
var
  line: string;
  offset, width, len : Integer;
  slLines : TStringList;
begin
  Result := TStringList.Create;

  //If we are blank string, add on line entry and leave.
  if s = '' then
  begin
    Result.Add('');
    Exit;
  end;

  width := FConsoleWidth - FIndent;
  slLines := TStringList.Create;
  try
    slLines.StrictDelimiter := True;
    slLines.Text := s;

    //Walk through the string list pulling of the console width of characters at a time.
    for line in slLines do
    begin
      {$IFDEF NEXTGEN}	
      len := line.Length;	
      {$ELSE}
      len := Length(line);
      {$ENDIF}
      
      if (width > 0) and (len > width) then
      begin
        offset := 1;
        while offset <= len do
        begin
          //Write a line as we have hit the limit of the console.
	  {$IFDEF NEXTGEN}
	  Result.Add(line.Substring(offset-1, width));
	  {$ELSE}
          Result.Add(Copy(line, offset, width));
	  {$ENDIF}
          Inc(offset, width);
        end;
      end
      else
        //Can write out on a single line
        Result.Add(line);
    end;
  finally
    slLines.Free;
  end;
end;

procedure TDUnitXConsoleWriterBase.Outdent(const value: integer);
begin
  SetIndent(FIndent - value);
end;

procedure TDUnitXConsoleWriterBase.SetIndent(const count: Integer);
begin
  if Count < 0 then
    FIndent := 0
  else
  begin
    FIndent := count;
    if not FRedirectedStdOut then
    begin
      if FIndent > FConsoleWidth - MINIMUM_CONSOLE_WIDTH then
        FIndent := FConsoleWidth - MINIMUM_CONSOLE_WIDTH;
    end;
  end;
end;

procedure TDUnitXConsoleWriterBase.Write(const s: string);
var
  // offset, width, len : Integer;
  slLines : TStringList;
  iLineIndx : integer;
begin
  slLines := InternalBreakupMessage(s);
  try
    //Write out all the lines execept the last one.
    for iLineIndx  := 0 to slLines.Count - 2 do
      InternalWriteLn(slLines[iLineIndx]);

    //Now write out the last one without an end of line character.
    if slLines.Count > 0 then
      InternalWrite(slLines[slLines.Count - 1]);
  finally
    FreeAndNil(slLines);
  end;

end;

procedure TDUnitXConsoleWriterBase.WriteLn;
begin
  WriteLn('');
end;

procedure TDUnitXConsoleWriterBase.WriteLn(const s: String);
var
  // offset, width, len : Integer;
  slLines : TStringList;
  iLineIndx : integer;
begin
  slLines := InternalBreakupMessage(s);
  try
    //Write out all the lines execept the last one.
    for iLineIndx  := 0 to slLines.Count - 1 do
      InternalWriteLn(slLines[iLineIndx]);
  finally
    FreeAndNil(slLines);
  end;
end;

end.
