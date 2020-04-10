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

unit DUnitX.Utils.XML;

interface

{$I DUnitX.inc}

function IsCharValidXML(wideChar: WideChar): Boolean;
function StripInvalidXML(const xmlString: string): string;
function EscapeForXML(const value: string; const isAttribute: boolean = True; const isCDATASection : Boolean = False): string;

implementation

uses
  {$IFDEF USE_NS}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

function IsCharValidXML(wideChar: WideChar): Boolean;
begin
  case Word(wideChar) of
    $0009, $000A, $000C, $000D,
      $0020..$D7FF,
      $E000..$FFFD, // Standard Unicode chars below $FFFF
      $D800..$DBFF, // High surrogate of Unicode character  = $10000 - $10FFFF
      $DC00..$DFFF: // Low surrogate of Unicode character  = $10000 - $10FFFF
      result := True;
  else
    result := False;
  end;
end;


function StripInvalidXML(const xmlString: string): string;
var
  index: integer;
  count: Integer;
begin
  {$IFNDEF NEXTGEN}
  count := Length(xmlString);
  setLength(result, count);
  for index := 1 to Count do
  begin
    if IsCharValidXML(WideChar(xmlString[index])) then
      result[index] := xmlString[index]
    else
      result[index] := ' ';
  end;
  {$ELSE}
  count := xmlString.Length;
  setLength(result, count);
  for index := 0 to Count - 1 do
  begin
    if IsCharValidXML(WideChar(xmlString.Chars[index])) then
    begin
      result := result.Remove(index, 1);
      result := result.Insert(index, xmlString.Chars[index]);
    end
    else
    begin
      result := result.Remove(index, 1);
      result := result.Insert(index, ' ');
    end;
  end;
  {$ENDIF}
end;


function EscapeForXML(const value: string; const isAttribute: boolean = True; const isCDATASection : Boolean = False): string;
begin
  result := StripInvalidXML(value);
  if isCDATASection  then
  begin
    Result := StringReplace(Result, ']]>', ']>',[rfReplaceAll]);
    exit;
  end;

  //note we are avoiding replacing &amp; with &amp;amp; !!
  Result := StringReplace(result, '&amp;', '[[-xy-amp--]]',[rfReplaceAll]);
  Result := StringReplace(result, '&', '&amp;',[rfReplaceAll]);
  Result := StringReplace(result, '[[-xy-amp--]]', '&amp;amp;',[rfReplaceAll]);
  Result := StringReplace(result, '<', '&lt;',[rfReplaceAll]);
  Result := StringReplace(result, '>', '&gt;',[rfReplaceAll]);

  if isAttribute then
  begin
    Result := StringReplace(result, '''', '&#39;',[rfReplaceAll]);
    Result := StringReplace(result, '"', '&quot;',[rfReplaceAll]);
  end;
end;

end.
