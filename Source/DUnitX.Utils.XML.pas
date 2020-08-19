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

function IsValidXMLChar(const wc: WideChar): Boolean;
function StripInvalidXML(const s: string): string;
function EscapeForXML(const value: string; const isAttribute: boolean = True; const isCDATASection : Boolean = False): string;

implementation

uses
  {$IFDEF USE_NS}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

function IsValidXMLChar(const wc: WideChar): Boolean;
begin
  case Word(wc) of
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


function StripInvalidXML(const s: string): string;
var
  i, count: Integer;
begin
  {$IFNDEF NEXTGEN}
  count := Length(s);
  setLength(result, count);
  for i := 1 to Count do // Iterate
  begin
    if IsValidXMLChar(WideChar(s[i])) then
      result[i] := s[i]
    else
      result[i] := ' ';
  end; // for}
  {$ELSE}
  count := s.Length;
  SetLength(result, count);
  for i := 0 to count - 1 do // Iterate
  begin
    if IsValidXMLChar(s.Chars[i]) then
    begin
      result := result.Remove(i, 1);
      result := result.Insert(i, s.Chars[i]);
    end
    else
    begin
      result := result.Remove(i, 1);
      result := result.Insert(i, s.Chars[i]);
    end;
  end; // for}
  {$ENDIF}
end;


function EscapeForXML(const value: string; const isAttribute: boolean = True; const isCDATASection : Boolean = False): string;
begin
  result := StripInvalidXML(value);
  {$IFNDEF NEXTGEN}
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
  {$ELSE}
  if isCDATASection  then
  begin
    Result := Result.Replace(']]>', ']>', [rfReplaceAll]);
    exit;
  end;

  //note we are avoiding replacing &amp; with &amp;amp; !!
  Result := Result.Replace('&amp;', '[[-xy-amp--]]',[rfReplaceAll]);
  Result := Result.Replace('&', '&amp;',[rfReplaceAll]);
  Result := Result.Replace('[[-xy-amp--]]', '&amp;amp;',[rfReplaceAll]);
  Result := Result.Replace('<', '&lt;',[rfReplaceAll]);
  Result := Result.Replace('>', '&gt;',[rfReplaceAll]);

  if isAttribute then
  begin
    Result := Result.Replace('''', '&#39;',[rfReplaceAll]);
    Result := Result.Replace('"', '&quot;',[rfReplaceAll]);
  end;
  {$ENDIF}
end;

end.
