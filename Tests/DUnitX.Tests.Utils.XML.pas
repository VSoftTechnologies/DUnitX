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

unit DUnitX.Tests.Utils.XML;

interface

{$I ..\Source\DUnitX.inc}

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TUtilsXMLTests = class
  public
    [Test]
    procedure Form_Feed_Is_Not_A_Valid_XML_Char;
    [Test]
    procedure CDATA_Escape_Reads_Back_As_The_Original_Text;
  end;

implementation

uses
  DUnitX.Utils.XML;

procedure TUtilsXMLTests.Form_Feed_Is_Not_A_Valid_XML_Char;
begin
  Assert.IsTrue(IsValidXMLChar(#$09), 'tab');
  Assert.IsFalse(IsValidXMLChar(#$0C), 'form feed');
  Assert.AreEqual('a b', StripInvalidXML('a'#$0C'b'));
end;

//The text of every CDATA section in xml, joined, as an XML parser reads it.
function ReadCDATASections(const xml : string) : string;
const
  CDATA_OPEN = '<![CDATA[';
  CDATA_CLOSE = ']]>';
var
  rest : string;
  p : Integer;
begin
  Result := '';
  rest := xml;
  p := Pos(CDATA_OPEN, rest);
  while p > 0 do
  begin
    Delete(rest, 1, p + Length(CDATA_OPEN) - 1);
    p := Pos(CDATA_CLOSE, rest);
    if p = 0 then
      Break;
    Result := Result + Copy(rest, 1, p - 1);
    Delete(rest, 1, p + Length(CDATA_CLOSE) - 1);
    p := Pos(CDATA_OPEN, rest);
  end;
end;

function CDATARoundTrip(const text : string) : string;
begin
  Result := ReadCDATASections('<![CDATA[' + EscapeForXML(text, False, True) + ']]>');
end;

procedure TUtilsXMLTests.CDATA_Escape_Reads_Back_As_The_Original_Text;
begin
  Assert.AreEqual('a]]>b', CDATARoundTrip('a]]>b'), False, 'a]]>b');
  Assert.AreEqual('x]]]>y', CDATARoundTrip('x]]]>y'), False, 'x]]]>y');
  Assert.AreEqual(']]>]]>', CDATARoundTrip(']]>]]>'), False, ']]>]]>');
  Assert.AreEqual('plain', CDATARoundTrip('plain'), False, 'plain');
end;

initialization
  TDUnitX.RegisterTestFixture(TUtilsXMLTests);

end.
