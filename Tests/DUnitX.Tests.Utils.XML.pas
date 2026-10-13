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

initialization
  TDUnitX.RegisterTestFixture(TUtilsXMLTests);

end.
