{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2016 Vincent Parrett                              }
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

unit DUnitX.Examples.AssertFailureCompare;

interface

{$I DUnitX.inc}

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TDUnitXAssertFailureCompare = class
  public
    [Test]
    procedure TestDefaultString;
    [Test]
    procedure TestXMLWithFormat;
    [Test]
    procedure TestXMLWithoutFormat;
    [Test]
    procedure TestCSVWithFormat;
  end;

implementation

uses
  {$IFDEF USE_NS}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  DUnitX.ComparableFormat.Xml,
  DUnitX.ComparableFormat.Csv;

procedure TDUnitXAssertFailureCompare.TestCSVWithFormat;
begin
  Assert.AreEqual('VALUE1,value2,"val, 3",value4,value5', 'VALUE1,value2,"val, 3",VALUE4,value5', TDUnitXComparableFormatCsv, False);
end;

procedure TDUnitXAssertFailureCompare.TestDefaultString;
begin
  Assert.AreEqual('Some fairly long string with a lot of characters and info.', 'Some long string with a lot of characters and info.');
end;

procedure TDUnitXAssertFailureCompare.TestXMLWithFormat;
begin
  Assert.AreEqual('<TestXMLWithFormat><testnode>value1</testnode></TestXMLWithFormat>', '<TestXMLWithFormat><testnode>value2</testnode></TestXMLWithFormat>', TDUnitXComparableFormatXml);
end;

procedure TDUnitXAssertFailureCompare.TestXMLWithoutFormat;
begin
  Assert.AreEqual('<TestXMLWithoutFormat><testnode>value1</testnode></TestXMLWithoutFormat>', '<TestXMLWithoutFormat><testnode>value2</testnode></TestXMLWithoutFormat>');
end;

initialization
  TDUnitX.RegisterTestFixture(TDUnitXAssertFailureCompare);

end.
