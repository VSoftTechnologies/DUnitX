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

unit DUnitX.Tests.Assert;


interface

uses
  DUnitX.TestFramework;


type
  {$M+}
  [TestFixture('Testing the Assert Class')]
  TTestAssert = class
  published
    procedure Test_Assert_AreEqual_Double;
    procedure Test_Assert_AreEqual_String;
  end;

implementation

uses
  SysUtils;

{ TTestAssert }

procedure TTestAssert.Test_Assert_AreEqual_Double;
var
  expected,
  actual,tolerance : Extended;
begin
  expected := 1.1;
  actual := 1.1;
  tolerance := 0;
  //should pass
  Assert.AreEqual(expected,actual,tolerance);
end;

procedure TTestAssert.Test_Assert_AreEqual_String;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TTestAssert);
end.
