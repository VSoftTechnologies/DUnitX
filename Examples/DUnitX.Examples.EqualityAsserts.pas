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

unit DUnitX.Examples.EqualityAsserts;

interface

uses
   DUnitX.TestFramework;

type
   [TestFixture]
   TDUnitXExamplesEqualityAsserts = class
   public
     [Test]
     procedure Assert_AreNotEqual;
   end;

implementation

{ TDUnitXExamplesEqualityAsserts }

procedure TDUnitXExamplesEqualityAsserts.Assert_AreNotEqual;
var
  valueToTest : boolean;
begin
  //String
  Assert.AreNotEqual('Not', 'Equal');

  //TClass
  Assert.AreNotEqual(TObject, TInterfacedObject);

  //Extended with tolerance
  Assert.AreNotEqual(1.81E10, 1.9E10, 0.1E9);

  //Generic
  valueToTest := false;
  Assert.IsFalse(valueToTest = true);
end;

initialization
  TDUnitX.RegisterTestFixture(TDUnitXExamplesEqualityAsserts);
end.
