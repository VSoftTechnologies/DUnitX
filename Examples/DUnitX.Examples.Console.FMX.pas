{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2015 Vincent Parrett & Contributors               }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
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

unit DUnitX.Examples.Console.FMX;

interface

{$I DUnitX.inc}

uses
  DUnitX.TestFramework;

type
  [TestFixture('Examples.Console.FMX', 'Tiny sample for the FMX pseudo-console')]
  TConsoleFMXExampleTests = class
  public
    [Test]
    procedure PassingAssertion;

    [Test]
    procedure AnotherPassingAssertion;

    [Test]
    [Ignore('Shows ignored output in the pseudo-console')]
    procedure IgnoredSample;
  end;

implementation

procedure TConsoleFMXExampleTests.PassingAssertion;
begin
  Assert.AreEqual(4, 2 + 2);
end;

procedure TConsoleFMXExampleTests.AnotherPassingAssertion;
begin
  Assert.IsTrue(True, 'FMX pseudo-console logger is alive');
end;

procedure TConsoleFMXExampleTests.IgnoredSample;
begin
  Assert.IsTrue(False, 'Should never run');
end;

initialization
  TDUnitX.RegisterTestFixture(TConsoleFMXExampleTests);

end.
