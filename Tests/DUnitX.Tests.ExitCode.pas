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

unit DUnitX.Tests.ExitCode;

interface

{$I DUnitX.inc}

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TExitCodeTests = class
  public
    // Simple mode keeps the existing behaviour: EXIT_ERRORS (1) for any failure, error or leak.
    [Test]
    [NamedTestCase('All passed',       '0,0,0,0,False,0')]
    [NamedTestCase('Errors',           '1,0,0,0,False,1')]
    [NamedTestCase('Failures',         '0,1,0,0,False,1')]
    [NamedTestCase('Leaks',            '0,0,1,0,False,1')]
    [NamedTestCase('Everything',       '3,2,1,0,False,1')]
    [NamedTestCase('Ignored',          '0,0,0,5,False,0')]
    [NamedTestCase('Ignored, failing', '0,0,0,5,True,1')]
    procedure Simple(const errors, failures, leaks, ignored : integer; const failOnIgnored : boolean; const expected : integer);

    [Test]
    [NamedTestCase('All passed',       '0,0,0,0,False,0')]
    [NamedTestCase('Errors',           '1,0,0,0,False,2')]
    [NamedTestCase('Failures',         '0,1,0,0,False,4')]
    [NamedTestCase('Leaks',            '0,0,1,0,False,8')]
    [NamedTestCase('Failures, leaks',  '0,2,3,0,False,12')]
    [NamedTestCase('Everything',       '3,2,1,4,True,30')]
    [NamedTestCase('Ignored',          '0,0,0,5,False,0')]
    [NamedTestCase('Ignored, failing', '0,0,0,5,True,16')]
    procedure Bitmask(const errors, failures, leaks, ignored : integer; const failOnIgnored : boolean; const expected : integer);

    [Test]
    procedure Default_Mode_Is_Simple;
  end;

implementation

{ TExitCodeTests }

procedure TExitCodeTests.Simple(const errors, failures, leaks, ignored : integer; const failOnIgnored : boolean;
  const expected : integer);
begin
  Assert.AreEqual(expected, TDUnitX.CalcExitCode(TDUnitXExitMode.Simple, failOnIgnored, errors, failures, leaks, ignored));
end;

procedure TExitCodeTests.Bitmask(const errors, failures, leaks, ignored : integer; const failOnIgnored : boolean;
  const expected : integer);
begin
  Assert.AreEqual(expected, TDUnitX.CalcExitCode(TDUnitXExitMode.Bitmask, failOnIgnored, errors, failures, leaks, ignored));
end;

procedure TExitCodeTests.Default_Mode_Is_Simple;
var
  options : TDUnitXOptions;
begin
  options := TDUnitXOptions.Create;
  try
    Assert.AreEqual(Ord(TDUnitXExitMode.Simple), Ord(options.ExitMode));
    Assert.IsFalse(options.FailOnIgnored);
  finally
    options.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TExitCodeTests);

end.
