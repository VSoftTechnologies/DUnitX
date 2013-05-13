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

unit DUnitX.Tests.Example;

interface

uses
  DUnitX.TestFramework;


type
  {$M+}
  [TestFixture('ExampleFixture1')]
  TMyExampleTests = class
  public
    [Test]
    //Run the same test with mulitiple parameters.
    //ideally we would like to implement this using
    //[TestCase('Case 1',[1,3,4])]
    //but the delphi compiler will not accept arrays of
    //TValue as parameters to attributes, so we have to use
    //a string constant.. the attribute will split the string
    //and attempt to convert the values to the parameters of the
    //test method.

    [TestCase('Case 1','1,2')]
    [TestCase('Case 2','3,4')]
    [TestCase('Case 3','5,6')]
    procedure TestOne(param1 : integer; param2 : integer);

    [TestCase('Case 3','Blah,1')]
    procedure AnotherTestMethod(const a : string; const b : integer);

    [Test]
    procedure TestTwo;

    //Disabled test
    [Test(false)]
    procedure DontCallMe;

    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;
  //published
    procedure TestMeAnyway;
  end;

  [TestFixture]
  TExampleFixture2 = class
    [SetupFixture]
    procedure SetupFixture;

    [TeardownFixture]
    procedure TearDownFixture;

  published
    procedure IAmATest;
  end;

implementation

uses
  SysUtils,
  classes,
  DUnitX.DUnitCompatibility;

{ TMyExampleTests }


procedure TMyExampleTests.DontCallMe;
begin
  TDUnitX.CurrentRunner.Status('DontCallMe called');
  raise Exception.Create('DontCallMe was called!!!!');
end;

procedure TMyExampleTests.Setup;
begin
  TDUnitX.CurrentRunner.Status('Setup called');
end;

procedure TMyExampleTests.TearDown;
begin
  TDUnitX.CurrentRunner.Status('TearDown called');
end;

procedure TMyExampleTests.AnotherTestMethod(const a: string; const b: integer);
begin
  TDUnitX.CurrentRunner.Status(Format('TestCaseBlah called with %s %d',[a,b]));
end;

procedure TMyExampleTests.TestMeAnyway;
begin
  TDUnitX.CurrentRunner.Status('TestMeAnyway called');
//  raise ENotImplemented.Create('I aint done');
end;

procedure TMyExampleTests.TestOne(param1 : integer; param2 : integer);
begin
  TDUnitX.CurrentRunner.Status(Format('TestOnce called with %d %d',[param1,param2]));
end;


procedure TMyExampleTests.TestTwo;
var
  x : TMyExampleTests;
begin
  TDUnitX.CurrentRunner.Status('TestTwo called');
  x := TMyExampleTests.Create;
  //CheckIs(x,TObject); //DUnit compatibility.
  TDUnitX.CurrentRunner.Status('hello world');
  Assert.IsTrue(x is TObject); /// a bit pointless since it's strongly typed.
end;

{ TExampleFixture2 }

procedure TExampleFixture2.IAmATest;
begin
  raise ENotImplemented.Create('Not implemented');
end;

procedure TExampleFixture2.SetupFixture;
begin
  TDUnitX.CurrentRunner.Log('Setting up...');
end;

procedure TExampleFixture2.TearDownFixture;
begin
  TDUnitX.CurrentRunner.Log('Tearing down');
end;

initialization
//I was hoping to use RTTI to discover the TestFixture classes, however unlike .NET
//if we don't touch the class somehow then the linker will remove
//the class from the resulting exe.
//We could just do this:
//TMyExampleTests.ClassName;
//TExampleFixture2.ClassName;
//which is enough to make the compiler link the classes into the exe, but that seems a
//bit redundent so I guess we'll just use manual registration. If you use the
//{$STRONGLINKTYPES ON} compiler directive then it will link the TestFixtures in and you
//can use RTTI. The downside to that is the resulting exe will potentially much larger.
//Not sure which version {$STRONGLINKTYPES ON} was introduced so we'll allow RTTI and
//manual registration for now.

//Register the test fixtures
  //TDUnitX.RegisterTestFixture(TMyExampleTests);
  //TDUnitX.RegisterTestFixture(TExampleFixture2);

end.
