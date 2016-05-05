{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2013 Vincent Parrett                              }
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

unit DUnitX.Examples.General;

{$I DUnitX.inc}

//{$IFDEF DELPHI_XE_UP}
//{$STRONGLINKTYPES ON}
//{$ENDIF}

interface

uses
  DUnitX.TestFramework;

type
  {$M+}
  [TestFixture('ExampleFixture1','General Example Tests')]
  TMyExampleTests = class
  public
    //Run the same test with mulitiple parameters.
    //ideally we would like to implement this using
    //[TestCase('Case 1',[1,3,4])]
    //but the delphi compiler will not accept arrays of
    //TValue as parameters to attributes, so we have to use
    //a string constant.. the attribute will split the string
    //and attempt to convert the values to the parameters of the
    //test method.

    [Test]
    [TestCase('Case 1','1,2')]
    [TestCase('Case 2','3,4')]
    [TestCase('Case 3','5,6')]
    procedure TestOne(param1 : integer; param2 : integer);

    [TestCase('Case 3','Blah,1')]
    procedure AnotherTestMethod(const a : string; const b : integer);

    [Test]
    [TestCase('Case4','password="",password=""')]
    procedure TestCaseWithStrings(const AInput : string; const AResult : string);

    [Test]
    procedure TestTwo;

    [Test]
    procedure TestError;

    [Test]
    [MaxTime(2000)]
    procedure TooLong;


    //Disabled test
    [Test(false)]
    procedure DontCallMe;

    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    procedure TestMeAnyway;

    [Test]
    procedure LogMessageTypes;

    [Test]
    [Ignore('Because I said so!!!')]
    procedure IgnoreMePublic;

  published
    //Because this is a published method, it doesn't require the [Test] attribute
    [Ignore('Because he said so!!!')]
    procedure IgnoreMePublished;
  end;

  [TestFixture]
  TExampleFixture2 = class
  private
    FTestsRun : integer;
  public
    [SetupFixture]
    procedure SetupFixture;

    [TeardownFixture]
    procedure TearDownFixture;
  published
    procedure IAmATest;
  end;

  [TestFixture]
  TExampleFixture3 = class
  public
    //will be used as SetupFixture
    constructor Create;
    //will be used as TeardownFixture
    destructor Destroy;override;
  published
    procedure ATest;
  end;

  TExampleFixture4 = class
  protected
    FObject: TObject;
  public
    [SetupFixture]
    procedure SetupFixture;

    [TeardownFixture]
    procedure TearDownFixture;
  end;

  [TestFixture]
  TExampleFixture5 = class(TExampleFixture4)
  public
    [Test]
    procedure Testing;
  end;

implementation

uses
  {$IFDEF USE_NS}
  System.SysUtils,
  System.Classes,
  WinApi.Windows,
  {$ELSE}
  SysUtils,
  Classes,
  Windows,
  {$ENDIF}
  DUnitX.DUnitCompatibility;

{ TMyExampleTests }

procedure TMyExampleTests.DontCallMe;
begin
  TDUnitX.CurrentRunner.Status('DontCallMe called');
  raise Exception.Create('DontCallMe was called!!!!');
end;

procedure TMyExampleTests.IgnoreMePublic;
begin
  TDUnitX.CurrentRunner.Status('IgnoreMePublic called');
  raise Exception.Create('IgnoreMePublic was called when it has IgnoreAttibute !!!!');
end;

procedure TMyExampleTests.IgnoreMePublished;
begin
  TDUnitX.CurrentRunner.Status('IgnoreMePublished called');
  raise Exception.Create('IgnoreMePublished was called when it has IgnoreAttibute !!!!');
end;

procedure TMyExampleTests.LogMessageTypes;
begin
  TDUnitX.CurrentRunner.Log(TLogLevel.Information, 'Information');
  TDUnitX.CurrentRunner.Log(TLogLevel.Warning, 'Warning');
  TDUnitX.CurrentRunner.Log(TLogLevel.Error, 'Error');
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
  TDUnitX.CurrentRunner.Status(Format('AnotherTestMethod called with %s %d',[a,b]));
end;

procedure TMyExampleTests.TestCaseWithStrings(const AInput, AResult: string);
begin
  TDUnitX.CurrentRunner.Status(Format('TestCaseWithStrings called with %s %s',[AInput,AResult]));
end;

procedure TMyExampleTests.TestError;
begin
  raise Exception.Create('Error.');
end;

procedure TMyExampleTests.TestMeAnyway;
begin
  TDUnitX.CurrentRunner.Status('TestMeAnyway called');
end;

procedure TMyExampleTests.TestOne(param1 : integer; param2 : integer);
begin
  TDUnitX.CurrentRunner.Status(Format('TestOnce called with %d %d',[param1,param2]));
end;

procedure TMyExampleTests.TestTwo;
{$IFDEF DELPHI_XE_UP}
var
  x : TStringList;
{$ENDIF}
begin
  TDUnitX.CurrentRunner.Status('TestTwo called');
  TDUnitX.CurrentRunner.Status('hello world');

  //No longer compatible for Delphi2010
{$IFDEF DELPHI_XE_UP}
  x := TStringList.Create;
  Assert.IsType<TObject>(x); /// a bit pointless since it's strongly typed.
  x.Free;
{$ENDIF}
end;

procedure TMyExampleTests.TooLong;
begin
  {$IFDEF DELPHI_XE_UP}
    TThread.Sleep(5000);
  {$ELSE}
    Windows.Sleep(5000);
  {$ENDIF}
end;

{ TExampleFixture2 }

procedure TExampleFixture2.IAmATest;
begin
  Inc(FTestsRun);
end;

procedure TExampleFixture2.SetupFixture;
begin
  FTestsRun := 0;
  TDUnitX.CurrentRunner.Log('Setting up...');
end;

procedure TExampleFixture2.TearDownFixture;
begin
  Assert.AreEqual(FTestsRun, 1);
  TDUnitX.CurrentRunner.Log('Tearing down');
end;

{ TExampleFixture3 }

procedure TExampleFixture3.ATest;
begin
  Assert.IsTrue(true);
end;

constructor TExampleFixture3.Create;
begin
  //Empty
end;

destructor TExampleFixture3.Destroy;
begin
  //Empty
  inherited;
end;

{ TExampleFixture4 }

procedure TExampleFixture4.SetupFixture;
begin
  FObject := TObject.Create;
end;

procedure TExampleFixture4.TearDownFixture;
begin
  FObject.Free;
end;

{ TExampleFixture5 }

procedure TExampleFixture5.Testing;
begin
  Assert.IsNotNull(FObject, 'Problem with inheritance');
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
//{$IFNDEF DELPHI_XE_UP}
  TDUnitX.RegisterTestFixture(TMyExampleTests);
  TDUnitX.RegisterTestFixture(TExampleFixture2);
  TDUnitX.RegisterTestFixture(TExampleFixture3);
  TDUnitX.RegisterTestFixture(TExampleFixture5);
//{$ENDIF}
end.
