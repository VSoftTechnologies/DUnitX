{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2017 Vincent Parrett                              }
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

unit DUnitX.Tests.Inheritance;

interface

{$I ..\Source\DUnitX.inc}

uses
  DUnitX.TestFramework;

type
{$M+}
  TMyBaseTestClass = class
  private
    FBlah : string;
  protected
    function GetSomething : string; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    [TearDownFixture]
    procedure TearDownFixture;

    [Test]
    procedure TestInBase;
  end;

  TMyDerivedTest = class(TMyBaseTestClass)
  protected
    function GetSomething : string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  [TestFixture]
  TRepeatTestSetupTests = class
  public
    [Test]
    procedure RepeatTest_Gets_The_Setup_And_TearDown_Of_A_Base_Class;
    [Test]
    procedure RepeatTest_Gets_The_Setup_And_TearDown_Declared_After_It;
  end;

implementation

uses
  DUnitX.Extensibility,
  DUnitX.TestFixture,
  DUnitX.FixtureProvider;

type
  TFixtureProviderAccess = class(TDUnitXFixtureProvider);

  //Not fixtures: the tests below generate their tests and inspect them.
  TRepeatSetupBase = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  end;

  TRepeatWithBaseSetup = class(TRepeatSetupBase)
  public
    [Test]
    [RepeatTest(2)]
    procedure Repeated;
  end;

  TRepeatWithLaterSetup = class
  public
    [Test]
    [RepeatTest(2)]
    procedure Repeated;
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  end;

procedure TRepeatSetupBase.Setup;
begin
end;

procedure TRepeatSetupBase.TearDown;
begin
end;

procedure TRepeatWithBaseSetup.Repeated;
begin
end;

procedure TRepeatWithLaterSetup.Repeated;
begin
end;

procedure TRepeatWithLaterSetup.Setup;
begin
end;

procedure TRepeatWithLaterSetup.TearDown;
begin
end;

//The repetitions of a [RepeatTest] method are the tests of a child fixture, which must run the fixture's Setup and TearDown.
procedure CheckRepeatChildHasSetupAndTearDown(const AClass : TClass);
var
  fixture : ITestFixture;
  provider : TDUnitXFixtureProvider;
  providerIntf : IFixtureProvider;
  child : ITestFixture;
  childCount : integer;
begin
  fixture := TDUnitXTestFixture.Create(AClass.ClassName, '', AClass, 'AUnit') as ITestFixture;
  provider := TDUnitXFixtureProvider.Create;
  providerIntf := provider;
  TFixtureProviderAccess(provider).GenerateTests(nil, fixture);

  childCount := 0;
  for child in fixture.Children do
  begin
    Inc(childCount);
    Assert.IsTrue(child.HasTests, 'the child fixture holds the repetitions');
    Assert.AreEqual('Setup', child.SetupMethodName, 'the repetitions have the fixture''s Setup');
    Assert.IsTrue(Assigned(child.SetupMethod), 'the repetitions have the fixture''s Setup');
    Assert.AreEqual('TearDown', child.TearDownMethodName, 'the repetitions have the fixture''s TearDown');
    Assert.IsTrue(Assigned(child.TearDownMethod), 'the repetitions have the fixture''s TearDown');
  end;
  Assert.AreEqual(1, childCount, 'the repetitions are one child fixture');
end;

{ TRepeatTestSetupTests }

procedure TRepeatTestSetupTests.RepeatTest_Gets_The_Setup_And_TearDown_Of_A_Base_Class;
begin
  CheckRepeatChildHasSetupAndTearDown(TRepeatWithBaseSetup);
end;

procedure TRepeatTestSetupTests.RepeatTest_Gets_The_Setup_And_TearDown_Declared_After_It;
begin
  CheckRepeatChildHasSetupAndTearDown(TRepeatWithLaterSetup);
end;

constructor TMyBaseTestClass.Create;
begin
  FBlah := 'base';
end;

destructor TMyBaseTestClass.Destroy;
begin
  //Empty
  inherited;
end;

function TMyBaseTestClass.GetSomething : string;
begin
  Result := 'base';
end;

procedure TMyBaseTestClass.TearDownFixture;
begin
  FBlah := '';
end;

procedure TMyBaseTestClass.TestInBase;
begin
  Writeln(GetSomething);
  Assert.Pass;
end;

constructor TMyDerivedTest.Create;
begin
  inherited;
  FBlah := 'derived';
end;

destructor TMyDerivedTest.Destroy;
begin
  //Empty
  inherited;
end;

function TMyDerivedTest.GetSomething : string;
begin
  Result := 'derived';
  Assert.Pass;
end;

initialization
  TDUnitX.RegisterTestFixture(TMyDerivedTest);
  TDUnitX.RegisterTestFixture(TRepeatTestSetupTests);

end.

