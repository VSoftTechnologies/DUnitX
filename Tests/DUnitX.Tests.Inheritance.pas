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

  [TestFixture]
  TOverriddenMethodTests = class
  public
    [Test]
    procedure Overrides_Of_Attributed_Methods_Run_Once;
  end;

  //A method's [Category] adds to its fixture's categories, so a filter on the fixture's category reaches it.
  [TestFixture]
  TMethodCategoryTests = class
  public
    [Test]
    procedure A_Method_Category_Adds_To_The_Fixture_Category;
    [Test]
    procedure A_Category_Is_Held_Once;
    [Test]
    procedure Excluding_The_Fixture_Category_Excludes_A_Method_With_Its_Own;
    [Test]
    procedure Including_Either_Category_Selects_A_Method_With_Its_Own;
  end;

implementation

uses
  DUnitX.Extensibility,
  DUnitX.InternalInterfaces,
  DUnitX.TestFixture,
  DUnitX.FixtureProvider,
  DUnitX.Filters,
  DUnitX.CategoryExpression;

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

type
  //Not fixtures: the test below generates their tests and runs them.
  TVirtualBase = class
  public
    [Setup]
    procedure Setup; virtual;
    [Test]
    procedure RunsTheOverride; virtual;
    [Test]
    procedure OverriddenInPublished; virtual;
  end;

  //Overrides the attributed methods without repeating the attributes.
  TVirtualDerived = class(TVirtualBase)
  public
    procedure Setup; override;
    procedure RunsTheOverride; override;
  published
    procedure OverriddenInPublished; override;
  end;

var
  //What the methods of TVirtualBase and TVirtualDerived ran.
  virtualCalls : string;

procedure TVirtualBase.Setup;
begin
  virtualCalls := virtualCalls + 'TVirtualBase.Setup,';
end;

procedure TVirtualBase.RunsTheOverride;
begin
  virtualCalls := virtualCalls + 'TVirtualBase.RunsTheOverride,';
end;

procedure TVirtualBase.OverriddenInPublished;
begin
  virtualCalls := virtualCalls + 'TVirtualBase.OverriddenInPublished,';
end;

procedure TVirtualDerived.Setup;
begin
  virtualCalls := virtualCalls + 'TVirtualDerived.Setup,';
end;

procedure TVirtualDerived.RunsTheOverride;
begin
  virtualCalls := virtualCalls + 'TVirtualDerived.RunsTheOverride,';
end;

procedure TVirtualDerived.OverriddenInPublished;
begin
  virtualCalls := virtualCalls + 'TVirtualDerived.OverriddenInPublished,';
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

{ TOverriddenMethodTests }

//An override that does not repeat the attributes of the method it overrides is what runs, and runs as one test.
procedure TOverriddenMethodTests.Overrides_Of_Attributed_Methods_Run_Once;
var
  fixture : ITestFixture;
  provider : TDUnitXFixtureProvider;
  providerIntf : IFixtureProvider;
  test : ITest;
  testCount : integer;
begin
  fixture := TDUnitXTestFixture.Create(TVirtualDerived.ClassName, '', TVirtualDerived, 'AUnit') as ITestFixture;
  provider := TDUnitXFixtureProvider.Create;
  providerIntf := provider;
  TFixtureProviderAccess(provider).GenerateTests(nil, fixture);

  virtualCalls := '';
  fixture.SetupMethod();
  Assert.AreEqual('TVirtualDerived.Setup,', virtualCalls, 'the Setup that runs');

  testCount := 0;
  for test in fixture.Tests do
  begin
    Inc(testCount);
    virtualCalls := '';
    (test as ITestExecute).Execute(nil);
    Assert.AreEqual('TVirtualDerived.' + test.Name + ',', virtualCalls, 'the code that runs for ' + test.Name);
  end;
  Assert.AreEqual(2, testCount, 'one test for each test method');
end;

type
  //Not a fixture: the tests below generate its tests in a fixture whose category is Integration.
  TCategorisedMethods = class
  public
    [Test]
    [Category('Slow')]
    procedure Slow;
    [Test]
    [Category(' integration ,Slow,, slow')]
    procedure Repeated;
  end;

procedure TCategorisedMethods.Slow;
begin
end;

procedure TCategorisedMethods.Repeated;
begin
end;

//Generates the tests of TCategorisedMethods in a fixture whose category is Integration, and returns the one named AName.
function CategorisedTest(out AFixture : ITestFixture; const AName : string) : ITest;
var
  provider : TDUnitXFixtureProvider;
  providerIntf : IFixtureProvider;
  test : ITest;
begin
  AFixture := TDUnitXTestFixture.Create(TCategorisedMethods.ClassName, 'Integration', TCategorisedMethods, 'AUnit') as ITestFixture;
  provider := TDUnitXFixtureProvider.Create;
  providerIntf := provider;
  TFixtureProviderAccess(provider).GenerateTests(nil, AFixture);
  result := nil;
  for test in AFixture.Tests do
    if test.Name = AName then
      result := test;
  Assert.IsNotNull(result, 'the test ' + AName + ' is generated');
end;

//A test's categories, in order, each followed by a semicolon
function CategoryList(const ATest : ITest) : string;
var
  category : string;
begin
  result := '';
  for category in ATest.Categories do
    result := result + category + ';';
end;

{ TMethodCategoryTests }

procedure TMethodCategoryTests.A_Method_Category_Adds_To_The_Fixture_Category;
var
  fixture : ITestFixture;
begin
  Assert.AreEqual('Integration;Slow;', CategoryList(CategorisedTest(fixture, 'Slow')), false);
end;

//Categories are trimmed and compared without case, as filters compare them, so each is held once, as first written.
procedure TMethodCategoryTests.A_Category_Is_Held_Once;
var
  fixture : ITestFixture;
begin
  Assert.AreEqual('Integration;Slow;', CategoryList(CategorisedTest(fixture, 'Repeated')), false);
end;

procedure TMethodCategoryTests.Excluding_The_Fixture_Category_Excludes_A_Method_With_Its_Own;
var
  fixture : ITestFixture;
  filter : ITestFilter;
begin
  //As --exclude:Integration builds it
  filter := TNotFilter.Create(TCategoryExpression.CreateFilter('Integration'));
  Assert.IsFalse(filter.Match(CategorisedTest(fixture, 'Slow')), 'excluded by its fixture''s category');
end;

procedure TMethodCategoryTests.Including_Either_Category_Selects_A_Method_With_Its_Own;
var
  fixture : ITestFixture;
  test : ITest;
begin
  test := CategorisedTest(fixture, 'Slow');
  Assert.IsTrue(TCategoryExpression.CreateFilter('Integration').Match(test), 'selected by its fixture''s category');
  Assert.IsTrue(TCategoryExpression.CreateFilter('Slow').Match(test), 'selected by its own category');
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
  TDUnitX.RegisterTestFixture(TOverriddenMethodTests);
  TDUnitX.RegisterTestFixture(TMethodCategoryTests);

end.

