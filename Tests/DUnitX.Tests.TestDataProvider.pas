unit DUnitX.Tests.TestDataProvider;

interface

uses
  System.Generics.Collections,
  DUnitX.Types,
  DUnitX.InternalDataProvider,
  DUnitX.TestDataProvider,
  DUnitX.TestFramework;

type
  TSampleData = record
    v1, v2 : integer;                   //simple 2 integer values
    AddEx : integer;                    //Expected value for Add
    Equal : boolean;                    //Expected value for Equal-Compare
  end;

  TSampleProvider = class(TTestDataProvider)
  private
    flist : TList<TSampleData>;

    procedure InitTestData;
  public
    constructor Create; override;
    function GetCaseCount(const methodName : string) : Integer; override;
    function GetCaseName(const methodName : string; const caseNumber : integer) : string; override;
    function GetCaseParams(const methodName : string; const caseNumber : integer) : TValuearray; override;
    destructor Destroy; override;
  end;

  [TestFixture]
  TestFixtureProviderTest = class(TObject)
  public
    [Test]
    [TestCaseProvider('Sampleprovider')]
    procedure spTstAdd(const value1, Value2 : integer; expected : integer);
    [Test]
    [TestCaseProvider('Sampleprovider')]
    procedure spTstEqual(const value1, Value2 : integer; expected : boolean);

  end;

  [TestFixture]
  TWillRaiseTestCaseTests = class
  public
    [Test]
    procedure WillRaise_Applies_To_TestCase_And_TestCaseProvider_Tests;
  end;

implementation

uses
  System.SysUtils,
  DUnitX.Exceptions,
  DUnitX.Extensibility,
  DUnitX.InternalInterfaces,
  DUnitX.TestFixture,
  DUnitX.FixtureProvider;

type
  TFixtureProviderAccess = class(TDUnitXFixtureProvider);

  //Not a fixture: the test below generates its tests and runs them itself.
  TWillRaiseTestCases = class
  public
    [Test]
    [TestCase('Zero', '0')]
    [WillRaise(EAbort)]
    procedure CaseDoesNotRaise(const value : integer);
    [Test]
    [TestCaseProvider('Sampleprovider')]
    [WillRaise(EAbort)]
    procedure ProviderCaseDoesNotRaise(const value1, value2 : integer; expected : boolean);
    [Test]
    [TestCase('Zero', '0')]
    [WillRaise(EAbort)]
    procedure CaseRaises(const value : integer);
  end;

{ TSampleProvider }

constructor TSampleProvider.Create;
begin
  inherited;
  flist := TList<TSampleData>.create;
  InitTestData;
end;

destructor TSampleProvider.Destroy;
begin
  flist.free;
  inherited;
end;

function TSampleProvider.GetCaseCount(const methodName : string) : Integer;
begin
  result := flist.count;
end;

function TSampleProvider.GetCaseName(const methodName : string; const caseNumber : integer) : string;
begin
  result := '';
  if (Methodname = 'spTstAdd') then
    result := 'Addtest'
  else
    result := 'Comparetest';

end;

function TSampleProvider.GetCaseParams(const methodName : string; const caseNumber : integer) : TValuearray;
begin
  SetLength(result, 0);
  if (caseNumber >= 0) and (caseNumber < flist.count) then
  begin
    SetLength(result, 3);
    result[0] := flist[caseNumber].v1;
    result[1] := flist[caseNumber].v2;
    if (Methodname = 'spTstAdd') then
      result[2] := flist[caseNumber].AddEx
    else
      result[2] := flist[caseNumber].Equal;
  end;
end;

procedure TSampleProvider.InitTestData;
var
  item : TSampleData;

begin
  item.v1 := 1;
  item.v2 := 1;
  item.AddEx := 2;
  item.Equal := true;
  flist.add(item);
  item.v1 := 1;
  item.v2 := 2;
  item.AddEx := 3;
  item.Equal := false;
  flist.add(item);
  item.v1 := 2;
  item.v2 := 3;
  item.AddEx := 5;
  item.Equal := false;
  flist.add(item);
  item.v1 := 2;
  item.v2 := 2;
  item.AddEx := 4;
  item.Equal := true;
  flist.add(item);
  item.v1 := 3;
  item.v2 := 3;
  item.AddEx := 6;
  item.Equal := true;
  flist.add(item);
end;

{ TestFixtureProviderTest }

procedure TestFixtureProviderTest.spTstAdd(const value1, Value2 : integer;
  expected : integer);
begin
  Assert.AreEqual(expected, (value1 + value2), 'Ok');
end;

procedure TestFixtureProviderTest.spTstEqual(const value1, Value2 : integer;
  expected : boolean);
begin
  Assert.AreEqual(expected, (value1 = value2), 'Ok');
end;

{ TWillRaiseTestCases }

procedure TWillRaiseTestCases.CaseDoesNotRaise(const value : integer);
begin
end;

procedure TWillRaiseTestCases.ProviderCaseDoesNotRaise(const value1, value2 : integer; expected : boolean);
begin
end;

procedure TWillRaiseTestCases.CaseRaises(const value : integer);
begin
  raise EAbort.Create('expected');
end;

{ TWillRaiseTestCaseTests }

procedure TWillRaiseTestCaseTests.WillRaise_Applies_To_TestCase_And_TestCaseProvider_Tests;
var
  fixture : ITestFixture;
  provider : TDUnitXFixtureProvider;
  providerIntf : IFixtureProvider;
  test : ITest;
  testExecute : ITestExecute;
  count : integer;
begin
  fixture := TDUnitXTestFixture.Create('AWillRaiseTestCaseFixture', '', TWillRaiseTestCases, 'AUnit') as ITestFixture;
  provider := TDUnitXFixtureProvider.Create;
  providerIntf := provider;
  TFixtureProviderAccess(provider).GenerateTests(nil, fixture);

  count := 0;
  for test in fixture.Tests do
  begin
    Inc(count);
    testExecute := test as ITestExecute;
    if test.MethodName = 'CaseRaises' then
      Assert.WillNotRaiseAny(
        procedure
        begin
          testExecute.Execute(nil);
        end, test.Name + ' raises the expected exception')
    else
      Assert.WillRaise(
        procedure
        begin
          testExecute.Execute(nil);
        end, ETestFailure, test.Name + ' does not raise the expected exception, so it must fail');
  end;
  Assert.AreEqual(7, count, 'one [TestCase] test, five provider tests and one [TestCase] test');
end;

initialization
  TestDataProviderManager.RegisterProvider('Sampleprovider', TSampleProvider);
  TDUnitX.RegisterTestFixture(TestFixtureProviderTest);
  TDUnitX.RegisterTestFixture(TWillRaiseTestCaseTests);
end.

