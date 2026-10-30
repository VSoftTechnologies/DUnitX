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

  [TestFixture]
  TEnumTestCaseConversionTests = class
  public
    [Test]
    procedure Unknown_Enum_Name_Errors_The_Test_Case;
  end;

  [TestFixture]
  TTestCaseValueCountTests = class
  public
    [Test]
    procedure TestCase_With_Too_Many_Values_Errors_The_Test_Case;
    [Test]
    procedure Provider_Or_Source_With_More_Values_Than_Parameters_Still_Runs;
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

  //Not a fixture: the test below generates its tests and runs them itself.
  TEnumConversionTestCases = class
  public type
    TConversionTestEnum = (ctOne, ctTwo);
  public
    [Test]
    [TestCase('Known', 'ctTwo')]
    procedure KnownName(const value : TConversionTestEnum);
    [Test]
    [TestCase('LongBoolTrue', 'True')]
    procedure LongBoolTrue(const value : LongBool);
    [Test]
    [TestCase('Unknown', 'ctBogus')]
    procedure UnknownName(const value : TConversionTestEnum);
  end;

  //Not a fixture: the test below generates its tests and runs them itself.
  TSeparatorTestCases = class
  public
    [Test]
    [TestCase('OtherSeparator', '1,000', '|')]
    procedure OtherSeparator(const value : string);
    [Test]
    [TestCase('SplitAtComma', '1,000')]
    procedure SplitAtComma(const value : string);
    [Test]
    [TestCase('NoParameters', '1')]
    procedure NoParameters;
  end;

  //Supplies one case with two values
  TwoValuesSourceAttribute = class(CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  //Not a fixture: the test below generates its tests and runs them itself.  Each method takes fewer values than it is
  //given, which a provider or a source may do.
  TMoreValuesThanParametersCases = class
  public
    [Test]
    [TestCaseProvider('Sampleprovider')]
    procedure FromProvider(const value1 : integer);
    [Test]
    [TwoValuesSource]
    procedure FromSource(const value1 : integer);
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

{ TEnumConversionTestCases }

procedure TEnumConversionTestCases.KnownName(const value : TConversionTestEnum);
begin
  Assert.IsTrue(value = ctTwo, 'ctTwo converts to ctTwo');
end;

procedure TEnumConversionTestCases.LongBoolTrue(const value : LongBool);
begin
  //GetEnumValue returns -1 for True on LongBool, so -1 alone cannot mean "not found"
  Assert.IsTrue(value, 'True converts to True');
end;

procedure TEnumConversionTestCases.UnknownName(const value : TConversionTestEnum);
begin
  Assert.Fail('A case with an unknown enum name must not run');
end;

{ TEnumTestCaseConversionTests }

procedure TEnumTestCaseConversionTests.Unknown_Enum_Name_Errors_The_Test_Case;
var
  fixture : ITestFixture;
  provider : TDUnitXFixtureProvider;
  providerIntf : IFixtureProvider;
  test : ITest;
  testExecute : ITestExecute;
  count : integer;
begin
  fixture := TDUnitXTestFixture.Create('AEnumConversionFixture', '', TEnumConversionTestCases, 'AUnit') as ITestFixture;
  provider := TDUnitXFixtureProvider.Create;
  providerIntf := provider;
  TFixtureProviderAccess(provider).GenerateTests(nil, fixture);

  count := 0;
  for test in fixture.Tests do
  begin
    Inc(count);
    testExecute := test as ITestExecute;
    if test.MethodName = 'UnknownName' then
      Assert.WillRaise(
        procedure
        begin
          testExecute.Execute(nil);
        end, ETestFrameworkException, test.Name + ' runs with an enum value that does not exist')
    else
      Assert.WillNotRaiseAny(
        procedure
        begin
          testExecute.Execute(nil);
        end, test.Name + ' converts its value');
  end;
  Assert.AreEqual(3, count, 'three [TestCase] tests');
end;

{ TSeparatorTestCases }

procedure TSeparatorTestCases.OtherSeparator(const value : string);
begin
  Assert.AreEqual('1,000', value);
end;

procedure TSeparatorTestCases.SplitAtComma(const value : string);
begin
  //Errors before it runs: '1,000' is split at the comma, into two values for one parameter
end;

procedure TSeparatorTestCases.NoParameters;
begin
  //Errors before it runs: one value for no parameters
end;

{ TwoValuesSourceAttribute }

function TwoValuesSourceAttribute.GetCaseInfoArray : TestCaseInfoArray;
begin
  SetLength(Result, 1);
  Result[0].Name := 'TwoValues';
  SetLength(Result[0].Values, 2);
  Result[0].Values[0] := 1;
  Result[0].Values[1] := 2;
end;

{ TMoreValuesThanParametersCases }

procedure TMoreValuesThanParametersCases.FromProvider(const value1 : integer);
begin
  Assert.IsTrue(value1 > 0, 'the first value of the case');
end;

procedure TMoreValuesThanParametersCases.FromSource(const value1 : integer);
begin
  Assert.AreEqual(1, value1, 'the first value of the case');
end;

{ TTestCaseValueCountTests }

procedure TTestCaseValueCountTests.TestCase_With_Too_Many_Values_Errors_The_Test_Case;
var
  fixture : ITestFixture;
  provider : TDUnitXFixtureProvider;
  providerIntf : IFixtureProvider;
  test : ITest;
  testExecute : ITestExecute;
  count : integer;
begin
  fixture := TDUnitXTestFixture.Create('ASeparatorFixture', '', TSeparatorTestCases, 'AUnit') as ITestFixture;
  provider := TDUnitXFixtureProvider.Create;
  providerIntf := provider;
  TFixtureProviderAccess(provider).GenerateTests(nil, fixture);

  count := 0;
  for test in fixture.Tests do
  begin
    Inc(count);
    testExecute := test as ITestExecute;
    if test.MethodName = 'OtherSeparator' then
      Assert.WillNotRaiseAny(
        procedure
        begin
          testExecute.Execute(nil);
        end, test.Name + ' keeps the comma in its value')
    else if test.MethodName = 'SplitAtComma' then
      Assert.WillRaiseWithMessage(
        procedure
        begin
          testExecute.Execute(nil);
        end, ETestFrameworkException, 'Too many values: 2 values for 1 parameter (does a value contain the separator?)',
        test.Name + ' runs with a value it has no parameter for')
    else
      Assert.WillRaiseWithMessage(
        procedure
        begin
          testExecute.Execute(nil);
        end, ETestFrameworkException, 'Too many values: 1 value for 0 parameters (does a value contain the separator?)',
        test.Name + ' runs with a value it has no parameter for');
  end;
  Assert.AreEqual(3, count, 'three [TestCase] tests');
end;

procedure TTestCaseValueCountTests.Provider_Or_Source_With_More_Values_Than_Parameters_Still_Runs;
var
  fixture : ITestFixture;
  provider : TDUnitXFixtureProvider;
  providerIntf : IFixtureProvider;
  test : ITest;
  testExecute : ITestExecute;
  providerCount : integer;
  sourceCount : integer;
begin
  fixture := TDUnitXTestFixture.Create('AMoreValuesFixture', '', TMoreValuesThanParametersCases, 'AUnit') as ITestFixture;
  provider := TDUnitXFixtureProvider.Create;
  providerIntf := provider;
  TFixtureProviderAccess(provider).GenerateTests(nil, fixture);

  providerCount := 0;
  sourceCount := 0;
  for test in fixture.Tests do
  begin
    if test.MethodName = 'FromProvider' then
      Inc(providerCount)
    else
      Inc(sourceCount);
    testExecute := test as ITestExecute;
    Assert.WillNotRaiseAny(
      procedure
      begin
        testExecute.Execute(nil);
      end, test.Name + ' uses the values it has parameters for');
  end;
  Assert.IsTrue(providerCount > 0, 'provider tests');
  Assert.AreEqual(1, sourceCount, 'source tests');
end;

initialization
  TestDataProviderManager.RegisterProvider('Sampleprovider', TSampleProvider);
  TDUnitX.RegisterTestFixture(TestFixtureProviderTest);
  TDUnitX.RegisterTestFixture(TWillRaiseTestCaseTests);
  TDUnitX.RegisterTestFixture(TEnumTestCaseConversionTests);
  TDUnitX.RegisterTestFixture(TTestCaseValueCountTests);
end.

