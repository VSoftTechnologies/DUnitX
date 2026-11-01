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

  [TestFixture]
  TTestCaseAttributeTests = class
  public
    //[TestCase('1,2,3')] names the case from its values, as Add(1,2,3), and passes them
    [Test]
    procedure TestCase_Names_Each_Case_From_Its_Values;
    //Two [TestCase]s with the same values would otherwise give two tests with the same name
    [Test]
    procedure TestCase_Names_Stay_Unique_When_Values_Repeat;
    //[NamedTestCase('First', '1,2,3')] keeps the name it is given, with the separator and trimming of the old [TestCase]
    [Test]
    procedure NamedTestCase_Keeps_The_Name_It_Is_Given;
    //[TestCaseSource('Sums')] takes its cases from a class function of the fixture
    [Test]
    procedure TestCaseSource_On_The_Fixture_Supplies_The_Cases;
    //[TestCaseSource(TSomeClass, 'Rows')] takes its cases from a class function of another class
    [Test]
    procedure TestCaseSource_On_Another_Class_Supplies_The_Cases;
    //A source that is missing, has the wrong signature, raises or returns nothing errors the test instead of
    //silently giving it no cases
    [Test]
    procedure TestCaseSource_That_Cannot_Be_Used_Errors_The_Test;
    //A source's rows are checked for count and conversion as [TestCase] values are
    [Test]
    procedure TestCaseSource_Values_Are_Checked;
    //The value count and conversion checks still apply to the renamed attributes
    [Test]
    procedure TestCase_And_NamedTestCase_Values_Are_Still_Checked;
    //An old [TestCase('Name', 'values')] still compiles, as values 'Name' with separator 'values', so it errors instead
    [Test]
    procedure Old_Two_Argument_TestCase_Errors_With_Advice;
    //Separators without a letter or digit do not trip that check, and NamedTestCase may use any separator
    [Test]
    procedure Ordinary_Separators_Do_Not_Trip_The_Old_Form_Check;
  end;

implementation

uses
  System.SysUtils,
  System.Rtti,
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

  //Not a fixture: the tests below generate its tests and run them themselves.
  TAutoNamedCases = class
  public
    [Test]
    [TestCase('1,2,3')]
    [TestCase('-5,-2,-7')]
    procedure Add(const a, b, expected : integer);
    [Test]
    [TestCase('1,2')]
    [TestCase('1,2')]
    [TestCase('1,2')]
    procedure Repeated(const a, b : integer);
    [Test]
    [NamedTestCase('First', '1,2,3')]
    [NamedTestCase('Spaced', ' 4 | 5 | 9 ', '|', true)]
    procedure NamedAdd(const a, b, expected : integer);
    [Test]
    [TestCase('abc')]
    procedure NotAnInteger(const value : integer);
    [Test]
    [TestCase('1,2')]
    procedure TooManyValues(const value : integer);
    [Test]
    [TestCase('1')]
    procedure TooFewValues(const a, b : integer);
    [Test]
    [NamedTestCase('Named', 'abc')]
    procedure NamedNotAnInteger(const value : integer);
    [Test]
    [NamedTestCase('Named', '1,2')]
    procedure NamedTooManyValues(const value : integer);
  end;

  //Not a fixture: the tests below generate its tests and run them themselves.
  TSeparatorCheckCases = class
  public
    //Written for the old TestCase, these are now the values 'Name', split at the characters of the old values
    [Test]
    [TestCase('Name', 'Bob')]
    procedure OldOneValue(const value : string);
    [Test]
    [TestCase('Name', '1,2')]
    procedure OldTwoValues(const a, b : integer);
    [Test]
    [TestCase('1;2', ';')]
    procedure Semicolon(const a, b : integer);
    [Test]
    [TestCase('1|2', '|')]
    procedure Bar(const a, b : integer);
    [Test]
    [TestCase('1 2', ' ')]
    procedure Space(const a, b : integer);
    [Test]
    [TestCase('1/2', '/')]
    procedure Slash(const a, b : integer);
    [Test]
    [TestCase('1'#9'2', #9)]
    procedure Tab(const a, b : integer);
    [Test]
    [NamedTestCase('Letter', '1x2', 'x')]
    procedure NamedLetterSeparator(const a, b : integer);
  end;

  //A source of cases for a fixture other than itself
  TExternalCaseSource = class
  public
    class function Rows : TArray<TArray<TValue>>; static;
  end;

  //Not a fixture: the tests below generate its tests and run them themselves.
  TCaseSourceCases = class
  public
    class function Sums : TArray<TArray<Integer>>; static;
    class function Words : TArray<string>; static;
    class function NoRows : TArray<TArray<Integer>>; static;
    class function Raises : TArray<TArray<Integer>>; static;
    class function TakesAParameter(const value : integer) : TArray<TArray<Integer>>; static;
    class function NotAnArray : Integer; static;
    class function ShortRow : TArray<TArray<Integer>>; static;
    class function LongRow : TArray<TArray<Integer>>; static;
    class function BadValue : TArray<TArray<string>>; static;
    class function EmptyRow : TArray<TArray<string>>; static;
    class function Lists : TArray<TArray<string>>; static;
    class function Overloaded(const value : integer) : TArray<Integer>; overload; static;
    class function Overloaded : TArray<Integer>; overload; static;
    function NotAClassMethod : TArray<TArray<Integer>>;

    [Test]
    [TestCaseSource('Sums')]
    procedure Add(const a, b, expected : integer);
    [Test]
    [TestCaseSource('Words')]
    procedure NotEmpty(const value : string);
    [Test]
    [TestCaseSource(TExternalCaseSource, 'Rows')]
    procedure Describe(const number : integer; const name : string; const even : boolean);
    [Test]
    [TestCaseSource('Missing')]
    procedure MissingSource(const value : integer);
    [Test]
    [TestCaseSource(TExternalCaseSource, 'Missing')]
    procedure MissingExternalSource(const value : integer);
    [Test]
    [TestCaseSource('NotAClassMethod')]
    procedure InstanceSource(const value : integer);
    [Test]
    [TestCaseSource('TakesAParameter')]
    procedure SourceWithAParameter(const value : integer);
    [Test]
    [TestCaseSource('NotAnArray')]
    procedure SourceNotAnArray(const value : integer);
    [Test]
    [TestCaseSource('NoRows')]
    procedure EmptySource(const value : integer);
    [Test]
    [TestCaseSource('Raises')]
    procedure RaisingSource(const value : integer);
    [Test]
    [TestCaseSource('ShortRow')]
    procedure AddShortRow(const a, b, expected : integer);
    [Test]
    [TestCaseSource('LongRow')]
    procedure AddLongRow(const a, b, expected : integer);
    [Test]
    [TestCaseSource('BadValue')]
    procedure AddBadValue(const a, b, expected : integer);
    [Test]
    [TestCaseSource('EmptyRow')]
    procedure OneString(const value : string);
    [Test]
    [TestCaseSource('Lists')]
    procedure OneList(const parts : TArray<string>);
    [Test]
    [TestCaseSource('Overloaded')]
    procedure FromOverload(const value : integer);
    [Test]
    [TestCaseSource('Missing')]
    [TestCaseSource('NoRows')]
    procedure TwoBadSources(const value : integer);
  end;

//Generates the tests of a class that is not registered as a fixture, as the runner would
function GenerateCases(const AClass : TClass) : ITestFixture;
var
  provider : TDUnitXFixtureProvider;
  providerIntf : IFixtureProvider;
begin
  Result := TDUnitXTestFixture.Create(AClass.ClassName, '', AClass, 'AUnit') as ITestFixture;
  provider := TDUnitXFixtureProvider.Create;
  providerIntf := provider;
  TFixtureProviderAccess(provider).GenerateTests(nil, Result);
end;

//The names of the tests generated for a method, in the order they were generated, separated by '|'
function TestNames(const AFixture : ITestFixture; const AMethodName : string) : string;
var
  test : ITest;
begin
  Result := '';
  for test in AFixture.Tests do
    if test.MethodName = AMethodName then
    begin
      if Result <> '' then
        Result := Result + '|';
      Result := Result + test.Name;
    end;
end;

function FindTest(const AFixture : ITestFixture; const AName : string) : ITestExecute;
var
  test : ITest;
begin
  for test in AFixture.Tests do
    if test.Name = AName then
      exit(test as ITestExecute);
  Result := nil;
  Assert.Fail('no test named ' + AName + ' among ' + IntToStr(AFixture.Tests.Count) + ' tests');
end;

procedure CheckRuns(const AFixture : ITestFixture; const AName : string);
var
  testExecute : ITestExecute;
begin
  testExecute := FindTest(AFixture, AName);
  Assert.WillNotRaiseAny(
    procedure
    begin
      testExecute.Execute(nil);
    end, AName + ' runs and passes with its values');
end;

procedure CheckErrors(const AFixture : ITestFixture; const AName, AMessage : string);
var
  testExecute : ITestExecute;
begin
  testExecute := FindTest(AFixture, AName);
  Assert.WillRaiseWithMessage(
    procedure
    begin
      testExecute.Execute(nil);
    end, ETestFrameworkException, AMessage, AName + ' errors');
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

{ TAutoNamedCases }

procedure TAutoNamedCases.Add(const a, b, expected : integer);
begin
  Assert.AreEqual(expected, a + b);
end;

procedure TAutoNamedCases.Repeated(const a, b : integer);
begin
  Assert.AreEqual(1, a);
  Assert.AreEqual(2, b);
end;

procedure TAutoNamedCases.NamedAdd(const a, b, expected : integer);
begin
  Assert.AreEqual(expected, a + b);
end;

procedure TAutoNamedCases.NotAnInteger(const value : integer);
begin
end;

procedure TAutoNamedCases.TooManyValues(const value : integer);
begin
end;

procedure TAutoNamedCases.TooFewValues(const a, b : integer);
begin
end;

procedure TAutoNamedCases.NamedNotAnInteger(const value : integer);
begin
end;

procedure TAutoNamedCases.NamedTooManyValues(const value : integer);
begin
end;

{ TSeparatorCheckCases }

procedure TSeparatorCheckCases.OldOneValue(const value : string);
begin
end;

procedure TSeparatorCheckCases.OldTwoValues(const a, b : integer);
begin
end;

procedure TSeparatorCheckCases.Semicolon(const a, b : integer);
begin
  Assert.AreEqual(3, a + b);
end;

procedure TSeparatorCheckCases.Bar(const a, b : integer);
begin
  Assert.AreEqual(3, a + b);
end;

procedure TSeparatorCheckCases.Space(const a, b : integer);
begin
  Assert.AreEqual(3, a + b);
end;

procedure TSeparatorCheckCases.Slash(const a, b : integer);
begin
  Assert.AreEqual(3, a + b);
end;

procedure TSeparatorCheckCases.Tab(const a, b : integer);
begin
  Assert.AreEqual(3, a + b);
end;

procedure TSeparatorCheckCases.NamedLetterSeparator(const a, b : integer);
begin
  Assert.AreEqual(3, a + b);
end;

{ TExternalCaseSource }

class function TExternalCaseSource.Rows : TArray<TArray<TValue>>;
begin
  Result := TArray<TArray<TValue>>.Create(
    TArray<TValue>.Create(2, 'two', True),
    TArray<TValue>.Create(3, 'three', False));
end;

{ TCaseSourceCases }

class function TCaseSourceCases.Sums : TArray<TArray<Integer>>;
begin
  Result := TArray<TArray<Integer>>.Create(
    TArray<Integer>.Create(1, 2, 3),
    TArray<Integer>.Create(-5, -2, -7));
end;

class function TCaseSourceCases.Words : TArray<string>;
begin
  Result := TArray<string>.Create('alpha', 'beta');
end;

class function TCaseSourceCases.NoRows : TArray<TArray<Integer>>;
begin
  Result := nil;
end;

class function TCaseSourceCases.Raises : TArray<TArray<Integer>>;
begin
  raise EAbort.Create('no rows today');
end;

class function TCaseSourceCases.TakesAParameter(const value : integer) : TArray<TArray<Integer>>;
begin
  Result := TArray<TArray<Integer>>.Create(TArray<Integer>.Create(value));
end;

class function TCaseSourceCases.NotAnArray : Integer;
begin
  Result := 1;
end;

class function TCaseSourceCases.ShortRow : TArray<TArray<Integer>>;
begin
  Result := TArray<TArray<Integer>>.Create(TArray<Integer>.Create(1, 2));
end;

class function TCaseSourceCases.LongRow : TArray<TArray<Integer>>;
begin
  Result := TArray<TArray<Integer>>.Create(TArray<Integer>.Create(1, 2, 3, 4));
end;

class function TCaseSourceCases.BadValue : TArray<TArray<string>>;
begin
  Result := TArray<TArray<string>>.Create(TArray<string>.Create('1', 'x', '3'));
end;

class function TCaseSourceCases.EmptyRow : TArray<TArray<string>>;
begin
  Result := TArray<TArray<string>>.Create(nil);
end;

class function TCaseSourceCases.Lists : TArray<TArray<string>>;
begin
  Result := TArray<TArray<string>>.Create(TArray<string>.Create('a', 'b', 'c'));
end;

class function TCaseSourceCases.Overloaded(const value : integer) : TArray<Integer>;
begin
  Result := TArray<Integer>.Create(value);
end;

class function TCaseSourceCases.Overloaded : TArray<Integer>;
begin
  Result := TArray<Integer>.Create(7);
end;

procedure TCaseSourceCases.OneString(const value : string);
begin
end;

procedure TCaseSourceCases.OneList(const parts : TArray<string>);
begin
  Assert.AreEqual(3, Length(parts), 'the row is the one array value');
end;

procedure TCaseSourceCases.FromOverload(const value : integer);
begin
  Assert.AreEqual(7, value);
end;

procedure TCaseSourceCases.TwoBadSources(const value : integer);
begin
end;

function TCaseSourceCases.NotAClassMethod : TArray<TArray<Integer>>;
begin
  Result := TArray<TArray<Integer>>.Create(TArray<Integer>.Create(1));
end;

procedure TCaseSourceCases.Add(const a, b, expected : integer);
begin
  Assert.AreEqual(expected, a + b);
end;

procedure TCaseSourceCases.NotEmpty(const value : string);
begin
  Assert.IsTrue(Length(value) > 3, value);
end;

procedure TCaseSourceCases.Describe(const number : integer; const name : string; const even : boolean);
begin
  Assert.AreEqual(not Odd(number), even, name);
  Assert.IsTrue(name <> '', 'a name');
end;

procedure TCaseSourceCases.MissingSource(const value : integer);
begin
end;

procedure TCaseSourceCases.MissingExternalSource(const value : integer);
begin
end;

procedure TCaseSourceCases.InstanceSource(const value : integer);
begin
end;

procedure TCaseSourceCases.SourceWithAParameter(const value : integer);
begin
end;

procedure TCaseSourceCases.SourceNotAnArray(const value : integer);
begin
end;

procedure TCaseSourceCases.EmptySource(const value : integer);
begin
end;

procedure TCaseSourceCases.RaisingSource(const value : integer);
begin
end;

procedure TCaseSourceCases.AddShortRow(const a, b, expected : integer);
begin
end;

procedure TCaseSourceCases.AddLongRow(const a, b, expected : integer);
begin
end;

procedure TCaseSourceCases.AddBadValue(const a, b, expected : integer);
begin
end;

{ TTestCaseAttributeTests }

procedure TTestCaseAttributeTests.TestCase_Names_Each_Case_From_Its_Values;
var
  fixture : ITestFixture;
begin
  fixture := GenerateCases(TAutoNamedCases);
  Assert.AreEqual('Add(1,2,3)|Add(-5,-2,-7)', TestNames(fixture, 'Add'));
  CheckRuns(fixture, 'Add(1,2,3)');
  CheckRuns(fixture, 'Add(-5,-2,-7)');
end;

procedure TTestCaseAttributeTests.TestCase_Names_Stay_Unique_When_Values_Repeat;
var
  fixture : ITestFixture;
begin
  fixture := GenerateCases(TAutoNamedCases);
  Assert.AreEqual('Repeated(1,2)|Repeated(1,2) #2|Repeated(1,2) #3', TestNames(fixture, 'Repeated'));
  CheckRuns(fixture, 'Repeated(1,2) #3');
end;

procedure TTestCaseAttributeTests.NamedTestCase_Keeps_The_Name_It_Is_Given;
var
  fixture : ITestFixture;
begin
  fixture := GenerateCases(TAutoNamedCases);
  Assert.AreEqual('NamedAdd.First|NamedAdd.Spaced', TestNames(fixture, 'NamedAdd'));
  CheckRuns(fixture, 'NamedAdd.First');
  CheckRuns(fixture, 'NamedAdd.Spaced');
end;

procedure TTestCaseAttributeTests.TestCaseSource_On_The_Fixture_Supplies_The_Cases;
var
  fixture : ITestFixture;
begin
  fixture := GenerateCases(TCaseSourceCases);
  Assert.AreEqual('Add(1, 2, 3)|Add(-5, -2, -7)', TestNames(fixture, 'Add'));
  CheckRuns(fixture, 'Add(1, 2, 3)');
  CheckRuns(fixture, 'Add(-5, -2, -7)');
  //A source of single values gives one value to each case
  Assert.AreEqual('NotEmpty(alpha)|NotEmpty(beta)', TestNames(fixture, 'NotEmpty'));
  CheckRuns(fixture, 'NotEmpty(alpha)');
end;

procedure TTestCaseAttributeTests.TestCaseSource_On_Another_Class_Supplies_The_Cases;
var
  fixture : ITestFixture;
begin
  fixture := GenerateCases(TCaseSourceCases);
  Assert.AreEqual('Describe(2, two, True)|Describe(3, three, False)', TestNames(fixture, 'Describe'));
  CheckRuns(fixture, 'Describe(2, two, True)');
  CheckRuns(fixture, 'Describe(3, three, False)');
end;

procedure TTestCaseAttributeTests.TestCaseSource_That_Cannot_Be_Used_Errors_The_Test;
const
  SIGNATURE = ' must be a class function with no parameters that returns an array';
var
  fixture : ITestFixture;
begin
  fixture := GenerateCases(TCaseSourceCases);
  CheckErrors(fixture, 'MissingSource.Missing', 'TestCaseSource method ''Missing'' not found on TCaseSourceCases (it must be public)');
  CheckErrors(fixture, 'MissingExternalSource.Missing', 'TestCaseSource method ''Missing'' not found on TExternalCaseSource (it must be public)');
  CheckErrors(fixture, 'InstanceSource.NotAClassMethod', 'TestCaseSource method TCaseSourceCases.NotAClassMethod' + SIGNATURE);
  CheckErrors(fixture, 'SourceWithAParameter.TakesAParameter', 'TestCaseSource method TCaseSourceCases.TakesAParameter' + SIGNATURE);
  CheckErrors(fixture, 'SourceNotAnArray.NotAnArray', 'TestCaseSource method TCaseSourceCases.NotAnArray' + SIGNATURE);
  CheckErrors(fixture, 'EmptySource.NoRows', 'TestCaseSource method TCaseSourceCases.NoRows returned no cases');
  CheckErrors(fixture, 'RaisingSource.Raises', 'TestCaseSource method TCaseSourceCases.Raises raised EAbort: no rows today');
  //Each source that cannot be used has a test of its own
  Assert.AreEqual('TwoBadSources.Missing|TwoBadSources.NoRows', TestNames(fixture, 'TwoBadSources'));
  //Of overloads, the one without parameters is the source
  Assert.AreEqual('FromOverload(7)', TestNames(fixture, 'FromOverload'));
  CheckRuns(fixture, 'FromOverload(7)');
end;

procedure TTestCaseAttributeTests.TestCaseSource_Values_Are_Checked;
var
  fixture : ITestFixture;
begin
  fixture := GenerateCases(TCaseSourceCases);
  CheckErrors(fixture, 'AddShortRow(1, 2)', 'Too few values: 2 values for 3 parameters');
  CheckErrors(fixture, 'AddLongRow(1, 2, 3, 4)', 'Too many values: 4 values for 3 parameters');
  CheckErrors(fixture, 'AddBadValue(1, x, 3)', 'Cannot convert ''x'' to Integer for parameter b');
  //An empty row is not one empty string
  CheckErrors(fixture, 'OneString()', 'Too few values: 0 values for 1 parameter');
  //For a method whose one parameter is an array, each row is that array
  Assert.AreEqual(0, Pos('|', TestNames(fixture, 'OneList')), 'one case: ' + TestNames(fixture, 'OneList'));
  CheckRuns(fixture, TestNames(fixture, 'OneList'));
end;

procedure TTestCaseAttributeTests.TestCase_And_NamedTestCase_Values_Are_Still_Checked;
var
  fixture : ITestFixture;
begin
  fixture := GenerateCases(TAutoNamedCases);
  CheckErrors(fixture, 'NotAnInteger(abc)', 'Cannot convert ''abc'' to Integer for parameter value');
  CheckErrors(fixture, 'TooManyValues(1,2)', 'Too many values: 2 values for 1 parameter (does a value contain the separator?)');
  CheckErrors(fixture, 'TooFewValues(1)', 'Too few values: 1 value for 2 parameters');
  CheckErrors(fixture, 'NamedNotAnInteger.Named', 'Cannot convert ''abc'' to Integer for parameter value');
  CheckErrors(fixture, 'NamedTooManyValues.Named', 'Too many values: 2 values for 1 parameter (does a value contain the separator?)');
end;

procedure TTestCaseAttributeTests.Old_Two_Argument_TestCase_Errors_With_Advice;
var
  fixture : ITestFixture;
begin
  fixture := GenerateCases(TSeparatorCheckCases);
  //Without the check this case would run, and pass, with the value 'Name' in place of 'Bob'
  CheckErrors(fixture, 'OldOneValue(Name)', 'TestCase separator ''Bob'' contains a letter or digit: this looks like ' +
    '[TestCase(''Name'', ''Bob'')] written for the old TestCase, which is now [NamedTestCase(''Name'', ''Bob'')]');
  //The check is reported in place of the value count
  CheckErrors(fixture, 'OldTwoValues(Name)', 'TestCase separator ''1,2'' contains a letter or digit: this looks like ' +
    '[TestCase(''Name'', ''1,2'')] written for the old TestCase, which is now [NamedTestCase(''Name'', ''1,2'')]');
end;

procedure TTestCaseAttributeTests.Ordinary_Separators_Do_Not_Trip_The_Old_Form_Check;
var
  fixture : ITestFixture;
begin
  fixture := GenerateCases(TSeparatorCheckCases);
  CheckRuns(fixture, 'Semicolon(1;2)');
  CheckRuns(fixture, 'Bar(1|2)');
  CheckRuns(fixture, 'Space(1 2)');
  CheckRuns(fixture, 'Slash(1/2)');
  CheckRuns(fixture, 'Tab(1'#9'2)');
  CheckRuns(fixture, 'NamedLetterSeparator.Letter');
end;

initialization
  TestDataProviderManager.RegisterProvider('Sampleprovider', TSampleProvider);
  TDUnitX.RegisterTestFixture(TestFixtureProviderTest);
  TDUnitX.RegisterTestFixture(TWillRaiseTestCaseTests);
  TDUnitX.RegisterTestFixture(TEnumTestCaseConversionTests);
  TDUnitX.RegisterTestFixture(TTestCaseValueCountTests);
  TDUnitX.RegisterTestFixture(TTestCaseAttributeTests);
end.

