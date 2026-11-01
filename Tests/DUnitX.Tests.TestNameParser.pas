unit DUnitX.Tests.TestNameParser;

interface

uses
  DUnitX.TestFrameWork;

type
  [TestFixture]
  TTestNameParserTests = class
  public
    [NamedTestCase('SingleName1', 'Test.Namespace.Fixture.Method')]
    [NamedTestCase('SingleName2', 'Test.Namespace.Fixture.Method,', '|')]
    [NamedTestCase('SingleName3', '  Test.Namespace.Fixture.Method  ')]
    [NamedTestCase('SingleName4', '  Test.Namespace.Fixture.Method  ,', '|')]
    [NamedTestCase('SingleName5', 'Test.Namespace.Fixture.Method()')]
    [NamedTestCase('SingleName6', 'Test.Namespace.Fixture.Method(''string,argument'')', '|')]
    [NamedTestCase('SingleName7', 'Test.Namespace.Fixture.Method(1,2,3)', '|')]
    [NamedTestCase('SingleName8', 'Test.Namespace.Fixture.Method<int,int>()', '|')]
    [NamedTestCase('SingleName9', 'Test.Namespace.Fixture.Method(")")')]
    [NamedTestCase('SingleName10', 'A')]
    procedure SingleName(const name : string);

    [NamedTestCase('TwoNames1', 'Test.Namespace.Fixture.Method1|Test.Namespace.Fixture.Method2', '|')]
    [NamedTestCase('TwoNames2', 'Test.Namespace.Fixture.Method1|Test.Namespace.Fixture.Method2,', '|')]
    [NamedTestCase('TwoNames3', 'Test.Namespace.Fixture.Method1(1,2)|Test.Namespace.Fixture.Method2(3,4)', '|')]
    [NamedTestCase('TwoNames4', 'Test.Namespace.Fixture.Method1("(")|Test.Namespace.Fixture.Method2("<")', '|')]
    [NamedTestCase('TwoNames5', 'Ab|C', '|')]
    procedure TwoNames(const name1 : string; const name2 : string);
  end;

implementation

uses
{$IFDEF USE_NS}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  DUnitX.TestNameParser;

// As NUnit's tests: name.Trim(' ', ',')
function TrimSeparators(const name : string) : string;
var
  first, last : integer;
begin
  first := 1;
  last := Length(name);
  while (first <= last) and ((name[first] = ' ') or (name[first] = ',')) do
    Inc(first);
  while (last >= first) and ((name[last] = ' ') or (name[last] = ',')) do
    Dec(last);
  result := Copy(name, first, last - first + 1);
end;

{ TTestNameParserTests }

procedure TTestNameParserTests.SingleName(const name : string);
var
  names : TArray<string>;
begin
  names := TTestNameParser.Parse(name);
  Assert.AreEqual(1, Integer(Length(names)));
  Assert.AreEqual(TrimSeparators(name), names[0]);
end;

procedure TTestNameParserTests.TwoNames(const name1, name2 : string);
var
  names : TArray<string>;
begin
  names := TTestNameParser.Parse(name1 + ',' + name2);
  Assert.AreEqual(2, Integer(Length(names)));
  Assert.AreEqual(TrimSeparators(name1), names[0]);
  Assert.AreEqual(TrimSeparators(name2), names[1]);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestNameParserTests);

end.

