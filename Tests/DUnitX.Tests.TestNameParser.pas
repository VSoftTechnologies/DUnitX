unit DUnitX.Tests.TestNameParser;

interface

uses
  DUnitX.TestFrameWork;

type
  [TestFixture]
  TTestNameParserTests = class
  public
    [TestCase('SingleName1', 'Test.Namespace.Fixture.Method')]
    [TestCase('SingleName2', 'Test.Namespace.Fixture.Method,', '|')]
    [TestCase('SingleName3', '  Test.Namespace.Fixture.Method  ')]
    [TestCase('SingleName4', '  Test.Namespace.Fixture.Method  ,', '|')]
    [TestCase('SingleName5', 'Test.Namespace.Fixture.Method()')]
    [TestCase('SingleName6', 'Test.Namespace.Fixture.Method(''string,argument'')')]
    [TestCase('SingleName7', 'Test.Namespace.Fixture.Method(1,2,3)')]
    [TestCase('SingleName8', 'Test.Namespace.Fixture.Method<int,int>()')]
    [TestCase('SingleName9', 'Test.Namespace.Fixture.Method(")")')]
    [TestCase('SingleName10', 'A')]
    procedure SingleName(const name : string);

    [TestCase('TwoNames1', 'Test.Namespace.Fixture.Method1|Test.Namespace.Fixture.Method2', '|')]
    [TestCase('TwoNames2', 'Test.Namespace.Fixture.Method1|Test.Namespace.Fixture.Method2,', '|')]
    [TestCase('TwoNames3', 'Test.Namespace.Fixture.Method1(1,2)|Test.Namespace.Fixture.Method2(3,4)', '|')]
    [TestCase('TwoNames4', 'Test.Namespace.Fixture.Method1("(")|Test.Namespace.Fixture.Method2("<")', '|')]
    [TestCase('TwoNames5', 'Ab|C', '|')]
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

