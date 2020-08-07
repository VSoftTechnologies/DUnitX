unit DUnitX.Tests.TestDataProvider;

interface
uses
  System.Generics.Collections,
  DUnitX.Types,
  DUnitX.InternalDataProvider,
  DUnitX.TestDataProvider,
  DUnitX.TestFramework;


type
  TSampleData = Record
                  v1,v2 : integer; //simple 2 integer values
                  AddEx : integer; //Expected value for Add
                  Equal : boolean; //Expected value for Equal-Compare
  End;

  TSampleProvider = Class(TTestDataProvider)
    private
       flist : TList<TSampleData>;

       Procedure InitTestData;
    public
      Constructor Create;Override;
      function GetCaseCount(const methodName : string) : Integer; override;
      function GetCaseName(const methodName : string; const caseNumber : integer) : string; override;
      function GetCaseParams(const methodName : string ; const caseNumber : integer) : TValuearray; override;
      Destructor Destroy;override;
  End;


  [TestFixture]
  TestFixtureProviderTest = class(TObject)
  public
    [Test]
    [TestCaseProvider('Sampleprovider')]
    Procedure spTstAdd(const value1,Value2:integer;expected:integer);
    [Test]
    [TestCaseProvider('Sampleprovider')]
    Procedure spTstEqual(const value1,Value2:integer;expected:boolean);

  end;

implementation


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

function TSampleProvider.GetCaseParams(const methodName : string ; const caseNumber : integer) : TValuearray;
begin
  SetLength(result,0);
  if (caseNumber >=0) and (caseNumber < flist.count) then
  begin
    SetLength(result,3);
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

procedure TestFixtureProviderTest.spTstAdd(const value1, Value2: integer;
  expected: integer);
begin
  Assert.AreEqual(expected,(value1+value2),'Ok');
end;

procedure TestFixtureProviderTest.spTstEqual(const value1, Value2: integer;
  expected: boolean);
begin
  Assert.AreEqual(expected,(value1=value2),'Ok');
end;

initialization
  TestDataProviderManager.RegisterProvider('Sampleprovider',TSampleProvider);
  TDUnitX.RegisterTestFixture(TestFixtureProviderTest);
end.
