unit ProviderExample;

interface
uses
  System.Generics.Collections,
  DUnitX.Types,
  DUnitX.TestDataProvider,
  DUnitX.TestFramework;

type
  //Just a record for doing a List of Data
  TTestData = Record
                Val1,Val2 : integer;
                AddExpect : integer;
                isEqual   : boolean;
  End;

  TSampleProvider = Class(TTestDataProviderBase)
    private
      flist : TList<TTestData>;  //Our list of Values we need;
    public
      //The constructor, for initializing the data
      Constructor Create;override;
      //Get the amount of cases, we want to create;
      function GetCaseAmount(Methodname:string):integer;override;
      //Get the name of the cases, depending on the Test-Function
      function GetCaseName(Methodname:string):String;override;
      //Get the Params for calling the Test-Function;Be aware of the order !
      function GetCaseParams(Methodname:string;casenr:integer):TValuearray;override;
      //Cleanup the instance
      Destructor Destroy;override;
  End;

  [TestFixture('ProviderExample1','Example using TestCaseProviders')]
  TProviderExample = class(TObject)
  public
    [Test]
    [TestCaseProvider('Demoprovider')]
    Procedure Addtest(const v1,v2:integer;expected:integer);
    [Test]
    [TestCaseProvider('Demoprovider')]
    [TestCase('Add normal','1,1,2,true')]
    Procedure Comparetest(const v1,v2:integer;expected:boolean);
  end;

implementation


{ TSampleProvider }

constructor TSampleProvider.Create;
var
  i : integer;
  item : TTestData;

begin
  inherited;
  flist := TList<TTestData>.create;
  //Just generate some Data for the tests
  Randomize();
  for i := 0 to 100 do
  begin
    item.Val1 := Random(100);
    item.Val2 := Random(100);
    item.AddExpect := item.Val1+Item.Val2;
    item.isEqual := item.Val1 = item.Val2;
    flist.Add(item);
  end;
end;

destructor TSampleProvider.Destroy;
begin
  flist.Free;
  inherited;
end;

function TSampleProvider.GetCaseAmount(Methodname: string): integer;
begin
  result := flist.Count;
end;

function TSampleProvider.GetCaseName(Methodname: string): String;
begin
  result := Methodname;
  if (Methodname = 'Addtest') then
    result := 'Add integer';
  if (Methodname = 'Comparetest') then
    result := 'Compare Integer';
end;

function TSampleProvider.GetCaseParams(Methodname: string;
  casenr: integer): TValuearray;
begin
  SetLength(result,0);
  if (casenr >= 0) and (casenr < flist.Count) then
  begin
    SetLength(result,3);
    result[0] := flist[casenr].Val1;
    result[1] := flist[casenr].Val2;
    if (Methodname = 'Addtest') then
      result[2] := flist[casenr].AddExpect;
    if (Methodname = 'Comparetest') then
      result[2] := flist[casenr].isEqual;
  end;
end;

{ TProviderExample }

procedure TProviderExample.Addtest(const v1, v2: integer; expected: integer);
begin
  Assert.AreEqual(expected,(v1+v2),'Ok');
end;

procedure TProviderExample.Comparetest(const v1, v2: integer;
  expected: boolean);
begin
  Assert.AreEqual(expected,(v1=v2),'Ok');
end;

initialization
  TestDataProviderManager.RegisterProvider('Demoprovider',TSampleProvider);
  TDUnitX.RegisterTestFixture(TProviderExample);
end.
