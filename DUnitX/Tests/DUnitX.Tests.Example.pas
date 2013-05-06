unit DUnitX.Tests.Example;

interface

uses
  DUnitX.TestFramework;


type
  {$M+}
  [TestFixture('ExampleFixture1')]
  TMyExampleTests = class
  public
    //TestCase source functions
    function DataSet1 : TValueArray;
    function DataSet2 : TValueArray;
    function DataSet3 : TValueArray;
    //trying to use this as a testcase source will fail
    function Invalid : string;

    [Test]
    //Run the same test with mulitiple parameters.
    //ideally we would like to implement this using
    //[TestCase([1,3,4])]
    //but the delpi compiler won't allow this.
    //so we use the attribute to describe which function will
    //supply the parameters. Note that the function must return
    //a TValueArray, and the array values must match the parameters
    //of the test method.
    [TestCaseSource('Case 1','DataSet1')]
    [TestCaseSource('Case 2','DataSet2')]
    [TestCaseSource('Case 3','DataSet4')]
    procedure TestOne(param1 : integer; param2 : integer);

    [Test]
    procedure TestTwo;

    //Disabled test
    [Test(false)]
    procedure DontCallMe;

    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;
  //published
    procedure TestMeAnyway;
  end;

  [TestFixture]
  TExampleFixture2 = class
    [SetupFixture]
    procedure SetupFixture;

    [TeardownFixture]
    procedure TearDownFixture;

  published
    procedure IAmATest;
  end;

implementation

uses
  SysUtils,
  classes,
  DUnitX.DUnitCompatibility;

{ TMyExampleTests }

function TMyExampleTests.DataSet1: TValueArray;
begin
  SetLength(result,2);
  result[0] := 1;
  result[1] := 2;
end;

function TMyExampleTests.DataSet2: TValueArray;
begin
  SetLength(result,2);
  result[0] := 100;
  result[1] := 200;
end;

function TMyExampleTests.DataSet3: TValueArray;
begin
  SetLength(result,2);
  result[0] := 5000;
  result[1] := 6000;
end;


procedure TMyExampleTests.DontCallMe;
begin
  Status('DontCallMe called');
  raise Exception.Create('DontCallMe was called!!!!');
end;

function TMyExampleTests.Invalid: string;
begin
  result :=  'sdfsdf';
end;

procedure TMyExampleTests.Setup;
begin
  Status('Setup called');
end;

procedure TMyExampleTests.TearDown;
begin
  Status('TearDown called');
end;

procedure TMyExampleTests.TestMeAnyway;
begin
  Status('TestMeAnyway called');
//  raise ENotImplemented.Create('I aint done');
end;

procedure TMyExampleTests.TestOne(param1 : integer; param2 : integer);
begin
  Status(Format('TestOnce called with %d %d',[param1,param2]));
end;


procedure TMyExampleTests.TestTwo;
var
  x : TMyExampleTests;
begin
  Status('TestTwo called');
  x := TMyExampleTests.Create;
  //CheckIs(x,TObject); //DUnit compatibility.
  Status('hello world');
//  Assert.IsType<TObject>(x); /// a bit pointless since it's strongly typed.
end;

{ TExampleFixture2 }

procedure TExampleFixture2.IAmATest;
begin
  raise ENotImplemented.Create('Not implemented');
end;

procedure TExampleFixture2.SetupFixture;
begin
  Log('Setting up...');
end;

procedure TExampleFixture2.TearDownFixture;
begin
  Log('Tearing down');
end;

initialization
//I was hoping to use RTTI to discover the TestFixture classes, however unlike .NET
//if we don't touch the class somehow then the linker will remove
//the class from the resulting exe.
//We could just do this:
TMyExampleTests.ClassName;
TExampleFixture2.ClassName;
//which is enough to make the compiler link the classes into the exe, but that seems a
//bit redundent so I guess we'll just use manual registration. If you use the
//{$STRONGLINKTYPES ON} compiler directive then it will link the TestFixtures in and you
//can use RTTI. The downside to that is the resulting exe will potentially much larger.
//Not sure which version {$STRONGLINKTYPES ON} was introduced so we'll allow RTTI and
//manual registration for now.

//Register the test fixtures
  TDUnitX.RegisterTestFixture(TMyExampleTests);
  TDUnitX.RegisterTestFixture(TExampleFixture2);

end.
