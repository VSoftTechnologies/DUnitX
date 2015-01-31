unit DUnitX.Tests.Inheritance;

interface


uses
  DUnitX.TestFramework;

type
 {$M+}
 TMyBaseTestClass = class
 private
  FBlah : string;
 protected
  function GetSomething : string;virtual;
 public
  constructor Create;virtual;
  destructor Destroy;override;
  [TearDownFixture]
  procedure TearDownFixture;
  [Test]
  procedure TestInBase;
 end;

 TMyDerivedTest = class(TMyBaseTestClass)
 protected
   function GetSomething : string;override;
 public
  constructor Create;override;
  destructor Destroy;override;
 end;

implementation

{ TMyBaseTestClass }

constructor TMyBaseTestClass.Create;
begin
  FBlah := 'base';
end;

destructor TMyBaseTestClass.Destroy;
begin

  inherited;
end;

function TMyBaseTestClass.GetSomething: string;
begin
  result := 'base';
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

{ TMyDerivedTest }

constructor TMyDerivedTest.Create;
begin
  inherited;
  FBlah := 'derived';
end;

destructor TMyDerivedTest.Destroy;
begin

  inherited;
end;

function TMyDerivedTest.GetSomething: string;
begin
  result := 'derived';
  Assert.Pass;
end;

initialization
  TDUnitX.RegisterTestFixture(TMyDerivedTest);
end.
