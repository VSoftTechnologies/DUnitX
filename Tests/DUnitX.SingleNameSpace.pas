unit DUnitX.SingleNameSpace;

interface

uses
  DUnitX.TestFramework,
  DUnitX.WeakReference;

type
  TSingleNamespaceTest = class
  public
    [Test]
    procedure ATest;
  end;

implementation

{ TSingleNamespaceTest }

procedure TSingleNamespaceTest.ATest;
begin
  Assert.IsTrue(true);
end;

initialization
  TDUnitX.RegisterTestFixture(TSingleNamespaceTest);
end.
