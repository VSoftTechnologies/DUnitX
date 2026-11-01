unit DUnitX.SingleNameSpace;

interface

{$I DUnitX.inc}

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

uses
  classes;

{ TSingleNamespaceTest }

procedure TSingleNamespaceTest.ATest;
begin
  TThread.Sleep(50);

  Assert.IsTrue(true);
end;

initialization
  TDUnitX.RegisterTestFixture(TSingleNamespaceTest);
end.

