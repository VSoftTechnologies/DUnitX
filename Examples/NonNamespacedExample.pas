unit NonNamespacedExample;

interface

uses
  DUnitX.TestFramework;

type
 {$M+}
  [TestFixture]
  TNonNamespaceTest = class
  public
    [Test]
    procedure PassingTest;
  end;

implementation


{ TNonNamespaceTest }

procedure TNonNamespaceTest.PassingTest;
begin
   Assert.Pass;
end;


initialization
  TDUnitX.RegisterTestFixture(TNonNamespaceTest);
end.
