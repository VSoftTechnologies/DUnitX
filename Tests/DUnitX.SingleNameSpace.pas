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
  {$IFDEF DELPHI_2010_DOWN}
  //D2010 doesn't have TThread.Sleep
  Windows,
  {$ENDIF}
  classes;


{ TSingleNamespaceTest }

procedure TSingleNamespaceTest.ATest;
begin
  {$IFDEF DELPHI_2010_DOWN}
    Windows.Sleep(50);// just so we can test duration output.
  {$ELSE}
    TThread.Sleep(50);
  {$ENDIF}

  Assert.IsTrue(true);
end;

initialization
  TDUnitX.RegisterTestFixture(TSingleNamespaceTest);
end.
