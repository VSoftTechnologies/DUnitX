unit DUnitX.Tests.DUnitCompatibility;

interface

uses
  DUnitX.TestFramework,
  DUnitX.DUnitCompatibility;

type
  // a typical DUnit like class
  {$M+}
  TMyDUnitTest = class(TTestCase)
  published
    procedure ATest;
  end;

implementation

{ TMyDUnitTest }

procedure TMyDUnitTest.ATest;
begin
  CheckTrue(true,'true is always true!');
end;

initialization
  TDUnitX.RegisterTestFixture(TMyDUnitTest);
end.
