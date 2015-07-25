unit DUnitX.Init;

interface

uses DUnitX.TestFramework, DUnitX.FixtureProviderPlugin;

implementation

procedure InitAssert;
begin
  DUnitX.TestFramework.Assert.TestFailure := ETestFailure;
  DUnitX.TestFramework.Assert.TestPass := ETestPass;
end;

initialization
 TDUnitX.RegisterPlugin(TDUnitXFixtureProviderPlugin.Create);
 InitAssert;

end.
