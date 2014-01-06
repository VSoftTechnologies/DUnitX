unit DUnitX.DesignReg;
interface

procedure Register;

implementation











uses
  DUnitX.Wizard, DUnitX.IoC, DUnitX.LaunchWizardForm, ToolsAPI
  , DUnitX.IDE_API;

{.$R ..\..\design-time\graphics\dclDUnitX.dcr}


function DesignTimeServices: TDUnitXIoC;
begin
result := TDUnitXIoC.Create;
DUnitX.LaunchWizardForm.RegisterStockLaunchWizardService( result);
DUnitX.IDE_API.RegisterStockDevEnviroService( result)
end;

procedure Register;
var
  Services: TDUnitXIoC;
begin
Services := DesignTimeServices;
Services.Resolve<IIDE_API>().RegisterPackageWizard(
  DUnitX.Wizard.TProjectWizard.Create( Services))
  // Ownership of Services is transferred, so no need to free.
end;

end.
