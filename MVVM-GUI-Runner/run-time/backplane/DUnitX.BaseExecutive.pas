unit DUnitX.BaseExecutive;
interface
uses DUnitX.TestFramework, Classes, DUnitX.SBD.uServiceProvider;

type

IExecutive = interface
  ['{45B82A12-5F95-4515-BFE6-CE54799E4E2F}']
    procedure StartUp;
    procedure ShutDown;
    function  Model: ITestRunner;
    procedure DeclareMainForm( MainForm: TComponent);
    function  Services: IServiceProvider;
    procedure RegisterTestFixtures;
  end;

TBaseExecutive = class( TInterfacedObject, IExecutive)
  private
    FMainForm: TComponent;

    procedure StartUp;
    procedure ShutDown;
    function  Model: ITestRunner;
    procedure DeclareMainForm( MainForm: TComponent);
    function  Services: IServiceProvider;

  protected
    FServices: IServiceProvider;
    FModel: ITestRunner;

    procedure RegisterTestFixtures;                       virtual; abstract;
    procedure RegisterServices;                           virtual; abstract;
  public
    constructor Create;
  end;


implementation






uses DUnitX.TestRunner;



constructor TBaseExecutive.Create;
begin
FServices := StandardServiceProvider;
RegisterServices;
FModel    := TDUnitX.CreateRunner // TDUnitXTestRunner.Create( False, nil)
end;

procedure TBaseExecutive.DeclareMainForm( MainForm: TComponent);
begin
FMainForm := MainForm
end;

function TBaseExecutive.Model: ITestRunner;
begin
result := FModel
end;


function TBaseExecutive.Services: IServiceProvider;
begin
result := FServices
end;

procedure TBaseExecutive.ShutDown;
begin
FServices.ShutDown;
FServices := nil;
FModel    := nil
end;

procedure TBaseExecutive.StartUp;
begin
end;



end.
