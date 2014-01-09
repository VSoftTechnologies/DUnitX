unit DUnitX.BaseExecutive;
interface
uses DUnitX.TestFramework, Classes, DUnitX.IoC, DUnitX.viewModel_LoggerContainer;

type

IExecutive = interface
  ['{45B82A12-5F95-4515-BFE6-CE54799E4E2F}']
    procedure StartUp;
    procedure ShutDown;
    function  Model: ITestRunner;
    procedure DeclareMainForm( MainForm: TComponent);
    function  Services: TDUnitXIoC;
    procedure RegisterTestFixtures;
  end;

TBaseExecutive = class( TInterfacedObject, IExecutive)
  private
    FMainForm: TComponent;

    procedure StartUp;
    procedure ShutDown;
    function  Model: ITestRunner;
    procedure DeclareMainForm( MainForm: TComponent);
    function  Services: TDUnitXIoC;

  protected
    FServices: TDUnitXIoC;
    FModel: ITestRunner;

    procedure RegisterTestFixtures;                       virtual; abstract;
    procedure RegisterServices;                           virtual;
    procedure AddLoggerFactory( const Addend: ILoggerContainerFactory);
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation






uses DUnitX.TestRunner, Generics.Collections, DUnitX.uStockSecondaryLoggerFactories;



procedure TBaseExecutive.AddLoggerFactory(
  const Addend: ILoggerContainerFactory);
begin
FServices.Resolve<ILoggerCentral>.Loggers.Add( Addend)
end;

constructor TBaseExecutive.Create;
begin
FServices := TDUnitXIoC.Create;
RegisterServices;
FModel    := TDUnitX.CreateRunner
end;

procedure TBaseExecutive.DeclareMainForm( MainForm: TComponent);
begin
FMainForm := MainForm
end;

destructor TBaseExecutive.Destroy;
begin
FServices.Free;
inherited
end;

function TBaseExecutive.Model: ITestRunner;
begin
result := FModel
end;


type
TLoggerCentral = class( TInterfacedObject, ILoggerCentral)
  private
    FLoggers: TList<ILoggerContainerFactory>;
    function Loggers: TList<ILoggerContainerFactory>;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function TLoggerCentral.Loggers: TList<ILoggerContainerFactory>;
begin
result := FLoggers
end;

constructor TLoggerCentral.Create;
begin
FLoggers := TList<ILoggerContainerFactory>.Create
end;

destructor TLoggerCentral.Destroy;
begin
FLoggers.Free;
inherited
end;

procedure TBaseExecutive.RegisterServices;
begin
FServices.RegisterType<ILoggerCentral>( True,
  function: ILoggerCentral
  begin
  result := TLoggerCentral.Create
  end, '');
DUnitX.uStockSecondaryLoggerFactories.AppendStockFactories( FServices.Resolve<ILoggerCentral>.Loggers)
end;

function TBaseExecutive.Services: TDUnitXIoC;
begin
result := FServices
end;

procedure TBaseExecutive.ShutDown;
begin
FServices.Clear;
FServices := nil;
FModel    := nil
end;

procedure TBaseExecutive.StartUp;
begin
end;



end.
