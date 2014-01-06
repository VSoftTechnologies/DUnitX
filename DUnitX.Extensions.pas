unit DUnitX.Extensions;
interface
uses DUnitX.Test, DUnitX.InternalInterfaces, DUnitX.TestFramework;

type

ICustomExecModulator = interface
  ['{BCC2B551-32D0-482A-A840-1EC556AB349F}']
    function Decorator( Base: IExecutionDecorator): IExecutionDecorator;
  end;

TCustomExecModulator = class abstract( TExecutionModulatorAttribute, IInterface, ICustomExecModulator)
  private
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  public
    function Decorator( Base: IExecutionDecorator): IExecutionDecorator; virtual; abstract;
  end;


  TBaseExecutionDecorator = class abstract( TAbstractExecutionDecorator)
    private
      FBase: IExecutionDecorator;
    protected
      function  GetRunner: ITestRunnerEx;                       override;
      procedure SetRunner( const Value: ITestRunnerEx);         override;
      function  ComputeWorkLoad( const test: ITest): integer;   override;
      function  MeasureProgress( const test: ITest): integer;   override;

      procedure BaseExecute(
        const context : ITestExecuteContext; const test: ITest; const threadId: Cardinal;
        const fixture: ITestFixture; const fixtureResult : IFixtureResult);

    public
      constructor Create( const ABase: IExecutionDecorator);
    end;

TRepeatDecorator = class( TBaseExecutionDecorator)
    protected
      FCount: integer;
      procedure ExecuteTest(
        const context : ITestExecuteContext; const test: ITest; const threadId: Cardinal;
        const fixture: ITestFixture; const fixtureResult : IFixtureResult);  override;
      procedure DecorateOn( const Subject: ITest);                           override;
      function  ComputeWorkLoad( const test: ITest): integer;   override;
      function  MeasureProgress( const test: ITest): integer;   override;
    public
      constructor Create( const ABase: IExecutionDecorator; ACount: integer);
  end;


function CreateDecorator( Attrib: TExecutionModulatorAttribute; Base: IExecutionDecorator): IExecutionDecorator;

implementation




uses SysUtils;

procedure TBaseExecutionDecorator.BaseExecute(
  const context: ITestExecuteContext; const test: ITest;
  const threadId: Cardinal; const fixture: ITestFixture;
  const fixtureResult: IFixtureResult);
begin
  FBase.ExecuteTest( context, test, threadId, fixture, fixtureResult)
end;

function TBaseExecutionDecorator.ComputeWorkLoad( const test: ITest): integer;
begin
  result := 1
end;

constructor TBaseExecutionDecorator.Create( const ABase: IExecutionDecorator);
begin
  FBase := ABase
end;

function TBaseExecutionDecorator.GetRunner: ITestRunnerEx;
begin
  result := FBase.Runner
end;

function TBaseExecutionDecorator.MeasureProgress( const test: ITest): integer;
begin
  // TODO:
  result := 0
end;

procedure TBaseExecutionDecorator.SetRunner( const Value: ITestRunnerEx);
begin
  FBase.Runner := Value
end;



function TRepeatDecorator.ComputeWorkLoad( const test: ITest): integer;
begin
  result := FBase.ComputeWorkLoad( test) * FCount
end;

constructor TRepeatDecorator.Create( const ABase: IExecutionDecorator;
  ACount: integer);
begin
  FBase  := ABase;
  FCount := ACount
end;

procedure TRepeatDecorator.DecorateOn( const Subject: ITest);
var
  SubjectInternalAccess: ITestExecute;
begin
  if Supports( Subject, ITestExecute, SubjectInternalAccess) then
    SubjectInternalAccess.SetCount( Subject.Count * FCount)
end;

procedure TRepeatDecorator.ExecuteTest(
  const context: ITestExecuteContext;
  const test: ITest; const threadId: Cardinal; const fixture: ITestFixture;
  const fixtureResult: IFixtureResult);
var
  Pass: integer;
begin
  for Pass := 0 to FCount - 1 do
    begin
    test.CurrentPassIndex := Pass;
    BaseExecute( context, test, threadId, fixture, fixtureResult);
    if fixtureResult.HasFailures or (fixtureResult.ErrorCount > 0) then break
    end
end;

function TRepeatDecorator.MeasureProgress(const test: ITest): integer;
begin
  // TODO:
  result := 0
end;

function RepeatsHelper_Decorator( Attrib: Repeats; Base: IExecutionDecorator): IExecutionDecorator;
begin
  result := TRepeatDecorator.Create( Base, Attrib.FCount)
end;

{ TCustomExecModulator }

function TCustomExecModulator.QueryInterface(const IID: TGUID;
  out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface( IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;

function TCustomExecModulator._AddRef: Integer;
begin
  result := -1
end;

function TCustomExecModulator._Release: Integer;
begin
  result := -1
end;


function CreateDecorator( Attrib: TExecutionModulatorAttribute; Base: IExecutionDecorator): IExecutionDecorator;
var
  Modulator: ICustomExecModulator;
begin
  if Supports( Attrib, ICustomExecModulator, Modulator) then
      result := Modulator.Decorator( Base)
    else if Attrib is Repeats then
      result := RepeatsHelper_Decorator( Repeats( Attrib), Base)
    else
      result := nil
end;


end.
