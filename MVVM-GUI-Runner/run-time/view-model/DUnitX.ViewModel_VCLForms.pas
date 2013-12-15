unit DUnitX.ViewModel_VCLForms;
interface
uses DUnitX.TestFramework, Forms, DUnitX.BaseExecutive, DUnitX.ViewModel_Tree,
     Classes, Controls, DUnitX.viewModel_LoggerContainer, Generics.Collections;

type
IViewModel_VCLForms = interface( ITestLogger)
  ['{B9BF01AA-1877-4073-9E43-C4A72F8FA8FC}']
    procedure FormLoaded( Form: TCustomForm);
    procedure FirstIdleEvent;
    procedure FormDestroyed;
    function  Executive: IExecutive;
    function  Model: ITestRunner;
    procedure Run;
    procedure AttachSecondaryLogger( const Addend: ITestLogger);
  end;

TViewModel_VCLForms = class( TInterfacedObject, ITestLogger, IViewModel_VCLForms, IComponentContext)
  private
    FExecutive: IExecutive;
    FModel: ITestRunner;
    FTree: IVisualTestSuiteTree;

    function  Executive: IExecutive;
    function  Model: ITestRunner;

  protected
    FForm: TCustomForm;
    FSecondaryLoggerContainerFactories: TList<ILoggerContainerFactory>;

    procedure FormLoaded( Form: TCustomForm);                                                         virtual;
    procedure FirstIdleEvent;                                                                         virtual;
    procedure FormDestroyed;                                                                          virtual;
    procedure OnTestingStarts(const threadId, testCount, testActiveCount : Cardinal);                 virtual;
    procedure OnStartTestFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);        virtual;
    procedure OnSetupFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);            virtual;
    procedure OnEndSetupFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);         virtual;
    procedure OnBeginTest(const threadId : Cardinal;const  Test: ITestInfo);                          virtual;
    procedure OnSetupTest(const threadId : Cardinal;const  Test: ITestInfo);                          virtual;
    procedure OnEndSetupTest(const threadId : Cardinal;const  Test: ITestInfo);                       virtual;
    procedure OnExecuteTest(const threadId : Cardinal;const  Test: ITestInfo);                        virtual;
    procedure OnTestSuccess(const threadId : Cardinal;const  Test: ITestResult);                      virtual;
    procedure OnTestError(const threadId : Cardinal;const Error: ITestError);                         virtual;
    procedure OnTestFailure(const threadId : Cardinal;const  Failure: ITestError);                    virtual;
    procedure OnTestIgnored(const threadId : Cardinal; const AIgnored: ITestResult);                  virtual;
    procedure OnTestMemoryLeak(const threadId : Cardinal; const AIgnored: ITestResult);               virtual;
    procedure OnLog(const logType : TLogLevel; const msg : string);                                   virtual;
    procedure OnTeardownTest(const threadId : Cardinal;const  Test: ITestInfo);                       virtual;
    procedure OnEndTeardownTest(const threadId : Cardinal; const Test: ITestInfo);                    virtual;
    procedure OnEndTest(const threadId : Cardinal;const  Test: ITestResult);                          virtual;
    procedure OnTearDownFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);         virtual;
    procedure OnEndTearDownFixture(const threadId : Cardinal; const fixture : ITestFixtureInfo);      virtual;
    procedure OnEndTestFixture(const threadId : Cardinal; const results : IFixtureResult);            virtual;
    procedure OnTestingEnds( const RunResults: IRunResults);                                          virtual;
    function  AcquireExecutive: IExecutive;                                                           virtual;

    // IComponentContext = interface
    function TreeOwner: TComponent;         virtual; abstract;
    function IComponentContext.Owner = TreeOwner;
    function TreeParent: TWinControl;       virtual; abstract;
    function IComponentContext.Parent = TreeParent;
    function TreeName: string;              virtual; abstract;
    function IComponentContext.Name = TreeName;

    procedure IntegrateSecondariesIntoMenus;              virtual; abstract;
    procedure AttachVisualTree;                           virtual; abstract;
    procedure AttachSecondaryLogger( const Addend: ITestLogger);

  public
    constructor Create;
    destructor  Destroy; override;

  private
    procedure AttachVCLForm( Form: TCustomForm);
    procedure AttachExecutive;
    procedure AttachModel;
    procedure RegisterTestFixtures;
    procedure LoadSecondaryLoggerContainerFactories;
    procedure AcquireVisualTree;
    procedure PopulateTree;
    procedure Run;
  end;


implementation













uses DUnitX.uExecutive, DUnitX.SBD.uServiceProvider, DUnitX.InternalInterfaces;

type
TNode = class abstract( TInterfacedObject, INodeRenderer)
  protected
    FToken: IVisualTestSuiteNode;
    FModel: TViewModel_VCLForms;

    procedure Attached( const Node: IVisualTestSuiteNode);
    procedure Detach;                                        virtual;
    function  GetState: TVisualTestSuiteNodeState;           virtual; abstract;
    function  GetKind: TVisualTestSuiteNodeKind;             virtual; abstract;
    function  GetDisplayName: string;                        virtual; abstract;
    function  GetFullCycleCount: integer;                    virtual; abstract;
    function  GetDoneCycleCount: integer;                    virtual; abstract;
  public
    constructor Create( Model1: TViewModel_VCLForms);
  end;

TFixtureNode = class( TNode)
  protected
    FFixture: ITestFixture;

    procedure Detach;                                        override;
    function  GetState: TVisualTestSuiteNodeState;           override;
    function  GetKind: TVisualTestSuiteNodeKind;             override;
    function  GetDisplayName: string;                        override;
    function  GetFullCycleCount: integer;                    override;
    function  GetDoneCycleCount: integer;                    override;
  public
    constructor Create( Model1: TViewModel_VCLForms; const Fixture1: ITestFixture);
  end;

TTestCaseNode = class( TNode)
  protected
    FTestCase: ITest;

    procedure Detach;                                        override;
    function  GetState: TVisualTestSuiteNodeState;           override;
    function  GetKind: TVisualTestSuiteNodeKind;             override;
    function  GetDisplayName: string;                        override;
    function  GetFullCycleCount: integer;                    override;
    function  GetDoneCycleCount: integer;                    override;
  public
    constructor Create( Model1: TViewModel_VCLForms; const TestCase1: ITest);
  end;

constructor TViewModel_VCLForms.Create;
begin
end;

destructor TViewModel_VCLForms.Destroy;
begin
FSecondaryLoggerContainerFactories.Free;
inherited
end;

procedure TViewModel_VCLForms.FormLoaded( Form: TCustomForm);
begin
AttachVCLForm( Form)
end;


procedure TViewModel_VCLForms.AttachVCLForm( Form: TCustomForm);
begin
FForm := Form;
AttachExecutive
end;

procedure TViewModel_VCLForms.AttachExecutive;
begin
FExecutive := AcquireExecutive;
if not assigned( FExecutive) then exit;
if Application.MainForm = FForm then
  FExecutive.DeclareMainForm( FForm)
end;


procedure TViewModel_VCLForms.FirstIdleEvent;
begin
AttachModel;
RegisterTestFixtures;
LoadSecondaryLoggerContainerFactories;
IntegrateSecondariesIntoMenus;              // Overriden in view
AcquireVisualTree;
AttachVisualTree;                           // Overriden in view
PopulateTree;
FExecutive.StartUp
end;


function TViewModel_VCLForms.AcquireExecutive: IExecutive;
begin
result := DUnitX.uExecutive.AcquireExecutive
end;

procedure TViewModel_VCLForms.AttachModel;
begin
FModel := FExecutive.Model;
FModel.AddLogger( self)
end;


procedure TViewModel_VCLForms.AttachSecondaryLogger( const Addend: ITestLogger);
begin
FModel.AddLogger( Addend)
end;

procedure TViewModel_VCLForms.AcquireVisualTree;
var
  InjectionOverride: IServiceProvider;
begin
InjectionOverride := StandardServiceProvider;
InjectionOverride.RegisterLiveService( IComponentContext, self as IComponentContext);
// The above sets up the tree's form owner, parent and component name.
FExecutive.Services.Acquire( self, IVisualTestSuiteTree, FTree, '', InjectionOverride)
end;

procedure TViewModel_VCLForms.PopulateTree;
var
  Fixture: ITestFixture;

  procedure PopulateSubTree( const RootFixture: ITestFixture; const ParentVisualNode: IVisualTestSuiteNode);
  var
    FixtureContext : IVisualTestSuiteTreeChangeContext;
    TestCaseContext: IVisualTestSuiteTreeChangeContext;
    Newbie, TestCaseNewbs: IVisualTestSuiteNode;
    Addend: TNode;
    TestCase: ITest;
    Child: ITestFixture;
    s: string;
  begin
  s := RootFixture.Name;
  FixtureContext := FTree.Change( ParentVisualNode);
  for Newbie in FixtureContext.Append( 1) do
    begin
    Addend := TFixtureNode.Create( self, RootFixture);
    Newbie.Datum := Pointer( Addend);
    FixtureContext.AttachRenderer( Newbie, Addend);
    for Child in RootFixture.Children do
      PopulateSubTree( Child, Newbie);
    for TestCase in RootFixture.Tests do
      begin
      TestCaseContext := FTree.Change( Newbie);
      for TestCaseNewbs in TestCaseContext.Append(1) do
        begin
        Addend := TTestCaseNode.Create( self, TestCase);
        TestCaseNewbs.Datum := Pointer( Addend);
        TestCaseContext.AttachRenderer( TestCaseNewbs, Addend)
        end
      end
    end;
  end;

begin
FTree.BeforePopulate;
for Fixture in FModel.BuildFixtures as ITestFixtureList do
  PopulateSubTree( Fixture, nil);
FTree.AfterPopulate
end;


procedure TViewModel_VCLForms.RegisterTestFixtures;
begin
FExecutive.RegisterTestFixtures
end;

procedure TViewModel_VCLForms.Run;
begin
FModel.Execute
end;

function TViewModel_VCLForms.Executive: IExecutive;
begin
result := FExecutive
end;

procedure TViewModel_VCLForms.FormDestroyed;
var
  Exec: IExecutive;
begin
FTree := nil;
if (Application.MainForm = FForm) and assigned( FExecutive) then
  begin
  Exec := FExecutive;
  FExecutive := nil;
  Exec.ShutDown
  end
end;

procedure TViewModel_VCLForms.LoadSecondaryLoggerContainerFactories;
var
  Gang: IInterfaceList;
  Member: IInterface;
begin
FSecondaryLoggerContainerFactories := TList<ILoggerContainerFactory>.Create;
if FExecutive.Services.AcquireGang( Self, ILoggerContainerFactory, Gang) then
  for Member in (Gang as IInterfaceListEx) do
    FSecondaryLoggerContainerFactories.Add( Member as ILoggerContainerFactory)
end;

function TViewModel_VCLForms.Model: ITestRunner;
begin
result := FModel
end;

procedure TViewModel_VCLForms.OnBeginTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin

end;

procedure TViewModel_VCLForms.OnEndSetupFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TViewModel_VCLForms.OnEndSetupTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin

end;

procedure TViewModel_VCLForms.OnEndTearDownFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TViewModel_VCLForms.OnEndTeardownTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin

end;

procedure TViewModel_VCLForms.OnEndTest(const threadId: Cardinal;
  const Test: ITestResult);
begin

end;

procedure TViewModel_VCLForms.OnEndTestFixture(const threadId: Cardinal;
  const results: IFixtureResult);
begin

end;

procedure TViewModel_VCLForms.OnExecuteTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin

end;

procedure TViewModel_VCLForms.OnLog(const logType: TLogLevel;
  const msg: string);
begin

end;

procedure TViewModel_VCLForms.OnSetupFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TViewModel_VCLForms.OnSetupTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin

end;

procedure TViewModel_VCLForms.OnStartTestFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TViewModel_VCLForms.OnTearDownFixture(const threadId: Cardinal;
  const fixture: ITestFixtureInfo);
begin

end;

procedure TViewModel_VCLForms.OnTeardownTest(const threadId: Cardinal;
  const Test: ITestInfo);
begin

end;

procedure TViewModel_VCLForms.OnTestError(const threadId: Cardinal;
  const Error: ITestError);
begin

end;

procedure TViewModel_VCLForms.OnTestFailure(const threadId: Cardinal;
  const Failure: ITestError);
begin

end;

procedure TViewModel_VCLForms.OnTestIgnored(const threadId: Cardinal;
  const AIgnored: ITestResult);
begin

end;

procedure TViewModel_VCLForms.OnTestingEnds(const RunResults: IRunResults);
begin

end;

procedure TViewModel_VCLForms.OnTestingStarts(const threadId, testCount,
  testActiveCount: Cardinal);
begin

end;

procedure TViewModel_VCLForms.OnTestMemoryLeak(const threadId: Cardinal;
  const AIgnored: ITestResult);
begin

end;

procedure TViewModel_VCLForms.OnTestSuccess(const threadId: Cardinal;
  const Test: ITestResult);
begin

end;


{ TNode }

procedure TNode.Attached( const Node: IVisualTestSuiteNode);
begin
FToken := Node
end;

constructor TNode.Create( Model1: TViewModel_VCLForms);
begin
FModel := Model1
end;

procedure TNode.Detach;
begin
FModel := nil;
FToken := nil
end;

{ TFixtureNode }

constructor TFixtureNode.Create(
  Model1: TViewModel_VCLForms; const Fixture1: ITestFixture);
begin
inherited Create( Model1);
FFixture := Fixture1
end;

procedure TFixtureNode.Detach;
begin
inherited Detach;
FFixture := nil
end;

function TFixtureNode.GetDisplayName: string;
begin
// TODO:
result := FFixture.Name
end;

function TFixtureNode.GetDoneCycleCount: integer;
begin
result := -1 // N/A
end;

function TFixtureNode.GetFullCycleCount: integer;
begin
result := -1 // N/A
end;

function TFixtureNode.GetKind: TVisualTestSuiteNodeKind;
begin
result := nkTestFixture
end;

function TFixtureNode.GetState: TVisualTestSuiteNodeState;
begin
// TODO:
result := sNeutral
end;

{ TTestCaseNode }

constructor TTestCaseNode.Create(
  Model1: TViewModel_VCLForms; const TestCase1: ITest);
begin
inherited Create( Model1);
FTestCase := TestCase1
end;

procedure TTestCaseNode.Detach;
begin
inherited Detach;
FTestCase := nil
end;

function TTestCaseNode.GetDisplayName: string;
begin
result := FTestCase.Name
end;

function TTestCaseNode.GetDoneCycleCount: integer;
begin
// TODO:
result := 0
end;

function TTestCaseNode.GetFullCycleCount: integer;
begin
// TODO:
result := 1
end;

function TTestCaseNode.GetKind: TVisualTestSuiteNodeKind;
begin
result := nkTestCase
end;

function TTestCaseNode.GetState: TVisualTestSuiteNodeState;
begin
// TODO:
result := sNeutral
end;

end.
