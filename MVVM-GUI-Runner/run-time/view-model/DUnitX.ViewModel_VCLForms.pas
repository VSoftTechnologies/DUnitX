unit DUnitX.ViewModel_VCLForms;
interface
uses DUnitX.TestFramework, Forms, DUnitX.BaseExecutive, DUnitX.ViewModel_Tree,
     Classes, Controls, DUnitX.viewModel_LoggerContainer, Generics.Collections;

type
TPutLevel = (lvDebug, lvNormal, lvHighLight);

IViewModel_VCLForms = interface( ITestLogger)
  ['{B9BF01AA-1877-4073-9E43-C4A72F8FA8FC}']
    procedure FormLoaded( Form: TCustomForm);
    procedure FirstIdleEvent;
    procedure FormDestroyed;
    function  Executive: IExecutive;
    function  Model: ITestRunner;
    procedure Run;
    procedure AttachSecondaryLogger( const Addend: ITestLogger);
    procedure ClearLog;
    procedure Put( Level: TPutLevel; const Line: string);
    procedure ToggleSelections;
    procedure ClearSelections;
    function  CanClearSelections: boolean;
    procedure SelectAll;
    procedure SelectFailed;
    function  CanSelectFailed: boolean;
  end;

TSuiteRunnerState = (rsIdle, rsSettingUp, rsExecuting, rsTearingDown, rsBetweenTests);

TViewModel_VCLForms = class( TInterfacedObject, ITestLogger, IViewModel_VCLForms)
  private
    FExecutive: IExecutive;
    FTree: IVisualTestSuiteTree;
    FTestCaseCount: integer;

    function  Executive: IExecutive;
    function  Model: ITestRunner;

  protected type
    TNode = class abstract( TInterfacedObject, INodeRenderer)
      protected
        FToken: IVisualTestSuiteNode;
        FModel: TViewModel_VCLForms;
        FState: TVisualTestSuiteNodeState;

        procedure Attached( const Node: IVisualTestSuiteNode);
        procedure Detach;                                        virtual;
        function  GetState: TVisualTestSuiteNodeState;           virtual;
        function  GetKind: TVisualTestSuiteNodeKind;             virtual; abstract;
        function  GetDisplayName: string;                        virtual; abstract;
        function  GetFullCycleCount: integer;                    virtual; abstract;
        function  GetDoneCycleCount: integer;                    virtual; abstract;
        procedure SetChecked( Value: boolean; Source: TSetCheckedSource);  virtual; abstract;
        function  IsChecked: boolean;                            virtual;
      public
        constructor Create( Model1: TViewModel_VCLForms);
      end;


  protected
    FForm: TCustomForm;
    FSecondaryLoggerContainerFactories: TList<ILoggerContainerFactory>;
    FModel: ITestRunner;
    FState: TSuiteRunnerState;
    FisInOperation: boolean;

    function  FindFixtureNode( const Fixture: ITestFixtureInfo; var Finding: TNode): boolean;
    function  FindTestCaseNode( const TestCase: ITestInfo; var Finding: TNode): boolean;

    procedure FixtureStateChange(
      const Fixture: ITestFixtureInfo;
      ListeningOnStates: TVisualTestSuiteNodeStateSet;
      NewState: TVisualTestSuiteNodeState);

    procedure TestCaseStateChange(
      const TestCase: ITestInfo;
      ListeningOnStates: TVisualTestSuiteNodeStateSet;
      NewState: TVisualTestSuiteNodeState);

    procedure EnterOperation( isEntering: boolean);                                                   virtual;
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

    function TreeOwner: TComponent;         virtual; abstract;
    function TreeParent: TWinControl;       virtual; abstract;
    function TreeName: string;              virtual; abstract;

    procedure IntegrateSecondariesIntoMenus;              virtual; abstract;
    procedure AttachVisualTree;                           virtual; abstract;
    procedure AttachSecondaryLogger( const Addend: ITestLogger);

    procedure ClearLog;                                           virtual; abstract;
    procedure Put( Level: TPutLevel; const Line: string);         virtual; abstract;
    procedure SetDisplayState( Value: TSuiteRunnerState);         virtual;
    procedure Breathe;                                            virtual; abstract;
    procedure InitiateView( TestCaseCount: integer);              virtual;
    procedure SetApplicationTitle( const Title: string);          virtual;

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
    procedure ToggleSelections;
    procedure ClearSelections;
    function  CanClearSelections: boolean;
    procedure SelectAll;
    procedure SelectFailed;
    function  CanSelectFailed: boolean;
    procedure SetClearAll( Value: boolean);

  private type
    TPerNode = reference to procedure( Leaf: TNode; var doBreak: boolean);
  private
    function  PerLeafNode( Proc: TPerNode): boolean; // Returns True if exit via PerNode break.
  end;


implementation













uses DUnitX.uExecutive, DUnitX.IoC, DUnitX.InternalInterfaces, SysUtils, Math;

type
TFixtureNode = class( TViewModel_VCLForms.TNode)
  protected
    FFixture: ITestFixture;

    procedure Detach;                                        override;
    function  GetKind: TVisualTestSuiteNodeKind;             override;
    function  GetDisplayName: string;                        override;
    function  GetFullCycleCount: integer;                    override;
    function  GetDoneCycleCount: integer;                    override;
    procedure SetChecked( Value: boolean; Source: TSetCheckedSource);  override;
    function  IsChecked: boolean;                            override;
  public
    constructor Create( Model1: TViewModel_VCLForms; const Fixture1: ITestFixture);
  end;

TTestCaseNode = class( TViewModel_VCLForms.TNode)
  protected
    FTestCase: ITest;

    procedure Detach;                                        override;
    function  GetKind: TVisualTestSuiteNodeKind;             override;
    function  GetDisplayName: string;                        override;
    function  GetFullCycleCount: integer;                    override;
    function  GetDoneCycleCount: integer;                    override;
    procedure SetChecked( Value: boolean; Source: TSetCheckedSource);  override;
    function  IsChecked: boolean;                            override;
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


procedure TViewModel_VCLForms.InitiateView( TestCaseCount: integer);
begin
SetDisplayState( rsIdle)
end;


procedure TViewModel_VCLForms.AttachVCLForm( Form: TCustomForm);
begin
FForm := Form;
AttachExecutive;
SetApplicationTitle( FExecutive.ApplicationTitle)
end;


procedure TViewModel_VCLForms.SetApplicationTitle( const Title: string);
begin
FForm.Caption := Title
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
InitiateView( FTestCaseCount);
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
begin
FTree := FExecutive.Services.Resolve<IVisualTestSuiteTreeFactory>('')
  .MakeVisualTestSuiteTree( TreeOwner, TreeParent, TreeName)
end;

procedure TViewModel_VCLForms.PopulateTree;
var
  Fixture: ITestFixture;
  TestCaseCount: integer;

  procedure PopulateSubTree( const Fixture1: ITestFixture; const ParentVisualNode: IVisualTestSuiteNode);
  var
    FixtureContext : IVisualTestSuiteTreeChangeContext;
    TestCaseContext: IVisualTestSuiteTreeChangeContext;
    Newbie, TestCaseNewbs: IVisualTestSuiteNode;
    Addend: TNode;
    TestCase: ITest;
    Child: ITestFixture;
  begin
  FixtureContext := FTree.Change( ParentVisualNode);
  for Newbie in FixtureContext.Append( 1) do
    begin
    Addend := TFixtureNode.Create( self, Fixture1);
    Newbie.Datum := Pointer( Addend);
    FixtureContext.AttachRenderer( Newbie, Addend);
    FTree.SetChecked( Newbie, False, csPopulation);
    for Child in Fixture1.Children do
      PopulateSubTree( Child, Newbie);
    for TestCase in Fixture1.Tests do
      begin
      TestCaseContext := FTree.Change( Newbie);
      for TestCaseNewbs in TestCaseContext.Append(1) do
        begin
        Addend := TTestCaseNode.Create( self, TestCase);
        if TestCase.Enabled then
          Inc( FTestCaseCount);
        TestCaseNewbs.Datum := Pointer( Addend);
        TestCaseContext.AttachRenderer( TestCaseNewbs, Addend);
        FTree.SetChecked( TestCaseNewbs, TestCase.Enabled, csPopulation);
        end
      end;
    FTree.SetChecked( Newbie, Fixture1.Enabled, csPostPopulate);
    end;
  end;

begin
FTestCaseCount := 0;
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
var
  GenericNode: IVisualTestSuiteNode;
begin
EnterOperation( True);
try
  for GenericNode in FTree.AllNodes do
    begin
    TNode( GenericNode.Datum).FState := sNeutral;
    FTree.InvalidateRendering( GenericNode)
    end;
  Breathe;
  FModel.Execute
finally
  EnterOperation( False)
  end
end;

procedure TViewModel_VCLForms.SetDisplayState( Value: TSuiteRunnerState);
begin
FState := Value
end;

procedure TViewModel_VCLForms.EnterOperation( isEntering: boolean);
begin
FisInOperation := isEntering
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
  Factory: ILoggerContainerFactory;
begin
FSecondaryLoggerContainerFactories := TList<ILoggerContainerFactory>.Create;
for Factory in FExecutive.Services.Resolve<ILoggerCentral>( '').Loggers do
  FSecondaryLoggerContainerFactories.Add( Factory)
end;

function TViewModel_VCLForms.Model: ITestRunner;
begin
result := FModel
end;


procedure TViewModel_VCLForms.OnTestingStarts(
  const threadId, testCount, testActiveCount: Cardinal);
begin
Put( lvDebug, Format( 'Testing started. Test case count = %d. Active tests = %d.', [testCount, testActiveCount]));
SetDisplayState( rsBetweenTests);
Breathe
end;

function TViewModel_VCLForms.FindFixtureNode( const Fixture: ITestFixtureInfo; var Finding: TNode): boolean;
var
  GenericNode: IVisualTestSuiteNode;
  Node: TNode;
  AsInfo: ITestFixtureInfo;
begin
Finding := nil;
for GenericNode in FTree.AllNodes do
  begin
  if assigned( GenericNode) then
      Node := TNode( GenericNode.Datum)
    else
      Node := nil;
  if (not assigned( Node)) or
     (not (Node is TFixtureNode)) or
     (not Supports( TFixtureNode( Node).FFixture, ITestFixtureInfo, AsInfo)) or
     (AsInfo <> Fixture) then continue;
  Finding := Node;
  break
  end;
result := assigned( Finding)
end;


function TViewModel_VCLForms.FindTestCaseNode(
  const TestCase: ITestInfo; var Finding: TNode): boolean;
var
  GenericNode: IVisualTestSuiteNode;
  Node: TNode;
  AsInfo: ITestInfo;
begin
Finding := nil;
for GenericNode in FTree.AllNodes do
  begin
  if assigned( GenericNode) then
      Node := TNode( GenericNode.Datum)
    else
      Node := nil;
  if (not assigned( Node)) or
     (not (Node is TTestCaseNode)) or
     (not Supports( TTestCaseNode( Node).FTestCase, ITestInfo, AsInfo)) or
     (AsInfo <> TestCase) then continue;
  Finding := Node;
  break
  end;
result := assigned( Finding)
end;

procedure TViewModel_VCLForms.FixtureStateChange(
  const Fixture: ITestFixtureInfo;
  ListeningOnStates: TVisualTestSuiteNodeStateSet;
  NewState: TVisualTestSuiteNodeState);
var
  Finding: TNode;
begin
if FindFixtureNode( Fixture, Finding) and
  (Finding.FState in ListeningOnStates) then
  begin
  Finding.FState := NewState;
  FTree.InvalidateRendering( Finding.FToken)
  end;
Breathe
end;


procedure TViewModel_VCLForms.TestCaseStateChange(
  const TestCase: ITestInfo;
  ListeningOnStates: TVisualTestSuiteNodeStateSet;
  NewState: TVisualTestSuiteNodeState);
var
  Finding: TNode;
begin
if FindTestCaseNode( TestCase, Finding) and
  (Finding.FState in ListeningOnStates) then
  begin
  Finding.FState := NewState;
  FTree.InvalidateRendering( Finding.FToken)
  end;
Breathe
end;


procedure TViewModel_VCLForms.OnStartTestFixture(
  const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin
end;

procedure TViewModel_VCLForms.OnSetupFixture(
  const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin
FixtureStateChange( Fixture, [sNeutral, sRunning, sTearingDown, sPassed, sFailed, sWarned], sSettingUp)
end;

procedure TViewModel_VCLForms.OnEndSetupFixture(
  const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin
FixtureStateChange( Fixture, [sSettingUp], sNeutral)
end;



procedure TViewModel_VCLForms.OnTearDownFixture(
  const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin
FixtureStateChange( Fixture, [sNeutral, sRunning, sSettingUp], sTearingDown)
end;

procedure TViewModel_VCLForms.OnEndTearDownFixture(
  const threadId: Cardinal; const fixture: ITestFixtureInfo);
begin
FixtureStateChange( Fixture, [sTearingDown], sNeutral)
end;



procedure TViewModel_VCLForms.OnEndTestFixture(
  const threadId: Cardinal; const results: IFixtureResult);
var
  NextState: TVisualTestSuiteNodeState;

  function WorstResultOfChildren( const Fixture: IFixtureResult): TVisualTestSuiteNodeState;
  var
    Child: IFixtureResult;
  begin
  result := sNeutral;
  for Child in Fixture.Children do
    begin
    if Child.ErrorCount > 0 then
        begin
        result := sError;
        break
        end
      else if Child.FailureCount > 0 then
        result := sFailed
      else if (Child.PassCount > 0) and (result <> sFailed) then
        result := sPassed;
    result := TVisualTestSuiteNodeState( Math.Max( Ord( result), Ord( WorstResultOfChildren( Child))))
    end;
  end;

begin
if results.ErrorCount > 0 then
    NextState := sError
  else if results.FailureCount > 0 then
    NextState := sFailed
  else if results.PassCount > 0 then
    NextState := sPassed
  else
    NextState := WorstResultOfChildren( results);
FixtureStateChange( results.Fixture, [sNeutral, sRunning, sSettingUp, sTearingDown, sPassed, sFailed, sWarned], NextState)
end;

procedure TViewModel_VCLForms.OnLog( const logType: TLogLevel; const msg: string);
var
  Lv: TPutLevel;
begin
case logType of
  ltInformation: Lv := lvNormal;
  ltWarning:     Lv := lvNormal;
  ltError:       Lv := lvHighLight;
  end;
Put( lvDebug, msg)
end;

procedure TViewModel_VCLForms.OnBeginTest(
  const threadId: Cardinal; const Test: ITestInfo);
begin
end;

procedure TViewModel_VCLForms.OnSetupTest(
  const threadId: Cardinal; const Test: ITestInfo);
begin
TestCaseStateChange( Test, [sSettingUp, sRunning, sTearingDown, sPassed, sFailed, sWarned, sError], sNeutral)
end;

procedure TViewModel_VCLForms.OnEndSetupTest(
  const threadId: Cardinal; const Test: ITestInfo);
begin
end;

procedure TViewModel_VCLForms.OnExecuteTest(
  const threadId: Cardinal; const Test: ITestInfo);
begin
TestCaseStateChange( Test, [sNeutral, sSettingUp, sTearingDown, sPassed, sFailed, sWarned, sError], sRunning)
end;

procedure TViewModel_VCLForms.OnTeardownTest(
  const threadId: Cardinal; const Test: ITestInfo);
begin
TestCaseStateChange( Test, [sNeutral, sSettingUp, sRunning, sPassed, sFailed, sWarned, sError], sTearingDown)
end;

procedure TViewModel_VCLForms.OnEndTeardownTest(
  const threadId: Cardinal; const Test: ITestInfo);
begin
end;

procedure TViewModel_VCLForms.OnEndTest(
  const threadId: Cardinal; const Test: ITestResult);
var
  Finding: TNode;
  NewState: TVisualTestSuiteNodeState;
begin
if not FindTestCaseNode( Test.Test, Finding) then exit;
case Test.ResultType of
  TTestResultType.Pass      : NewState := sPassed;
  TTestResultType.Failure   : NewState := sFailed;
  TTestResultType.Error     : NewState := sError;
  TTestResultType.Ignored   : NewState := sNeutral;
  TTestResultType.MemoryLeak: NewState := sWarned;
  end;
TestCaseStateChange( Test.Test, [sNeutral, sSettingUp, sRunning, sTearingDown, sPassed, sFailed, sWarned, sError, sMessage], NewState)
end;

procedure TViewModel_VCLForms.OnTestError(
  const threadId: Cardinal; const Error: ITestError);
begin
end;

procedure TViewModel_VCLForms.OnTestFailure(
  const threadId: Cardinal; const Failure: ITestError);
begin
end;

procedure TViewModel_VCLForms.OnTestIgnored(const threadId: Cardinal;
  const AIgnored: ITestResult);
begin
end;

procedure TViewModel_VCLForms.OnTestingEnds( const RunResults: IRunResults);
begin
Put( lvDebug, Format( 'Test count = %d', [RunResults.TestCount]));
Put( lvDebug, Format( 'Failure count = %d', [RunResults.FailureCount]));
Put( lvDebug, Format( 'Error count = %d', [RunResults.ErrorCount]));
Put( lvDebug, Format( 'Ignored count = %d', [RunResults.IgnoredCount]));
Put( lvDebug, Format( 'Pass count = %d', [RunResults.PassCount]));
Put( lvDebug, Format( 'Memory leak count = %d', [RunResults.MemoryLeakCount]));
Put( lvDebug, Format( 'Success rate = %d', [RunResults.SuccessRate]));
Put( lvNormal, 'Testing finished. ');
SetDisplayState( rsIdle)
end;

procedure TViewModel_VCLForms.OnTestMemoryLeak(
  const threadId: Cardinal; const AIgnored: ITestResult);
begin
end;

procedure TViewModel_VCLForms.OnTestSuccess(
  const threadId: Cardinal; const Test: ITestResult);
begin
end;

function TViewModel_VCLForms.PerLeafNode( Proc: TPerNode): boolean;
var
  GenericNode: IVisualTestSuiteNode;
  Child: IVisualTestSuiteNode;
  Node: TNode;
  isLeaf: boolean;
  doBreak: boolean;
begin
result := False;
for GenericNode in FTree.AllNodes do
  begin
  isLeaf := True;
  for Child in FTree.Nodes( GenericNode) do
    begin
    isLeaf := False;
    break
    end;
  if not isLeaf then continue;
  Node := TNode( GenericNode.Datum);
  Proc( Node, result);
  if result then break
  end
end;


function TViewModel_VCLForms.CanClearSelections: boolean;
begin
result := PerLeafNode( procedure( Leaf: TNode; var doBreak: boolean)
  begin
  doBreak := Leaf.IsChecked
  end)
end;

function TViewModel_VCLForms.CanSelectFailed: boolean;
begin
result := PerLeafNode( procedure( Leaf: TNode; var doBreak: boolean)
  begin
  doBreak := Leaf.FState in [sError, sFailed]
  end)
end;

procedure TViewModel_VCLForms.SetClearAll( Value: boolean);
begin
PerLeafNode( procedure( Leaf: TNode; var doBreak: boolean)
  begin
  doBreak := False;
  Leaf.SetChecked( Value, csUser);
  FTree.SetChecked( Leaf.FToken, Leaf.IsChecked, csUser)
  end)
end;

procedure TViewModel_VCLForms.ClearSelections;
begin
SetClearAll( False)
end;

procedure TViewModel_VCLForms.SelectAll;
begin
SetClearAll( True)
end;

procedure TViewModel_VCLForms.SelectFailed;
begin
PerLeafNode( procedure( Leaf: TNode; var doBreak: boolean)
  begin
  doBreak := False;
  if Leaf.FState in [sError, sFailed] then
    begin
    Leaf.SetChecked( True, csUser);
    FTree.SetChecked( Leaf.FToken, Leaf.IsChecked, csUser)
    end
  end)
end;

procedure TViewModel_VCLForms.ToggleSelections;
begin
PerLeafNode( procedure( Leaf: TNode; var doBreak: boolean)
  begin
  doBreak := False;
  Leaf.SetChecked( not Leaf.IsChecked, csUser);
  FTree.SetChecked( Leaf.FToken, Leaf.IsChecked, csUser)
  end)
end;


{ TNode }

procedure TViewModel_VCLForms.TNode.Attached( const Node: IVisualTestSuiteNode);
begin
FToken := Node
end;

constructor TViewModel_VCLForms.TNode.Create( Model1: TViewModel_VCLForms);
begin
FModel := Model1
end;

procedure TViewModel_VCLForms.TNode.Detach;
begin
FModel := nil;
FToken := nil
end;

function TViewModel_VCLForms.TNode.GetState: TVisualTestSuiteNodeState;
begin
result := FState
end;

function TViewModel_VCLForms.TNode.IsChecked: boolean;
begin
result := True
end;

{ TFixtureNode }

constructor TFixtureNode.Create(
  Model1: TViewModel_VCLForms; const Fixture1: ITestFixture);
begin
inherited Create( Model1);
FFixture := Fixture1;
FState   := sNeutral
end;

procedure TFixtureNode.Detach;
begin
inherited Detach;
FFixture := nil
end;

function TFixtureNode.GetDisplayName: string;
begin
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


function TFixtureNode.IsChecked: boolean;
begin
result := FFixture.Enabled
end;

procedure TFixtureNode.SetChecked( Value: boolean; Source: TSetCheckedSource);
begin
if (Source = csUser) and (FFixture.Enabled <> Value) then
  FFixture.Enabled := Value
  // Assume it is successful.
  // If not, we must test for failure and propagate back to the view
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
result := FTestCase.Count
end;

function TTestCaseNode.GetKind: TVisualTestSuiteNodeKind;
begin
result := nkTestCase
end;


function TTestCaseNode.IsChecked: boolean;
begin
result := FTestCase.Enabled
end;

procedure TTestCaseNode.SetChecked( Value: boolean; Source: TSetCheckedSource);
begin
if (Source = csUser) and (FTestCase.Enabled <> Value) then
  FTestCase.Enabled := Value
  // Assume it is successful.
  // If not, we must test for failure and propagate back to the view
end;

end.
