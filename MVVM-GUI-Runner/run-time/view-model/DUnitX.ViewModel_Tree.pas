unit DUnitX.ViewModel_Tree;
interface
uses Classes, Vcl.Controls;

type

TVisualTestSuiteNodeState = (sNeutral, sSettingUp, sRunning, sTearingDown, sPassed, sFailed, sWarned, sError, sMessage);
TVisualTestSuiteNodeStateSet = set of TVisualTestSuiteNodeState;
TVisualTestSuiteNodeKind = (nkNeutral, nkMessage, nkTestCase, nkTestFixture, nkGroup);

IVisualTestSuiteNode = interface
  ['{660A0B5D-7DCC-4C62-B3A2-54CD873DCDC5}']
    {$REGION 'property accesssors'}
    function  GetDatum: pointer;
    procedure SetDatum( Value: pointer);
    {$ENDREGION}
    property Datum: pointer    read GetDatum write SetDatum;
  end;

TInserPosition = (iBefore, iAfter);

TSetCheckedSource = (csPopulation, csPostPopulate, csUser);
INodeRenderer = interface
  ['{1C46A49A-44C3-4505-B7A7-D46D7B1AFC51}']
    procedure Attached( const Node: IVisualTestSuiteNode);
    procedure Detach;
    function  GetState: TVisualTestSuiteNodeState;
    function  GetKind: TVisualTestSuiteNodeKind;
    function  GetDisplayName: string;
    function  GetFullCycleCount: integer;
    function  GetDoneCycleCount: integer;
    procedure SetChecked( Value: boolean; Source: TSetCheckedSource);
  end;

IVisualTestSuiteTreeChangeContext = interface
  ['{B81C4E46-E097-4FB3-9D7F-5DCD526AE8D3}']
    function  ChildNodes: IEnumerable<IVisualTestSuiteNode>;
    function  Insert( const Sibling: IVisualTestSuiteNode; Position: TInserPosition; AddCount: integer): IVisualTestSuiteNode;
    function  Append( AddCount: integer): IEnumerable<IVisualTestSuiteNode>;
    procedure AttachRenderer( const Newbie: IVisualTestSuiteNode; const Renderer: INodeRenderer);
    procedure Delete( const Victim: IVisualTestSuiteNode);
  end;

IVisualTestSuiteTree = interface
  ['{92018040-E850-4275-8357-0E200E223A2E}']
  // When constructed by a ServiceProvider, IComponentContext may be injected as a data member.
    function  FactoryDisplayName: string;
    function  Nodes( const Parent: IVisualTestSuiteNode): IEnumerable<IVisualTestSuiteNode>;
    function  AllNodes: IEnumerable<IVisualTestSuiteNode>;
    function  Change( const Parent: IVisualTestSuiteNode): IVisualTestSuiteTreeChangeContext;
    procedure InvalidateRendering( const DeltaNode: IVisualTestSuiteNode);
    procedure BeforePopulate;
    procedure AfterPopulate;
    procedure SetChecked( const Node: IVisualTestSuiteNode; Value: boolean; Source: TSetCheckedSource);
  end;

IVisualTestSuiteTreeFactory = interface
  ['{991B77D7-E057-47FB-8B9B-0A2253AB1168}']
    function MakeVisualTestSuiteTree( AOwner: TComponent; AParent: TWinControl; const AName: string): IVisualTestSuiteTree;
  end;

implementation

end.
