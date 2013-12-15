unit DUnitX.uTestSuiteVirtualTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, VirtualTrees, DUnitX.ViewModel_Tree, DUnitX.SBD.uServiceProvider,
  ImgList;

type
  TTestSuiteVirtualTreeObj = class;
  TTestSuiteVirtualTree = class(TFrame)
    imglstStates16x16: TImageList;
    Tree: TVirtualStringTree;
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
     FObj:  TTestSuiteVirtualTreeObj;
  private const
    KindImages : array[ TVisualTestSuiteNodeKind ] of integer = (
      // nkNeutral, nkMessage, nkTestCase, nkTestFixture, nkGroup
                -1,        -1,          0,             1,      -1);
    StateImages: array[ TVisualTestSuiteNodeState] of integer = (
      // sNeutral, sSettingUp, sRunning, sTearingDown, sPassed, sFailed, sWarned, sMessage
               -1,          -1,      -1,           -1,      -1,      -1,      -1,       -1);
  end;

  TTestSuiteVirtualTreeObj = class( TInterfacedObject, IVisualTestSuiteTree)
  private
    FFrame: TTestSuiteVirtualTree;
    FTree: TVirtualStringTree;
    function  FactoryDisplayName: string;
    function  Nodes( const Parent: IVisualTestSuiteNode): IEnumerable<IVisualTestSuiteNode>;
    function  Change( const Parent: IVisualTestSuiteNode): IVisualTestSuiteTreeChangeContext;
    procedure InvalidateRendering( const DeltaNode: IVisualTestSuiteNode);
    procedure BeforePopulate;
    procedure AfterPopulate;

  private type
    TChangeContext = class( TInterfacedObject, IVisualTestSuiteTreeChangeContext)
      private
        FTreeObj: TTestSuiteVirtualTreeObj;
        FParent: IVisualTestSuiteNode;
        FParentV: PVirtualNode;
        function  ChildNodes: IEnumerable<IVisualTestSuiteNode>;
        function  Insert( const Sibling: IVisualTestSuiteNode; Position: TInserPosition; AddCount: integer): IVisualTestSuiteNode;
        function  Append( AddCount: integer): IEnumerable<IVisualTestSuiteNode>;
        procedure AttachRenderer( const Newbie: IVisualTestSuiteNode; const Renderer: INodeRenderer);
        procedure Delete( const Victim: IVisualTestSuiteNode);
      public
        constructor Create( TreeObj1: TTestSuiteVirtualTreeObj; const Parent1: IVisualTestSuiteNode);
        destructor Destroy; override;
      end;

  private
    [Injection] Context: IComponentContext;

  public
    [Configuration] constructor ServiceModeCreate;
    destructor Destroy; override;
  end;

implementation






{$R *.dfm}


type
RNodeDatum = record
    FToken: IVisualTestSuiteNode;
    FRenderer: INodeRenderer;
  end;
PNodeDatum = ^RNodeDatum;

IVisualTestSuiteNodeEx = interface( IVisualTestSuiteNode)
  ['{AACA02D2-26B9-41F4-8C15-6AA2ECEA90F3}']
    function GetNode: PVirtualNode;
  end;

TVisualTestSuiteNode = class( TInterfacedObject, IVisualTestSuiteNode, IVisualTestSuiteNodeEx)
  private
    FDatum: pointer;
    FNode: PVirtualNode;
    function  GetDatum: pointer;
    procedure SetDatum( Value: pointer);
    function  GetNode: PVirtualNode;
  public
    constructor Create( Node1: PVirtualNode);
  end;

TTestSuiteNodeList = class( TInterfacedObject, IEnumerable, IEnumerable<IVisualTestSuiteNode>)
  private
    FTree:TVirtualStringTree;
    FParentV, FStartV: PVirtualNode;
    FAddCount: integer;
    function GetEnumerator: IEnumerator;
    function GetEnumeratorIntf: IEnumerator<IVisualTestSuiteNode>;
    function IEnumerable<IVisualTestSuiteNode>.GetEnumerator = GetEnumeratorIntf;
  public
    constructor Create( Tree1:TVirtualStringTree; ParentV1, StartV1: PVirtualNode; AddCount1: integer);
  end;

TTestSuiteNodeCursor = class( TInterfacedObject, IEnumerator, IEnumerator<IVisualTestSuiteNode>)
  private
    FTree:TVirtualStringTree;
    FParentV, FStartV: PVirtualNode;
    FAddCount: integer;
    FCurrentV: PVirtualNode;
    FIndex   : integer;

    function  GetCurrent: TObject;
    function  MoveNext: Boolean;
    procedure Reset;
    function  GetCurrentIntf: IVisualTestSuiteNode;
    function  IEnumerator<IVisualTestSuiteNode>.GetCurrent = GetCurrentIntf;
  public
    constructor Create( Tree1:TVirtualStringTree; ParentV1, StartV1: PVirtualNode; AddCount1: integer);
  end;


constructor TTestSuiteVirtualTreeObj.ServiceModeCreate;
var
  Owner: TComponent;
  Parent: TWinControl;
begin
FFrame := TTestSuiteVirtualTree.Create( nil);
FFrame.FObj := self;
FTree := FFrame.Tree;
FFrame.RemoveComponent( FTree);
FFrame.RemoveControl( FTree);
FTree.NodeDataSize := SizeOf( RNodeDatum);
if assigned( Context) then
    begin
    Owner := Context.Owner;
    Parent := Context.Parent;
    FTree.Name := Context.Name;
    if assigned( Owner) and (Owner is TWinControl) and (not assigned( Parent)) then
      Parent := Owner as TWinControl;
    if assigned( Parent) and (not assigned( Owner)) then
      Owner := Parent;
    Context := nil
    end
  else
    begin
    Owner  := nil;
    Parent := nil
    end;
if assigned( Owner) then
  Owner.InsertComponent( FTree);
if assigned( Parent) then
  FTree.Parent := Parent
end;

destructor TTestSuiteVirtualTreeObj.Destroy;
begin
FFrame.Free;
inherited
end;

function TTestSuiteVirtualTreeObj.Change(
  const Parent: IVisualTestSuiteNode): IVisualTestSuiteTreeChangeContext;
begin
result := TChangeContext.Create( self, Parent)
end;

function TTestSuiteVirtualTreeObj.FactoryDisplayName: string;
begin
result := FTree.ClassName
end;

procedure TTestSuiteVirtualTreeObj.InvalidateRendering(
  const DeltaNode: IVisualTestSuiteNode);
begin
FTree.InvalidateNode( (DeltaNode as IVisualTestSuiteNodeEx).GetNode)
end;

function TTestSuiteVirtualTreeObj.Nodes(
  const Parent: IVisualTestSuiteNode): IEnumerable<IVisualTestSuiteNode>;
var
  ParentV: PVirtualNode;
begin
if assigned( Parent) then
    ParentV := (Parent as IVisualTestSuiteNodeEx).GetNode
  else
    ParentV := nil;
result := TTestSuiteNodeList.Create( FTree, ParentV, nil, MaxInt)
end;




constructor TTestSuiteVirtualTreeObj.TChangeContext.Create(
  TreeObj1: TTestSuiteVirtualTreeObj; const Parent1: IVisualTestSuiteNode);
begin
FTreeObj := TreeObj1;
FParent  := Parent1;
FTreeObj.FTree.BeginUpdate;
if assigned( FParent) then
    FParentV := (FParent as IVisualTestSuiteNodeEx).GetNode
  else
    FParentV := nil
end;

destructor TTestSuiteVirtualTreeObj.TChangeContext.Destroy;
begin
FTreeObj.FTree.EndUpdate;
inherited
end;

function TTestSuiteVirtualTreeObj.TChangeContext.Append(
  AddCount: integer): IEnumerable<IVisualTestSuiteNode>;
var
  StartV: PVirtualNode;
begin
with FTreeObj.FTree do
  begin
  StartV := GetLastChildNoInit( FParentV);
  ChildCount[ FParentV] := ChildCount[ FParentV] + AddCount
  end;
if assigned( FParentV) then
  Include( FParentV.States, vsHasChildren);
result := TTestSuiteNodeList.Create( FTreeObj.FTree, FParentV, StartV, AddCount)
end;

procedure TTestSuiteVirtualTreeObj.TChangeContext.AttachRenderer(
  const Newbie: IVisualTestSuiteNode; const Renderer: INodeRenderer);
var
  DatumRec: PNodeDatum;
begin
DatumRec := FTreeObj.FTree.GetNodeData( (Newbie as IVisualTestSuiteNodeEx).GetNode);
DatumRec^.FRenderer := Renderer;
if assigned( Renderer) then
  Renderer.Attached( Newbie)
end;

function TTestSuiteVirtualTreeObj.TChangeContext.ChildNodes: IEnumerable<IVisualTestSuiteNode>;
begin
result := TTestSuiteNodeList.Create( FTreeObj.FTree, FParentV, nil, MaxInt)
end;

procedure TTestSuiteVirtualTreeObj.TChangeContext.Delete(
  const Victim: IVisualTestSuiteNode);
begin
// TODO:

end;


function TTestSuiteVirtualTreeObj.TChangeContext.Insert(
  const Sibling: IVisualTestSuiteNode; Position: TInserPosition;
  AddCount: integer): IVisualTestSuiteNode;
begin
// TODO:

end;

procedure TTestSuiteVirtualTreeObj.BeforePopulate;
begin
FTree.BeginUpdate;
FTree.Clear
end;

procedure TTestSuiteVirtualTreeObj.AfterPopulate;
var
  Node: PVirtualNode;
begin
for Node in FTree.Nodes do
  FTree.Expanded[ Node] := True;
FTree.EndUpdate
end;

procedure TTestSuiteVirtualTree.TreeFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  DatumRec: PNodeDatum;
  Rndr: INodeRenderer;
begin
DatumRec := Sender.GetNodeData( Node);
Rndr := DatumRec^.FRenderer;
if assigned( Rndr) then
  Rndr.Detach;
Finalize( DatumRec^)
end;

procedure TTestSuiteVirtualTree.TreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  DatumRec: PNodeDatum;
  Rndr: INodeRenderer;
begin
if Column <> 0 then exit;
DatumRec := Sender.GetNodeData( Node);
Rndr := DatumRec^.FRenderer;
if not assigned( Rndr) then exit;
case Kind of
  ikNormal,
  ikSelected: ImageIndex := KindImages [ Rndr.GetKind ];
  ikState:    ImageIndex := StateImages[ Rndr.GetState];
  ikOverlay:  ;
  end;
end;

procedure TTestSuiteVirtualTree.TreeGetText(
  Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  DatumRec: PNodeDatum;
  Rndr: INodeRenderer;
  Count: integer;
begin
if TextType <> ttNormal then exit;
DatumRec := Sender.GetNodeData( Node);
Rndr := DatumRec^.FRenderer;
if not assigned( Rndr) then exit;
case Column of
  0: CellText := Rndr.GetDisplayName;
  1: begin
     Count := Rndr.GetFullCycleCount;
     if Count = -1 then
         CellText := ''
       else
         CellText := Format( '%d / %d', [Rndr.GetDoneCycleCount, Count])
     end;
  end;
end;

procedure TTestSuiteVirtualTree.TreeInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
//
end;

{ TVisualTestSuiteNode }

constructor TVisualTestSuiteNode.Create( Node1: PVirtualNode);
begin
FNode := Node1;
FDatum := nil
end;

function TVisualTestSuiteNode.GetDatum: pointer;
begin
result := FDatum
end;

function TVisualTestSuiteNode.GetNode: PVirtualNode;
begin
result := FNode
end;

procedure TVisualTestSuiteNode.SetDatum( Value: pointer);
begin
FDatum := Value
end;


constructor TTestSuiteNodeList.Create(
  Tree1: TVirtualStringTree; ParentV1, StartV1: PVirtualNode; AddCount1: integer);
begin
FTree     := Tree1;
FParentV  := ParentV1;
FStartV   := StartV1;
FAddCount := AddCount1
end;

function TTestSuiteNodeList.GetEnumerator: IEnumerator;
begin
result := nil
end;

function TTestSuiteNodeList.GetEnumeratorIntf: IEnumerator<IVisualTestSuiteNode>;
begin
result := TTestSuiteNodeCursor.Create( FTree, FParentV, FStartV, FAddCount)
end;

{ TTestSuiteNodeCursor }

constructor TTestSuiteNodeCursor.Create(Tree1: TVirtualStringTree; ParentV1,
  StartV1: PVirtualNode; AddCount1: integer);
begin
FTree     := Tree1;
FParentV  := ParentV1;
FStartV   := StartV1;
FAddCount := AddCount1;
Reset
end;

procedure TTestSuiteNodeCursor.Reset;
begin
FCurrentV := FStartV;
FIndex    := 0
end;

function TTestSuiteNodeCursor.GetCurrent: TObject;
begin
result := nil
end;

function TTestSuiteNodeCursor.GetCurrentIntf: IVisualTestSuiteNode;
var
  DatumRec: PNodeDatum;
begin
DatumRec := FTree.GetNodeData( FCurrentV);
result := DatumRec.FToken;
if assigned( result) then exit;
Include( FCurrentV.States, vsInitialUserData);
FCurrentV.CheckType := ctTriStateCheckBox;
result := TVisualTestSuiteNode.Create( FCurrentV);
DatumRec.FToken := result
end;

function TTestSuiteNodeCursor.MoveNext: boolean;
var
  NextV: PVirtualNode;
  s: string;
begin
if FTree.ChildCount[ FParentV] >= 2 then
  begin
  s := '1';
  end;
if assigned( FCurrentV) then
    NextV := FCurrentV^.NextSibling
  else if (not assigned( FStartV)) and assigned( FParentV) then
    NextV := FParentV.FirstChild
  else if not assigned( FStartV) then
    NextV := FTree.GetFirstNoInit
  else
    NextV := nil;
result := assigned( NextV) and (FIndex < FAddCount);
if not result then exit;
FCurrentV := NextV;
Inc( FIndex)
end;


end.
