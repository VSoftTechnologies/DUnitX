unit DUnitX.frmReadMemoPage;

interface

uses
  {$if CompilerVersion >= 23}
    // XE2+
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ActnMan,
  {$else}
    // D2010, XE
    Graphics, Controls, Forms, Dialogs, StdCtrls, ActnMan,
  {$ifend}
    Windows, Messages, SysUtils, Variants, Classes, DUnitX.WizardPageIntf,
    DUnitX.WizardStates;

type
TInitItemSelectionProc  = reference to procedure( const PassInState: IInterface; var MemoText: string);

  TfrmReadMemo = class( TFrame, IWizardPage)
    memoReadMe: TMemo;
  private
    FPageId: string;
    FNextPage: string;
    FPassInState: IInterface;

  private
    function  Frame: TFrame;
    function  Can_Back( const PageId: string): boolean;
    function  IsValid( const PageId: string; Level: TValidationLevel; const State: IInterface): boolean;
    function  isFinal( const PageId: string): boolean;
    function  Post( const PageId: string; var State: IInterface; var NextPageId: string): boolean;

  public type
    TFactory = class( TInterfacedObject, IWizardPageFactory)
      private
        FPageId: string;
        FNextPage: string;
        FOnInit: TInitItemSelectionProc;
        function  handlesPageId( const PageId: string): boolean;
        function  GetPage( const PageId: string; AOwner: TComponent; AParent: TWinControl; AColours: TCustomActionBarColorMap; const State: IInterface; isForward: boolean): IWizardPage;
        procedure ReleasePage( const PageId: string; var Page: IWizardPage);
      public
        constructor Create( const PageId1, NextPage1: string; OnInit1: TInitItemSelectionProc);
      end;

  private
    function  GetText: string;
    procedure SetText( const Value: string);

  public
    property Text: string read GetText write SetText;
  end;

implementation









{$R *.dfm}


function TfrmReadMemo.Can_Back( const PageId: string): boolean;
begin
result := True
end;

function TfrmReadMemo.Frame: TFrame;
begin
result := self
end;

function TfrmReadMemo.GetText: string;
begin
result := memoReadMe.Lines.Text
end;

function TfrmReadMemo.isFinal( const PageId: string): boolean;
begin
result := FNextPage = ''
end;

function TfrmReadMemo.IsValid(
  const PageId: string; Level: TValidationLevel; const State: IInterface): boolean;
begin
result := True
end;

function TfrmReadMemo.Post(
  const PageId: string; var State: IInterface; var NextPageId: string): boolean;
begin
result := True;
State := FPassInState;
NextPageId := FNextPage
end;

procedure TfrmReadMemo.SetText( const Value: string);
begin
memoReadMe.Lines.Text := Value
end;


constructor TfrmReadMemo.TFactory.Create(
  const PageId1, NextPage1: string; OnInit1: TInitItemSelectionProc);
begin
FPageId   := PageId1;
FNextPage := NextPage1;
FOnInit   := OnInit1
end;

function TfrmReadMemo.TFactory.GetPage(
  const PageId: string; AOwner: TComponent;
  AParent: TWinControl; AColours: TCustomActionBarColorMap;
  const State: IInterface; isForward: boolean): IWizardPage;
var
  Construct: TfrmReadMemo;
  Init: IWizardInitialState;
  Imtm: IWizardIntermediateState;
  MemoText: string;
begin
Construct := TfrmReadMemo.Create( AOwner);
result    := Construct as IWizardPage;
if Construct.Parent <> AParent then
  Construct.Parent := AParent;
Construct.Align := alClient;
if Supports( State, IWizardIntermediateState, Imtm) then
    Construct.FPassInState := Imtm.Clone
  else if Supports( State, IWizardInitialState, Init) then
    Construct.FPassInState := CreateWizardIntermediateState( Init.Data)
  else
    Construct.FPassInState := State;
Construct.FPageId   := FPageId;
Construct.FNextPage := FNextPage;
MemoText := Construct.Text;
if assigned( FOnInit) then
  FOnInit( State, MemoText);
Construct.Text := MemoText
end;

function TfrmReadMemo.TFactory.handlesPageId( const PageId: string): boolean;
begin
result := PageId = FPageId
end;

procedure TfrmReadMemo.TFactory.ReleasePage(
  const PageId: string; var Page: IWizardPage);
var
  Obj: TObject;
begin
if assigned( Page) then
    Obj := Page.Frame
  else
    Obj := nil;
Page := nil;
Obj.Free
end;

end.
