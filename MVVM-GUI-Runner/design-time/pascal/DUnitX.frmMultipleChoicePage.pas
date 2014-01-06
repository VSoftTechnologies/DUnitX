unit DUnitX.frmMultipleChoicePage;

interface

uses
  {$if CompilerVersion >= 23}
    // XE2+
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.ActnMan, Vcl.StdCtrls, Vcl.ExtCtrls,
  {$else}
    // D2010, XE
    Graphics, Controls, Forms, Dialogs, ActnMan, StdCtrls, ExtCtrls,
  {$ifend}
    Windows, Messages, SysUtils, Variants, Classes, DUnitX.WizardPageIntf;

type
TParseItemSelectionProc = reference to procedure( const PassInState: IInterface; SelectedItem: integer; var PassOutState: IInterface);
TInitItemSelectionProc  = reference to procedure( const PassInState: IInterface; var SelectedItem: integer);

  TfrmMultipleChoice = class( TFrame, IWizardPage)
    lblTitle: TStaticText;
    lblInstruction: TStaticText;
    rgChoice: TRadioGroup;
  private
    function  Frame: TFrame;
    function  Can_Back( const PageId: string): boolean;
    function  IsValid( const PageId: string; Level: TValidationLevel; const State: IInterface): boolean;
    function  isFinal( const PageId: string): boolean;
    function  Post( const PageId: string; var State: IInterface; var NextPageId: string): boolean;

  public
    FPageId: string;
    FNextPage: string;

  public type
    TFactory = class( TInterfacedObject, IWizardPageFactory)
      private
        FPageId, FNextId: string;
        FTitle: string;
        FInstructionText: string;
        FGroupCaption: string;
        FItems: TStrings;
        FParseItemSelectionProc: TParseItemSelectionProc;
        FInitItemSelectionProc : TInitItemSelectionProc;

        function  handlesPageId( const PageId: string): boolean;
        function  GetPage( const PageId: string; AOwner: TComponent; AParent: TWinControl; AColours: TCustomActionBarColorMap; const State: IInterface; isForward: boolean): IWizardPage;
        procedure ReleasePage( const PageId: string; var Page: IWizardPage);

      public
        constructor Create( const PageId1, NextId1, Title1, InstructionText1, GroupCaption1, Items1: string;
          InitItemSelectionProc1: TInitItemSelectionProc;
          ParseItemSelectionProc1: TParseItemSelectionProc);
        destructor  Destroy; override;
      end;

  {$REGION 'property accessors'}
  private
    function  GetItemIndex: integer;
    procedure SetItemIndex( Value: integer);
  {$ENDREGION}

  private
    FPassInState: IInterface;
    FParseItemSelectionProc: TParseItemSelectionProc;

  public
    property ItemIndex: integer   read GetItemIndex write SetItemIndex;
  end;

implementation

























{$R *.dfm}


function TfrmMultipleChoice.Can_Back( const PageId: string): boolean;
begin
result := True
end;

function TfrmMultipleChoice.Frame: TFrame;
begin
result := self
end;

function TfrmMultipleChoice.GetItemIndex: integer;
begin
result := rgChoice.ItemIndex
end;

function TfrmMultipleChoice.isFinal( const PageId: string): boolean;
begin
result := FNextPage = ''
end;

function TfrmMultipleChoice.IsValid(
  const PageId: string; Level: TValidationLevel; const State: IInterface): boolean;
begin
result := True
end;

function TfrmMultipleChoice.Post(
  const PageId: string; var State: IInterface; var NextPageId: string): boolean;
begin
result := True;
NextPageId := FNextPage;
if assigned( FParseItemSelectionProc) then
    FParseItemSelectionProc( FPassInState, ItemIndex, State)
  else
    State := nil
end;

procedure TfrmMultipleChoice.SetItemIndex( Value: integer);
begin
rgChoice.ItemIndex := Value
end;


constructor TfrmMultipleChoice.TFactory.Create(
  const PageId1, NextId1, Title1, InstructionText1, GroupCaption1, Items1: string;
  InitItemSelectionProc1: TInitItemSelectionProc;
  ParseItemSelectionProc1: TParseItemSelectionProc);
begin
FPageId := PageId1;
FNextId := NextId1;
FTitle  := Title1;
FInstructionText := InstructionText1;
FGroupCaption := GroupCaption1;
FParseItemSelectionProc := ParseItemSelectionProc1;
FInitItemSelectionProc  := InitItemSelectionProc1;
FItems := TStringList.Create;
FItems.StrictDelimiter := True;
FItems.Delimiter := Items1[1];
FItems.DelimitedText := Copy( Items1, 2, MaxInt)
end;

destructor TfrmMultipleChoice.TFactory.Destroy;
begin
FItems.Free;
inherited
end;

function TfrmMultipleChoice.TFactory.GetPage(
  const PageId: string;
  AOwner: TComponent; AParent: TWinControl; AColours: TCustomActionBarColorMap;
  const State: IInterface; isForward: boolean): IWizardPage;
var
  Construct: TfrmMultipleChoice;
  i: integer;
begin
Construct := TfrmMultipleChoice.Create( AOwner);
Construct.lblTitle.Caption := FTitle;
Construct.FPageId   := PageId;
Construct.FNextPage := self.FNextId;
Construct.lblInstruction.Caption := FInstructionText;
Construct.FParseItemSelectionProc := FParseItemSelectionProc;
Construct.rgChoice.Caption := FGroupCaption;
Construct.rgChoice.Items := FItems;
i := 0;
Construct.FPassInState := State;
if assigned( FInitItemSelectionProc) then
  FInitItemSelectionProc( Construct.FPassInState, i);
Construct.ItemIndex := i;
if Construct.Parent <> AParent then
   Construct.Parent := AParent;
result := Construct
end;

function TfrmMultipleChoice.TFactory.handlesPageId(
  const PageId: string): boolean;
begin
result := PageId = FPageId
end;

procedure TfrmMultipleChoice.TFactory.ReleasePage(
  const PageId: string; var Page: IWizardPage);
begin
Page.Frame.Free;
Page := nil
end;

end.
