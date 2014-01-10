unit DUnitX.fmProjectWizard;

interface

uses
  {$if CompilerVersion >= 23}
    // XE2+
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnMan, Vcl.ActnColorMaps,
    Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnList, Vcl.ImgList, Vcl.ExtCtrls,
    Vcl.ToolWin, Vcl.ActnCtrls,
    {$if CompilerVersion >= 24}
      System.Actions,
    {$ifend}
  {$else}
    // D2010, XE
    Graphics, Controls, Forms, Dialogs, ActnMan, ActnColorMaps,
    PlatformDefaultStyleActnCtrls, ActnList, ImgList, ExtCtrls,
    ToolWin, ActnCtrls,
  {$ifend}
    Windows, Messages, SysUtils, Variants, Classes, DUnitX.IoC, DUnitX.Generics,
    Generics.Collections, DUnitX.WizardPageIntf, DUnitX.WizardStates;

type
  TfmProjectWizard = class(TForm)
    tlbarNav: TActionToolBar;
    pnlPageClient: TPanel;
    imglstGlyphs16x16: TImageList;
    actmngrWizardActions: TActionManager;
    clmapTwilight: TTwilightColorMap;
    actBack: TAction;
    actNext: TAction;
    actFinish: TAction;
    actCancel: TAction;
    procedure actBackExecute(Sender: TObject);
    procedure actBackUpdate(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actNextUpdate(Sender: TObject);
    procedure actFinishExecute(Sender: TObject);
    procedure actFinishUpdate(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private type
    THistorian = class abstract( TObject)
      public
        procedure RecordPassage( Wiz: TfmProjectWizard); virtual; abstract;
        class function isForward: boolean;               virtual; abstract;
      end;

  private
    function  FindFactory( const PageToFind: string; var Finding: IWizardPageFactory): boolean;
    procedure TurnToPage( const NewPageId: string; DirectionalHistorian: THistorian);
    procedure ReleasePage;

  public
    FServices: TDUnitXIoC;   // Provides IWizardPageFactoryGang
    FStartPage: string;
    FEnvironment: IWizardInitialState;
    FUserData: IWizardFinalState;

  private type
    RHistoricalState = record
      FPage: string;
      FState: IInterface;
      constructor Create( const Page1: string; const State1: IInterface);
      end;

  private
    FCurrentPageId: string;
    FGang: IWizardPageFactoryGang;
    FPage: IWizardPage;
    FFactory: IWizardPageFactory;
    FHistory: TStack<RHistoricalState>;
    FState: IInterface;
  end;

var
  fmProjectWizard: TfmProjectWizard;

implementation

























{$R *.dfm}

type
TForwardHistorian = class sealed( TfmProjectWizard.THistorian)
  public
    procedure RecordPassage( Wiz: TfmProjectWizard); override;
    class function isForward: boolean;               override;
  end;

TReverseHistorian = class sealed( TfmProjectWizard.THistorian)
  private
    FRollbackState: IInterface;
  public
    procedure RecordPassage( Wiz: TfmProjectWizard); override;
    class function isForward: boolean;               override;
    constructor Create( const RollbackState1: IInterface);
  end;


procedure TfmProjectWizard.FormShow( Sender: TObject);
begin
if assigned( FPage) then Exit;
FGang := FServices.Resolve<IWizardPageFactoryGang>;
FHistory.Clear;
FCurrentPageId := '';
TurnToPage( FStartPage, TReverseHistorian.Create( FEnvironment))
end;


function TfmProjectWizard.FindFactory(
  const PageToFind: string; var Finding: IWizardPageFactory): boolean;
var
  Factory: IWizardPageFactory;
begin
result := False;
if not assigned( FGang) then exit;
for Factory in FGang do
  begin
  result := Factory.handlesPageId( PageToFind);
  if not result then continue;
  Finding := Factory;
  break
  end
end;


procedure TfmProjectWizard.TurnToPage( const NewPageId: string; DirectionalHistorian: THistorian);
var
  OldFactory : IWizardPageFactory;
  OldPage    : IWizardPage;
  OldPageId  : string;
begin
OldFactory := FFactory;
OldPage    := FPage;
OldPageId  := FCurrentpageId;
if not FindFactory( NewPageId, FFactory) then exit;
try
  DirectionalHistorian.RecordPassage( self);
  FCurrentPageId := NewPageId;
  if assigned( OldPage) and assigned( OldPage.Frame) and
     (OldPage.Frame.Name <> '') then
     OldPage.Frame.Name := '';
  FPage := FFactory.GetPage( FCurrentPageId, self, pnlPageClient, clmapTwilight, FState, DirectionalHistorian.isForward);
  if assigned( OldFactory) and assigned( OldPage) then
    OldFactory.ReleasePage( OldPageId, OldPage)
finally
  DirectionalHistorian.Free
  end
end;

procedure TfmProjectWizard.FormCreate(Sender: TObject);
begin
FHistory := TStack<RHistoricalState>.Create
end;

procedure TfmProjectWizard.FormDestroy( Sender: TObject);
begin
ReleasePage;
FHistory.Free
end;


procedure TfmProjectWizard.ReleasePage;
begin
if (not assigned( FFactory)) or (not assigned( FPage)) then exit;
FFactory.ReleasePage( FCurrentPageId, FPage);
FPage := nil;
FCurrentPageId := ''
end;


procedure TfmProjectWizard.actBackExecute( Sender: TObject);
var
  Subtractend: RHistoricalState;
begin
Subtractend := FHistory.Pop;
TurnToPage( Subtractend.FPage, TReverseHistorian.Create( Subtractend.FState))
end;

procedure TfmProjectWizard.actBackUpdate( Sender: TObject);
begin
(Sender as TAction).Enabled := (FHistory.Count > 0) and
         ((not assigned( FPage)) or FPage.Can_Back( FCurrentPageId))
end;

procedure TfmProjectWizard.actCancelExecute( Sender: TObject);
begin
ModalResult := mrCancel
end;

procedure TfmProjectWizard.actFinishExecute(Sender: TObject);
var
  NextPage: string;
  DataIntf: IWizardFinalState;
  Im: IWizardIntermediateState;
begin
if FPage.IsValid( FCurrentPageId, vThorough, FState) and
   FPage.Post( FCurrentPageId, FState, NextPage) and
   (Supports( FState, IWizardFinalState) or Supports( FState, IWizardIntermediateState)) then
   begin
   if Supports( FState, IWizardFinalState, DataIntf) then
       FUserData := DataIntf
     else if Supports( FState, IWizardIntermediateState, Im) then
       FUserData := Im.FinalState
   end;
ModalResult := mrOk
end;

procedure TfmProjectWizard.actFinishUpdate(Sender: TObject);
begin
(Sender as TAction).Enabled :=
  assigned( FPage) and
  FPage.IsValid( FCurrentPageId, vQuick, FState) and
  FPage.isFinal( FCurrentPageId)
end;

procedure TfmProjectWizard.actNextExecute( Sender: TObject);
var
  NextPage: string;
begin
if FPage.IsValid( FCurrentPageId, vThorough, FState) and
   FPage.Post( FCurrentPageId, FState, NextPage) then
     TurnToPage( NextPage, TForwardHistorian.Create)
end;

procedure TfmProjectWizard.actNextUpdate(Sender: TObject);
begin
(Sender as TAction).Enabled :=
  assigned( FPage) and
  FPage.IsValid( FCurrentPageId, vQuick, FState) and
  (not FPage.isFinal( FCurrentPageId))
end;



constructor TfmProjectWizard.RHistoricalState.Create(
  const Page1: string; const State1: IInterface);
begin
FPage  := Page1;
FState := State1
end;

// Inputs:
//   isNewProjectGroup
//   CurrentProjectName
//   CurrentProjectPath
//   CurrentPlatform
//   BDSProjectPath
//
// State
//   isNewProjectGroup
//   CurrentProjectName
//   CurrentProjectPath
//   CurrentPlatform
//   BDSProjectPath
//   ProjectName
//   Location
//   Application Title
//   Tree choice
//   Platform choice
//   View choice
//   Clone()
//   getter and setter access for all members.
//
//    Page 1
//    =======
//     ProjectName
//     Location
//     Add to project group (checkbox)
//     Application Title
//
//    Page 2
//    =======
//     Tree: (1) bundled tvirtualstringtree; (2) own tvirtualstringtree; (3) ttreeview
//
//    Page 3
//    =======
//     Target: Win32, Win64
//
//    Page 4
//    =======
//     Test Runner:
//          GUI
//        Console
//
//    Page 5
//    =======
//      Summary
//
//    Page 6
//    =======
//      Read-Me


class function TForwardHistorian.isForward: boolean;
begin
result := True
end;

procedure TForwardHistorian.RecordPassage( Wiz: TfmProjectWizard);
var
  Addend: TfmProjectWizard.RHistoricalState;
begin
Addend.FPage  := Wiz.FCurrentPageId;
Addend.FState := Wiz.FState;
Wiz.FHistory.Push( Addend)
end;


constructor TReverseHistorian.Create( const RollbackState1: IInterface);
begin
FRollbackState := RollbackState1
end;

class function TReverseHistorian.isForward: boolean;
begin
result := False
end;

procedure TReverseHistorian.RecordPassage( Wiz: TfmProjectWizard);
begin
Wiz.FState := FRollbackState
end;

end.
