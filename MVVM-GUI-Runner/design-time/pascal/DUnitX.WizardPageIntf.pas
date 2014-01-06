unit DUnitX.WizardPageIntf;
interface
uses
  {$if CompilerVersion >= 23}
    // XE2+
    Vcl.Forms, Vcl.Controls, Vcl.ActnMan,
  {$else}
    // D2010, XE
    Forms, Controls, ActnMan,
  {$ifend}
  Classes, DUnitX.Generics;

type

TValidationLevel = (vQuick, vThorough);
IWizardPage = interface
  ['{DBA4D9D4-5C71-4C7A-A716-016B0E1390BE}']
    function  Frame: TFrame;
    function  Can_Back( const PageId: string): boolean;
    function  IsValid( const PageId: string; Level: TValidationLevel; const State: IInterface): boolean;
    function  isFinal( const PageId: string): boolean;
    function  Post( const PageId: string; var State: IInterface; var NextPageId: string): boolean;
  end;

IWizardPageFactory = interface
  ['{5C8300F5-3109-40D1-97AE-A6142076CC52}']
    function  handlesPageId( const PageId: string): boolean;
    function  GetPage( const PageId: string; AOwner: TComponent; AParent: TWinControl; AColours: TCustomActionBarColorMap; const State: IInterface; isForward: boolean): IWizardPage;
    procedure ReleasePage( const PageId: string; var Page: IWizardPage);
  end;

IWizardPageFactoryGang = interface( IList<IWizardPageFactory>)
  ['{91DDFF87-7CC8-43E2-AAF8-23B1CB0A31C3}']
  end;


implementation





end.
