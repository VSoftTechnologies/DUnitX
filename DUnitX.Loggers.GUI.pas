unit DUnitX.Loggers.GUI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ActnList, StdActns, ActnCtrls, ToolWin, ActnMan, ActnMenus, ImgList, PlatformDefaultStyleActnCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    ActionManager1: TActionManager;
    MainImageList: TImageList;
    ActionMainMenuBar1: TActionMainMenuBar;
    ActionToolBar1: TActionToolBar;
    FileExit1: TFileExit;
    MainPanel: TPanel;
    StatusBar1: TStatusBar;
    TreeviewPanel: TPanel;
    TreeView1: TTreeView;
    ReportAreaPanel: TPanel;
    ProgressPanel: TPanel;
    MainSplitter: TSplitter;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
