unit DUnitX.Loggers.GUI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.StdActns, Vcl.ActnCtrls, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnMenus, Vcl.ImgList, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

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
