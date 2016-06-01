unit DUnitX.Examples.UITest;

interface

{$I DUnitX.inc}

uses
  {$IFDEF USE_NS}
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  {$ELSE}
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  {$ENDIF}
  DUnitX.TestFramework;

type
  //Normally this class would be in a separate unit from the form, but
  //combining for simplicity and complete example.
  [TestFixture]
  TUITest = class(TObject)
  public
    [Test]
    procedure TestForm;
  end;

  TfrmUITest = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TUITest.TestForm;
var
  TestForm: TfrmUITest;
  i: integer;
begin
  TestForm := TfrmUITest.Create(nil);
  try
    //Check everything is blank by default
    TDUnitX.CurrentRunner.Log('Checking initial state of UI');
    Assert.IsEmpty(TestForm.Edit1.Text);
    Assert.IsEmpty(TestForm.Memo1.Text);

    TDUnitX.CurrentRunner.Log('Testing button');
    for i := 1 to 3 do
    begin
      TestForm.Edit1.Text := 'Test ' + IntToStr(i);
      TestForm.Button1.Click;
    end;

    TDUnitX.CurrentRunner.Log('Checking results');
    Assert.AreEqual(3, TestForm.Memo1.Lines.Count);
    Assert.AreEqual('Test 1'#13#10'Test 2'#13#10'Test 3'#13#10, TestForm.Memo1.Text);
  finally
    TestForm.Free;
  end;
end;

procedure TfrmUITest.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add(Edit1.Text);
end;

initialization
  TDUnitX.RegisterTestFixture(TUITest);

end.
