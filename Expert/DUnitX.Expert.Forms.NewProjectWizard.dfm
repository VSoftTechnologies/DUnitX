object frmDunitXNewProject: TfrmDunitXNewProject
  Left = 0
  Top = 0
  Caption = 'New DUnitX Project Wizard'
  ClientHeight = 324
  ClientWidth = 284
  Color = clBtnFace
  Constraints.MinHeight = 145
  Constraints.MinWidth = 250
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    284
    324)
  PixelsPerInch = 96
  TextHeight = 13
  object gbTestUnitOpt: TGroupBox
    Left = 8
    Top = 160
    Width = 268
    Height = 125
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Test Unit Options'
    TabOrder = 3
    DesignSize = (
      268
      125)
    object lblClassName: TLabel
      Left = 16
      Top = 72
      Width = 113
      Height = 13
      Caption = 'TestFixture Class Name'
    end
    object chkCreateSetupTearDown: TCheckBox
      Left = 16
      Top = 24
      Width = 236
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Create Setup and TearDown Methods'
      TabOrder = 0
    end
    object chkCreateSampleMethods: TCheckBox
      Left = 16
      Top = 48
      Width = 236
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Create Sample Test Methods'
      TabOrder = 1
    end
    object edtClassName: TEdit
      Left = 16
      Top = 88
      Width = 236
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
  end
  object btnOK: TButton
    Left = 120
    Top = 292
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 200
    Top = 292
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object chkCreateTestUnit: TCheckBox
    Left = 8
    Top = 136
    Width = 268
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Create Test Unit'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = chkCreateTestUnitClick
  end
  object chkAddToProjectGroup: TCheckBox
    Left = 8
    Top = 8
    Width = 268
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Add to Existing Project Group'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object rgReportLeakOptions: TRadioGroup
    Left = 8
    Top = 31
    Width = 268
    Height = 90
    Caption = 'Report Leak Options'
    ItemIndex = 0
    Items.Strings = (
      'Nome'
      'FastMM 4'
      'FastMM 5')
    TabOrder = 1
  end
end
