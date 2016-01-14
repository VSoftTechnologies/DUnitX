object frmDunitXNewProject: TfrmDunitXNewProject
  Left = 0
  Top = 0
  Caption = 'New DUnitX Project Wizard'
  ClientHeight = 206
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
    206)
  PixelsPerInch = 96
  TextHeight = 13
  object gbTestUnitOpt: TGroupBox
    Left = 8
    Top = 56
    Width = 268
    Height = 113
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Test Unit Options'
    TabOrder = 2
    DesignSize = (
      268
      113)
    object lblClassName: TLabel
      Left = 16
      Top = 70
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
      Top = 47
      Width = 236
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Create Sample Test Methods'
      TabOrder = 1
    end
    object edtClassName: TEdit
      Left = 16
      Top = 89
      Width = 236
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
  end
  object btnOK: TButton
    Left = 120
    Top = 173
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 201
    Top = 173
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object chkCreateTestUnit: TCheckBox
    Left = 8
    Top = 33
    Width = 268
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Create Test Unit'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = chkCreateTestUnitClick
  end
  object chkAddToProjectGroup: TCheckBox
    Left = 8
    Top = 10
    Width = 268
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Add to Existing Project Group'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
end
