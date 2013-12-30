object frmDunitXNewUnit: TfrmDunitXNewUnit
  Left = 0
  Top = 0
  Caption = 'New Unit Wizard'
  ClientHeight = 126
  ClientWidth = 250
  Color = clBtnFace
  Constraints.MinHeight = 145
  Constraints.MinWidth = 250
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    250
    126)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 234
    Height = 76
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Test Unit Options'
    TabOrder = 0
    DesignSize = (
      234
      76)
    object CheckBox1: TCheckBox
      Left = 16
      Top = 24
      Width = 202
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Create Setup and TearDown Methods'
      TabOrder = 0
      ExplicitWidth = 313
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 47
      Width = 202
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Create Sample Test Methods'
      TabOrder = 1
      ExplicitWidth = 313
    end
  end
  object Button1: TButton
    Left = 86
    Top = 93
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    ExplicitLeft = 286
    ExplicitTop = 269
  end
  object Button2: TButton
    Left = 167
    Top = 93
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    ExplicitLeft = 367
    ExplicitTop = 269
  end
end
