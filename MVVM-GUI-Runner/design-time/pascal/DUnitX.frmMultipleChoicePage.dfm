object frmMultipleChoice: TfrmMultipleChoice
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  Color = clMoneyGreen
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  DesignSize = (
    451
    304)
  object lblTitle: TStaticText
    Left = 5
    Top = 0
    Width = 212
    Height = 17
    Caption = 'Specify <Title to be set at run-time>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object lblInstruction: TStaticText
    Left = 0
    Top = 20
    Width = 437
    Height = 54
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '<Instruction text to be set at run-time>'
    TabOrder = 1
  end
  object rgChoice: TRadioGroup
    Left = 3
    Top = 80
    Width = 434
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = '<RadioGroup caption to be set at run-time>'
    ItemIndex = 0
    Items.Strings = (
      '<Items to be set...>'
      '<... at run-time>')
    TabOrder = 2
  end
end
