object frmPathPage: TfrmPathPage
  Left = 0
  Top = 0
  Width = 295
  Height = 214
  Color = clMoneyGreen
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  DesignSize = (
    295
    214)
  object lblLocation: TLabel
    Left = 18
    Top = 61
    Width = 44
    Height = 13
    Caption = 'Location:'
    FocusControl = edtLocation
  end
  object edtLocation: TButtonedEdit
    Left = 76
    Top = 58
    Width = 213
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Images = imglstButtons16x16
    RightButton.HotImageIndex = 0
    RightButton.ImageIndex = 0
    RightButton.PressedImageIndex = 0
    RightButton.Visible = True
    TabOrder = 0
    Text = 'edtLocation'
    OnRightButtonClick = edtLocationRightButtonClick
  end
  object lstbxSelectDirectory: TDirectoryOutline
    Left = 120
    Top = 61
    Width = 169
    Height = 148
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    Options = [ooDrawFocusRect]
    PictureLeaf.Data = {
      46030000424D460300000000000036000000280000000E0000000E0000000100
      2000000000001003000000000000000000000000000000000000800080008000
      8000800080008000800080008000800080008000800080008000800080008000
      8000800080008000800080008000800080008000800080008000800080008000
      8000800080008000800080008000800080008000800080008000800080008000
      8000800080008000800080008000800080008000800080008000800080008000
      8000800080008000800080008000800080008000800080008000800080008000
      8000800080000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008000800080008000800080000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF000000000080008000800080008000800000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000008000
      800080008000800080000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000080008000800080008000
      800000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF00000000008000800080008000800080000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF000000000080008000800080008000800000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000008000
      8000800080008000800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008000800080008000800080008000
      80008000800000000000FFFFFF0000FFFF00FFFFFF0000FFFF00000000008000
      8000800080008000800080008000800080008000800080008000800080008080
      8000000000000000000000000000000000008080800080008000800080008000
      8000800080008000800080008000800080008000800080008000800080008000
      8000800080008000800080008000800080008000800080008000800080008000
      80008000800080008000}
    TabOrder = 1
    Visible = False
    OnExit = lstbxSelectDirectoryExit
    Data = {10}
  end
  object lblTitle: TStaticText
    Left = 5
    Top = 0
    Width = 184
    Height = 17
    Caption = 'Specify DUnitX Source Directory'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
  end
  object lblInstruction: TStaticText
    Left = 8
    Top = 23
    Width = 281
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Select the DUnitX source directory. This is the directory that c' +
      'ontains the file DUnitX.TestFramework.pas'
    TabOrder = 3
  end
  object imglstButtons16x16: TImageList
    Left = 96
    Top = 144
    Bitmap = {
      494C010101000500080010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      000000000000000000000000000000000000000000000000000000000000078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE001594C300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000078DBE0047BA
      E800078DBE0096DEFD0061CBF70064CDF70064CDF80065CDF70065CDF80066CE
      F8005EC9F40050B9E0008BD5EA00078DBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000078DBE004FC0
      EB00078DBE00A1E6FF006ED5FA0071D6FB0071D6FA0072D6FB0072D6FB0073D7
      FC006AD2F70057BFE10093DAEB00078DBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000078DBE00078DBE0055C4
      EB00078DBE00A8ECFF0078DDFB007CDEFB007CDEFB007CDEFB007DDEFB007DDF
      FB0074DAF8005EC4E10097DDEB00078DBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE0047BAE800078DBE005CCA
      ED00078DBE00ABF2FF007FE5FB0083E6FC0082E6FC0083E6FC0082E6FC0083E7
      FD007AE2F9005CC6E10098E0EB00078DBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE004FC0EB00078DBE0060CF
      EC00078DBE00EFFFFF00B9FAFF00BDFBFF00BEFBFF00BDFBFF00BDFBFF00BFFC
      FF00B5F6FF009ADCEC00BAEBF300078DBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE0055C4EB00078DBE006DD8
      EF00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE005CCAED00078DBE008DEE
      FA008DEEFA008DEEFA008DEEFA008DEEFA008DEEFA008DEEFA008DEEFA008DEE
      FA008DEEFA00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE0060CFEC00078DBE00DEFE
      FF00ABFFFF00ACFFFF00ACFFFF00AEFFFF00AEFFFF00AFFFFF00AFFFFF00B2FF
      FF00A4FDFF00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE006DD8EF00078DBE00078D
      BE00DEFDFE009FFCFE00AEFDFE0062CEE400078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE008DEEFA008DEEFA008DEE
      FA00078DBE00078DBE00078DBE00078DBE008DEEFA008DEEFA008DEEFA00078D
      BE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE00DEFEFF00ABFFFF00ACFF
      FF00ACFFFF00AEFFFF00AEFFFF00AFFFFF00AFFFFF00B2FFFF00A4FDFF00078D
      BE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000078DBE00DEFDFE009FFC
      FE00AEFDFE0062CEE400078DBE00078DBE00078DBE00078DBE00078DBE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00E001000000000000C000000000000000
      C000000000000000800000000000000000000000000000000000000000000000
      0000000000000000000300000000000000030000000000000003000000000000
      000F000000000000000F000000000000801F000000000000C3FF000000000000
      FFFF000000000000FFFF00000000000000000000000000000000000000000000
      000000000000}
  end
end
