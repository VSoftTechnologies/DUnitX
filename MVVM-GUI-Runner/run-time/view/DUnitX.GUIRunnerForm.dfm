object mfmGUIRunner: TmfmGUIRunner
  Left = 0
  Top = 0
  Caption = 'MVVM Gui Runner'
  ClientHeight = 611
  ClientWidth = 898
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object splTestCases: TSplitter
    Left = 0
    Top = 438
    Width = 898
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 137
    ExplicitWidth = 148
  end
  object tlbrMain: TActionToolBar
    Left = 0
    Top = 0
    Width = 898
    Height = 23
    ActionManager = actmngrMain
    Caption = 'Main ToolBar'
    ColorMap.HighlightColor = 15660791
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 15660791
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Spacing = 0
  end
  object sbarMain: TStatusBar
    Left = 0
    Top = 592
    Width = 898
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Text = 'Idle'
        Width = 200
      end
      item
        Alignment = taRightJustify
        Text = 'Test cases = 0'
        Width = 100
      end
      item
        Text = 'of 1'
        Width = 100
      end>
  end
  object memoLog: TRichEdit
    Left = 0
    Top = 441
    Width = 898
    Height = 134
    Align = alBottom
    BorderStyle = bsNone
    Color = clInfoBk
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      '[Log output goes here.]')
    ParentFont = False
    PopupMenu = popupLogMemo
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object pnlTestCases: TPanel
    Left = 0
    Top = 23
    Width = 898
    Height = 415
    Align = alClient
    Caption = 'Tree of test cases goes here'
    TabOrder = 3
  end
  object pbarTests: TProgressBar
    Left = 0
    Top = 575
    Width = 898
    Height = 17
    Align = alBottom
    TabOrder = 4
  end
  object appevMain: TApplicationEvents
    OnIdle = appevMainIdle
    Left = 32
    Top = 184
  end
  object actmngrMain: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = actSelectAll
            Caption = '&Select all'
          end
          item
            Action = actSelectFailed
            Caption = 'S&elect failed'
          end
          item
            Action = actToggleSelection
            Caption = '&Toggle selections'
          end
          item
            Action = actClear
            Caption = '&Clear selections'
          end
          item
            Caption = '-'
          end
          item
            Action = actRun
            Caption = '&Run'
          end
          item
            Action = actAbort
            Caption = '&Abort'
          end
          item
            Caption = '-'
          end>
        ActionBar = tlbrMain
      end>
    Images = imglstGlyphs16x16
    Left = 112
    Top = 184
    StyleName = 'XP Style'
    object actRun: TAction
      Category = 'Operation'
      Caption = 'Run'
      OnExecute = actRunExecute
    end
    object actAbort: TAction
      Category = 'Operation'
      Caption = 'Abort'
      OnExecute = actAbortExecute
      OnUpdate = actAbortUpdate
    end
    object actSelectAll: TAction
      Category = 'Selection'
      Caption = 'Select all'
      OnExecute = actSelectAllExecute
    end
    object actSelectFailed: TAction
      Category = 'Selection'
      Caption = 'Select failed'
      OnExecute = actSelectFailedExecute
      OnUpdate = actSelectFailedUpdate
    end
    object actClear: TAction
      Category = 'Selection'
      Caption = 'Clear selections'
      OnExecute = actClearExecute
      OnUpdate = actClearUpdate
    end
    object actToggleSelection: TAction
      Category = 'Selection'
      Caption = 'Toggle selections'
      OnExecute = actToggleSelectionExecute
    end
    object actAttachLogger: TAction
      Category = 'Attach'
      Caption = 'Attach new %s logger'
      OnExecute = actAttachLoggerExecute
    end
    object actDetachLogger: TAction
      Category = 'Per Secondary Logger'
      Caption = 'Detach'
    end
    object actEditLoggerProps: TAction
      Category = 'Per Secondary Logger'
      Caption = 'Logger properties'
    end
    object actPrimaryLvlInformation: TAction
      Category = 'Primary Log Level'
      Caption = 'Information'
      Checked = True
      GroupIndex = 1
    end
    object actPrimaryLvlWarning: TAction
      Category = 'Primary Log Level'
      Caption = 'Warning'
      GroupIndex = 1
    end
    object actPrimaryLvlError: TAction
      Category = 'Primary Log Level'
      Caption = 'Error'
      GroupIndex = 1
    end
    object actSecondaryLvlInformation: TAction
      Category = 'Per Secondary Log Level'
      Caption = 'Information'
    end
    object actSecondaryLvlWarning: TAction
      Category = 'Per Secondary Log Level'
      Caption = 'Warning'
    end
    object actSecondaryLvlError: TAction
      Category = 'Per Secondary Log Level'
      Caption = 'Error'
    end
    object actHaltOnFirstFailure: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Halt on first failure'
      Checked = True
    end
    object actFailOnLeak: TAction
      Category = 'Options'
      AutoCheck = True
      Caption = 'Fail on memory leak'
    end
    object actNotYetDeveloped: TAction
      Category = 'Options'
      Caption = '(placemarker - Not yet developed)'
    end
    object actAbout: TAction
      Category = 'Options'
      Caption = 'About ...'
    end
    object actClearLog: TAction
      Category = 'Log'
      Caption = 'Clear'
      OnExecute = actClearLogExecute
    end
  end
  object coloursXP: TXPColorMap
    HighlightColor = 15660791
    BtnSelectedColor = clBtnFace
    UnusedColor = 15660791
    Left = 200
    Top = 184
  end
  object imglstGlyphs16x16: TImageList
    Left = 296
    Top = 184
    Bitmap = {
      494C0101020008001C0010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000733A2D00733A2D00783C2A00793D2A00753B2B00693531000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000006D332700853C130095440D0096450D00873D1200703425000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000763B
      2B00763B2B007B3D290080402700814126007F3F27007E3F28007F3F27007F40
      27006A3531000000000000000000000000000000000000000000000000007037
      2A00A04B0C00CD772700E8AD7000F3CCA100F4CDA300E9B17600D07C2C00A64F
      0A006F3529000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000006A3632008D4C
      2A00984E1F00A2511500AF580F00B55B0B00B2580A00A45213008B4621007C3E
      2900814126006E372F000000000000000000000000000000000086411D00C062
      0B00F0C29200FFFEFA00FEFAF700F5E3D100F5E2D000FDF8F400FFFFFD00F2C9
      9E00C66911007B3A210000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000069363400BC763E00CF7D
      3100D1771E00D16E0800CC610000CC630000D06C0500CE660200C55C0000A954
      110083412600824126006B36300000000000000000008C451C00C1610700F7DB
      BD00FFFEFE00E0A36900CE6F1500C9610000C9610000CC6A0C00DB965300FDFA
      F700FAE5CC00C6680D006F352800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AD724D00F5AC5E00E698
      4800DE8A3400D5771800E0A26200FFF8F000FFF8F000FFF8F000EABC8D00D170
      0C00B75B0700874324007D3F28000000000000000000AF520100ECBD8B00FFFF
      FF00DA8F4500C85B0000C95E0000CB650000CA620000D37D2700D98E4A00D585
      3400FDFAF700F3CB9F00A64E0600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000082442C00E8AF7900F6B77500EBA3
      5A00E4924000F0C79E00FFF8F000E1A15F00D37B2200D88C3E00E9BE9300D175
      1800CE650000A8541300844225006C363000A04D1000CE772100FFFDFB00E8B6
      8400CE670000FFFFFF00DD985200CB620000C85D0000E4AE7900FFFFFF00C85E
      0000DE9E5E00FFFFFF00CF7B2800703525000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AB6C4200FFD19C00FBC28900F5C3
      8F00F5CDA500FFF8F000EEC39800E3A26100E1A26200D5833400C75B0000CA64
      0000CD670000C4630500984C1B00783C2A00AF550700E5AA6F00FFFFFF00DD8F
      3F00D87B1C00FFFFFF00FFFFFF00E4AA6F00CB600000E3AB7300FFFFFF00CB66
      0100CD6C0A00FDFAF600E9B17500873D11000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C2824C00FFD4A000FFCB9600FED7
      AE00FFF8F000FBECDD00F5CFA900F3CEA900F1CCA600E4A96C00CE690400CB66
      0100CB660000CC670000B45A0D0082412500BB5F0A00F0CAA100FCF4ED00E193
      4300E08D3800FFFFFF00FFFFFF00FFFFFF00EBC09400E7B58200FFFFFF00CC68
      0400C9600000F5E3D000F3CEA40095440C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CA854A00FFD7A600FFCB9500FFDA
      B500FFF8F000FDF1E400FFF8F000FFF8F000FFF8F000FFF8F000DD904300D06D
      0A00CC670200CB660000CC6700008E481F00C1650F00F2CDA600FDF7F000E9A1
      5800E89C4E00FFFFFF00FFFFFF00FFFFFF00F1D2B200EBBC8E00FFFFFF00CC68
      0400C9610000F6E6D400F3CCA10094440C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D3976200FFE0BF00FFCF9D00FFCE
      9C00FFE3C600FFF8F000FBCD9E00F6C38E00F1B98100ECB07300E0903E00D87C
      1F00D2741500CD690400D168000087442300C1640D00EEBC8800FFFFFF00F1B7
      7C00EFA96100FFFFFF00FFFFFF00F1C99F00DB812500EAB98700FFFFFF00CB66
      0000CE6F0E00FEFCFA00E7AC6D00853B13000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D9A16C00F3D3B300FFE7CB00FFCC
      9600FFCC9700FFF8F000FEEAD400FAC18600F4B06C00ECA35900EFBF8E00ECBC
      8B00DA822900D7781800C5670D006B362F00BF600600E5A05900FFFDFA00FBE0
      C400F7B87600FFFFFF00F4CA9E00E6994900DF883200EDC09300FFFFFF00C95F
      0000E2A96E00FFFFFE00CC7520006D3428000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E2A36300FFFBF200FFE1
      C200FFCB9600FFD09E00FFF8F000FFF8F000FFF8F000FFF8F000FDF5EB00F0BF
      8C00E2924000E68D3100A35B2A000000000000000000D57F2B00FAD9B800FFFF
      FF00FEDCB900F8BB7D00F0AB6300E9A05600E1903D00E0944700DC914600DB93
      4B00FFFFFF00EFC08C00A04A0800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CD6A0600F3C79A00FFFD
      F600FFE8D000FFD2A400FFCC9600FFD8AF00FFE0BF00FFDAB500F8BF8500F0AC
      6600F3AB6000CB8346006A3633000000000000000000C3620400E79E5500FEEB
      D700FFFFFF00FBDFC200F1B87D00E9A25900E2944500DD8E3D00E6B17C00FFFF
      FF00F6D8B700BE5F06006B342C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CF6D0D00F2C2
      8F00FFF9EE00FFF8EE00FFE9D100FFDDB900FFD9B100FFDAB300FFDEB800FFD4
      A300D89A63007941320000000000000000000000000000000000C6670C00E69E
      5500FAD9B600FFFBF600FFFFFF00FEF8F200FDF6EF00FFFFFF00FEF9F200ECB8
      8400BE5F09007538260000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CF6D
      0D00E1954700F6CDA300FFEBD600FFF1E000FFF1DF00FFEBD400F1CDAC00C289
      5E00C2895E00000000000000000000000000000000000000000000000000C060
      0500D27E2C00E49F5A00EEBA8600F2CAA000F0C59900E4A76800CC741E00A64D
      0600783A27000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D47A1F00D47A1F00D5843600CF823B00C0722E00C0722E000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B65C0A00B8601200B9611300B25A0F00A24F0E008E451A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00F81FF81F00000000E007E00700000000
      C003C00300000000800180010000000080018001000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000080018001000000008001800100000000C003C00300000000
      E007E00700000000F81FF81F0000000000000000000000000000000000000000
      000000000000}
  end
  object popupLogMemo: TPopupActionBar
    Left = 400
    Top = 184
    object miClearLog: TMenuItem
      Action = actClearLog
    end
  end
end
