object fmProjectWizard: TfmProjectWizard
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'DUnitX Project Wizard'
  ClientHeight = 345
  ClientWidth = 455
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tlbarNav: TActionToolBar
    Left = 0
    Top = 322
    Width = 455
    Height = 23
    ActionManager = actmngrWizardActions
    Align = alBottom
    Caption = 'tlbarNav'
    ColorMap.HighlightColor = 15660791
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 15660791
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Spacing = 10
  end
  object pnlPageClient: TPanel
    Left = 0
    Top = 0
    Width = 455
    Height = 322
    Align = alClient
    Caption = 'pnlPageClient'
    ShowCaption = False
    TabOrder = 1
  end
  object imglstGlyphs16x16: TImageList
    Left = 240
    Top = 16
  end
  object actmngrWizardActions: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = actBack
            Caption = '&Back'
          end
          item
            Action = actNext
            Caption = '&Next'
          end
          item
            Action = actFinish
            Caption = '&Finish'
          end
          item
            Caption = '-'
          end
          item
            Action = actCancel
            Caption = '&Cancel'
          end>
        ActionBar = tlbarNav
      end>
    Images = imglstGlyphs16x16
    Left = 240
    Top = 72
    StyleName = 'Platform Default'
    object actBack: TAction
      Caption = 'Back'
      OnExecute = actBackExecute
      OnUpdate = actBackUpdate
    end
    object actNext: TAction
      Caption = 'Next'
      OnExecute = actNextExecute
      OnUpdate = actNextUpdate
    end
    object actFinish: TAction
      Caption = 'Finish'
      OnExecute = actFinishExecute
      OnUpdate = actFinishUpdate
    end
    object actCancel: TAction
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
  end
  object clmapTwilight: TTwilightColorMap
    HighlightColor = clBlack
    BtnFrameColor = clBlack
    DisabledColor = cl3DDkShadow
    Left = 240
    Top = 128
  end
end
