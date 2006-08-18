object MainForm: TMainForm
  Left = 194
  Top = 99
  Width = 527
  Height = 312
  Caption = 'Project JEDI HtmlHelp Inclusion Tool'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 247
    Width = 519
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 41
    Width = 129
    Height = 206
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 1
    object lbRegistry: TListBox
      Left = 1
      Top = 1
      Width = 127
      Height = 204
      Align = alClient
      BevelOuter = bvNone
      ItemHeight = 13
      PopupMenu = PopupMenu1
      TabOrder = 0
      OnClick = lbRegistryEnter
      OnEnter = lbRegistryEnter
      OnExit = lbRegistryEnter
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 519
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object sbAddHelpfile: TSpeedButton
      Left = 8
      Top = 4
      Width = 32
      Height = 32
      Hint = 'Adds a helpfile to JHIT'
      HelpType = htKeyword
      Flat = True
      Glyph.Data = {
        96010000424D9601000000000000760000002800000018000000180000000100
        04000000000020010000CE0E0000D80E00001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        8888888888888888888888888888888888888888888888888888888888888888
        888777777777777888888888887000000000007888888888887FFFFFFFFFF078
        88888888887FFFFFFFFFF07888888888887FFFFFFFFFF07888888888887FFFFF
        FFFFF07888888888887FFFFFFFFFF07888888888887FFFFFFFFFF07888888888
        887FFFFFFFFFF07888888888887FFFFFFF88807888888888887FF8FFF7000088
        888888888F7F8FFFF7FF08888888888888FFF8F8F7F08888888888FFFFFFFFFF
        F70888888888888888FFF77777888888888888888F8F8F888888888888888888
        888F88888888888888888888888F88888888888888888888888F888888888888
        8888888888888888888888888888888888888888888888888888}
      OnClick = sbAddHelpfileClick
    end
    object btSaveExit: TSpeedButton
      Left = 43
      Top = 4
      Width = 32
      Height = 32
      Hint = 'Adds a helpfile to JHIT'
      HelpType = htKeyword
      Flat = True
      Glyph.Data = {
        96010000424D9601000000000000760000002800000018000000180000000100
        04000000000020010000CE0E0000D80E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        777777777777777777777777777777777777777878787407FFFFFFFFF7777777
        77777847F4466767F777774777784047F4866676F777770477040477F4466767
        F777774047404777F4866676F777778404047777F4466767F777777840407777
        F4866676F777777784047777F4466767F777777780407477F4866676F7777704
        04047477F4466767F777774740407477F4866676F777777704040477F4466767
        F777777780404777F4866676F777777778447777F4466767F777777704877777
        F4866676F777777744477777F4466767F777777704077777F4866666F7777777
        77777777F4488887F777777777777777FFFFFFFFF77777777777777777777777
        7777777777777777777777777777777777777777777777777777}
      OnClick = btSaveExitClick
    end
  end
  object Panel3: TPanel
    Left = 129
    Top = 41
    Width = 390
    Height = 206
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 3
    object GroupBox1: TGroupBox
      Left = 8
      Top = 8
      Width = 369
      Height = 145
      Caption = 'Path && Filename Settings'
      TabOrder = 0
      object lbNo1: TLabel
        Left = 8
        Top = 24
        Width = 42
        Height = 13
        HelpType = htKeyword
        Caption = 'Filename'
      end
      object edPath: TEdit
        Left = 8
        Top = 40
        Width = 321
        Height = 21
        TabOrder = 0
        OnExit = edPathExit
      end
      object edIndexFile: TEdit
        Left = 8
        Top = 104
        Width = 321
        Height = 21
        TabOrder = 1
        OnExit = edIndexFileExit
      end
      object cbIndexFile: TCheckBox
        Left = 8
        Top = 80
        Width = 145
        Height = 17
        Caption = 'Use different Index file'
        TabOrder = 2
        OnClick = cbIndexFileClick
      end
      object btBrowseFile: TButton
        Left = 334
        Top = 40
        Width = 23
        Height = 22
        Caption = '...'
        TabOrder = 3
        OnClick = btBrowseFileClick
      end
      object btBrowseIndex: TButton
        Left = 334
        Top = 104
        Width = 23
        Height = 22
        Caption = '...'
        TabOrder = 4
        OnClick = btBrowseIndexClick
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 96
    Top = 160
    object Help1: TMenuItem
      Caption = '&Help'
      object N2: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Caption = '&About'
      end
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'Compiled Help File|*.CHM|Compiled Help Index|*.CHI'
    Left = 96
    Top = 88
  end
  object PopupMenu1: TPopupMenu
    Left = 96
    Top = 120
    object Delete1: TMenuItem
      Caption = '&Delete'
      OnClick = Delete1Click
    end
    object Modify1: TMenuItem
      Caption = '&Rename'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Addhelpfile1: TMenuItem
      Caption = '&Add helpfile'
      OnClick = sbAddHelpfileClick
    end
  end
end
