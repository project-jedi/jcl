object fmJclDFMTest: TfmJclDFMTest
  Left = 192
  Top = 107
  Width = 696
  Height = 480
  Caption = 'JEDI DFMTest'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnDFM2Tree: TButton
      Left = 0
      Top = 8
      Width = 75
      Height = 25
      Caption = 'DFM 2 Tree'
      TabOrder = 0
      OnClick = btnDFM2TreeClick
    end
    object btnDFMGetComps: TButton
      Left = 88
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Components 2 Memo'
      TabOrder = 1
      OnClick = btnDFMGetCompsClick
    end
    object btnCleanDFM: TButton
      Left = 216
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Clean DFM'
      TabOrder = 2
      OnClick = btnCleanDFMClick
    end
    object btnExtractImageLists: TButton
      Left = 304
      Top = 8
      Width = 105
      Height = 25
      Caption = 'Extract ImageLists'
      TabOrder = 3
      OnClick = btnExtractImageListsClick
    end
    object btnLoadLayout: TButton
      Left = 424
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Load Layout'
      TabOrder = 4
      OnClick = btnLoadLayoutClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 688
    Height = 412
    ActivePage = tsTV
    Align = alClient
    TabIndex = 0
    TabOrder = 1
    object tsTV: TTabSheet
      Caption = 'TreeView'
      object tvDFMTree: TTreeView
        Left = 0
        Top = 0
        Width = 680
        Height = 384
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
    end
    object tsComponents: TTabSheet
      Caption = 'Components'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 680
        Height = 384
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object tsSkipList: TTabSheet
      Caption = 'SkipList'
      ImageIndex = 2
      object memSkipProperties: TMemo
        Left = 0
        Top = 0
        Width = 680
        Height = 384
        Align = alClient
        Lines.Strings = (
          '*.DesignSize'
          'TPageControl.TabIndex'
          'TJvPageControl.TabIndex'
          'TImage.Proportional'
          'TJvComboBox.AutoDropDown'
          'TComboBox.AutoDropDown'
          'TMenuItem.AutoCheck'
          'TAction.AutoCheck'
          'TAction.GroupIndex')
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object tsImage: TTabSheet
      Caption = 'Images'
      ImageIndex = 3
      OnResize = tsImageResize
      object Image: TImage
        Left = 0
        Top = 0
        Width = 680
        Height = 384
        Align = alClient
      end
    end
    object tsLayout: TTabSheet
      Caption = 'Layout'
      ImageIndex = 4
      object Panel2: TPanel
        Left = 16
        Top = 16
        Width = 649
        Height = 345
        Caption = 'Panel2'
        TabOrder = 0
        object Panel3: TPanel
          Left = 1
          Top = 199
          Width = 647
          Height = 145
          Align = alBottom
          Caption = 'Panel3'
          TabOrder = 0
          object Panel4: TPanel
            Left = 1
            Top = 103
            Width = 645
            Height = 41
            Align = alBottom
            Caption = 'Panel4'
            TabOrder = 0
          end
        end
      end
    end
    object tsTVItems: TTabSheet
      Caption = 'TreeView Items'
      ImageIndex = 5
      object tvItems: TTreeView
        Left = 0
        Top = 0
        Width = 680
        Height = 384
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
    end
  end
  object btnLoadTreeViewItems: TButton
    Left = 512
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Load TreeView Items'
    TabOrder = 2
    OnClick = btnLoadTreeViewItemsClick
  end
  object OpenDialog: TOpenDialog
    Filter = '*.dfm|*.dfm'
    Left = 624
    Top = 8
  end
  object SaveDialog: TSaveDialog
    Left = 656
    Top = 8
  end
end
