object ProductFrame: TProductFrame
  Left = 0
  Top = 0
  Width = 791
  Height = 421
  HorzScrollBar.Range = 398
  AutoScroll = False
  TabOrder = 0
  object Splitter: TSplitter
    Left = 426
    Top = 0
    Width = 5
    Height = 421
    Cursor = crHSplit
    Align = alRight
    MinSize = 150
    ResizeStyle = rsUpdate
    OnCanResize = SplitterCanResize
  end
  object InfoPanel: TPanel
    Left = 431
    Top = 0
    Width = 360
    Height = 421
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Label2: TLabel
      Left = 9
      Top = 8
      Width = 71
      Height = 13
      Caption = 'Installation &Log'
    end
    object InfoDisplay: TRichEdit
      Left = 8
      Top = 24
      Width = 346
      Height = 298
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clInfoBk
      Font.Charset = OEM_CHARSET
      Font.Color = clInfoText
      Font.Height = -11
      Font.Name = 'Lucida Console'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentFont = False
      PlainText = True
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object OptionsGroupBox: TGroupBox
      Left = 8
      Top = 333
      Width = 346
      Height = 81
      Anchors = [akLeft, akRight, akBottom]
      Caption = '&Advanced Options'
      TabOrder = 1
      object BPLPathLabel: TLabel
        Left = 8
        Top = 19
        Width = 42
        Height = 13
        Caption = '.bpl Path'
      end
      object DCPPathLabel: TLabel
        Left = 8
        Top = 51
        Width = 46
        Height = 13
        Caption = '.dcp Path'
      end
      object BplPathEdit: TEdit
        Left = 68
        Top = 16
        Width = 249
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = PathEditChange
      end
      object DcpPathEdit: TEdit
        Left = 68
        Top = 48
        Width = 249
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = PathEditChange
      end
      object Button1: TButton
        Left = 322
        Top = 16
        Width = 17
        Height = 22
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        TabStop = False
        OnClick = PathSelectBtnClick
      end
      object Button2: TButton
        Left = 322
        Top = 48
        Width = 17
        Height = 22
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 3
        TabStop = False
        OnClick = PathSelectBtnClick
      end
    end
  end
  object ComponentsTreePanel: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 421
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 132
      Height = 13
      Caption = '&Select components to install'
    end
    object TreeView: TTreeView
      Left = 8
      Top = 24
      Width = 414
      Height = 390
      Anchors = [akLeft, akTop, akRight, akBottom]
      HideSelection = False
      Indent = 19
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 0
      ToolTips = False
      OnCustomDrawItem = TreeViewCustomDrawItem
      OnKeyPress = TreeViewKeyPress
      OnMouseDown = TreeViewMouseDown
    end
  end
end
