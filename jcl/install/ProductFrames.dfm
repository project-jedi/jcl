object ProductFrame: TProductFrame
  Left = 0
  Top = 0
  Width = 791
  Height = 424
  HorzScrollBar.Range = 398
  AutoScroll = False
  TabOrder = 0
  object Splitter: TSplitter
    Left = 360
    Top = 0
    Width = 5
    Height = 424
    Cursor = crHSplit
    MinSize = 150
    ResizeStyle = rsUpdate
    OnCanResize = SplitterCanResize
  end
  object ComponentsTreePanel: TPanel
    Left = 0
    Top = 0
    Width = 360
    Height = 424
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
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
      Width = 348
      Height = 392
      Anchors = [akLeft, akTop, akRight, akBottom]
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnCustomDrawItem = TreeViewCustomDrawItem
      OnKeyPress = TreeViewKeyPress
      OnMouseDown = TreeViewMouseDown
    end
  end
  object InfoPanel: TPanel
    Left = 365
    Top = 0
    Width = 426
    Height = 424
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Label2: TLabel
      Left = 5
      Top = 8
      Width = 52
      Height = 13
      Caption = '&Information'
    end
    object InfoDisplay: TRichEdit
      Left = 4
      Top = 24
      Width = 412
      Height = 305
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Lucida Console'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
    object OptionsGroupBox: TGroupBox
      Left = 4
      Top = 336
      Width = 412
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
        Width = 315
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = PathEditChange
      end
      object DcpPathEdit: TEdit
        Left = 68
        Top = 48
        Width = 315
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = PathEditChange
      end
      object Button1: TButton
        Left = 388
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
        Left = 388
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
end
