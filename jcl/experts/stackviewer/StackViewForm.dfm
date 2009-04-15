inherited frmStackView: TfrmStackView
  Caption = 'Exception Stack View'
  ClientHeight = 331
  ClientWidth = 356
  KeyPreview = True
  PopupMenu = nil
  ShowHint = False
  PixelsPerInch = 96
  TextHeight = 13
  inherited Splitter1: TSplitter
    Width = 356
    Constraints.MinHeight = 3
  end
  object Splitter2: TSplitter [1]
    Left = 145
    Top = 54
    Height = 277
  end
  inherited ToolBar1: TToolBar
    Width = 356
    ParentShowHint = False
    ShowHint = False
    object ToolButton1: TToolButton
      Left = 4
      Top = 0
      Action = acLoadStack
    end
    object ToolButton2: TToolButton
      Left = 27
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton3: TToolButton
      Left = 35
      Top = 0
      Action = acJumpToCodeLine
    end
    object ToolButton4: TToolButton
      Left = 58
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object ToolButton5: TToolButton
      Left = 66
      Top = 0
      Action = acOptions
    end
    object ToolButton6: TToolButton
      Left = 89
      Top = 0
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object ToolButton7: TToolButton
      Left = 97
      Top = 0
      Action = acUpdateLocalInfo
    end
  end
  object cboxThread: TComboBox [3]
    Left = 0
    Top = 33
    Width = 356
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    Visible = False
    OnChange = cboxThreadChange
  end
  object tv: TTreeView [4]
    Left = 0
    Top = 54
    Width = 145
    Height = 277
    Align = alLeft
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 2
    OnChange = tvChange
  end
  inherited DockActionList: TActionList
    Top = 216
  end
  object ActionList1: TActionList
    Left = 56
    Top = 216
    object acJumpToCodeLine: TAction
      Caption = 'Jump to code line'
      Hint = 'Jump to the code line of the selected stack line'
      OnExecute = acJumpToCodeLineExecute
    end
    object acLoadStack: TAction
      Caption = 'Load Stack'
      Hint = 'Load Stack from file'
      OnExecute = acLoadStackExecute
    end
    object acOptions: TAction
      Caption = 'Options'
      OnExecute = acOptionsExecute
    end
    object acUpdateLocalInfo: TAction
      Caption = 'Update Local Info'
      OnExecute = acUpdateLocalInfoExecute
    end
  end
  object PopupActionBar1: TPopupActionBar
    Left = 136
    Top = 216
    object mnuJumpToCodeLine: TMenuItem
      Action = acJumpToCodeLine
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object StayonTop2: TMenuItem
      Action = StayOnTopCmd
    end
    object Dockable2: TMenuItem
      Action = DockableCmd
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 56
    Top = 264
  end
end
