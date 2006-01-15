object JclVersionCtrlOptionsFrame: TJclVersionCtrlOptionsFrame
  Left = 0
  Top = 0
  Width = 340
  Height = 432
  Anchors = [akLeft, akTop, akRight, akBottom]
  TabOrder = 0
  TabStop = True
  Width = 340
  Height = 432
  object LabelIcons: TLabel
    Left = 16
    Top = 106
    Width = 38
    Height = 13
    Caption = 'RsIcons'
    FocusControl = ComboBoxIcons
  end
  object LabelMenuOrganization: TLabel
    Left = 16
    Top = 130
    Width = 99
    Height = 13
    Caption = 'RsMenuOrganization'
    FocusControl = TreeViewMenu
  end
  object CheckBoxHideDisabledActions: TCheckBox
    Left = 16
    Top = 31
    Width = 185
    Height = 17
    Action = ActionHideUnSupportedActions
    TabOrder = 0
  end
  object ComboBoxIcons: TComboBox
    Left = 72
    Top = 103
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    Items.Strings = (
      'RsNoIcon'
      'RsJCLIcons'
      'RsAutoIcons')
  end
  object TreeViewMenu: TTreeView
    Left = 16
    Top = 149
    Width = 228
    Height = 268
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideSelection = False
    Indent = 19
    RightClickSelect = True
    RowSelect = True
    ShowRoot = False
    TabOrder = 2
    OnEdited = TreeViewMenuEdited
    OnEditing = TreeViewMenuEditing
  end
  object CheckBoxDisableActions: TCheckBox
    Left = 16
    Top = 8
    Width = 201
    Height = 17
    Action = ActionDisableActions
    TabOrder = 3
  end
  object ButtonNewSeparator: TButton
    Left = 250
    Top = 180
    Width = 87
    Height = 25
    Action = ActionNewSeparator
    Anchors = [akTop, akRight]
    TabOrder = 4
  end
  object ButtonDelete: TButton
    Left = 250
    Top = 242
    Width = 87
    Height = 25
    Action = ActionDeleteItem
    Anchors = [akTop, akRight]
    TabOrder = 5
  end
  object ButtonRename: TButton
    Left = 250
    Top = 273
    Width = 87
    Height = 25
    Action = ActionRenameItem
    Anchors = [akTop, akRight]
    TabOrder = 6
  end
  object ButtonMoveUp: TButton
    Left = 250
    Top = 304
    Width = 87
    Height = 25
    Action = ActionMoveItemUp
    Anchors = [akTop, akRight]
    TabOrder = 7
  end
  object ButtonMoveDown: TButton
    Left = 250
    Top = 335
    Width = 87
    Height = 25
    Action = ActionMoveItemDown
    Anchors = [akTop, akRight]
    TabOrder = 8
  end
  object CheckBoxSaveConfirmation: TCheckBox
    Left = 16
    Top = 54
    Width = 201
    Height = 17
    Action = ActionSaveConfirmation
    TabOrder = 9
  end
  object ButtonNewAction: TButton
    Left = 250
    Top = 211
    Width = 87
    Height = 25
    Action = ActionNewAction
    Anchors = [akTop, akRight]
    TabOrder = 10
  end
  object ButtonNewSubMenu: TButton
    Left = 250
    Top = 149
    Width = 87
    Height = 25
    Action = ActionNewSubMenu
    Anchors = [akTop, akRight]
    TabOrder = 11
  end
  object CheckBoxActOnTopSandbox: TCheckBox
    Left = 16
    Top = 77
    Width = 201
    Height = 17
    Action = ActionActOnTopSandbox
    TabOrder = 12
  end
  object ActionListVersionCtrl: TActionList
    Left = 256
    Top = 64
    object ActionDisableActions: TAction
      AutoCheck = True
      Caption = 'RsDisableActions'
      OnUpdate = ActionDisableActionsUpdate
    end
    object ActionHideUnSupportedActions: TAction
      AutoCheck = True
      Caption = 'RsHideUnsupportedActions'
      OnUpdate = ActionHideUnSupportedActionsUpdate
    end
    object ActionSaveConfirmation: TAction
      AutoCheck = True
      Caption = 'RsSaveConfirmation'
      OnUpdate = ActionSaveConfirmationUpdate
    end
    object ActionNewSubMenu: TAction
      Caption = 'RsNewSubMenu'
      OnExecute = ActionNewSubMenuExecute
      OnUpdate = ActionNewSubMenuUpdate
    end
    object ActionNewSeparator: TAction
      Caption = 'RsNewSeparator'
      OnExecute = ActionNewSeparatorExecute
      OnUpdate = ActionNewSeparatorUpdate
    end
    object ActionNewAction: TAction
      Caption = 'RsNewAction'
      OnExecute = ActionNewActionExecute
      OnUpdate = ActionNewActionUpdate
    end
    object ActionDeleteItem: TAction
      Caption = 'RsDeleteItem'
      OnExecute = ActionDeleteItemExecute
      OnUpdate = ActionDeleteItemUpdate
    end
    object ActionRenameItem: TAction
      Caption = 'RsRenameItem'
      OnExecute = ActionRenameItemExecute
      OnUpdate = ActionRenameItemUpdate
    end
    object ActionMoveItemUp: TAction
      Caption = 'RsMoveItemUp'
      OnExecute = ActionMoveItemUpExecute
      OnUpdate = ActionMoveItemUpUpdate
    end
    object ActionMoveItemDown: TAction
      Caption = 'RsMoveItemDown'
      OnExecute = ActionMoveItemDownExecute
      OnUpdate = ActionMoveItemDownUpdate
    end
    object ActionActOnTopSandbox: TAction
      AutoCheck = True
      Caption = 'RsActOnTopSandbox'
      OnUpdate = ActionActOnTopSandboxUpdate
    end
  end
  object PopupMenuActions: TPopupMenu
    Left = 296
    Top = 64
  end
end
