inherited JclOtaExcDlgSystemPage: TJclOtaExcDlgSystemPage
  object LabelLogFileName: TLabel
    Left = 170
    Top = 139
    Width = 55
    Height = 13
    Caption = 'RsFileName'
  end
  object CheckBoxDelayed: TCheckBox
    Left = 120
    Top = 18
    Width = 265
    Height = 17
    Caption = 'RsDelayedStackTrace'
    TabOrder = 0
  end
  object CheckBoxHookDll: TCheckBox
    Left = 120
    Top = 49
    Width = 265
    Height = 17
    Caption = 'RsHookDll'
    TabOrder = 1
  end
  object CheckBoxLogFile: TCheckBox
    Left = 120
    Top = 113
    Width = 265
    Height = 17
    Caption = 'RsLogFile'
    TabOrder = 2
    OnClick = CheckBoxLogFileClick
  end
  object EditLogFileName: TEdit
    Left = 240
    Top = 136
    Width = 145
    Height = 21
    TabOrder = 3
  end
  object CheckBoxModuleList: TCheckBox
    Left = 120
    Top = 176
    Width = 265
    Height = 17
    Caption = 'RsModuleList'
    TabOrder = 4
    OnClick = CheckBoxModuleListClick
  end
  object CheckBoxOSInfo: TCheckBox
    Left = 120
    Top = 232
    Width = 265
    Height = 17
    Caption = 'RsOSInfo'
    TabOrder = 5
  end
  object CheckBoxActiveControls: TCheckBox
    Left = 120
    Top = 264
    Width = 265
    Height = 17
    Caption = 'RsActiveControls'
    TabOrder = 6
  end
  object CheckBoxMainThreadOnly: TCheckBox
    Left = 120
    Top = 81
    Width = 265
    Height = 17
    Caption = 'RsMainThreadOnly'
    TabOrder = 7
  end
  object CheckBoxUnitVersioning: TCheckBox
    Left = 152
    Top = 199
    Width = 233
    Height = 17
    Caption = 'RsUnitVersioning'
    TabOrder = 8
  end
end
