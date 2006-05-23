inherited JclOtaExcDlgSystemPage: TJclOtaExcDlgSystemPage
  object LabelLogFileName: TLabel
    Left = 162
    Top = 135
    Width = 55
    Height = 13
    Caption = 'RsFileName'
  end
  object CheckBoxDelayed: TCheckBox
    Left = 120
    Top = 26
    Width = 265
    Height = 17
    Caption = 'RsDelayedStackTrace'
    TabOrder = 0
  end
  object CheckBoxHookDll: TCheckBox
    Left = 120
    Top = 65
    Width = 265
    Height = 17
    Caption = 'RsHookDll'
    TabOrder = 1
  end
  object CheckBoxLogFile: TCheckBox
    Left = 120
    Top = 104
    Width = 265
    Height = 17
    Caption = 'RsLogFile'
    TabOrder = 2
    OnClick = CheckBoxLogFileClick
  end
  object EditLogFileName: TEdit
    Left = 240
    Top = 132
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
  end
  object CheckBoxOSInfo: TCheckBox
    Left = 120
    Top = 216
    Width = 265
    Height = 17
    Caption = 'RsOSInfo'
    TabOrder = 5
  end
  object CheckBoxActiveControls: TCheckBox
    Left = 120
    Top = 256
    Width = 265
    Height = 17
    Caption = 'RsActiveControls'
    TabOrder = 6
  end
end
