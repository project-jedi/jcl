object JclDebugIdeConfigFrame: TJclDebugIdeConfigFrame
  Left = 0
  Top = 0
  Width = 369
  Height = 375
  TabOrder = 0
  TabStop = True
  object CheckBoxGenerateJdbg: TCheckBox
    Left = 32
    Top = 56
    Width = 321
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'RsDebugGenerateJdbg'
    TabOrder = 0
  end
  object CheckBoxInsertJdbg: TCheckBox
    Left = 32
    Top = 87
    Width = 321
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'RsDebugInsertJdbg'
    TabOrder = 1
  end
  object CheckBoxEnableExpert: TCheckBox
    Left = 16
    Top = 24
    Width = 337
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'RsDebugEnableExpert'
    TabOrder = 2
    OnClick = CheckBoxEnableExpertClick
  end
end
