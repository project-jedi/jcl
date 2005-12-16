object FrameJclOptions: TFrameJclOptions
  Left = 0
  Top = 0
  Width = 419
  Height = 265
  TabOrder = 0
  TabStop = True
  object GroupBoxWizard: TGroupBox
    Left = 8
    Top = 8
    Width = 403
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Uses wizard '
    TabOrder = 0
    object LabelIniFile: TLabel
      Left = 8
      Top = 18
      Width = 110
      Height = 13
      Alignment = taRightJustify
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '&Configuration file:'
    end
    object CheckBoxWizardActive: TCheckBox
      Left = 16
      Top = 48
      Width = 201
      Height = 17
      Caption = '&Active'
      TabOrder = 2
    end
    object CheckBoxWizardConfirm: TCheckBox
      Left = 16
      Top = 72
      Width = 201
      Height = 17
      Caption = '&Prompt to confirm changes'
      TabOrder = 3
    end
    object EditIniFile: TEdit
      Left = 123
      Top = 15
      Width = 252
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object ButtonIniFile: TButton
      Left = 377
      Top = 15
      Width = 18
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = ButtonIniFileClick
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'ini'
    Filter = 'Configuration files (*.ini)|*.ini|All files (*.*)|*.*'
    FilterIndex = 0
    Title = 'Select JEDI Uses wizard configuration file'
    Left = 376
    Top = 72
  end
end
