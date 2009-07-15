object JclFormCpuInfo: TJclFormCpuInfo
  Left = 468
  Top = 438
  BorderStyle = bsDialog
  Caption = 'Local CPU Informations'
  ClientHeight = 296
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelName: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Name :'
  end
  object LabelVendor: TLabel
    Left = 8
    Top = 40
    Width = 41
    Height = 13
    Caption = 'Vendor :'
  end
  object LabelFrequency: TLabel
    Left = 160
    Top = 40
    Width = 58
    Height = 13
    Caption = 'Frequency :'
  end
  object EditName: TEdit
    Left = 64
    Top = 8
    Width = 249
    Height = 21
    Enabled = False
    ParentColor = True
    TabOrder = 0
    Text = 'EditName'
  end
  object EditVendor: TEdit
    Left = 64
    Top = 40
    Width = 81
    Height = 21
    Enabled = False
    ParentColor = True
    TabOrder = 1
    Text = 'EditVendor'
  end
  object EditFrequency: TEdit
    Left = 232
    Top = 40
    Width = 81
    Height = 21
    Enabled = False
    ParentColor = True
    TabOrder = 2
    Text = 'EditFrequency'
  end
  object CheckBoxMMX: TCheckBox
    Left = 8
    Top = 72
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'MMX'
    Enabled = False
    TabOrder = 3
  end
  object CheckBoxExMMX: TCheckBox
    Left = 8
    Top = 95
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'MMX Extensions'
    Enabled = False
    TabOrder = 4
  end
  object CheckBox3DNow: TCheckBox
    Left = 8
    Top = 118
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = '3DNow!'
    Enabled = False
    TabOrder = 5
  end
  object CheckBoxEx3DNow: TCheckBox
    Left = 8
    Top = 141
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = '3DNow! Extensions'
    Enabled = False
    TabOrder = 6
  end
  object CheckBox64Bits: TCheckBox
    Left = 8
    Top = 164
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = '64 bits'
    Enabled = False
    TabOrder = 7
  end
  object CheckBoxSSE1: TCheckBox
    Left = 161
    Top = 72
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'SSE Version 1'
    Enabled = False
    TabOrder = 8
  end
  object CheckBoxSSE2: TCheckBox
    Left = 161
    Top = 95
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'SSE Version 2'
    Enabled = False
    TabOrder = 9
  end
  object CheckBoxSSE3: TCheckBox
    Left = 161
    Top = 118
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'SSE Version 3'
    Enabled = False
    TabOrder = 10
  end
  object ButtonClose: TButton
    Left = 128
    Top = 263
    Width = 83
    Height = 25
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 15
  end
  object CheckBoxSSSE3: TCheckBox
    Left = 161
    Top = 141
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Suppl. SSE Version 3'
    Enabled = False
    TabOrder = 11
  end
  object CheckBoxSSE4A: TCheckBox
    Left = 161
    Top = 164
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'SSE Version 4 A'
    Enabled = False
    TabOrder = 12
  end
  object CheckBoxSSE5: TCheckBox
    Left = 161
    Top = 210
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'SSE Version 5'
    Enabled = False
    TabOrder = 14
  end
  object CheckBoxSSE4B: TCheckBox
    Left = 161
    Top = 187
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'SSE Version 4 B'
    Enabled = False
    TabOrder = 13
  end
  object CheckBoxAVX: TCheckBox
    Left = 161
    Top = 233
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'AVX'
    Enabled = False
    TabOrder = 16
  end
  object CheckBoxEnabledFPU: TCheckBox
    Left = 8
    Top = 187
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Enabled FPU'
    Enabled = False
    TabOrder = 17
  end
  object CheckBoxEnabledSSE: TCheckBox
    Left = 8
    Top = 210
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Enabled SSE'
    Enabled = False
    TabOrder = 18
  end
  object CheckBoxEnabledAVX: TCheckBox
    Left = 8
    Top = 233
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Enabled AVX'
    Enabled = False
    TabOrder = 19
  end
end
