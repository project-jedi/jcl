object Form1: TForm1
  Left = 222
  Top = 107
  Width = 487
  Height = 357
  Caption = 'JclExprEval Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 54
    Height = 13
    Caption = 'E&xpression:'
    FocusControl = ExpressionInput
  end
  object Label2: TLabel
    Left = 12
    Top = 40
    Width = 49
    Height = 13
    Caption = 'Functions:'
  end
  object ExpressionInput: TEdit
    Left = 80
    Top = 8
    Width = 305
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 0
    Top = 60
    Width = 479
    Height = 270
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object EnterButton: TButton
    Left = 396
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Evaluate'
    Default = True
    TabOrder = 2
    OnClick = EnterButtonClick
  end
  object FuncList: TComboBox
    Left = 80
    Top = 36
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 3
    OnClick = FuncListClick
  end
end
