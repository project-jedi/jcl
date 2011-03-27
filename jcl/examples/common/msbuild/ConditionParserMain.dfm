object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'MsBuild condition evaluator'
  ClientHeight = 301
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelProperties: TLabel
    Left = 8
    Top = 8
    Width = 53
    Height = 13
    Caption = '&Properties:'
    FocusControl = MemoProperties
  end
  object LabelConditions: TLabel
    Left = 8
    Top = 151
    Width = 54
    Height = 13
    Caption = '&Conditions:'
    FocusControl = MemoConditions
  end
  object MemoProperties: TMemo
    Left = 8
    Top = 27
    Width = 451
    Height = 118
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'ZERO=0'
      'HUNDRED=100')
    TabOrder = 0
  end
  object MemoConditions: TMemo
    Left = 8
    Top = 170
    Width = 451
    Height = 123
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
end
