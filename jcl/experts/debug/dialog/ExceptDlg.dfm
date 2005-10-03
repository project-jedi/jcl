object ExceptionDialog: TExceptionDialog
  Left = 363
  Top = 284
  ActiveControl = OkBtn
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'ExceptionDialog'
  ClientHeight = 255
  ClientWidth = 432
  Color = clBtnFace
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 3
    Top = 91
    Width = 428
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object OkBtn: TButton
    Left = 352
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object DetailsMemo: TMemo
    Left = 4
    Top = 101
    Width = 424
    Height = 150
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 3
    WantReturns = False
    WordWrap = False
  end
  object DetailsBtn: TButton
    Left = 352
    Top = 60
    Width = 75
    Height = 25
    Hint = 'Show or hide additional information|'
    Anchors = [akTop, akRight]
    Caption = '&Details'
    Enabled = False
    TabOrder = 2
    OnClick = DetailsBtnClick
  end
  object TextLabel: TMemo
    Left = 56
    Top = 8
    Width = 281
    Height = 75
    Hint = 'Use Ctrl+C to copy the report to the clipboard'
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    Ctl3D = True
    Lines.Strings = (
      'TextLabel')
    ParentColor = True
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 0
    WantReturns = False
  end
end
