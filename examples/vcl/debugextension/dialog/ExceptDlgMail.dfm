inherited ExceptionDialogMail: TExceptionDialogMail
  Caption = 'ExceptionDialogMail'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited DetailsMemo: TMemo
    TabOrder = 4
  end
  inherited DetailsBtn: TButton
    TabOrder = 3
  end
  object SendBtn: TButton
    Left = 352
    Top = 32
    Width = 75
    Height = 25
    Hint = 'Send bug report using default mail client'
    Anchors = [akTop, akRight]
    Caption = '&Send'
    TabOrder = 2
    OnClick = SendBtnClick
  end
end
