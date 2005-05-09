object JclSIMDViewFrm: TJclSIMDViewFrm
  Left = 231
  Top = 644
  Width = 850
  Height = 305
  Caption = ')'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 784
    Top = 0
    Height = 278
    Align = alRight
  end
  object ListBoxRegs: TListBox
    Left = 0
    Top = 0
    Width = 784
    Height = 278
    Style = lbOwnerDrawFixed
    Align = alClient
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    PopupMenu = PopupMenuRegs
    TabOrder = 0
    OnDrawItem = ListBoxRegsDrawItem
    OnMouseDown = ListBoxRegsMouseDown
  end
  object ListBoxMXCSR: TListBox
    Left = 787
    Top = 0
    Width = 55
    Height = 278
    Style = lbOwnerDrawFixed
    Align = alRight
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    PopupMenu = PopupMenuMXCSR
    TabOrder = 1
    OnDrawItem = ListBoxMXCSRDrawItem
    OnMouseDown = ListBoxMXCSRMouseDown
    OnMouseMove = ListBoxMXCSRMouseMove
  end
  object PopupMenuRegs: TPopupMenu
    AutoPopup = False
    OnPopup = PopupMenuRegsPopup
    Left = 64
    Top = 48
    object MenuItemDisplay: TMenuItem
      Caption = '&Display'
      object MenuItemBytes: TMenuItem
        Caption = '&Bytes'
        ShortCut = 16437
        OnClick = MenuItemDisplayClick
      end
      object MenuItemWords: TMenuItem
        Caption = '&Words'
        ShortCut = 16438
        OnClick = MenuItemDisplayClick
      end
      object MenuItemDWords: TMenuItem
        Caption = '&Double Words'
        ShortCut = 16439
        OnClick = MenuItemDisplayClick
      end
      object MenuItemQWords: TMenuItem
        Caption = '&Quads Words'
        ShortCut = 16440
        OnClick = MenuItemDisplayClick
      end
      object MenuItemSeparator1: TMenuItem
        Caption = '-'
      end
      object MenuItemSingles: TMenuItem
        Caption = '&Singles'
        ShortCut = 16441
        OnClick = MenuItemDisplayClick
      end
      object MenuItemDoubles: TMenuItem
        Caption = '&Doubles'
        ShortCut = 16432
        OnClick = MenuItemDisplayClick
      end
    end
    object MenuItemFormat: TMenuItem
      Caption = '&Format'
      object MenuItemBinary: TMenuItem
        Caption = '&Binary'
        ShortCut = 16433
        OnClick = MenuItemFormatClick
      end
      object MenuItemSigned: TMenuItem
        Caption = '&Signed decimal'
        ShortCut = 16434
        OnClick = MenuItemFormatClick
      end
      object MenuItemUnsigned: TMenuItem
        Caption = '&Unsigned decimal'
        ShortCut = 16435
        OnClick = MenuItemFormatClick
      end
      object MenuItemHexa: TMenuItem
        Caption = '&Hexadecimal'
        ShortCut = 16436
        OnClick = MenuItemFormatClick
      end
    end
    object MenuItemModify: TMenuItem
      Caption = '&Modify'
      OnClick = MenuItemModifyClick
    end
    object MenuItemSeparator2: TMenuItem
      Caption = '-'
    end
    object MenuItemStayOnTop: TMenuItem
      Caption = '&Stay on top'
      OnClick = MenuItemStayOnTopClick
    end
    object MenuItemCpuInfo: TMenuItem
      Caption = 'CPU Informations...'
      OnClick = MenuItemCpuInfoClick
    end
  end
  object PopupMenuMXCSR: TPopupMenu
    AutoPopup = False
    Left = 800
    Top = 48
    object MenuItemComplement: TMenuItem
      Caption = '&Complement bit'
      ShortCut = 16468
      OnClick = MenuItemComplementClick
    end
  end
end
