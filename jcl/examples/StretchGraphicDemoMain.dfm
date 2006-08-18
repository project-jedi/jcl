object StretchDemoForm: TStretchDemoForm
  Left = 222
  Top = 115
  Width = 335
  Height = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 327
    Height = 266
    ActivePage = StretchedPage
    Align = alClient
    TabOrder = 0
    object OriginalPage: TTabSheet
      Caption = 'Original'
      object ScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 319
        Height = 238
        HorzScrollBar.Tracking = True
        VertScrollBar.Tracking = True
        Align = alClient
        TabOrder = 0
        object OriginalImage: TImage
          Left = 0
          Top = 0
          Width = 315
          Height = 234
          AutoSize = True
        end
      end
    end
    object StretchedPage: TTabSheet
      Caption = 'Stretched'
      ImageIndex = 1
      object StretchedImage: TImage
        Left = 0
        Top = 0
        Width = 319
        Height = 238
      end
    end
    object FilesPage: TTabSheet
      Caption = 'Files'
      ImageIndex = 2
      object FileListBox: TListBox
        Left = 0
        Top = 0
        Width = 319
        Height = 238
        Align = alClient
        Columns = 3
        ItemHeight = 13
        TabOrder = 0
        OnClick = FileListBoxClick
      end
    end
  end
  object OpenDialog: TOpenPictureDialog
    Filter = 
      'All (*.jpg;*.jpeg;*.bmp)|*.jpg;*.jpeg;*.bmp|JPEG Image File (*.j' +
      'pg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp'
    Left = 176
  end
  object MainMenu: TMainMenu
    Left = 144
    object Fil1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = 'Open...'
        OnClick = OpenFile
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ExitItem: TMenuItem
        Caption = 'E&xit'
        OnClick = ExitApp
      end
    end
    object Filter1: TMenuItem
      Caption = '&Resampling Filter'
      object Box1: TMenuItem
        Caption = 'Bo&x'
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
      object riangle1: TMenuItem
        Tag = 1
        Caption = '&Triangle'
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
      object Hermite1: TMenuItem
        Tag = 2
        Caption = '&Hermite'
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
      object Bell1: TMenuItem
        Tag = 3
        Caption = '&Bell'
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
      object Spline1: TMenuItem
        Tag = 4
        Caption = '&Spline'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
      object Lanczos31: TMenuItem
        Tag = 5
        Caption = '&Lanczos 3'
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
      object Mitchell1: TMenuItem
        Tag = 6
        Caption = '&Mitchell'
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object PreserveAspectRatio1: TMenuItem
        Caption = 'Preserve Aspect Ratio'
        Checked = True
        OnClick = PreserveAspectRatio1Click
      end
    end
    object PrevItem: TMenuItem
      Caption = ' &<< '
      Hint = 'Next File'
      OnClick = PrevFile
    end
    object NextItem: TMenuItem
      Caption = ' &>> '
      Hint = 'Previous File'
      OnClick = NextFile
    end
  end
end
