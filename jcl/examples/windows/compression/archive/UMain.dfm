object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 301
  ClientWidth = 691
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 691
    Height = 49
    Align = alTop
    TabOrder = 0
    object Bevel1: TBevel
      Left = 271
      Top = -1
      Width = 2
      Height = 50
      Shape = bsLeftLine
    end
    object ButtonOpen: TButton
      Left = 8
      Top = 13
      Width = 75
      Height = 25
      Action = ActionOpen
      TabOrder = 0
    end
    object ButtonExtractSelected: TButton
      Left = 89
      Top = 13
      Width = 96
      Height = 25
      Action = ActionExtractSelected
      TabOrder = 1
    end
    object ButtonExtractAll: TButton
      Left = 191
      Top = 13
      Width = 75
      Height = 25
      Action = ActionExtractAll
      TabOrder = 2
    end
    object ButtonNew: TButton
      Left = 279
      Top = 13
      Width = 75
      Height = 25
      Action = ActionNew
      TabOrder = 3
    end
    object ButtonAddFile: TButton
      Left = 360
      Top = 13
      Width = 75
      Height = 25
      Action = ActionAddFile
      TabOrder = 4
    end
    object ButtonAddDirectory: TButton
      Left = 441
      Top = 13
      Width = 75
      Height = 25
      Action = ActionAddDirectory
      TabOrder = 5
    end
    object ButtonSave: TButton
      Left = 522
      Top = 13
      Width = 75
      Height = 25
      Action = ActionSave
      TabOrder = 6
    end
  end
  object ListView1: TListView
    Left = 0
    Top = 49
    Width = 691
    Height = 236
    Align = alClient
    Columns = <
      item
        Caption = 'Local name'
        Width = 150
      end
      item
        Caption = 'Archive name'
        Width = 150
      end
      item
        Caption = 'Size'
        Width = 30
      end
      item
        Caption = 'Compressed'
        Width = 30
      end
      item
        Caption = 'Creation'
      end
      item
        Caption = 'Last access'
      end
      item
        Caption = 'Last write'
      end
      item
        Caption = 'Comment'
        Width = 30
      end
      item
        Caption = 'OS'
        Width = 20
      end
      item
        Caption = 'FS'
        Width = 20
      end
      item
        Caption = 'User'
        Width = 20
      end
      item
        Caption = 'Group'
        Width = 20
      end
      item
        Caption = 'CRC'
      end>
    MultiSelect = True
    OwnerData = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnData = ListView1Data
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 285
    Width = 691
    Height = 16
    Align = alBottom
    TabOrder = 2
  end
  object ActionList1: TActionList
    Left = 128
    Top = 80
    object ActionOpen: TAction
      Category = 'Decompress'
      Caption = '&Open'
      OnExecute = ActionOpenExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionExtractSelected: TAction
      Category = 'Decompress'
      Caption = '&Extract selected'
      OnExecute = ActionExtractSelectedExecute
      OnUpdate = ActionExtractSelectedUpdate
    end
    object ActionExtractAll: TAction
      Category = 'Decompress'
      Caption = 'Extract &all'
      OnExecute = ActionExtractAllExecute
      OnUpdate = ActionExtractAllUpdate
    end
    object ActionNew: TAction
      Category = 'Compression'
      Caption = '&New'
      OnExecute = ActionNewExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionAddFile: TAction
      Category = 'Compression'
      Caption = 'Add &file'
      OnExecute = ActionAddFileExecute
      OnUpdate = ActionAddFileUpdate
    end
    object ActionAddDirectory: TAction
      Category = 'Compression'
      Caption = 'Add &directory'
      OnExecute = ActionAddDirectoryExecute
      OnUpdate = ActionAddDirectoryUpdate
    end
    object ActionSave: TAction
      Category = 'Compression'
      Caption = '&Save'
      OnExecute = ActionSaveExecute
      OnUpdate = ActionSaveUpdate
    end
  end
  object OpenDialogArchive: TOpenDialog
    Filter = 
      'Zip archive (*.zip)|*.zip|BZip2 archive (*.bz2)|*.bz2|Sevenzip a' +
      'rchive (*.7z)|*.7z|Tar archive (*.tar)|*.tar|GZip archive (*.gz)' +
      '|*.gz|Rar archive (*.rar)|*.rar|Arj archive (*.arj)|*.arj|Z arch' +
      'ive (*.z)|*.z|Lzh archive (*.lzh)|*.lzh|Nsis archive (*.nsis)|*.' +
      'nsis|Iso image (*.iso)|*.iso|Cab archive (*.cab)|*.cab|Chm file ' +
      '(*.chm)|*.chm|Rpm archive (*.rpm)|*.rpm|Deb archive (*.deb)|*.de' +
      'b|Cpio archive (*.cpio)|*.cpio|Split archive (*.001)|*.001'
    FilterIndex = 0
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open an archive'
    Left = 168
    Top = 80
  end
  object SaveDialogArchive: TSaveDialog
    DefaultExt = '*.zip'
    Filter = 
      'Zip archive (*.zip)|*.zip|BZip2 archive (*.bz2)|*.bz2|Sevenzip a' +
      'rchive (*.7z)|*.7z|Tar archive (*.tar)|*.tar|GZip archive (*.gz)' +
      '|*.gz|Splitted archive (*.001)|*.001'
    FilterIndex = 0
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofCreatePrompt, ofNoReadOnlyReturn, ofEnableSizing]
    Title = 'Create a new archive'
    Left = 208
    Top = 80
  end
  object OpenDialogFile: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 168
    Top = 120
  end
end
