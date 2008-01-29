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
  object ListView1: TListView
    Left = 0
    Top = 81
    Width = 691
    Height = 204
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
    TabOrder = 0
    ViewStyle = vsReport
    OnData = ListView1Data
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 285
    Width = 691
    Height = 16
    Align = alBottom
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 691
    Height = 81
    ActivePage = TabSheetReadOnly
    Align = alTop
    TabOrder = 2
    object TabSheetReadOnly: TTabSheet
      Caption = 'Read-only'
      object ButtonOpen: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Action = ActionOpen
        TabOrder = 0
      end
      object ButtonExtractSelected: TButton
        Left = 97
        Top = 16
        Width = 96
        Height = 25
        Action = ActionExtractSelected
        TabOrder = 1
      end
      object ButtonExtractAll: TButton
        Left = 199
        Top = 16
        Width = 75
        Height = 25
        Action = ActionExtractAll
        TabOrder = 2
      end
    end
    object TabSheetWriteOnly: TTabSheet
      Caption = 'Write-only'
      ImageIndex = 1
      object ButtonNew: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Action = ActionNew
        TabOrder = 0
      end
      object ButtonAddFile: TButton
        Left = 97
        Top = 16
        Width = 75
        Height = 25
        Action = ActionAddFile
        TabOrder = 1
      end
      object ButtonAddDirectory: TButton
        Left = 178
        Top = 16
        Width = 75
        Height = 25
        Action = ActionAddDirectory
        TabOrder = 2
      end
      object ButtonSave: TButton
        Left = 259
        Top = 16
        Width = 75
        Height = 25
        Action = ActionSave
        TabOrder = 3
      end
    end
    object TabSheetReadWrite: TTabSheet
      Caption = 'Read and write'
      ImageIndex = 2
      object ButtonNewRW: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Action = ActionNewRW
        TabOrder = 0
      end
      object ButtonOpenRW: TButton
        Left = 97
        Top = 16
        Width = 75
        Height = 25
        Action = ActionOpenRW
        TabOrder = 1
      end
      object ButtonDeleteRW: TButton
        Left = 178
        Top = 16
        Width = 75
        Height = 25
        Action = ActionDelete
        TabOrder = 2
      end
      object ButtonAddFileRW: TButton
        Left = 259
        Top = 16
        Width = 75
        Height = 25
        Action = ActionAddFile
        TabOrder = 3
      end
      object ButtonAddDirectoryRW: TButton
        Left = 340
        Top = 16
        Width = 75
        Height = 25
        Action = ActionAddDirectory
        TabOrder = 4
      end
      object ButtonExtractSelectedRW: TButton
        Left = 421
        Top = 16
        Width = 92
        Height = 25
        Action = ActionExtractSelected
        TabOrder = 5
      end
      object ButtonExtractAllRW: TButton
        Left = 519
        Top = 16
        Width = 75
        Height = 25
        Action = ActionExtractAll
        TabOrder = 6
      end
      object ButtonSaveRW: TButton
        Left = 600
        Top = 16
        Width = 75
        Height = 25
        Action = ActionSave
        TabOrder = 7
      end
    end
  end
  object ActionList1: TActionList
    Left = 64
    Top = 152
    object ActionOpen: TAction
      Category = 'ReadOnly'
      Caption = '&Open'
      OnExecute = ActionOpenExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionExtractSelected: TAction
      Category = 'ReadOnly'
      Caption = '&Extract selected'
      OnExecute = ActionExtractSelectedExecute
      OnUpdate = ActionExtractSelectedUpdate
    end
    object ActionExtractAll: TAction
      Category = 'ReadOnly'
      Caption = 'Extract &all'
      OnExecute = ActionExtractAllExecute
      OnUpdate = ActionExtractAllUpdate
    end
    object ActionNew: TAction
      Category = 'WriteOnly'
      Caption = '&New'
      OnExecute = ActionNewExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionAddFile: TAction
      Category = 'WriteOnly'
      Caption = 'Add &file'
      OnExecute = ActionAddFileExecute
      OnUpdate = ActionAddFileUpdate
    end
    object ActionAddDirectory: TAction
      Category = 'WriteOnly'
      Caption = 'Add &directory'
      OnExecute = ActionAddDirectoryExecute
      OnUpdate = ActionAddDirectoryUpdate
    end
    object ActionSave: TAction
      Category = 'WriteOnly'
      Caption = '&Save'
      OnExecute = ActionSaveExecute
      OnUpdate = ActionSaveUpdate
    end
    object ActionDelete: TAction
      Category = 'ReadWrite'
      Caption = '&Delete'
      OnExecute = ActionDeleteExecute
      OnUpdate = ActionDeleteUpdate
    end
    object ActionNewRW: TAction
      Category = 'ReadWrite'
      Caption = '&New'
      OnExecute = ActionNewRWExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionOpenRW: TAction
      Category = 'ReadWrite'
      Caption = '&Open'
      OnExecute = ActionOpenRWExecute
      OnUpdate = ActionAlwaysEnabled
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
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open an archive for extraction'
    Left = 104
    Top = 152
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
    Left = 144
    Top = 152
  end
  object OpenDialogFile: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 104
    Top = 216
  end
  object OpenDialogArchiveRW: TOpenDialog
    Filter = 
      'Zip archive (*.zip)|*.zip|BZip2 archive (*.bz2)|*.bz2|Sevenzip a' +
      'rchive (*.7z)|*.7z|Tar archive (*.tar)|*.tar|GZip archive (*.gz)' +
      '|*.gz|Split archive (*.001)|*.001'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open an archive for modification'
    Left = 104
    Top = 184
  end
end
