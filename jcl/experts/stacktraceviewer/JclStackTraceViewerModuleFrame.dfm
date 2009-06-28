object frmModule: TfrmModule
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object lv: TListView
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    Columns = <
      item
        Caption = 'StartAddr'
      end
      item
        Caption = 'EndAddr'
      end
      item
        Caption = 'SystemModule'
      end
      item
        Caption = 'FileName'
      end
      item
        Caption = 'BinFileVersion'
      end
      item
        Caption = 'FileVersion'
      end
      item
        Caption = 'FileDescription'
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
