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
        Caption = 'Handle'
      end
      item
        Caption = 'FileName'
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
