object frmStack2: TfrmStack2
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object lbStack: TListBox
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Style = lbOwnerDrawVariable
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = lbStackDblClick
    OnDrawItem = lbStackDrawItem
    OnMeasureItem = lbStackMeasureItem
  end
end
