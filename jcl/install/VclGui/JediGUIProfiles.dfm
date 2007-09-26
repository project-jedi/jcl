object ProfilesFrame: TProfilesFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  AutoScroll = True
  TabOrder = 0
  object MemoComment: TMemo
    Left = 16
    Top = 16
    Width = 281
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    Lines.Strings = (
      
        'Select profile in the list below. Note that only remote profiles' +
        ' logged on local computer and local profiles are available.'
      
        'If a profile has not IDE settings, the JCL won'#39't be installed on' +
        ' it.')
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
    WordWrap = False
  end
end
