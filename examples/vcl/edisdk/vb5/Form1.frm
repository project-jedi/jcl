VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   5670
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   6885
   LinkTopic       =   "Form1"
   ScaleHeight     =   5670
   ScaleWidth      =   6885
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command2 
      Caption         =   "Create File"
      Height          =   495
      Left            =   3120
      TabIndex        =   2
      Top             =   240
      Width           =   3375
   End
   Begin VB.TextBox Text1 
      Height          =   4695
      Left            =   240
      MultiLine       =   -1  'True
      ScrollBars      =   3  'Both
      TabIndex        =   1
      Top             =   840
      Width           =   6375
   End
   Begin VB.CommandButton Command1 
      Caption         =   "LoadFile"
      Height          =   495
      Left            =   240
      TabIndex        =   0
      Top             =   240
      Width           =   2535
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
  Dim F As EDICOMFile
  Dim I As Integer
  Set F = New EDICOMFile
  F.Options = 0
  F.Options = F.Options Or foVariableDelimiterDetection
  F.Options = F.Options Or foUseAltDelimiterDetection
  F.Options = F.Options Or foRemoveCrLf
  F.Options = F.Options Or foRemoveCr
  F.Options = F.Options Or foRemoveLf
  F.Options = F.Options Or foIgnoreGarbageAtEndOfFile
  F.LoadFromFile (App.Path & "\sample.edi")
  Text1.Text = F.Data
  F.Disassemble
  Text1.Text = Text1.Text & vbCrLf
  Text1.Text = Text1.Text & F.Interchange(0).SegmentISA.SegmentId
  Text1.Text = Text1.Text & vbCrLf
  Text1.Text = Text1.Text & F.Interchange(0).FunctionalGroup(0).SegmentGS.SegmentId
  Text1.Text = Text1.Text & vbCrLf
  Text1.Text = Text1.Text & F.Interchange(0).FunctionalGroup(0).TransactionSet(0).SegmentST.SegmentId
  For I = 0 To F.Interchange(0).FunctionalGroup(0).TransactionSet(0).SegmentCount - 1 Step 1
    F.Interchange(0).FunctionalGroup(0).TransactionSet(0).Segment(I).Assemble
    Text1.Text = Text1.Text & vbCrLf
    Text1.Text = Text1.Text & F.Interchange(0).FunctionalGroup(0).TransactionSet(0).Segment(I).Data
  Next
  Text1.Text = Text1.Text & vbCrLf
  Text1.Text = Text1.Text & F.Interchange(0).FunctionalGroup(0).TransactionSet(0).SegmentSE.SegmentId
  Text1.Text = Text1.Text & vbCrLf
  Text1.Text = Text1.Text & F.Interchange(0).FunctionalGroup(0).SegmentGE.SegmentId
  Text1.Text = Text1.Text & vbCrLf
  Text1.Text = Text1.Text & F.Interchange(0).SegmentIEA.SegmentId
End Sub

Private Sub Command2_Click()
  Dim C As EDICOMFile
  Dim I, F, T, S, E As Integer
  Set C = New EDICOMFile
    
  I = C.AddInterchange
  C.Interchange(I).SetDelimiters "~" & vbCrLf, "*", ">"
  With C.Interchange(I).SegmentISA
    .SegmentId = "ISA"
    .DeleteElements
    .AddElements (17)
    .Element(0).Data = "data"
    .Element(1).Data = "data"
    .Element(2).Data = "data"
    .Element(3).Data = "data"
    .Element(4).Data = "data"
    .Element(5).Data = "data"
    .Element(6).Data = "data"
    .Element(7).Data = "data"
    .Element(8).Data = "data"
    .Element(9).Data = "data"
    .Element(10).Data = "data"
    .Element(11).Data = "data"
    .Element(12).Data = "data"
    .Element(13).Data = "data"
    .Element(14).Data = "data"
    .Element(15).Data = "data"
    .Element(16).Data = C.Interchange(I).Delimiters.SS
  End With
  
  F = C.Interchange(I).AddFunctionalGroup
  With C.Interchange(I).FunctionalGroup(F).SegmentGS
    .SegmentId = "GS"
    .DeleteElements
    .AddElements (8)
    .Element(0).Data = "data"
    .Element(1).Data = "data"
    .Element(2).Data = "data"
    .Element(3).Data = "data"
    .Element(4).Data = "data"
    .Element(5).Data = "data"
    .Element(6).Data = "data"
    .Element(7).Data = "data"
  End With
  
  T = C.Interchange(I).FunctionalGroup(F).AddTransactionSet
  With C.Interchange(I).FunctionalGroup(F).TransactionSet(T).SegmentST
    .SegmentId = "ST"
    .DeleteElements
    .AddElements (2)
    .Element(0).Data = "data"
    .Element(1).Data = "data"
  End With
  
  S = C.Interchange(I).FunctionalGroup(F).TransactionSet(T).AddSegment
  With C.Interchange(I).FunctionalGroup(F).TransactionSet(T).Segment(S)
    .SegmentId = "TST"
    .AddElements (2)
    .Element(0).Data = "data 1"
    .Element(1).Data = "data 2"
  End With
  
  With C.Interchange(I).FunctionalGroup(F).TransactionSet(T).SegmentSE
    .SegmentId = "SE"
    .DeleteElements
    .AddElements (2)
    .Element(0).Data = "data"
    .Element(1).Data = "data"
  End With
 
  With C.Interchange(I).FunctionalGroup(F).SegmentGE
    .SegmentId = "GE"
    .DeleteElements
    .AddElements (2)
    .Element(0).Data = "data"
    .Element(1).Data = "data"
  End With
  
  With C.Interchange(I).SegmentIEA
    .SegmentId = "IEA"
    .DeleteElements
    .AddElements (2)
    .Element(0).Data = "data"
    .Element(1).Data = "data"
  End With
  
  Text1.Text = C.Assemble
  
End Sub
