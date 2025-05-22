object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 706
  ClientWidth = 1034
  Color = clGradientActiveCaption
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsMDIForm
  WindowState = wsMaximized
  OnCreate = FormCreate
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 225
    Top = 50
    Height = 637
    ExplicitLeft = 256
    ExplicitTop = 248
    ExplicitHeight = 100
  end
  object Splitter2: TSplitter
    Left = 846
    Top = 50
    Height = 637
    Align = alRight
    ExplicitLeft = 233
    ExplicitTop = 58
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 50
    Width = 225
    Height = 637
    Align = alLeft
    AutoExpand = True
    BorderStyle = bsNone
    Ctl3D = True
    Indent = 19
    MultiSelect = True
    MultiSelectStyle = []
    ParentCtl3D = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ToolTips = False
    ExplicitHeight = 656
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1034
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Builder UI'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    ExplicitTop = -6
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 687
    Width = 1034
    Height = 19
    Panels = <>
    ExplicitLeft = 440
    ExplicitTop = 640
    ExplicitWidth = 0
  end
  object Panel2: TPanel
    Left = 849
    Top = 50
    Width = 185
    Height = 637
    Align = alRight
    Caption = 'Panel2'
    TabOrder = 3
    ExplicitLeft = 641
    ExplicitTop = -54
    object Button1: TButton
      Left = 1
      Top = 611
      Width = 183
      Height = 25
      Align = alBottom
      Caption = 'Render'
      TabOrder = 0
      OnClick = Button1Click
      ExplicitLeft = 110
      ExplicitTop = 612
      ExplicitWidth = 75
    end
    object Memo: TMemo
      Left = 1
      Top = 1
      Width = 183
      Height = 610
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 1
      ExplicitLeft = -14
      ExplicitTop = 0
      ExplicitWidth = 199
      ExplicitHeight = 637
    end
  end
end
