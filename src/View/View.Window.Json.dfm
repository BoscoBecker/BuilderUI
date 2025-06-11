object FormJson: TFormJson
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Options to View and edit your Json'
  ClientHeight = 693
  ClientWidth = 1095
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 941
    Top = 41
    Height = 652
    Align = alRight
    ExplicitLeft = 990
    ExplicitTop = 47
    ExplicitHeight = 622
  end
  object SynEditJson: TSynEdit
    Left = 0
    Top = 41
    Width = 941
    Height = 652
    Align = alClient
    Color = clWhite
    Ctl3D = True
    ParentCtl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    BorderStyle = bsNone
    Gutter.BorderStyle = gbsNone
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Gutter.LineNumberStart = 0
    Highlighter = SynJSON
    RightEdge = 0
    OnChange = SynEditJsonChange
    OnStatusChange = SynEditJsonStatusChange
    FontSmoothing = fsmClearType
    ExplicitWidth = 873
    ExplicitHeight = 622
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1095
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object SkLabelTitle: TSkLabel
      Left = 0
      Top = 0
      Width = 1095
      Height = 33
      Align = alTop
      TextSettings.Font.Size = 25.000000000000000000
      TextSettings.HorzAlign = Center
      Words = <
        item
          Caption = 'View and edit your Json'
        end>
      ExplicitLeft = 363
      ExplicitTop = 8
      ExplicitWidth = 304
    end
  end
  object SynEditMiniMap: TSynEdit
    Left = 944
    Top = 41
    Width = 151
    Height = 652
    Cursor = crArrow
    Align = alRight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -3
    Font.Name = 'Courier New'
    Font.Style = []
    Font.Quality = fqAntialiased
    TabOrder = 2
    OnEnter = SynEditMiniMapEnter
    OnMouseUp = SynEditMiniMapMouseUp
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    BorderStyle = bsNone
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 0
    Gutter.Visible = False
    Gutter.Width = 0
    Lines.Strings = (
      'SynEditMiniMap')
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoNoCaret, eoNoSelection, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoSpecialLineDefaultFg, eoTabsToSpaces]
    ReadOnly = True
    RightEdge = 0
    ScrollBars = ssNone
    TabWidth = 2
    WordWrapGlyph.Visible = False
    OnSpecialLineColors = SynEditMiniMapSpecialLineColors
    FontSmoothing = fsmNone
    ExplicitHeight = 622
  end
  object SynJSON: TSynJSONSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    AttributeAttri.Foreground = 16744448
    NumberAttri.Foreground = clTeal
    SymbolAttri.Foreground = clFuchsia
    ValueAttri.Foreground = 25284
    Left = 1016
    Top = 632
  end
end
