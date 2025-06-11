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
    Lines.Strings = (
      '{'
      '  "Forms": ['
      '    {'
      '      "Name": "FrmCustomerRegistration",'
      '      "Caption": "Customer Registration",'
      '      "Type": "TForm",'
      '      "Align": "Client",'
      '      "Width": 800,'
      '      "Height": 600,'
      '      "Children": ['
      '        {'
      '          "Name": "LblTituloCliente",'
      '          "Type": "TLabel",'
      '          "Text": "Customer Registration",'
      '          "Position": { "X": 5, "Y": 10 },'
      '          "Width": 300,'
      '          "Height": 30,'
      '          "FontSize": 16,'
      '          "FontStyle": ["Bold"]'
      '        },'
      '        {'
      '          "Name": "LblNomeCompleto",'
      '          "Type": "TLabel",'
      '          "Text": "Full Name:",'
      '          "Position": { "X": 5, "Y": 50 },'
      '          "Width": 300,'
      '          "Height": 20'
      '        },'
      '        {'
      '          "Name": "EdtNomeCompleto",'
      '          "Type": "TEdit",'
      '          "Text": "",'
      '          "Position": { "X": 5, "Y": 70 },'
      '          "Width": 300,'
      '          "Height": 30'
      '        },'
      '        {'
      '          "Name": "LblTelefone",'
      '          "Type": "TLabel",'
      '          "Text": "Phone:",'
      '          "Position": { "X": 5, "Y": 110 },'
      '          "Width": 300,'
      '          "Height": 20'
      '        },'
      '        {'
      '          "Name": "EdtTelefone",'
      '          "Type": "TEdit",'
      '          "Text": "",'
      '          "Position": { "X": 5, "Y": 130 },'
      '          "Width": 300,'
      '          "Height": 30'
      '        },'
      '        {'
      '          "Name": "BtnSalvarCliente",'
      '          "Type": "TButton",'
      '          "Caption": "Save",'
      '          "Position": { "X": 5, "Y": 180 },'
      '          "Width": 140,'
      '          "Height": 40'
      '        },'
      '        {'
      '          "Name": "BtnCancelarCliente",'
      '          "Type": "TButton",'
      '          "Caption": "Cancel",'
      '          "Position": { "X": 165, "Y": 180 },'
      '          "Width": 140,'
      '          "Height": 40'
      '        }'
      '      ]'
      '    },'
      '    {'
      '      "Name": "FrmAddressCustomer",'
      '      "Caption": "Customer Address",'
      '      "Type": "TForm",'
      '      "Align": "Client",'
      '      "Width": 800,'
      '      "Height": 600,'
      '      "Children": ['
      '        {'
      '          "Name": "LblTituloEndereco",'
      '          "Type": "TLabel",'
      '          "Text": "Customer Address",'
      '          "Position": { "X": 5, "Y": 10 },'
      '          "Width": 300,'
      '          "Height": 30,'
      '          "FontSize": 16,'
      '          "FontStyle": ["Bold"]'
      '        },'
      '        {'
      '          "Name": "LblLogradouro",'
      '          "Type": "TLabel",'
      '          "Text": "Street:",'
      '          "Position": { "X": 5, "Y": 50 },'
      '          "Width": 300,'
      '          "Height": 20'
      '        },'
      '        {'
      '          "Name": "EdtLogradouro",'
      '          "Type": "TEdit",'
      '          "Text": "",'
      '          "Position": { "X": 5, "Y": 70 },'
      '          "Width": 300,'
      '          "Height": 30'
      '        },'
      '        {'
      '          "Name": "LblNumero",'
      '          "Type": "TLabel",'
      '          "Text": "Number:",'
      '          "Position": { "X": 5, "Y": 110 },'
      '          "Width": 150,'
      '          "Height": 20'
      '        },'
      '        {'
      '          "Name": "EdtNumero",'
      '          "Type": "TEdit",'
      '          "Text": "",'
      '          "Position": { "X": 5, "Y": 130 },'
      '          "Width": 150,'
      '          "Height": 30'
      '        },'
      '        {'
      '          "Name": "LblCidade",'
      '          "Type": "TLabel",'
      '          "Text": "City:",'
      '          "Position": { "X": 5, "Y": 170 },'
      '          "Width": 150,'
      '          "Height": 20'
      '        },'
      '        {'
      '          "Name": "EdtCidade",'
      '          "Type": "TEdit",'
      '          "Caption": "",'
      '          "Position": { "X": 5, "Y": 190 },'
      '          "Width": 150,'
      '          "Height": 30'
      '        },'
      '        {'
      '          "Name": "BtnSalvarEndereco",'
      '          "Type": "TButton",'
      '          "Caption": "Save Address",'
      '          "Position": { "X": 5, "Y": 240 },'
      '          "Width": 150,'
      '          "Height": 40'
      '        }'
      '      ]'
      '    },'
      '    {'
      '      "Type": "TForm",'
      '      "Name": "FrmButtons",'
      '      "Caption": "Button List",'
      '      "Align": "Client",'
      '      "Width": 800,'
      '      "Height": 600,'
      '      "Children": ['
      '        {'
      '          "Type": "TButton",'
      '          "Name": "BtnLimparCampos",'
      '          "Caption": "Clear Fields",'
      '          "Position": { "X": 5, "Y": 100 },'
      '          "Width": 150,'
      '          "Height": 40'
      '        },'
      '        {'
      '          "Type": "TButton",'
      '          "Name": "BtnGerarPDF",'
      '          "Caption": "Export PDF",'
      '          "Position": { "X": 165, "Y": 150 },'
      '          "Width": 140,'
      '          "Height": 40'
      '        },'
      '        {'
      '          "Type": "TButton",'
      '          "Name": "BtnEnviarEmail",'
      '          "Caption": "Send by Email",'
      '          "Position": { "X": 5, "Y": 200 },'
      '          "Width": 120,'
      '          "Height": 120'
      '        }'
      '      ]'
      '    },'
      '    {'
      '      "Name": "FrmLogin",'
      '      "Caption": "Login",'
      '      "Type": "TForm",'
      '      "Align": "Client",'
      '      "Width": 800,'
      '      "Height": 600,'
      '      "Children": ['
      '        {'
      '          "Name": "PnlLogin",'
      '          "Type": "TPanel",'
      '          "Caption": "",'
      '          "Position": { "X": 80, "Y": 60 },'
      '          "Width": 340,'
      '          "Height": 220,'
      '          "Color": "#F0F0F0",'
      '          "Children": ['
      '            {'
      '              "Name": "LblTitulo",'
      '              "Type": "TLabel",'
      '              "Text": "Welcome! Please Login",'
      '              "Position": { "X": 20, "Y": 20 },'
      '              "Width": 300,'
      '              "Height": 30,'
      '              "FontSize": 18,'
      '              "FontStyle": ["Bold"],'
      '              "Alignment": "Center"'
      '            },'
      '            {'
      '              "Name": "LblUsuario",'
      '              "Type": "TLabel",'
      '              "Text": "Username:",'
      '              "Position": { "X": 20, "Y": 70 },'
      '              "Width": 80,'
      '              "Height": 20'
      '            },'
      '            {'
      '              "Name": "EdtUsuario",'
      '              "Type": "TEdit",'
      '              "Text": "",'
      '              "Position": { "X": 110, "Y": 65 },'
      '              "Width": 200,'
      '              "Height": 28'
      '            },'
      '            {'
      '              "Name": "LblSenha",'
      '              "Type": "TLabel",'
      '              "Text": "Password:",'
      '              "Position": { "X": 20, "Y": 110 },'
      '              "Width": 80,'
      '              "Height": 20'
      '            },'
      '            {'
      '              "Name": "EdtSenha",'
      '              "Type": "TEdit",'
      '              "Text": "",'
      '              "PasswordChar": "*",'
      '              "Position": { "X": 110, "Y": 105 },'
      '              "Width": 200,'
      '              "Height": 28'
      '            },'
      '            {'
      '              "Name": "BtnEntrar",'
      '              "Type": "TButton",'
      '              "Caption": "Login",'
      '              "Position": { "X": 110, "Y": 160 },'
      '              "Width": 120,'
      '              "Height": 36'
      '            }'
      '          ]'
      '        }'
      '      ]'
      '    },'
      '    {'
      '      "Name": "FrmPrincipal",'
      '      "Caption": "Main Application",'
      '      "Type": "TForm",'
      '      "Align": "Client",'
      '      "Width": 800,'
      '      "Height": 600,'
      '      "Children": ['
      '        {'
      '          "Name": "PnlTopo",'
      '          "Type": "TPanel",'
      '          "Caption": "",'
      '          "Position": { "X": 0, "Y": 0 },'
      '          "Width": 800,'
      '          "Height": 60,'
      '          "Color": "#1976D2",'
      '          "Children": ['
      '            {'
      '              "Name": "LblAppTitle",'
      '              "Type": "TLabel",'
      '              "Text": "BuilderUI - Main Screen",'
      '              "Position": { "X": 20, "Y": 15 },'
      '              "Width": 400,'
      '              "Height": 30,'
      '              "FontSize": 20,'
      '              "FontStyle": ["Bold"],'
      '              "FontColor": "#FFFFFF"'
      '            }'
      '          ]'
      '        },'
      '        {'
      '          "Name": "PnlMenu",'
      '          "Type": "TPanel",'
      '          "Caption": "",'
      '          "Position": { "X": 0, "Y": 60 },'
      '          "Width": 180,'
      '          "Height": 540,'
      '          "Color": "#E3E3E3",'
      '          "Children": ['
      '            {'
      '              "Name": "BtnClientes",'
      '              "Type": "TButton",'
      '              "Caption": "Customers",'
      '              "Position": { "X": 20, "Y": 30 },'
      '              "Width": 140,'
      '              "Height": 40'
      '            },'
      '            {'
      '              "Name": "BtnProdutos",'
      '              "Type": "TButton",'
      '              "Caption": "Products",'
      '              "Position": { "X": 20, "Y": 80 },'
      '              "Width": 140,'
      '              "Height": 40'
      '            },'
      '            {'
      '              "Name": "BtnSair",'
      '              "Type": "TButton",'
      '              "Caption": "Exit",'
      '              "Position": { "X": 20, "Y": 500 },'
      '              "Width": 140,'
      '              "Height": 40'
      '            }'
      '          ]'
      '        },'
      '        {'
      '          "Name": "PnlConteudo",'
      '          "Type": "TPanel",'
      '          "Caption": "",'
      '          "Position": { "X": 180, "Y": 60 },'
      '          "Width": 620,'
      '          "Height": 540,'
      '          "Color": "#FFFFFF",'
      '          "Children": ['
      '            {'
      '              "Name": "LblBemVindo",'
      '              "Type": "TLabel",'
      '              "Text": "Welcome to BuilderUI!",'
      '              "Position": { "X": 40, "Y": 40 },'
      '              "Width": 400,'
      '              "Height": 40,'
      '              "FontSize": 18,'
      '              "FontStyle": ["Bold"]'
      '            }'
      '          ]'
      '        }'
      '      ]'
      '    },'
      '    {'
      '      "Name": "FrmSobre",'
      '      "Caption": "About BuilderUI",'
      '      "Type": "TForm",'
      '      "Align": "Client",'
      '      "Width": 800,'
      '      "Height": 600,'
      '      "Children": ['
      '        {'
      '          "Name": "LblTituloSobre",'
      '          "Type": "TLabel",'
      '          "Text": "About BuilderUI in Delphi",'
      '          "Position": { "X": 30, "Y": 20 },'
      '          "Width": 400,'
      '          "Height": 32,'
      '          "FontSize": 18,'
      '          "FontStyle": ["Bold"]'
      '        },'
      '        {'
      '          "Name": "ImgLogo",'
      '          "Type": "TImage",'
      '          "Position": { "X": 30, "Y": 70 },'
      '          "Width": 120,'
      '          "Height": 120,'
      '          "Picture": "logo_builderui.png"'
      '        },'
      '        {'
      '          "Name": "LblDescricao",'
      '          "Type": "TLabel",'
      
        '          "Text": "BuilderUI is a solution developed in Delphi t' +
        'o create dynamic interfaces from JSON files. With it, you can bu' +
        'ild registration, query, and navigation screens without changing' +
        ' the source code, making development faster, more flexible, and ' +
        'modern.",'
      '          "Position": { "X": 170, "Y": 80 },'
      '          "Width": 400,'
      '          "Height": 100,'
      '          "WordWrap": true,'
      '          "FontSize": 12'
      '        }'
      '      ]'
      '    }'
      '  ]'
      '}')
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
