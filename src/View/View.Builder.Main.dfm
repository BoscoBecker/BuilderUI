object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 706
  ClientWidth = 1034
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsMDIForm
  Visible = True
  WindowState = wsMaximized
  OnClick = BtnRenderClick
  OnCreate = FormCreate
  TextHeight = 15
  object Splitter2: TSplitter
    Left = 789
    Top = 42
    Height = 616
    Align = alRight
    ExplicitLeft = 232
    ExplicitTop = 49
    ExplicitHeight = 622
  end
  object SkPaintBackground: TSkPaintBox
    AlignWithMargins = True
    Left = 244
    Top = 45
    Width = 542
    Height = 610
    Align = alClient
    OnDraw = SkPaintBackgroundDraw
    ExplicitTop = 51
    ExplicitHeight = 604
  end
  object Splitter1: TSplitter
    Left = 50
    Top = 42
    Height = 616
    ExplicitLeft = 824
    ExplicitTop = 59
    ExplicitHeight = 622
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 687
    Width = 1034
    Height = 19
    Panels = <>
  end
  object PanelRenderJson: TPanel
    Left = 792
    Top = 42
    Width = 242
    Height = 616
    Align = alRight
    BevelOuter = bvNone
    Caption = 'PanelRenderJson'
    TabOrder = 1
    Visible = False
    ExplicitTop = 65
    ExplicitHeight = 622
    object BtnRender: TButton
      Left = 0
      Top = 591
      Width = 242
      Height = 25
      Align = alBottom
      Caption = 'Render'
      TabOrder = 0
      OnClick = BtnRenderClick
      ExplicitTop = 597
      ExplicitWidth = 210
    end
    object Memo: TMemo
      Left = 0
      Top = 35
      Width = 242
      Height = 521
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 1
      OnChange = MemoChange
      ExplicitLeft = 6
      ExplicitTop = 33
      ExplicitWidth = 210
      ExplicitHeight = 562
    end
    object PanelValidateJson: TPanel
      Left = 0
      Top = 556
      Width = 242
      Height = 35
      Align = alBottom
      BevelOuter = bvNone
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 2
      ExplicitTop = 8
      ExplicitWidth = 210
      object SkLblVerify: TSkLabel
        Left = 16
        Top = 8
        Width = 0
        Height = 0
        Words = <
          item
          end>
      end
    end
    object Panel13: TPanel
      Left = 0
      Top = 0
      Width = 242
      Height = 35
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      ExplicitTop = 8
      ExplicitWidth = 210
      object Image12: TImage
        Left = 202
        Top = 0
        Width = 40
        Height = 35
        Align = alRight
        Center = True
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000140000
          001408060000008D891D0D000000017352474200AECE1CE90000000473424954
          080808087C086488000000C34944415478DA6364A032601C7206B202B112947D
          0F887FE3D087531DBA81EA406C0465BF07E2BD580C0519E60CC48250FE3920BE
          498C81D80C45378CA081200D2E402C80C550062C867D00E23DF8BC8CCF500642
          86E1321097A10C840CC367203E43711A468C81E8612604C4A1401C0035986803
          7119D60275F1052076C4662823918681345600B11D921856431989340C1466DC
          407C0088F5F1194A2861A347800016430B8178023106E28A4D7443F11A486CE1
          00323401CA5E80CFCB1483C16F2000C0903915693203080000000049454E44AE
          426082}
        OnClick = Image12Click
        ExplicitLeft = 144
        ExplicitTop = 1
        ExplicitHeight = 39
      end
      object SkLabel3: TSkLabel
        Left = 16
        Top = 8
        Width = 75
        Height = 19
        Words = <
          item
            Caption = 'Render Json'
          end>
      end
    end
  end
  object SplitView1: TSplitView
    Left = 0
    Top = 42
    Width = 50
    Height = 616
    CloseStyle = svcCompact
    DockSite = True
    FullRepaint = False
    Locked = True
    Opened = False
    OpenedWidth = 100
    ParentShowHint = False
    Placement = svpLeft
    ShowHint = True
    TabOrder = 2
    ExplicitTop = 65
    ExplicitHeight = 622
    object lblOpenModel: TLabel
      Left = 0
      Top = 179
      Width = 50
      Height = 15
      Align = alTop
      Alignment = taCenter
      Caption = 'Open Model'
      Visible = False
      ExplicitWidth = 66
    end
    object lblTreeJson: TLabel
      Left = 0
      Top = 235
      Width = 50
      Height = 15
      Align = alTop
      Alignment = taCenter
      Caption = 'Tree Json '
      Visible = False
    end
    object lblRenderJson: TLabel
      Left = 0
      Top = 123
      Width = 50
      Height = 15
      Align = alTop
      Alignment = taCenter
      Caption = 'Render Json'
      Visible = False
      ExplicitWidth = 63
    end
    object Panel6: TPanel
      Left = 0
      Top = 575
      Width = 50
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      TabOrder = 0
      ExplicitTop = 581
      ExplicitWidth = 200
      object Image6: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Align = alClient
        Center = True
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
          00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
          080808087C086488000001044944415478DA6364A031601C2C168801B1051073
          43F95F81F80410BFA296057E4886C300C8924DD4B04011EA7A6C00E48BFBA45A
          A00B0D922F402C08C5F8C07B28E601E227407C139F0532406C4B64B0E10287A1
          1661B5005B58930A50E206D90250B038E3D0240AC4B1402C0FE53F04E2C540FC
          1A87FABD0CD014866C8100107BE230BC8D017B2AAAC261C97620FE802D888C80
          581D4DAC08884D70B8F40C10F7A1898122F91CB6206280BAD20F4D6C0E1073E1
          B000E4FA7C34B14D50DF0D8C05340D227C91DC8EC517DF80B892818448A67932
          05019A663410A079510102EA508BBE427D2346C0C057486A091676D800D58B6B
          6C80A6150E03031DAA4CB201CD2D00006CFB4419805E41F20000000049454E44
          AE426082}
        OnClick = Image6Click
        ExplicitLeft = 4
        ExplicitTop = -64
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object Panel9: TPanel
      Left = 0
      Top = 250
      Width = 50
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      TabOrder = 1
      ExplicitTop = 41
      ExplicitWidth = 200
      object Image9: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Align = alClient
        Center = True
        ParentShowHint = False
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
          00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
          080808087C086488000000C24944415478DA6364A031601C2C16B002B11110CB
          42D9F780F81C10FFA69605B6402C8326761F884F50CB82482C6220D7AF193216
          D03C8840116B01C44A50F615203ECB40C5488681FFA4EAA3BB0502405C0FC401
          40AC40A2E50F8078011037E2B3600210E79368303A9808C405B82CF800C4FC40
          6C08C417480C2203203E0FF589222E0B088531C9F274B3005698A903311B105F
          66402DCCC89687594028A7922D0FB380505943B63CDD2CA07910C10A3371281B
          24895C98912D3F68EAE4C16B010032714A19EE6C91330000000049454E44AE42
          6082}
        ShowHint = True
        OnClick = Image9Click
        ExplicitLeft = 4
        ExplicitTop = -64
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object Panel10: TPanel
      Left = 0
      Top = 0
      Width = 50
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      TabOrder = 2
      ExplicitWidth = 200
      object Image10: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Align = alClient
        Center = True
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
          00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
          080808087C0864880000005B4944415478DA6364A031601C76163800F17C2056
          20D3BC07409C08C40770590052204FA1A3416628E2B2E00310F35368C143E410
          C016440B28F005C8F0047C41447530FC2C7060184DA6A3C974D05BE0C0309A4C
          4793E9A0B30000D7791E19EC6227AC0000000049454E44AE426082}
        OnClick = Image6Click
        ExplicitLeft = 4
        ExplicitTop = -64
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 41
      Width = 50
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      TabOrder = 3
      ExplicitWidth = 200
      object Image1: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Align = alClient
        Center = True
        OnClick = Image6Click
        ExplicitLeft = 4
        ExplicitTop = -64
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 534
      Width = 50
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      TabOrder = 4
      ExplicitTop = 581
      ExplicitWidth = 200
      object Image2: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Align = alClient
        Center = True
        OnClick = Image6Click
        ExplicitLeft = 4
        ExplicitTop = -64
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object Panel7: TPanel
      Left = 0
      Top = 138
      Width = 50
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      TabOrder = 5
      ExplicitTop = 41
      ExplicitWidth = 200
      object Image3: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Align = alClient
        Center = True
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
          00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
          080808087C086488000000C44944415478DA6364A031601C36160800713D1007
          00B10211FABE01F15A203E03C40F80F81E105FC167C10420CE27C1618B81783B
          9AD84D203E87CB820F40CC4F8205A940FC154D0CC4DF84CB82FF24180E025138
          C49753C307AF197007274E0B1C80780110CB1361F84C20BE46AA05C82092C4E0
          1AB560D4822166C13228FD00881381F800AD2C8059A288CD82102066A58205E0
          10C266812E10EB9069C174064499F610881570D56846402C03C4DC245A20C900
          290841A57302038E38A02A18FA160000E33529192B82BF530000000049454E44
          AE426082}
        OnClick = Image3Click
        ExplicitLeft = 4
        ExplicitTop = -64
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object Panel8: TPanel
      Left = 0
      Top = 82
      Width = 50
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      TabOrder = 6
      ExplicitTop = 41
      ExplicitWidth = 200
      object Image4: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Align = alClient
        Center = True
        OnClick = Image6Click
        ExplicitLeft = 4
        ExplicitTop = -64
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object Panel12: TPanel
      Left = 0
      Top = 194
      Width = 50
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      TabOrder = 7
      ExplicitTop = 41
      ExplicitWidth = 200
      object Image8: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Align = alClient
        Center = True
        ParentShowHint = False
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
          00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
          080808087C086488000000CF4944415478DA6364A03160A4A7050E403C1F8815
          B0A83B00C48140FC81120B1E00B13C1EB585403C81120BFE13507B01880D6969
          013100140A0B80B8915616C0C044202EA0A505209F28225BC00DC4C940FC9A4A
          168802F15C20FE0AB3C01988C5A8E803107805C47B61164452D97018580EB3C0
          0F1A4CD4045F817813DD8248178875A86CC11520BE0CB30094A42CA86CC10920
          BE0FB3400C1A4CD4047B41C1849CD1A89D92968308640BA89992C02908DD026A
          46343882D12D8059A248814F402EBF0F331C9B055407A3161004007B3226199A
          A3EA8D0000000049454E44AE426082}
        ShowHint = True
        OnClick = Image6Click
        ExplicitLeft = 4
        ExplicitTop = -64
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
  end
  object PanelTree: TPanel
    Left = 53
    Top = 42
    Width = 188
    Height = 616
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'PanelTree'
    TabOrder = 3
    Visible = False
    ExplicitTop = 65
    ExplicitHeight = 622
    object Panel11: TPanel
      Left = 0
      Top = 0
      Width = 188
      Height = 35
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitTop = 8
      ExplicitWidth = 199
      object Image5: TImage
        Left = 148
        Top = 0
        Width = 40
        Height = 35
        Align = alRight
        Center = True
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000140000
          001408060000008D891D0D000000017352474200AECE1CE90000000473424954
          080808087C086488000000C34944415478DA6364A032601C7206B202B112947D
          0F887FE3D087531DBA81EA406C0465BF07E2BD580C0519E60CC48250FE3920BE
          498C81D80C45378CA081200D2E402C80C550062C867D00E23DF8BC8CCF500642
          86E1321097A10C840CC367203E43711A468C81E8612604C4A1401C0035986803
          7119D60275F1052076C4662823918681345600B11D921856431989340C1466DC
          407C0088F5F1194A2861A347800016430B8178023106E28A4D7443F11A486CE1
          00323401CA5E80CFCB1483C16F2000C0903915693203080000000049454E44AE
          426082}
        OnClick = Image5Click
        ExplicitLeft = 144
        ExplicitTop = 1
        ExplicitHeight = 39
      end
      object Image7: TImage
        Left = 108
        Top = 0
        Width = 40
        Height = 35
        Align = alRight
        Center = True
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000140000
          001408060000008D891D0D000000017352474200AECE1CE90000000473424954
          080808087C086488000000A14944415478DA6364A03260A4A58109402C4FA639
          0F807821B2810E40BC9F42C73902F10198810A200E052E7C0875D403F4301403
          6267120DDB0BC4AF601C6C91A208C416441A760288EF230BE08A650BA8C1F8C0
          7DA88128005FB27186060136F00AEA550C80CF406E20F6C321B70988BF926A20
          2E57E2741D3106EA02B10E9AD81520BE3C6AE03032901588ED1850930E467623
          C540640033F4153E45542FB1014D812215545E7E4C0000000049454E44AE4260
          82}
        OnClick = Image6Click
        ExplicitLeft = 144
        ExplicitTop = 1
        ExplicitHeight = 39
      end
      object SkLabel1: TSkLabel
        Left = 16
        Top = 8
        Width = 76
        Height = 19
        Words = <
          item
            Caption = 'Json objects'
          end>
      end
    end
    object TreeView1: TTreeView
      Left = 0
      Top = 35
      Width = 188
      Height = 581
      Align = alClient
      AutoExpand = True
      BorderStyle = bsNone
      Ctl3D = True
      Indent = 19
      MultiSelect = True
      MultiSelectStyle = []
      ParentCtl3D = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      ToolTips = False
      ExplicitWidth = 190
      ExplicitHeight = 587
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 658
    Width = 1034
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Loading  . . .'
    TabOrder = 4
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1034
    Height = 42
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Builder UI'
    DockSite = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Segoe UI'
    Font.Style = []
    Padding.Bottom = 15
    ParentBackground = False
    ParentFont = False
    TabOrder = 5
  end
end
