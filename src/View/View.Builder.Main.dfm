object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Builder UI for  Template Forms'
  ClientHeight = 706
  ClientWidth = 1034
  Color = clWhite
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsMDIForm
  Position = poScreenCenter
  Visible = True
  WindowState = wsMaximized
  OnCanResize = FormCanResize
  OnClick = BtnRenderClick
  OnCreate = FormCreate
  OnShow = FormShow
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
  object Splitter1: TSplitter
    Left = 273
    Top = 42
    Height = 616
    ExplicitLeft = 244
    ExplicitTop = 36
  end
  object SkPaintBackground: TSkPaintBox
    AlignWithMargins = True
    Left = 279
    Top = 45
    Width = 507
    Height = 610
    Align = alClient
    OnResize = SkPaintBackgroundResize
    OnDraw = SkPaintBackgroundDraw
    ExplicitLeft = 6
    ExplicitTop = 24
    ExplicitWidth = 542
    ExplicitHeight = 540
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
    object BtnRender: TButton
      Left = 0
      Top = 591
      Width = 242
      Height = 25
      Align = alBottom
      Caption = 'Render'
      TabOrder = 0
      OnClick = BtnRenderClick
    end
    object Memo: TMemo
      Left = 0
      Top = 33
      Width = 242
      Height = 523
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 1
      OnChange = MemoChange
      ExplicitTop = 35
      ExplicitHeight = 521
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
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      object Image12: TImage
        Left = 202
        Top = 0
        Width = 40
        Height = 33
        Cursor = crHandPoint
        Hint = 'Close'
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
    ExplicitTop = -1
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
      object ImgSettings: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Cursor = crHandPoint
        Hint = 'Settings'
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
        OnClick = ImgSettingsClick
        ExplicitLeft = 4
        ExplicitTop = -64
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object Panel9: TPanel
      Left = 0
      Top = 123
      Width = 50
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      TabOrder = 1
      ExplicitTop = 250
      object Image9: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Cursor = crHandPoint
        Hint = 'Tree of components'
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
      object Image10: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Cursor = crHandPoint
        Align = alClient
        Center = True
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
          00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
          080808087C0864880000005B4944415478DA6364A031601C76163800F17C2056
          20D3BC07409C08C40770590052204FA1A3416628E2B2E00310F35368C143E410
          C016440B28F005C8F0047C41447530FC2C7060184DA6A3C974D05BE0C0309A4C
          4793E9A0B30000D7791E19EC6227AC0000000049454E44AE426082}
        OnClick = ImgSettingsClick
        ExplicitLeft = 4
        ExplicitTop = -64
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object Panel7: TPanel
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
      ExplicitTop = 67
      object ImageRenderJson: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Cursor = crHandPoint
        Hint = 'Render Json'
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
        OnClick = ImageRenderJsonClick
        ExplicitLeft = 4
        ExplicitTop = 6
      end
    end
    object Panel12: TPanel
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
      TabOrder = 4
      ExplicitTop = 194
      object ImageOpenTemplate: TImage
        Left = 2
        Top = 2
        Width = 46
        Height = 37
        Cursor = crHandPoint
        Hint = 'Open template'
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
        OnClick = ImgSettingsClick
        ExplicitLeft = 4
        ExplicitTop = -64
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object PanelSettings: TPanel
      Left = 0
      Top = 561
      Width = 50
      Height = 14
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 5
      Visible = False
      ExplicitTop = 520
      object SkLabelSettings: TSkLabel
        AlignWithMargins = True
        Left = 3
        Top = -2
        Width = 44
        Height = 13
        Align = alBottom
        AutoSize = False
        TextSettings.Font.Size = 10.000000000000000000
        TextSettings.HorzAlign = Center
        Words = <
          item
            Caption = 'Settings'
          end>
        ExplicitLeft = 0
        ExplicitTop = 559
        ExplicitWidth = 50
      end
    end
  end
  object PanelTree: TPanel
    Left = 50
    Top = 42
    Width = 223
    Height = 616
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'PanelTree'
    Padding.Left = 5
    Padding.Right = 5
    TabOrder = 3
    Visible = False
    object Panel11: TPanel
      Left = 5
      Top = 33
      Width = 213
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 0
      ExplicitLeft = 0
      ExplicitWidth = 223
      object ImageExpand: TImage
        Left = 32
        Top = 0
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'Expand Tree'
        Align = alLeft
        Center = True
        ParentShowHint = False
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
          00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
          080808087C0864880000007A4944415478DA6364A031601C51168801B105947D
          02885F51DB023F20E686B2BF02F1266A5B1089C65F3E6AC1D0B4009614B989D1
          8C058052174A1246B7003929920B509230BA052140CC4A4B0B681E44F8C07F34
          3E517A472D185A167C00627E28FB21102B50DB0207205E00652700F1016A5B40
          1618FA16000092DE1C19EE8DA86E0000000049454E44AE426082}
        ShowHint = True
        OnClick = ImageExpandClick
      end
      object ImageColapse: TImage
        Left = 0
        Top = 0
        Width = 32
        Height = 32
        Cursor = crHandPoint
        Hint = 'Colapse Tree'
        Align = alLeft
        Center = True
        ParentShowHint = False
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
          00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
          080808087C086488000000534944415478DA6364A031601CB560D482510BE86F
          8118105B0031371E3DCBF0C83D00E244203E80CB023F028613B2006689222E0B
          42809895420B1E02B102AD8208647802BE20A23A18B560D482510BE860010041
          470A19E888BC3E0000000049454E44AE426082}
        ShowHint = True
        OnClick = ImageColapseClick
      end
    end
    object TreeView1: TTreeView
      Left = 5
      Top = 90
      Width = 213
      Height = 526
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
      OnClick = TreeView1Click
      ExplicitLeft = 48
      ExplicitTop = 138
      ExplicitWidth = 223
      ExplicitHeight = 558
    end
    object PanelSearchComponents: TPanel
      Left = 5
      Top = 65
      Width = 213
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      Caption = 'PanelSearchComponents'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 2
      Visible = False
      ExplicitLeft = 0
      ExplicitTop = 35
      ExplicitWidth = 188
      object SearchBoxComponents: TSearchBox
        Left = 24
        Top = 0
        Width = 189
        Height = 25
        Align = alClient
        TabOrder = 0
        TextHint = 'Type: label1'
        OnChange = SearchBoxComponentsChange
        ExplicitWidth = 164
        ExplicitHeight = 23
      end
      object ActivityIndicatorSearch: TActivityIndicator
        Left = 0
        Top = 0
        Align = alLeft
        IndicatorSize = aisSmall
      end
    end
    object Panel3: TPanel
      Left = 5
      Top = 0
      Width = 213
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 3
      ExplicitLeft = 0
      ExplicitTop = 8
      ExplicitWidth = 223
      object Image1: TImage
        Left = 173
        Top = 0
        Width = 40
        Height = 33
        Cursor = crHandPoint
        Hint = 'Close'
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
        ExplicitLeft = 144
        ExplicitTop = 1
        ExplicitHeight = 39
      end
      object Image2: TImage
        Left = 133
        Top = 0
        Width = 40
        Height = 33
        Cursor = crHandPoint
        Hint = 'Filter'
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
        OnClick = Image2Click
        ExplicitLeft = 144
        ExplicitTop = 1
        ExplicitHeight = 39
      end
      object SkLabel2: TSkLabel
        Left = 6
        Top = 8
        Width = 80
        Height = 19
        Words = <
          item
            Caption = 'Components'
          end>
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 658
    Width = 1034
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
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
  object PanelToolPalette: TPanel
    Left = 400
    Top = 570
    Width = 289
    Height = 54
    Cursor = crHandPoint
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 6
    OnMouseDown = PanelToolPaletteMouseDown
    OnMouseMove = PanelToolPaletteMouseMove
    OnMouseUp = PanelToolPaletteMouseUp
    object SkPaintBox1: TSkPaintBox
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 281
      Height = 46
      Align = alClient
      OnMouseDown = SkPaintBox1MouseDown
      OnMouseMove = SkPaintBox1MouseMove
      OnMouseUp = SkPaintBox1MouseUp
      OnDraw = SkPaintBox1Draw
      ExplicitLeft = 48
      ExplicitTop = 240
      ExplicitWidth = 50
      ExplicitHeight = 50
    end
    object Image6: TImage
      Left = 8
      Top = 9
      Width = 41
      Height = 36
      Center = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
        00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
        080808087C086488000000C64944415478DA6364A031601C480B6C81581C88F7
        00F1075A581009A57F5362093116506409B116906D092916906509A916906C09
        3916906409B916106D092516106509A51610B4841A16E0B5845A1680C05720DE
        444B0BEE03F1096A5AF00D88EF01F14B207E0F0D260C408E057640EC03C47B81
        388190B748B1E009107700B115929822103F20D782102066857AFD04D402908B
        E723A95948C817F82C1003624106483823872FC8C5F2C4FA829C2A13DD178E40
        7C809A1680C00420CEA73488A802686E01004E093B1963965899000000004945
        4E44AE426082}
    end
    object Image11: TImage
      Left = 55
      Top = 9
      Width = 41
      Height = 36
      Center = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
        00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
        080808087C086488000000E84944415478DA6364A031601C5616B002B12E10CB
        003137117A4581D80E888391C41E00F102206EC466811110AB93E1484F208E45
        139B08C405E81684407D412A10851A880C403E5144B720920CC3616019163146
        BA5B406E108112C46C622C10832A7E02C46C40AC04C43A580C0B02621320E602
        E2330C9038D022C6026C0094AA8C90F87140EC41A4CF88B200E4623F24FE1CA8
        CB878E05A09C8D1C0FC10CA83997240BD02359116A013A00C58331542D2C9235
        89B180E6C974E4E6646C85DD43205640B7003DC5100BB0A52C507DD0806E0108
        80722D25150EC8E50B608663B380EA60E85B0000D1351E1964AFF98F00000000
        49454E44AE426082}
    end
    object Image13: TImage
      Left = 102
      Top = 9
      Width = 41
      Height = 36
      Center = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
        00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
        080808087C0864880000009F4944415478DA6364A031601C7616B002B12E10CB
        00313791667C05E27B407C85180B8C80589D4CC7DE04E273842C0881FA821CF0
        15EAC0067C16449269380C2C03E246644B6861C1072016A4A50528E68E5A306A
        C110B48092A2025438CE266401A824D521D3025B20CE24640108800A2C528A6B
        6EA81E98E10F8158019F0520D000C4F564FA046F61876E490110F3136930C8E5
        0B180814D7540743DF0200FC271C19E38E4A080000000049454E44AE426082}
    end
    object Image14: TImage
      Left = 149
      Top = 9
      Width = 41
      Height = 36
      Center = True
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
    end
    object Image15: TImage
      Left = 196
      Top = 9
      Width = 41
      Height = 36
      Center = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
        00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
        080808087C0864880000010F4944415478DA6364A031601CEC167003B111100B
        42F9EF81F81C107FA5860520C33D8198154DFC37106F8759428905B6402C8343
        EE09101FA6D482102CAE870190EB37916B810210D703F10120FE454D0B0C8038
        1F8813A0FC6A20BE4F4A10E14A11A650173BA019F201880BB0F8026B24E34A11
        20FE2420E647133F08C40D407C9A81C8648A2F452802712B94BD10882700F105
        62C3156601BE1401F29D0DD4C50F883598540BAC81B891120BF005912E105742
        D90B8078223941842B92D91820612E80267E00EA23A223196609AE64DA00C4F6
        68963C02E25A0648B24406649745A08C064AF7F1503EC9198D58A000F5D17E06
        2A1715E88066851D0CD0BCB8A6798503B3846655265180E6160000857D4A19C6
        F28A3B0000000049454E44AE426082}
    end
    object Image16: TImage
      Left = 243
      Top = 9
      Width = 41
      Height = 36
      Center = True
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
        00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
        080808087C086488000000944944415478DA6364A031601CB560D482510BE016
        0800713F100740D90B80B810883F40E55981D8088865A1EC7B407C0E887F13D2
        0FB3600310FBA359BE108813A06C5B20964193BF0FC42708E98759F01F8BEF40
        AE1784B223B1C8835CBF86907E7C167C847A975C0BC0FAE91644B0887100627E
        A86401036A245B00B138940D32FC2C036A2463D53F7CF2C1A805A3160C620B00
        7C3A2A1929AFC9F80000000049454E44AE426082}
    end
  end
end
