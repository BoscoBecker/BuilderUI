object FormBuilderMain: TFormBuilderMain
  Left = 0
  Top = 0
  AlphaBlend = True
  Caption = 'Forms Builder UI - Create and Export Layout Forms Easily'
  ClientHeight = 794
  ClientWidth = 1117
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
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseMove = FormMouseMove
  OnShow = FormShow
  TextHeight = 15
  object SplitterLeft: TSplitter
    Left = 289
    Top = 57
    Height = 689
    ExplicitLeft = 772
    ExplicitTop = 65
  end
  object SplitterRight: TSplitter
    Left = 813
    Top = 57
    Height = 689
    Align = alRight
    Visible = False
    ExplicitLeft = 690
    ExplicitTop = 65
  end
  object SkPaintBackground: TSkPaintBox
    AlignWithMargins = True
    Left = 295
    Top = 60
    Width = 515
    Height = 683
    Align = alClient
    OnMouseMove = SkPaintBackgroundMouseMove
    OnDraw = SkPaintBackgroundDraw
    ExplicitLeft = 294
    ExplicitTop = 57
    ExplicitWidth = 507
  end
  object StatusBarBottom: TStatusBar
    Left = 0
    Top = 775
    Width = 1117
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 300
      end
      item
        Width = 200
      end
      item
        Width = 200
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 150
      end>
  end
  object PanelRenderJson: TPanel
    Left = 816
    Top = 57
    Width = 301
    Height = 689
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object SplitterBottomTool: TSplitter
      Left = 0
      Top = 610
      Width = 301
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = -8
      ExplicitTop = 676
      ExplicitWidth = 242
    end
    object PanelTopRender: TPanel
      Left = 0
      Top = 0
      Width = 301
      Height = 35
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object SkPaintBoxRenderJson: TSkPaintBox
        Left = 0
        Top = 0
        Width = 301
        Height = 35
        Align = alClient
        OnDraw = SkPaintBoxRenderJsonDraw
        ExplicitWidth = 242
        ExplicitHeight = 34
      end
      object ImageCloseRender: TImage
        Left = 0
        Top = 0
        Width = 40
        Height = 34
        Cursor = crHandPoint
        Hint = 'Close'
        Align = alCustom
        Anchors = [akLeft]
        Center = True
        ParentShowHint = False
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
        ShowHint = True
        OnClick = ImageCloseRenderClick
      end
      object SkLabel3: TSkLabel
        Left = 211
        Top = 8
        Width = 75
        Height = 19
        Align = alCustom
        Anchors = [akTop, akRight]
        Words = <
          item
            Caption = 'Render Json'
          end>
        ExplicitLeft = 152
      end
    end
    object PanelToolJsonRender: TPanel
      Left = 0
      Top = 580
      Width = 301
      Height = 30
      Align = alBottom
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 1
      object ImageCopyToClipboard: TImage
        Left = 267
        Top = 0
        Width = 34
        Height = 30
        Cursor = crHandPoint
        Hint = 'Copy'
        Align = alRight
        Center = True
        ParentShowHint = False
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000140000
          001408060000008D891D0D000000017352474200AECE1CE90000000473424954
          080808087C086488000001424944415478DACD933D4B034110862785954D2C62
          25E45209210109D6F9208D9D85602B76969A3FA0016B4D9D46497E846982DA07
          898DA55EC0CA2A16A96CF20EF72E2E4BEE7653080E3C70BBB7F7DCECCC6E4ED2
          2302453ECF402C01914B99BF0727CEDC05E8AD2BDC003BE07DC5DA0938061FA1
          4295B5C11638037567ED103C802F300E11EE829A352E1053C385F56E4C71A6B0
          0A2ABE1AFD4BE1BEFC1E9F09B7AFC2326870FE193C850855D6B1C62330A0B00F
          0EAD772510FB844764C8CCDE9C2D37C11EB8052DCD3254786DC9DC1AAAF43154
          58649D06194DC98373496ED1FC4FBBBC2DC94DF1C58F243766E1136A9498E926
          C77A340EC00DC72A79019FD63711B803A7E27479555C814B53F094354D49694A
          96508B3E05AF606E65A64DD363D30B159ABF9BE8F227C28CCD4DF9A6D8BB6593
          49C4E7A993A1998F898408D78A25CB534D151CA37F970000000049454E44AE42
          6082}
        ShowHint = True
        OnClick = ImageCopyToClipboardClick
        ExplicitLeft = 201
        ExplicitTop = 5
        ExplicitHeight = 34
      end
      object ImageLoadJson: TImage
        Left = 0
        Top = 0
        Width = 34
        Height = 30
        Cursor = crHandPoint
        Hint = 'Load json'
        Align = alLeft
        Center = True
        ParentShowHint = False
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000140000
          001408060000008D891D0D000000017352474200AECE1CE90000000473424954
          080808087C086488000000FA4944415478DA6364A03260C422C60AC48238D4F3
          03F16D520C0419E609C4DC38D46B01B12110FB106BA02E10EBE07100C8C01A20
          DE8ACB5074039D81588C08031970194AAA81F240DC8EC4C73094540341C00E88
          4D1810E1BC12881B2931101DBC02E2BD43DF4050F87C858A8902F16B3431181B
          44DF27C6C06020BE0EC4B140FC0D88673040621764480B940D120365825C520C
          4C07E2C50C90E4820C4031FD10EA42A20C8C03E23350AF8212F26120FE0F3500
          246682A487A08175500D95405C04150779B30DEAFD16A8CB0F415D5A41D75826
          543860035780F8322E0309155FE80014EBDB81F8372E0361860A1269E07B64C3
          701948110000854B3A153A1A4DE00000000049454E44AE426082}
        ShowHint = True
        OnClick = ImageLoadJsonClick
        ExplicitLeft = 6
        ExplicitTop = -6
      end
      object ImageSaveJson: TImage
        Left = 34
        Top = 0
        Width = 34
        Height = 30
        Cursor = crHandPoint
        Hint = 'Save json'
        Align = alLeft
        Center = True
        ParentShowHint = False
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000140000
          001408060000008D891D0D000000017352474200AECE1CE90000000473424954
          080808087C0864880000012E4944415478DA6364A03260C422C60AC48238D4F3
          03F16D520C0419E609C4DC38D46B01B12110FB106BA02E10EBE07100C8C01A20
          DE8ACB5074039D81588C08031970194AAA81F240DC8EC4C73094540341C00E88
          4D1810E1BC12881B2931101DBC02E2BD840C04B9E021D48BC6403C1388E3A09A
          77007130101F82AA5B488C81200DD7A111D002C42250CD207006883380782D03
          249272493150136AD01934752083B8A0AE24E8C21020F600E2255097810C5F01
          C42E40CC091507892D82060B13105F05E20BB80C748086DD760648AE790D7521
          2876BF41D930578362BA1F8879182039E802A5B18C9CD01D81F8C0A03210E45D
          5B0648720281025004915A3820832206445282818BA4165FC8401EEA5D64B581
          A416B0E8C01788E740D91341DE662452233EB000880DA0980100C9A339452A50
          EC060000000049454E44AE426082}
        ShowHint = True
        OnClick = ImageSaveJsonClick
        ExplicitLeft = 201
        ExplicitTop = 5
        ExplicitHeight = 34
      end
      object ImagePrettyJson: TImage
        Left = 68
        Top = 0
        Width = 34
        Height = 30
        Cursor = crHandPoint
        Hint = 'Json Pretty'
        Align = alLeft
        Center = True
        ParentShowHint = False
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
          00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
          080808087C0864880000013A4944415478DACD95B16A024114459F8585D868A3
          8D90B481D86B115052D9E74B825D922EF91B7B2B1149915436DAD81930456C4C
          639526F7E21B18C79D75765CC10B87C515EF9137BB330539730A972068800AA8
          83AADEDB801FBD7EC70A58DAB24A7DA1E413FC661134C16DC669CCC00A8C55D6
          F50962CA993278043760023A49028EA51759FE04AEC11CDC9991B9829E4A62CB
          BF401F0CCC97B6A0A1E653CA5FC116BCCB6E3DF6045967EF2B171DD3CC15DC83
          5A0EE5CC1A8C5CC103283A452FA004DEAC8263E5CC9F598710011FBBA54A24A0
          DC2B481A11FFED33B8528904947B47E45B645B2201E54CE222A73DA6462201E5
          4CE263CAC4BC686EF8060FCD87BCB60A3B43B176D63C373B660A16F68D3CB7EB
          83F23401C371B5E5F89A701C1F92F1C0B1C3A78BA75A4DF68FCCB55E57693FBE
          8843FFA4FC039CA14E190DE1D9B50000000049454E44AE426082}
        Proportional = True
        ShowHint = True
        OnClick = ImagePrettyJsonClick
        ExplicitLeft = 43
        ExplicitTop = -4
        ExplicitHeight = 28
      end
      object ImageHighSize: TImage
        Left = 233
        Top = 0
        Width = 34
        Height = 30
        Cursor = crHandPoint
        Hint = 'High size'
        Align = alRight
        Center = True
        ParentShowHint = False
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000140000
          001408060000008D891D0D000000017352474200AECE1CE90000000473424954
          080808087C086488000000EB4944415478DAB5940111C2300C45330773C07030
          1CCC0148280E4001430138A0533250000E280E8602488E00254B773DAECBDDBF
          5B6FE94BD2A6C920B16563030BD41C95F3DAA11AE16350136FDDB05F0F48B0AB
          127486BAF077856AC5FF8E7D9C04AE503BE17C4395BCE91DF42832245BA3F612
          58A3364A5621A34067FEDEF2FE2030F6B21E43403AEC03975944021D97BF4459
          2D93929DBA4860CEC13FC7337A1F2607565CB28BDC4FE5D2199E34A081D7A510
          6C1A097430702935246E1B1F98BCB1FD9208EE3FBD16FA7DAA3E3D72FA6738DC
          E1DBBFEAF85AC0EFF8B2C2C7880C2D04C657124B0E7C026D4E3815DC6B6C2700
          00000049454E44AE426082}
        ShowHint = True
        OnClick = ImageHighSizeClick
        ExplicitLeft = 201
        ExplicitTop = 5
        ExplicitHeight = 34
      end
      object ImageClearJson: TImage
        Left = 102
        Top = 0
        Width = 34
        Height = 30
        Cursor = crHandPoint
        Hint = 'Clear '
        Align = alLeft
        Center = True
        ParentShowHint = False
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000140000
          001408060000008D891D0D000000017352474200AECE1CE90000000473424954
          080808087C0864880000011C4944415478DA6364200E0840E90F841432126158
          0010AF87B203817803A506820CF087B237422DA0C8C003406C0F651F04628751
          038932500188E3A1EC0820D680B26F00F10A287B21103F20C640900BF613E172
          107084FA00AF81C85E24043082009B81D381588F48032F0171263E0359813884
          48C360600D10FFC665A00C10DB9268E061207E82CB400B205644E26B01B10810
          1FC263E07D203E81CBC010A8B7612003884581B8198F81BFA1DEC63010544479
          A2296E87BA3015498C1B88DB80B805885F43C5B633408B36640375815807CDC0
          65503A0A2D186A807831D42010B802C497D10D1403626724BE3CD485E806DA41
          83E23A5250EC05E257D8C2901B8A41801FC9C520177C84B2E5805816CA3E0AC4
          5FA1186BA4500C0036AD3515D9A1006D0000000049454E44AE426082}
        ShowHint = True
        OnClick = ImageClearJsonClick
        ExplicitLeft = 90
        ExplicitTop = 6
      end
    end
    object Memo: TMemo
      Left = 0
      Top = 35
      Width = 301
      Height = 545
      Align = alClient
      BorderStyle = bsNone
      ScrollBars = ssBoth
      TabOrder = 2
      OnChange = MemoChange
    end
    object PanelInfoValidation: TPanel
      Left = 0
      Top = 613
      Width = 301
      Height = 76
      Align = alBottom
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 3
      object LabelInfoJson: TLabel
        Left = 0
        Top = 33
        Width = 301
        Height = 43
        Align = alClient
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowFrame
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        WordWrap = True
        StyleElements = [seFont, seClient]
        ExplicitWidth = 3
        ExplicitHeight = 15
      end
      object PanelInfoRenderToForms: TPanel
        Left = 0
        Top = 0
        Width = 301
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Render forms   '
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
        object ImageErro: TImage
          Left = 242
          Top = 0
          Width = 59
          Height = 33
          Cursor = crHandPoint
          Align = alRight
          Center = True
          ParentShowHint = False
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
            00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
            080808087C0864880000014E4944415478DACD95AD52C34014856F050A03A618
            0A3C40F14580C150CF43F00E40053FEFC043D4178301010A530C0E6811D41483
            AAE19C99BB33DB9BDD6D362D4CCFCC37996C9273927B37BB35F963D596216013
            AC810DB0AE6363F0A5C7CFAA01346D79A63131E4097CE704EC82666635FAE0A5
            4C401573A767F09A0A6059DA15CD9D7AE295CB06B435C469151C815BF063EE8D
            5D1BEB582180B365DF981C8013F006AE3D239A9F821D7003EECD730F60680342
            B5A7D119D8F642C4337F075781AF63B3FB36E010D40335F5433E746C2B614E8D
            C09D0D38062B1216433AA0A1E703701131A726A09B1B70AE6F2EFA2597B901A9
            12B99A0F74AC21C5C6CF2C51ACC9B6A122C5C6976A726A9ADA86FA8D2F3D4DA9
            45FC68FC8B7BEEE4DF978A582FCA6AE662374F48C13C1540B15C7B32DD939058
            8E47C9DC707C71767157ABCBF49639D2E330F5F0526CFA73E9178C2558191137
            54600000000049454E44AE426082}
          ShowHint = True
          Visible = False
          ExplicitLeft = 216
        end
        object ImageOk: TImage
          Left = 183
          Top = 0
          Width = 59
          Height = 33
          Cursor = crHandPoint
          Hint = 'Render Json'
          Align = alRight
          Center = True
          ParentShowHint = False
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
            00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
            080808087C086488000000964944415478DA6364A031601CB580120B0C80783F
            94DD00C413A96DC103209647E21F00E244A838552CF88F45EC03A9BE21D50292
            7D43AE0544FB86120B88F20D352C80F926106A194D2C6080FA409196163C0462
            055A59F0118803681544078138818106910C727503104FC0A7885C0BF0BA9A12
            0B887235B116805C875CD811ED6A622D0015D707A06C925C4DAC055401A31610
            0400690C2C1988D807510000000049454E44AE426082}
          ShowHint = True
          Visible = False
          OnClick = ImageOkClick
          ExplicitLeft = 168
        end
      end
    end
  end
  object SplitViewMain: TSplitView
    Left = 0
    Top = 57
    Width = 50
    Height = 689
    BiDiMode = bdLeftToRight
    CloseStyle = svcCompact
    Color = clWhite
    DockSite = True
    FullRepaint = False
    Locked = True
    Opened = False
    OpenedWidth = 100
    ParentBiDiMode = False
    ParentShowHint = False
    Placement = svpLeft
    ShowHint = True
    TabOrder = 2
    object SkLabelTreeJson: TSkLabel
      AlignWithMargins = True
      Left = 3
      Top = 164
      Width = 44
      Height = 13
      Align = alTop
      AutoSize = False
      TextSettings.Font.Size = 10.000000000000000000
      TextSettings.HorzAlign = Center
      Words = <
        item
          Caption = 'Tree json'
        end>
      ExplicitLeft = 5
      ExplicitTop = 161
    end
    object SkLabelLoadTemplate: TSkLabel
      AlignWithMargins = True
      Left = 3
      Top = 104
      Width = 44
      Height = 13
      Align = alTop
      AutoSize = False
      TextSettings.Font.Size = 10.000000000000000000
      TextSettings.HorzAlign = Center
      Words = <
        item
          Caption = 'Load'
        end>
      ExplicitLeft = 0
      ExplicitTop = 77
    end
    object SkLabelRender: TSkLabel
      AlignWithMargins = True
      Left = 3
      Top = 44
      Width = 44
      Height = 13
      Align = alTop
      AutoSize = False
      TextSettings.Font.Size = 10.000000000000000000
      TextSettings.HorzAlign = Center
      Words = <
        item
          Caption = 'Render'
        end>
      ExplicitLeft = 5
      ExplicitTop = 41
    end
    object Panel6: TPanel
      Left = 0
      Top = 648
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
    object PanelTreeComponents: TPanel
      Left = 0
      Top = 180
      Width = 50
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      ParentBackground = False
      TabOrder = 1
      object ImageTreeComponents: TImage
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
          080808087C086488000000EF4944415478DA6364A03160A481990E403C1FCA4E
          A4960546402C03C4DC40DC01C47250F107D4B00064B83A127F3A10F343D90F49
          B100C5EB407C00CA0E01625620DE0EC41FA0EA1640E512085980CBEB0F81B812
          4DED726C06E0B3009FD71F017105A516E0F53A8120222A1545E2731912D00562
          1D243ED1A988580B6096283240E20A2315A17809C9EBA458800C50821264C103
          209687D9C84064EA2016802CF8C04046EA20C502142F512988A89A8A4080ACB2
          88580BC82E8BF06620062A9445F8321055CA229825D8321055CA227480E27506
          2A9445C402B2CB22522DC15916511B80826801944DB046A3180000F6E44915F1
          E291320000000049454E44AE426082}
        ShowHint = True
        OnClick = ImageTreeComponentsClick
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
      Color = clWhite
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      ParentBackground = False
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
    object PanelLoad: TPanel
      Left = 0
      Top = 60
      Width = 50
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      ParentBackground = False
      TabOrder = 3
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
    object PanelOpenTemplate: TPanel
      Left = 0
      Top = 120
      Width = 50
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      Padding.Left = 2
      Padding.Top = 2
      Padding.Right = 2
      Padding.Bottom = 2
      ParentBackground = False
      TabOrder = 4
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
      Top = 634
      Width = 50
      Height = 14
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 5
      Visible = False
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
  object PanelBottomInfo: TPanel
    Left = 0
    Top = 746
    Width = 1117
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 3
  end
  object PanelToolPalette: TPanel
    Left = 364
    Top = 675
    Width = 383
    Height = 48
    Cursor = crHandPoint
    BevelOuter = bvNone
    Color = clWhite
    DoubleBuffered = True
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 4
    OnMouseDown = PanelToolPaletteMouseDown
    OnMouseMove = PanelToolPaletteMouseMove
    OnMouseUp = PanelToolPaletteMouseUp
    object SkPaintBox1: TSkPaintBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 377
      Height = 42
      Align = alClient
      OnMouseDown = SkPaintBox1MouseDown
      OnMouseMove = SkPaintBox1MouseMove
      OnMouseUp = SkPaintBox1MouseUp
      OnDraw = SkPaintBox1Draw
      ExplicitLeft = 2
    end
    object ImageSelectMode: TImage
      Left = 8
      Top = 6
      Width = 41
      Height = 36
      Hint = 'Select mode'
      Center = True
      ParentShowHint = False
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
      ShowHint = True
      OnClick = ImageSelectModeClick
    end
    object ImageArrangeWindows: TImage
      Left = 55
      Top = 6
      Width = 41
      Height = 36
      Hint = 'Arrange windows'
      Center = True
      ParentShowHint = False
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
        00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
        080808087C086488000000C24944415478DA6364A031601C56160800713D1007
        00B10291FA1F00F102206E24C68209409C4FA64327027101210B3E00313F9916
        807CA248C882FF641A8ECD2CA22CF808C47380F806107F25D21290BA7B407C85
        180B7A81F82C99BEB909C4E708599000C4BFC8B400E4934D842C8822D3701858
        3E6A01C916A06734AA5BD0C000298B6866010880CA235061274F2B0B904124AD
        2D08016256320DC79AD1D0812E10EB906901A82CBA4CC802103002621920E626
        C1E5F76186136301C560E85B0000C8632D19AD37BDD40000000049454E44AE42
        6082}
      ShowHint = True
      OnClick = ImageArrangeWindowsClick
    end
    object ImageBackground: TImage
      Left = 102
      Top = 6
      Width = 41
      Height = 36
      Hint = 'Background'
      Center = True
      ParentShowHint = False
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
        00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
        080808087C0864880000009F4944415478DA6364A031601C7616B002B12E10CB
        00313791667C05E27B407C85180B8C80589D4CC7DE04E273842C0881FA821CF0
        15EAC0067C16449269380C2C03E246644B6861C1072016A4A50528E68E5A306A
        C110B48092A2025438CE266401A824D521D3025B20CE24640108800A2C528A6B
        6EA81E98E10F8158019F0520D000C4F564FA046F61876E490110F3136930C8E5
        0B180814D7540743DF0200FC271C19E38E4A080000000049454E44AE426082}
      ShowHint = True
      OnClick = ImageBackgroundClick
    end
    object ImageTreeView: TImage
      Left = 149
      Top = 6
      Width = 41
      Height = 36
      Hint = 'Component explorer'
      Center = True
      ParentShowHint = False
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
        00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
        080808087C086488000000EF4944415478DA6364A03160A481990E403C1FCA4E
        A4960546402C03C4DC40DC01C47250F107D4B00064B83A127F3A10F343D90F49
        B100C5EB407C00CA0E01625620DE0EC41FA0EA1640E512085980CBEB0F81B812
        4DED726C06E0B3009FD71F017105A516E0F53A8120222A1545E2731912D00562
        1D243ED1A988580B6096283240E20A2315A17809C9EBA458800C50821264C103
        209687D9C84064EA2016802CF8C04046EA20C502142F512988A89A8A4080ACB2
        88580BC82E8BF06620062A9445F8321055CA229825D8321055CA227480E27506
        2A9445C402B2CB22522DC15916511B80826801944DB046A3180000F6E44915F1
        E291320000000049454E44AE426082}
      ShowHint = True
      OnClick = ImageTreeViewClick
    end
    object ImageShare: TImage
      Left = 288
      Top = 6
      Width = 41
      Height = 36
      Hint = 'Share'
      Center = True
      ParentShowHint = False
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
      ShowHint = True
      OnClick = ImageShareClick
    end
    object ImageOptions: TImage
      Left = 334
      Top = 6
      Width = 41
      Height = 36
      Hint = 'Options'
      Center = True
      ParentShowHint = False
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
        00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
        080808087C086488000000944944415478DA6364A031601CB560D482510BE016
        0800713F100740D90B80B810883F40E55981D8088865A1EC7B407C0E887F13D2
        0FB3600310FBA359BE108813A06C5B20964193BF0FC42708E98759F01F8BEF40
        AE1784B223B1C8835CBF86907E7C167C847A975C0BC0FAE91644B0887100627E
        A86401036A245B00B138940D32FC2C036A2463D53F7CF2C1A805A3160C620B00
        7C3A2A1929AFC9F80000000049454E44AE426082}
      ShowHint = True
    end
    object ImageZoomIn: TImage
      Left = 196
      Top = 6
      Width = 41
      Height = 36
      Hint = 'Zoom In'
      Center = True
      ParentShowHint = False
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
        00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
        080808087C0864880000011F4944415478DA6364A031601C680BC4A0581C8805
        A162EF81F82510BF8262B22C6005620B209621A0FF09109F00E2DFA4582000C4
        7640CC4D64287C05E24340FC81180B402EF724C170644BB663F309BA05B67882
        A51D88FF0371150EF97B407C129F05A0C874C6E3CA65503A0A8F9ABD0C68118F
        6C812E10EB5068C11520BE8CCB0267A82F28B1E015D417582D08618044320C80
        C25C9E013F78C0801A27A0485E434D0B1E027125B116D03C88681EC9344FA620
        4028A38140250EF9D740BC075D905A4505CC010D40BC009F0520406A6107F395
        2E949D886C09A5C5352858E602B11C9A7820106FC067010CC02A1B108D5CE180
        221256E92400F17C347D0F805891180B8805E8968032A002352D008100209E80
        64E1016A5B8015D0DC020097853F198E18F5E80000000049454E44AE426082}
      ShowHint = True
      OnClick = ImageZoomInClick
    end
    object ImageZoomOut: TImage
      Left = 243
      Top = 6
      Width = 41
      Height = 36
      Hint = 'Zoom Out'
      Center = True
      ParentShowHint = False
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D49484452000000180000
        00180806000000E0773DF8000000017352474200AECE1CE90000000473424954
        080808087C086488000001134944415478DA6364A031601C680BC4A0581C8805
        A162EF81F82510BF8262B22C6005620B209621A0FF09109F00E2DFA4582000C4
        7640CC4D64287C05E24340FC81180B402EF724C170644BB663F309BA05B64404
        0B2E700F884FE2B3001499CE641A0E037B19D0221ED9025D20D6A1D0822B407C
        199705CE505F50025E417D81D58210064824C3403B10CB1330F001105721F141
        91BC869A163C04E24A622DA07910D13C92699E4C4180928CF61A88F7A00B52AB
        A8000150A26800E205F82C0001520B3B1000A5245D283B11D9124A8B6B50B0CC
        05623934F14020DE80CF0218805536201AB9C2014524ACD24900E2F968FA1E00
        B1223116100BD02D016540056A5A000201403C01C9C203D4B6002BA0B905007F
        473619DF1CB7140000000049454E44AE426082}
      ShowHint = True
      OnClick = ImageZoomOutClick
    end
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 1117
    Height = 57
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
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
    object SkLabel1: TSkLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 1111
      Height = 36
      Align = alClient
      BiDiMode = bdRightToLeft
      ParentBiDiMode = False
      TextSettings.Decorations.Style = Dashed
      TextSettings.Font.Size = 30.000000000000000000
      TextSettings.HorzAlign = Center
      TextSettings.LetterSpacing = 2.000000000000000000
      TextSettings.Trimming = Character
      TextSettings.VertAlign = Trailing
      Words = <
        item
          Caption = 'Forms Builder UI'
        end>
      ExplicitWidth = 1008
      ExplicitHeight = 35
    end
  end
  object PanelTree: TPanel
    Left = 50
    Top = 57
    Width = 239
    Height = 689
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'PanelTree'
    TabOrder = 6
    Visible = False
    object PanelExpandTree: TPanel
      Left = 0
      Top = 60
      Width = 239
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      DoubleBuffered = True
      ParentBackground = False
      ParentDoubleBuffered = False
      TabOrder = 0
      object ImageExpand: TImage
        Left = 64
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
        ExplicitLeft = 32
      end
      object ImageColapse: TImage
        Left = 32
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
        ExplicitLeft = 0
      end
      object ActivityIndicatorExplorer: TActivityIndicator
        Left = 0
        Top = 0
        Align = alLeft
      end
    end
    object PanelTopExplorer: TPanel
      Left = 0
      Top = 0
      Width = 239
      Height = 35
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      DoubleBuffered = True
      ParentBackground = False
      ParentDoubleBuffered = False
      TabOrder = 1
      object SkPaintBoxExplorer: TSkPaintBox
        Left = 0
        Top = 0
        Width = 239
        Height = 35
        Align = alClient
        OnDraw = SkPaintBoxExplorerDraw
        ExplicitLeft = 72
        ExplicitTop = 16
        ExplicitWidth = 50
        ExplicitHeight = 50
      end
      object ImageCloseExplorer: TImage
        Left = 198
        Top = 0
        Width = 40
        Height = 34
        Cursor = crHandPoint
        Hint = 'Close'
        Align = alCustom
        Anchors = [akRight]
        Center = True
        ParentShowHint = False
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
        ShowHint = True
        OnClick = ImageCloseExplorerClick
        ExplicitLeft = 182
        ExplicitTop = 1
      end
      object ImageFilterExplorer: TImage
        Left = 157
        Top = 0
        Width = 40
        Height = 34
        Cursor = crHandPoint
        Hint = 'Filter'
        Align = alCustom
        Anchors = [akRight]
        Center = True
        ParentShowHint = False
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
        ShowHint = True
        OnClick = ImageFilterExplorerClick
        ExplicitLeft = 141
      end
      object SkLabelExplorer: TSkLabel
        Left = 6
        Top = 8
        Width = 51
        Height = 19
        Words = <
          item
            Caption = 'Explorer'
          end>
      end
    end
    object PanelSearchComponents: TPanel
      Left = 0
      Top = 35
      Width = 239
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      Caption = 'PanelSearchComponents'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 2
      Visible = False
      object SearchBoxComponents: TSearchBox
        Left = 0
        Top = 0
        Width = 239
        Height = 25
        Align = alClient
        TabOrder = 0
        TextHint = 'Type: label1'
        OnChange = SearchBoxComponentsChange
        ExplicitHeight = 23
      end
    end
    object TreeViewExplorer: TTreeView
      Left = 0
      Top = 92
      Width = 239
      Height = 597
      Align = alClient
      AutoExpand = True
      BevelOuter = bvNone
      BorderStyle = bsNone
      Ctl3D = True
      Indent = 19
      MultiSelectStyle = [msControlSelect, msShiftSelect]
      ParentCtl3D = False
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 3
      ToolTips = False
      OnClick = TreeViewExplorerClick
    end
  end
end
