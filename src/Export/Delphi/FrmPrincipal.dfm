object UFrmPrincipal: TFrmPrincipal
  Left = 0
  Top =  0
  Width = 800
  Height = 600
  Caption = 'Main Application'
  ClientWidth = 800
  ClientHeight = 600
  object PnlTopo: TPanel
    Caption = ''
    Left = 0
    Top = 0
    Width = 800
    Height = 60
    Color = $00D27619
    object LblAppTitle: TLabel
      Caption = 'BuilderUI - Main Screen'
      Left = 20
      Top = 15
      Width = 400
      Height = 30
      Font.Size = 20
      Font.Color = $00FFFFFF
    end
  end
  object PnlMenu: TPanel
    Caption = ''
    Left = 0
    Top = 60
    Width = 180
    Height = 540
    Color = $00E3E3E3
    object BtnClientes: TButton
      Caption = 'Customers'
      Left = 20
      Top = 30
      Width = 140
      Height = 40
    end
    object BtnProdutos: TButton
      Caption = 'Products'
      Left = 20
      Top = 80
      Width = 140
      Height = 40
    end
    object BtnSair: TButton
      Caption = 'Exit'
      Left = 20
      Top = 500
      Width = 140
      Height = 40
    end
  end
  object PnlConteudo: TPanel
    Caption = ''
    Left = 180
    Top = 60
    Width = 620
    Height = 540
    Color = $00FFFFFF
    object LblBemVindo: TLabel
      Caption = 'Welcome to BuilderUI!'
      Left = 40
      Top = 40
      Width = 400
      Height = 40
      Font.Size = 18
    end
  end
end
