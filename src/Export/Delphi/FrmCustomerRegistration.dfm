object UFrmCustomerRegistration: TFrmCustomerRegistration
  Left = 0
  Top =  0
  Width = 800
  Height = 600
  Caption = 'Customer Registration'
  ClientWidth = 800
  ClientHeight = 600
  object LblTituloCliente: TLabel
    Caption = 'Customer Registration'
    Left = 5
    Top = 10
    Width = 300
    Height = 30
    Font.Size = 16
  end
  object LblNomeCompleto: TLabel
    Caption = 'Full Name:'
    Left = 5
    Top = 50
    Width = 300
    Height = 20
  end
  object EdtNomeCompleto: TEdit
    Text = ''
    Left = 5
    Top = 70
    Width = 300
    Height = 30
  end
  object LblTelefone: TLabel
    Caption = 'Phone:'
    Left = 5
    Top = 110
    Width = 300
    Height = 20
  end
  object EdtTelefone: TEdit
    Text = ''
    Left = 5
    Top = 130
    Width = 300
    Height = 30
  end
  object BtnSalvarCliente: TButton
    Caption = 'Save'
    Left = 5
    Top = 180
    Width = 140
    Height = 40
  end
  object BtnCancelarCliente: TButton
    Caption = 'Cancel'
    Left = 165
    Top = 180
    Width = 140
    Height = 40
  end
end
