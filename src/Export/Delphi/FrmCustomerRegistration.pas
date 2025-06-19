unit UFrmCustomerRegistration;

interface

uses  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
      Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.Grids, 
      Data.DB, Vcl.DBGrids; 


type
  TFrmCustomerRegistration = class(TForm)
    LblTituloCliente: TLabel;
    LblNomeCompleto: TLabel;
    EdtNomeCompleto: TEdit;
    LblTelefone: TLabel;
    EdtTelefone: TEdit;
    BtnSalvarCliente: TButton;
    BtnCancelarCliente: TButton;
  end;


var 
   FrmCustomerRegistration: TFrmCustomerRegistration;

implementation

{$R *.dfm} 

end.
