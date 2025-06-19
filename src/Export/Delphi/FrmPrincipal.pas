unit UFrmPrincipal;

interface

uses  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
      Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.Grids, 
      Data.DB, Vcl.DBGrids; 


type
  TFrmPrincipal = class(TForm)
    PnlTopo: TPanel;
    LblAppTitle: TLabel;
    PnlMenu: TPanel;
    BtnClientes: TButton;
    BtnProdutos: TButton;
    BtnSair: TButton;
    PnlConteudo: TPanel;
    LblBemVindo: TLabel;
  end;


var 
   FrmPrincipal: TFrmPrincipal;

implementation

{$R *.dfm} 

end.
