unit UFrmSobre;

interface

uses  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
      Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.Grids, 
      Data.DB, Vcl.DBGrids; 


type
  TFrmSobre = class(TForm)
    LblTituloSobre: TLabel;
    ImgLogo: TImage;
    LblDescricao: TLabel;
  end;


var 
   FrmSobre: TFrmSobre;

implementation

{$R *.dfm} 

end.
