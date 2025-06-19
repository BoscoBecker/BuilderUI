unit UFrmLoginScreen;

interface

uses  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
      Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.Grids, 
      Data.DB, Vcl.DBGrids; 


type
  TFrmLoginScreen = class(TForm)
    PanelLeft: TPanel;
    LogoImage: TImage;
    LblWelcome: TLabel;
    LblDescription: TLabel;
    PanelRight: TPanel;
    LblEmail: TLabel;
    EditEmail: TEdit;
    LblPassword: TLabel;
    EditPassword: TEdit;
    ChkRememberMe: TCheckBox;
    LblForgotPassword: TLabel;
    BtnLogin: TButton;
    BtnSignUp: TButton;
    LblFollow: TLabel;
    PanelSocialIcons: TPanel;
    IconFacebook: TImage;
    IconTwitter: TImage;
    IconInstagram: TImage;
  end;


var 
   FrmLoginScreen: TFrmLoginScreen;

implementation

{$R *.dfm} 

end.
