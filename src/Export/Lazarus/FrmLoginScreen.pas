unit UFrmLoginScreen;

interface

uses  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, DBGrids, DB;


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

{$R *.lfm} 

end.
