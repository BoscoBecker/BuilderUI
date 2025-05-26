unit View.Builder.Frames.Properties;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, System.Skia,
  Vcl.ComCtrls, Vcl.Skia, Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
  TFrameInscpector = class(TFrame)
    StringGrid1: TStringGrid;
    Panel13: TPanel;
    Image12: TImage;
    SkLabel3: TSkLabel;
    StatusBar1: TStatusBar;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
