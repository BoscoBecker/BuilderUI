unit View.Menu.Context.Windows;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, Vcl.Skia,

  Util.Form.Arranger;

type
  TFormContextWindows = class(TForm)
    SkLabelCascade: TSkLabel;
    SkLabelStacked: TSkLabel;
    SkLabelSideBySide: TSkLabel;
    SkLabelTitle: TSkLabel;
    ImageCascade: TImage;
    ImageStacked: TImage;
    ImageSidebySide: TImage;
    procedure ImageCascadeClick(Sender: TObject);
    procedure ImageStackedClick(Sender: TObject);
    procedure ImageSidebySideClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    private
      FForm :TObjectList<TForm>;
    public
      constructor Create(AOwner: TComponent;const Form :TObjectList<TForm>); reintroduce;
      destructor Destroy; override;
  end;

var
  FormContextWindows: TFormContextWindows;

implementation

{$R *.dfm}

constructor TFormContextWindows.Create(AOwner: TComponent;const Form: TObjectList<TForm>);
begin
  inherited Create(AOwner);
  FForm:= Form;
end;

destructor TFormContextWindows.Destroy;
begin
  inherited;
end;

procedure TFormContextWindows.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = VK_ESCAPE then Close;
end;

procedure TFormContextWindows.ImageSidebySideClick(Sender: TObject);
begin
  TFormArranger.ArrangeSideBySide(TFormArranger.FormsToArray(FForm));
end;

procedure TFormContextWindows.ImageStackedClick(Sender: TObject);
begin
  TFormArranger.ArrangeStacked(TFormArranger.FormsToArray(FForm));
end;

procedure TFormContextWindows.ImageCascadeClick(Sender: TObject);
begin
  TFormArranger.ArrangeCascade(TFormArranger.FormsToArray(FForm))
end;

end.
