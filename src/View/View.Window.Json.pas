unit View.Window.Json;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  SynEditMiscClasses, SynEditSearch, SynEdit, SynEditHighlighter,
  SynHighlighterJSON, System.Skia, Vcl.Skia, SynEditCodeFolding,
  SynHighlighterDWS;

type
  TFormJson = class(TForm)
    SynJSON: TSynJSONSyn;
    SynEditJson: TSynEdit;
    Panel1: TPanel;
    SkLabelTitle: TSkLabel;
    Splitter1: TSplitter;
    SynEditMiniMap: TSynEdit;
    procedure FormShow(Sender: TObject);
    procedure SynEditJsonChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SynEditJsonStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure SynEditMiniMapEnter(Sender: TObject);
    procedure SynEditMiniMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SynEditMiniMapSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
  private
    FJson: string;
  public
    procedure SetJson(const Value: string);
    property Json: string read FJson write SetJson;
  end;

var
  FormJson: TFormJson;

implementation

{$R *.dfm}

procedure TFormJson.FormCreate(Sender: TObject);
begin
  SynEditMiniMap.DoubleBuffered := True;
  SynEditJson.DoubleBuffered := True;
  SynEditMiniMap.SetLinesPointer(SynEditJson);
end;

procedure TFormJson.FormResize(Sender: TObject);
begin
  SynEditJsonStatusChange(Self, []);
end;

procedure TFormJson.FormShow(Sender: TObject);
begin
  SynEditJson.Clear;
  SynEditJson.Lines.Text:= FJson;
end;

procedure TFormJson.SetJson(const Value: string);
begin
  FJson := Value;
end;

procedure TFormJson.SynEditJsonChange(Sender: TObject);
begin
  SetJson(SynEditJson.lines.Text);
end;

procedure TFormJson.SynEditJsonStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if SynEditMiniMap.Tag = SynEditJson.TopLine then
    Exit;
  SynEditMiniMap.Tag := SynEditJson.TopLine;
  SynEditMiniMap.TopLine :=
    Max(1, SynEditJson.TopLine - (SynEditMiniMap.LinesInWindow -
    SynEditJson.LinesInWindow) div 2);
  SynEditMiniMap.Invalidate;
end;

procedure TFormJson.SynEditMiniMapEnter(Sender: TObject);
begin
  SynEditJson.SetFocus;
end;

procedure TFormJson.SynEditMiniMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Coord: TDisplayCoord;
begin
  Coord := SynEditMiniMap.PixelsToNearestRowColumn(X, Y);
  SynEditJson.CaretXY := SynEditJson.DisplayToBufferPos(Coord);
  SynEditJson.Invalidate;
  SynEditJson.TopLine := Max(1, Coord.Row - (SynEditJson.LinesInWindow div 2));
end;

procedure TFormJson.SynEditMiniMapSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  Special := (Cardinal(Line - SynEditJson.TopLine) <= Cardinal(SynEditJson.LinesInWindow));
  BG := clBtnFace;
end;

end.
