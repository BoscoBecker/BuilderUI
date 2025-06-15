unit Service.Component.Manager.Highlighter;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms;

type
  TShapeHighlighter = class
  strict private
    FShape: TShape;
    procedure CreateShape(AOwner: TForm; AParent: TWinControl);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Highlight(Control: TControl; Owner: TForm);
    procedure Hide;

    property Shape: TShape read FShape;
  end;

implementation

uses Vcl.Graphics;

constructor TShapeHighlighter.Create;
begin
  inherited;
  FShape := nil;
end;

destructor TShapeHighlighter.Destroy;
begin
  FShape:=nil;
  inherited;
end;

procedure TShapeHighlighter.CreateShape(AOwner: TForm; AParent: TWinControl);

procedure SafeFreeShape;
  begin
    try
      if Assigned(FShape) then
      begin
        if not (csDestroying in TComponent(FShape).ComponentState) then
          FreeAndNil(FShape);
      end;
    except
      FShape := nil;
    end;
  end;

begin
  SafeFreeShape;
  FShape := TShape.Create(AOwner);
  FShape.Parent := AParent;
  FShape.Brush.Style := bsClear;
  FShape.Pen.Color := clGrayText;
  FShape.Pen.Width := 1;
  FShape.Pen.Mode := pmMask;
  FShape.Pen.Style := psDot;
  FShape.Visible := False;
end;


procedure TShapeHighlighter.Highlight(Control: TControl; Owner: TForm);
begin
  if not Assigned(Control) or not Assigned(Control.Parent) then Exit;
  if (csDestroying in Control.ComponentState) or
     (csDestroying in Control.Parent.ComponentState) then Exit;

  CreateShape(Owner, Control.Parent);

  FShape.SetBounds(
    Control.Left - 2,
    Control.Top - 2,
    Control.Width + 4,
    Control.Height + 4
  );

  FShape.Visible := True;
  FShape.SendToBack;
end;

procedure TShapeHighlighter.Hide;
begin
  if Assigned(FShape) then
    FShape.Visible := False;
end;

end.

