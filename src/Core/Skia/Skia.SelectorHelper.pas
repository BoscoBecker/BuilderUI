unit Skia.SelectorHelper;

interface

uses
  System.Classes, System.Types, System.UITypes, Vcl.Controls,  Skia, Vcl.Skia;

type
  TSkiaSelectorHelper = class
  private
    FSelector: TSkPaintBox;
    FSelected: TControl;
    procedure DrawSelector(Sender: TObject; Canvas: ISkCanvas; const Dest: TRectF);
    procedure CreateSelector(Parent: TWinControl);
  public
    constructor Create;
    procedure Select(Control: TControl);
    procedure Clear;
    function IsVisible: Boolean;
    property Selected: TControl read FSelected;
  end;

implementation

{ TSkiaSelectorHelper }

constructor TSkiaSelectorHelper.Create;
begin
  FSelector := nil;
  FSelected := nil;
end;

procedure TSkiaSelectorHelper.CreateSelector(Parent: TWinControl);
begin
  if not Assigned(FSelector) then
  begin
    FSelector := TSkPaintBox.Create(Parent);
    FSelector.Parent := Parent;
    FSelector.Enabled := False; // Impede interação
    FSelector.SendToBack;       // Se necessário
    FSelector.OnDraw := DrawSelector;
  end;
end;

//procedure TSkiaSelectorHelper.DrawSelector(Sender: TObject; Canvas: ISkCanvas; const Dest: TRectF);
//begin
//  Canvas.Save;
//  Canvas.DrawCircle(Dest.CenterPoint, 4, TSkPaint.Create(clRed));
//  Canvas.Restore;
//end;

procedure TSkiaSelectorHelper.DrawSelector(Sender: TObject; Canvas: ISkCanvas; const Dest: TRectF);
var
  Paint: ISkPaint;
  R: TRectF;
  HandleRadius: Single;
  Points: array[0..3] of TPointF;
  i: Integer;
begin
  if not Assigned(FSelected) then Exit;

  R := TRectF.Create(PointF(0, 0), FSelector.Width, FSelector.Height);

  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.Color := TAlphaColors.Dodgerblue;
  Paint.StrokeWidth := 2;
  Canvas.DrawRect(R, 0, 0, Paint);

  // Bolinhas nos cantos
  Paint.Style := TSkPaintStyle.Fill;
  HandleRadius := 4;

  Points[0] := R.TopLeft;
  Points[1] := PointF(R.Right, R.Top);
  Points[2] := R.BottomRight;
  Points[3] := PointF(R.Left, R.Bottom);

  for i := 0 to 3 do
    Canvas.DrawCircle(Points[i], HandleRadius, Paint);
end;

procedure TSkiaSelectorHelper.Select(Control: TControl);
begin
  if not Assigned(Control) or not Assigned(Control.Parent) then Exit;

  CreateSelector(Control.Parent);
  FSelected := Control;

  FSelector.SetBounds(
    Control.Left - 3,
    Control.Top - 3,
    Control.Width + 6,
    Control.Height + 6
  );

  FSelector.BringToFront;
  FSelector.Visible := True;
  FSelector.Repaint;
end;

procedure TSkiaSelectorHelper.Clear;
begin
  if Assigned(FSelector) then
    FSelector.Visible := False;
  FSelected := nil;
end;

function TSkiaSelectorHelper.IsVisible: Boolean;
begin
  Result := Assigned(FSelector) and FSelector.Visible;
end;

end.

