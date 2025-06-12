unit Service.Skia.Draw;

interface

uses
  System.Types, System.UITypes, System.Skia, Vcl.Skia;

type  TBuilderBackground = ( bClear, bGrid);

type
  TSkiaDrawService = class
    public class procedure DrawBackground(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; Background: TBuilderBackground; var FPaint: ISkPaint);static;
    public class procedure DrawGradientBox(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; var FPaint: ISkPaint);static;
    public class procedure DrawExplorerBorder(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; var FPaint: ISkPaint);static;
    public class procedure DrawBoxRenderJsonBorder(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; var FPaint: ISkPaint); static;
  end;

implementation

uses
  System.Math;

class procedure TSkiaDrawService.DrawBackground(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; Background: TBuilderBackground; var FPaint: ISkPaint);
const
  GridSize = 20;
begin
  ACanvas.Save;
  case Background of
    bClear: ACanvas.Clear(TAlphaColors.White);
    bGrid:
    begin
      ACanvas.Clear($FFF5F5F5);
      FPaint := TSkPaint.Create;
      FPaint.Style := TSkPaintStyle.Stroke;
      FPaint.Color := $FFDDDDDD;
      FPaint.StrokeWidth := 1;
      var X := ADest.Left;
      while X <= ADest.Right do
      begin
        ACanvas.DrawLine(X, ADest.Top, X, ADest.Bottom, FPaint);
        X := X + GridSize;
      end;
      var Y := ADest.Top;
      while Y <= ADest.Bottom do
      begin
        ACanvas.DrawLine(ADest.Left, Y, ADest.Right, Y, FPaint);
        Y := Y + GridSize;
      end;
    end;
  end;
  ACanvas.Restore;
end;

class procedure TSkiaDrawService.DrawGradientBox(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; var FPaint: ISkPaint);
begin
  FPaint := TSkPaint.Create;
  FPaint.Shader := TSkShader.MakeGradientSweep(ADest.CenterPoint,
    [$FFF2F2F2, $FFCCCCCC, $FF999999, $FFCCCCCC, $FFF2F2F2]);
  ACanvas.DrawPaint(FPaint);
end;

class procedure TSkiaDrawService.DrawExplorerBorder(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; var FPaint: ISkPaint);
var
  Shader: ISkShader;
  BorderRect: TRectF;
  Center: TPointF;
begin
  FPaint := TSkPaint.Create;
  FPaint.Style := TSkPaintStyle.Stroke;
  FPaint.StrokeWidth := 1;

  Center := PointF(ADest.Left + ADest.Width / 2, ADest.Top + ADest.Height / 2);
  Shader := TSkShader.MakeGradientSweep(Center,[$FFB0B0B0, $FFD0D0D0, $FFFFFFFF, $FFD0D0D0, $FFB0B0B0]);
  FPaint.Shader := Shader;
  BorderRect := ADest;

  InflateRect(BorderRect, -FPaint.StrokeWidth / 2, -FPaint.StrokeWidth / 2);
  ACanvas.DrawRect(BorderRect, FPaint);
end;

class procedure TSkiaDrawService.DrawBoxRenderJsonBorder(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; var FPaint: ISkPaint);
var
  Shader: ISkShader;
  BorderRect: TRectF;
  Center: TPointF;
begin
  FPaint := TSkPaint.Create;
  FPaint.Style := TSkPaintStyle.Stroke;
  FPaint.StrokeWidth := 1;

  Center := PointF(ADest.Left + ADest.Width / 2, ADest.Top + ADest.Height / 2);
  Shader := TSkShader.MakeGradientSweep(Center,[$FFFFFFFF, $FFD0D0D0, $FFB0B0B0, $FFD0D0D0, $FFFFFFFF]);
  FPaint.Shader := Shader;
  BorderRect := ADest;

  InflateRect(BorderRect, -FPaint.StrokeWidth / 2, -FPaint.StrokeWidth / 2);
  ACanvas.DrawRect(BorderRect, FPaint);
end;

end.
