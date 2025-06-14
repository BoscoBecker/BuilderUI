{}
{ Project: BuilderUI Forms for Windows }
{ A visual form builder for Windows based on Delphi }
{ }
{ Copyright (c) 2024 João Bosco Becker }
{ }
{ Contributors to this file: João Bosco Becker }
{ }
{ You can get the latest version of this file at: }
{ https://github.com/BoscoBecker/BuilderUI }
{ }
{ This library is free software; you can redistribute it and/or modify it }
{ under the terms of the GNU Lesser General Public License as published by the }
{ Free Software Foundation; either version 2.1 of the License, or (at your option) }
{ any later version. }
{ }
{ This library is distributed in the hope that it will be useful, but WITHOUT ANY }
{ WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A }
{ PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. }
{ }
{ You should have received a copy of the GNU Lesser General Public License along }
{ with this library; if not, write to the Free Software Foundation, Inc., }
{ 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA. }
{ You may also obtain a copy of the license at: }
{ https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html }
{ }
{ João Bosco Becker - https://github.com/BoscoBecker }
unit Service.Skia.Draw;

interface

uses
  System.Types, System.UITypes, System.Skia, Vcl.Skia, Enum.Utils;

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
