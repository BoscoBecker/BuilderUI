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

uses System.Types, System.UITypes, System.Skia,  System.Math, Vcl.Skia, Enum.Utils, SysUtils;

type
  TSkiaDrawService = class
    public class procedure DrawBackground(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; Background: TBuilderBackground; var FPaint: ISkPaint; const ShowHorizontal, ShowVertical: Boolean );
    public class procedure DrawGradientBox(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; var FPaint: ISkPaint);static;
    public class procedure DrawExplorerBorder(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; var FPaint: ISkPaint);static;
    public class procedure DrawRulers(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; const ShowHorizontal, ShowVertical, ShowVerticalRigth: Boolean); static;
    public class procedure DrawBoxRenderJsonBorder(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single; var FPaint: ISkPaint); static;
  end;

implementation

class procedure TSkiaDrawService.DrawBackground(ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single;
                                                Background: TBuilderBackground; var FPaint: ISkPaint; const ShowHorizontal,
                                                ShowVertical: Boolean);
const
  GridSize = 20;
  RulerThickness = 24;
var
  GridArea: TRectF;
begin
  ACanvas.Save;

  case Background of
    bClear:
      ACanvas.Clear(TAlphaColors.White);

    bGrid:
    begin
      ACanvas.Clear($FFF5F5F5);

      FPaint := TSkPaint.Create;
      FPaint.Style := TSkPaintStyle.Stroke;
      FPaint.Color := $FFDFDFDF;
      FPaint.StrokeWidth := 1;

      GridArea := ADest;
      if ShowHorizontal then
        GridArea.Top := GridArea.Top + RulerThickness;
      if ShowVertical then
        GridArea.Left := GridArea.Left + RulerThickness;

      var X := GridArea.Left;
      while X <= GridArea.Right do
      begin
        ACanvas.DrawLine(X, GridArea.Top, X, GridArea.Bottom, FPaint);
        X := X + GridSize;
      end;

      var Y := GridArea.Top;
      while Y <= GridArea.Bottom do
      begin
        ACanvas.DrawLine(GridArea.Left, Y, GridArea.Right, Y, FPaint);
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


class procedure TSkiaDrawService.DrawRulers(
  ACanvas: ISkCanvas;
  const ADest: TRectF;
  const AOpacity: Single;
  const ShowHorizontal, ShowVertical, ShowVerticalRigth: Boolean
);
const
  RulerThickness = 24;
  TickSizeMajor = 10;
  TickSizeMinor = 5;
  TickStep = 10;
var
  TextPaint: ISkPaint;
  Text: string;
  RulerPaint: ISkPaint;
begin
  var Font := TSkFont.Create(nil, 9);
  RulerPaint := TSkPaint.Create;
  try
    RulerPaint.Style := TSkPaintStyle.Stroke;
    RulerPaint.Color := $FF8A8A8A;
    RulerPaint.StrokeWidth := 1;

    TextPaint := TSkPaint.Create;
    TextPaint.Color := $FF404040;
    TextPaint.Style := TSkPaintStyle.Fill;

    ACanvas.Save;
    if ShowHorizontal then
    begin
      ACanvas.DrawRect(RectF(ADest.Left, ADest.Top, ADest.Right, ADest.Top + RulerThickness), RulerPaint);

      var i := Trunc(ADest.Left) div TickStep * TickStep;
      while i < ADest.Right do
      begin
        if (i mod 50 = 0) then
        begin
          ACanvas.DrawLine(i, ADest.Top, i, ADest.Top + TickSizeMajor, RulerPaint);
          Text := i.ToString;
          ACanvas.DrawSimpleText(
            Text,
            i + 2,
            ADest.Top + TickSizeMajor + 8,
            Font,
            TextPaint
          );
        end
        else
          ACanvas.DrawLine(i, ADest.Top, i, ADest.Top + TickSizeMinor, RulerPaint);

        Inc(i, TickStep);
      end;
    end;

    if ShowVertical then
    begin
      ACanvas.DrawRect(RectF(ADest.Left, ADest.Top, ADest.Left + RulerThickness, ADest.Bottom), RulerPaint);
      var i := Trunc(ADest.Top) div TickStep * TickStep;
      while i < ADest.Bottom do
      begin
        if (i mod 50 = 0) then
        begin
          ACanvas.DrawLine(ADest.Left, i, ADest.Left + TickSizeMajor, i, RulerPaint);
          Text := i.ToString;
          ACanvas.DrawSimpleText(
            Text,
            ADest.Left + TickSizeMajor + 2,
            i + 5,
            Font,
            TextPaint
          );
        end
        else
          ACanvas.DrawLine(ADest.Left, i, ADest.Left + TickSizeMinor, i, RulerPaint);

        Inc(i, TickStep);
      end;
    end;

    if ShowVerticalRigth then
    begin
      ACanvas.DrawRect(RectF(ADest.Right - RulerThickness, ADest.Top, ADest.Right, ADest.Bottom), RulerPaint);
      var i := Trunc(ADest.Top) div TickStep * TickStep;
      while i < ADest.Bottom do
      begin
        if (i mod 50 = 0) then
        begin
          ACanvas.DrawLine(ADest.Right - TickSizeMajor, i, ADest.Right, i, RulerPaint);
          Text := i.ToString;
          ACanvas.DrawSimpleText(
            Text,
            ADest.Right - RulerThickness + 2,
            i + 5,
            Font,
            TextPaint
          );
        end
        else
          ACanvas.DrawLine(ADest.Right - TickSizeMinor, i, ADest.Right, i, RulerPaint);

        Inc(i, TickStep);
      end;
    end;


  finally
    ACanvas.Restore;
    Font.Free;
  end;
end;

end.
