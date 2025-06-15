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

