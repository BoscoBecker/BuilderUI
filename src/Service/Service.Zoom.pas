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
unit Service.Zoom;

interface

type
  TZoomService = class
  private
    FZoom: Single;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ZoomIn;
    procedure ZoomOut;
    function GetZoom: Single;
    property Zoom: Single read FZoom write FZoom;
  end;

implementation

constructor TZoomService.Create;
begin
  FZoom := 1.0;
end;

destructor TZoomService.destroy;
begin
  inherited;
end;

function TZoomService.GetZoom: Single;
begin
  result:= FZoom;
end;

procedure TZoomService.ZoomIn;
begin
  FZoom := FZoom * 1.1;
end;

procedure TZoomService.ZoomOut;
begin
  FZoom := FZoom / 1.1;
end;

end.
