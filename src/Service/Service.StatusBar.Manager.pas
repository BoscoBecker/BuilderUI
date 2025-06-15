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

unit Service.StatusBar.Manager;

interface

uses Vcl.ComCtrls, system.SysUtils, Service.Zoom;

type
  TStatusBarManager = class
  private
    FStatusBar: TStatusBar;
    FZoom: TZoomService;
  public
    constructor Create(AStatusBar: TStatusBar);
    destructor Destroy; override;
    procedure SetZoom(ZoomPercent: Integer);
    procedure SetPosition(X, Y: Integer);
    procedure SetSelected(const Name: string);
    procedure SetComponentCount(const Name: string);
    procedure SetProjectName(const Name: string);
    procedure SetMode(const Mode: string);
    procedure SetDate(const ADate: TDateTime);
    procedure UpdateAll(const X, Y, Zoom: Integer; const Selected, Project, Mode: string; const ADate: TDateTime);
  end;

  implementation

constructor TStatusBarManager.Create(AStatusBar: TStatusBar);
begin
  FStatusBar := AStatusBar;
  FZoom:= TZoomService.Create;
end;


destructor TStatusBarManager.Destroy;
begin
  if FStatusBar <> nil then
    FStatusBar.Free;
  if FZoom <> nil then
    FZoom.Free;
  inherited;
end;

procedure TStatusBarManager.SetComponentCount(const Name: string);
begin
  FStatusBar.Panels[2].Text := Format('Component count: %s ', [Name]);
end;

procedure TStatusBarManager.SetDate(const ADate: TDateTime);
begin
  FStatusBar.Panels[3].Text := 'Date: ' + FormatDateTime('YYYY/MM/DD', ADate);
end;

procedure TStatusBarManager.SetMode(const Mode: string);
begin
  FStatusBar.Panels[5].Text := 'Mode: ' + Mode;
end;

procedure TStatusBarManager.SetPosition(X, Y: Integer);
begin
  FStatusBar.Panels[0].Text := Format('X: %d Y: %d', [X, Y]);
end;

procedure TStatusBarManager.SetProjectName(const Name: string);
begin
  FStatusBar.Panels[6].Text := 'Project: ' + Name;
end;

procedure TStatusBarManager.SetSelected(const Name: string);
begin
  FStatusBar.Panels[1].Text := 'Selected: ' + Name;
end;

procedure TStatusBarManager.SetZoom(ZoomPercent: Integer);
begin
  FStatusBar.Panels[4].Text := Format('Zoom: %d%%', [ZoomPercent]);
end;

procedure TStatusBarManager.UpdateAll(const X, Y, Zoom: Integer; const Selected, Project, Mode: string; const ADate: TDateTime);
begin
  SetPosition(X, Y);
  SetSelected(Selected);
  SetZoom(Zoom);
  SetProjectName(Project);
  SetMode(Mode);
  SetDate(ADate);
end;

end.

