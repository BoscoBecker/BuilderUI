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
unit Service.JsonFile;

interface

uses
  System.Classes, System.SysUtils, Vcl.Dialogs, Vcl.StdCtrls, Clipbrd;

type
  TJsonFileService = class
  public
    class function OpenJsonFromFile(var AJson: string): Boolean; static;
    class function SaveJsonToFile(const AJson: string): Boolean; static;
    class procedure CopyJsonToClipboard(const AJson: string); static;
    class procedure ClearMemoJson(AMemo: TMemo); static;
  end;

implementation

uses
  System.IOUtils;

class function TJsonFileService.OpenJsonFromFile(var AJson: string): Boolean;
var
  OpenDialog: TOpenDialog;
begin
  Result := False;
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'JSON Files (*.json)|*.json';
    if OpenDialog.Execute then
    begin
      if not String(OpenDialog.FileName).Trim.IsEmpty then
      begin
        AJson := TFile.ReadAllText(OpenDialog.FileName, TEncoding.UTF8);
        Result := True;
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

class function TJsonFileService.SaveJsonToFile(const AJson: string): Boolean;
var
  SaveDialog: TSaveDialog;
begin
  Result := False;
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'JSON Files (*.json)|*.json';
    if SaveDialog.Execute then
    begin
      if not String(SaveDialog.FileName).Trim.IsEmpty then
      begin
        TFile.WriteAllText(SaveDialog.FileName, AJson, TEncoding.UTF8);
        Result := True;
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

class procedure TJsonFileService.CopyJsonToClipboard(const AJson: string);
begin
  Clipboard.AsText := AJson;
end;

class procedure TJsonFileService.ClearMemoJson(AMemo: TMemo);
begin
  AMemo.Lines.Clear;
end;

end.
