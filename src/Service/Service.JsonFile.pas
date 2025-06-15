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
  System.Classes, System.SysUtils, Vcl.Dialogs, Vcl.StdCtrls, Clipbrd, SynEdit;

type
  TJsonFileService = class
   public class function OpenJsonFromFile(var AJson: string): Boolean; static;
   public class function SaveJsonToFile(const AJson: string): Boolean; static;
   public class procedure CopyJsonToClipboard(const AJson: string); static;
   public class procedure CutJsonToClipboard(const ASynEdit: TSynEdit); static;
   public class procedure PasteJsonToClipboard(const ASynEdit: TSynEdit); static;
   public class procedure SelectAll(const ASynEdit: TSynEdit); static;
   public class procedure ClearMemoJson(const ASynEdit: TSynEdit); static;
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

class procedure TJsonFileService.SelectAll(const ASynEdit: TSynEdit);
begin
  ASynEdit.SelectAll;
end;

class procedure TJsonFileService.CopyJsonToClipboard(const AJson: string);
begin
  Clipboard.AsText := AJson;
end;

class procedure TJsonFileService.CutJsonToClipboard(const ASynEdit: TSynEdit);
begin
  ASynEdit.CutToClipboard;
end;

class procedure TJsonFileService.ClearMemoJson(const ASynEdit: TSynEdit);
begin
  ASynEdit.Lines.Clear;
end;

class procedure TJsonFileService.PasteJsonToClipboard(const ASynEdit: TSynEdit);
begin
  ASynEdit.PasteFromClipboard;
end;

end.
