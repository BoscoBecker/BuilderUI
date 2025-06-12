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
