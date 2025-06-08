unit Strategy.Export.Delphi;

interface

uses
  Strategy.IExport, System.Classes, System.SysUtils, System.Json;

type
  TDelphiExport = class(TInterfacedObject, IExportData)
  public
    procedure ExportData(const APath, ATextAJson: string);
  end;

implementation

procedure TDelphiExport.ExportData(const APath, ATextAJson: string);
var
  FileStream: TStringList;
begin
  FileStream := TStringList.Create;
  try
    FileStream.Text := ATextAJson;
    FileStream.SaveToFile(APath, TEncoding.UTF8);
  finally
    FileStream.Free;
  end;
end;

end.
