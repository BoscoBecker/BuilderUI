unit Strategy.Export.CSharp;

interface

uses Strategy.IExport, System.Classes, System.SysUtils, System.Json;

type
  TCSharpExport = class(TInterfacedObject,IExportData)
  public
    procedure ExportData(const APath, ATextAJson: string);
end;

implementation

procedure TCSharpExport.ExportData(const APath, ATextAJson: string);
begin
  var FileStream := TStringList.Create;
  try
    FileStream.Text := ATextAJson;
    FileStream.SaveToFile(APath, TEncoding.UTF8);
  finally
    FileStream.Free;
  end;

end;

end.
