unit Strategy.Export.Delphi;

interface

uses Strategy.IExport;

type
  TDelphiExport = class(TInterfacedObject, IExportData)
  public
    procedure ExportData(const AJSON: string);
  end;



implementation

{ TDelphiExport }

procedure TDelphiExport.ExportData(const AJSON: string);
begin
  ///
end;

end.
