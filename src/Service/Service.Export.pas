unit Service.Export;

interface

uses System.Generics.Collections, System.JSON, Strategy.Export.Delphi,  Vcl.ComCtrls,
     Strategy.Export.CSharp, Factory.ICodeGenerator, Factory.CodeGeneratorFactory;

type
  TExportService = class
   public
    class procedure ExportForm(const Json: TJSONObject; const FormName, Path, Technology: string; OnlyGUI: Boolean); static;
  end;

implementation

class procedure TExportService.ExportForm(const Json: TJSONObject; const FormName, Path, Technology: string; OnlyGUI: Boolean);
begin
  if Technology = 'Delphi' then
  begin
    var Generator := TCodeGeneratorFactory.CreateGenerator(Technology);
    var DelphiFiles := TDelphiExport.Create;
    var Components := Generator.FindFormByName(Json, FormName);
    try
      Generator.GenerateCode(Components);
      if OnlyGUI then
        DelphiFiles.ExportData(Path + '\' + FormName + '.dfm', Generator.GUIText)
      else
      begin
        DelphiFiles.ExportData(Path + '\' + FormName + '.dfm', Generator.GUIText);
        DelphiFiles.ExportData(Path + '\' + FormName + '.pas', Generator.CodeText);
      end;
    finally
      DelphiFiles.Free;
    end;
  end
  else if Technology = 'CSharp' then
  begin
    var Generator := TCodeGeneratorFactory.CreateGenerator(Technology);
    var CSharpFile := TCSharpExport.Create;
    var Components := Generator.FindFormByName(Json, FormName);
    try
      Generator.GenerateCode(Components);
      CSharpFile.ExportData(Path + '\' + FormName + '.cs', Generator.CodeText);
    finally
      CSharpFile.Free;
    end;
  end;
end;


end.
