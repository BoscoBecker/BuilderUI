unit Service.Export;

interface

uses System.Generics.Collections, System.JSON, Strategy.Export.Delphi,  Vcl.ComCtrls,
     Strategy.Export.CSharp, Factory.ICodeGenerator, Factory.CodeGeneratorFactory,
     Factory.CodeGenerator.CSharp;

type
  TExportService = class
    public class procedure ExportForm(const Json: TJSONObject; const FormName, Path, Technology: string; OnlyGUI: Boolean); static;
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
    var CSharpFileGUI:= TCSharp.Create;
    var Components := Generator.FindFormByName(Json, FormName);
    try
      Generator.GenerateCode(Components);
      CSharpFileGUI.GenerateDesignerCode(Components);
      CSharpFile.ExportData(Path + '\' + FormName + '.cs', Generator.CodeText);
      CSharpFile.ExportData(Path + '\' + FormName + '.Designer.cs', CSharpFileGUI.GUIText);
    finally
      CSharpFile.Free;
    end;
  end;
end;


end.
