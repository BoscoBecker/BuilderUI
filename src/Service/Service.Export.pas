{}
{ Project: BuilderUI Forms for Windows }
{ A visual form builder for Windows based on Delphi }
{ }
{ Copyright (c) 2024 Jo�o Bosco Becker }
{ }
{ Contributors to this file: Jo�o Bosco Becker }
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
{ Jo�o Bosco Becker - https://github.com/BoscoBecker }

unit Service.Export;

interface

uses System.Generics.Collections, System.JSON,  System.SysUtils, Vcl.ComCtrls,

     Strategy.Export.Delphi,  Strategy.Export.Lazarus, Strategy.Export.CSharp,
     Factory.ICodeGenerator, Factory.CodeGeneratorFactory, Factory.CodeGenerator.CSharp;

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
      DelphiFiles.ExportData(IncludeTrailingPathDelimiter(Path) + FormName + '.dfm', Generator.GUIText);
      if not OnlyGUI then
        DelphiFiles.ExportData(IncludeTrailingPathDelimiter(Path) + FormName + '.pas', Generator.CodeText);
    finally
      DelphiFiles.Free;
    end;
  end
  else if Technology = 'CSharp' then
  begin
    var Generator := TCodeGeneratorFactory.CreateGenerator(Technology);
    var CSharpFile := TCSharpExport.Create;
    var CSharpFileGUI := TCSharpGenerator.Create;
    var Components := Generator.FindFormByName(Json, FormName);
    try
      Generator.GenerateCode(Components);
      CSharpFileGUI.GenerateDesignerCode(Components);
      CSharpFile.ExportData(IncludeTrailingPathDelimiter(Path) + FormName + '.Designer.cs', CSharpFileGUI.GUIText);
      if not OnlyGUI then
        CSharpFile.ExportData(IncludeTrailingPathDelimiter(Path) + FormName + '.cs', Generator.CodeText);
    finally
      CSharpFile.Free;
      CSharpFileGUI.Free;
    end;
  end else
  if Technology = 'Lazarus' then
  begin
    var Generator := TCodeGeneratorFactory.CreateGenerator(Technology);
    var LazarusFiles := TLazarusExport.Create;
    var Components := Generator.FindFormByName(Json, FormName);
    try
      Generator.GenerateCode(Components);
      LazarusFiles.ExportData(IncludeTrailingPathDelimiter(Path) + FormName + '.lfm', Generator.GUIText);
      if not OnlyGUI then
        LazarusFiles.ExportData(IncludeTrailingPathDelimiter(Path) + FormName + '.pas', Generator.CodeText);
    finally
      LazarusFiles.Free;
    end;
  end
end;


end.
