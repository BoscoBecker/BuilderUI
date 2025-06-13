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
