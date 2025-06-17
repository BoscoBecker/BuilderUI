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
