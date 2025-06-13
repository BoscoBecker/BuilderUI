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
unit Factory.CodeGeneratorFactory;

interface

uses System.Classes, System.SysUtils,
     Factory.ICodeGenerator,Factory.CodeGenerator.Delphi, Factory.CodeGenerator.CSharp;

type
  TCodeGeneratorFactory = class
    class function CreateGenerator(Language: string): ICodeGenerator;
  end;

implementation

{ TCodeGeneratorFactory }

class function TCodeGeneratorFactory.CreateGenerator(Language: string): ICodeGenerator;
begin
    if SameText(Language, 'Delphi') then
      Result := TDelphiGenerator.Create
    else
    if SameText(Language,'CSharp') then
      result:= TCSharp.Create
    else raise Exception.Create('Not Supported Language yet.');

end;

end.
