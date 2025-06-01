unit Factory.ICodeGeneratorFactory;

interface

uses System.Classes, System.SysUtils,
     Factory.ICodeGenerator,Factory.CodeGenerator.Delphi;

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
    else raise Exception.Create('Not Supported Language yet.');

end;

end.
