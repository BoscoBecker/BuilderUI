unit Factory.ICodeGenerator;

interface

uses System.Json;

type
  ICodeGenerator = interface
    ['{1C4B0C41-4F37-42BC-9A23-A1EA902E1F63}']
    function GenerateCode(const Json: TJSONObject; const Indent: string = '  '): string;
    function FindFormByName(Json: TJSONObject; const AName: string): TJSONObject;
    function GetGUIText: string;
    function GetCodeText: string;

    property GUIText: string read GetGUIText;
    property CodeText: string read GetCodeText;
  end;


implementation

end.
