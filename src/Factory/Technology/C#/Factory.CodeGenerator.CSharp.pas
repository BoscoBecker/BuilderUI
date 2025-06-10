unit Factory.CodeGenerator.CSharp;

interface

uses System.Json, System.SysUtils, System.Generics.Collections, Factory.ICodeGenerator;

type
  TCsharp = class(TInterfacedObject,ICodeGenerator)
  private
    FGUIText: string;
    FCodeText: string;
    function GenerateComponent(const CompJson: TJSONObject; const Indent: string = '    '): string;
    procedure SetGUIText(const Value: string);
    procedure SetCodeText(const Value: string);
  public
    function GenerateCode(const Json: TJSONObject; const Indent: string = '    '): string;
    function FindFormByName(Json: TJSONObject; const AName: string): TJSONObject;
    function GetGUIText: string;
    function GetCodeText: string;
    property GUIText: string read GetGUIText write SetGUIText;
    property CodeText: string read GetCodeText write SetCodeText;
  end;

implementation


function TCsharp.FindFormByName(Json: TJSONObject; const AName: string): TJSONObject;
var
  FormsArray: TJSONArray;
  FormObj: TJSONObject;
  I: Integer;
begin
  Result := nil;
  if Json.TryGetValue<TJSONArray>('Forms', FormsArray) then
  begin
    for I := 0 to FormsArray.Count - 1 do
    begin
      if (FormsArray.Items[I] is TJSONObject) then
      begin
        FormObj := TJSONObject(FormsArray.Items[I]);
        if SameText(FormObj.GetValue<string>('Name', ''), AName) then
          Exit(FormObj);
      end;
    end;
  end;
end;

function TCsharp.GenerateComponent(const CompJson: TJSONObject; const Indent: string): string;
var
  CompType, CompName, PropName, Line: string;
  Children: TJSONArray;
  I: Integer;
begin
  CompType := CompJson.GetValue<string>('Type', '');
  CompName := CompJson.GetValue<string>('Name', '');
  Line := '';

  if (CompType = '') or (CompName = '') then
    Exit('');

  // Exemplo: this.LblTituloCliente = new Label();
  Line := Indent + 'this.' + CompName + ' = new ' + Copy(CompType, 2, MaxInt) + '();' + sLineBreak;

  for var Pair in CompJson do
  begin
    PropName := Pair.JsonString.Value;

    if (PropName = 'Type') or (PropName = 'Name') or (PropName = 'Children') then
      Continue;

    if (PropName = 'Position') and (Pair.JsonValue is TJSONObject) then
    begin
      var PosObj := Pair.JsonValue as TJSONObject;
      var X := PosObj.GetValue<Integer>('X', 0);
      var Y := PosObj.GetValue<Integer>('Y', 0);
      Line := Line + Indent + 'this.' + CompName + '.Location = new System.Drawing.Point(' + X.ToString + ', ' + Y.ToString + ');' + sLineBreak;
      Continue;
    end;

    if (PropName = 'Width') or (PropName = 'Height') then
    begin
      Line := Line + Indent + 'this.' + CompName + '.' + PropName + ' = ' + Pair.JsonValue.Value + ';' + sLineBreak;
      Continue;
    end;

    if (PropName = 'Caption') or (PropName = 'Text') then
    begin
      Line := Line + Indent + 'this.' + CompName + '.Text = "' + Pair.JsonValue.Value + '";' + sLineBreak;
      Continue;
    end;

    // Adicione outros mapeamentos de propriedades conforme necessário
  end;

  // Adiciona o componente aos controles do parent (assume que é o form)
  Line := Line + Indent + 'this.Controls.Add(this.' + CompName + ');' + sLineBreak;

  // Recursão para filhos
  if CompJson.TryGetValue('Children', Children) then
    for I := 0 to Children.Count - 1 do
      Line := Line + GenerateComponent(Children.Items[I] as TJSONObject, Indent);

  Result := Line;
end;

function TCsharp.GenerateCode(const Json: TJSONObject; const Indent: string): string;
var
  FormName, Caption: string;
  Width, Height: Integer;
  Children: TJSONArray;
  GUIText: string;
begin
  FormName := Json.GetValue<string>('Name', 'MyForm');
  Caption := Json.GetValue<string>('Caption', FormName);
  Width := Json.GetValue<Integer>('Width', 800);
  Height := Json.GetValue<Integer>('Height', 600);

  // Geração do código C# WinForms
  GUIText :=
    'using System;' + sLineBreak +
    'using System.Windows.Forms;' + sLineBreak +
    'using System.Drawing;' + sLineBreak + sLineBreak +
    'public class ' + FormName + ' : Form' + sLineBreak +
    '{' + sLineBreak +
    Indent + 'public ' + FormName + '()' + sLineBreak +
    Indent + '{' + sLineBreak +
    Indent + '    this.Text = "' + Caption + '";' + sLineBreak +
    Indent + '    this.Width = ' + Width.ToString + ';' + sLineBreak +
    Indent + '    this.Height = ' + Height.ToString + ';' + sLineBreak;

  if Json.TryGetValue('Children', Children) and (Children is TJSONArray) then
    for var I := 0 to Children.Count - 1 do
      GUIText := GUIText + GenerateComponent(Children.Items[I] as TJSONObject, Indent + '    ');

  GUIText := GUIText + Indent + '}' + sLineBreak + '}';

  SetGUIText(GUIText);
  SetCodeText(GUIText); // Para WinForms, GUI e code são praticamente o mesmo

  Result := FGUIText;
end;

function TCsharp.GetCodeText: string;
begin
  Result := FCodeText;
end;

function TCsharp.GetGUIText: string;
begin
  Result := FGUIText;
end;

procedure TCsharp.SetCodeText(const Value: string);
begin
  FCodeText := Value;
end;

procedure TCsharp.SetGUIText(const Value: string);
begin
  FGUIText := Value;
end;

end.
