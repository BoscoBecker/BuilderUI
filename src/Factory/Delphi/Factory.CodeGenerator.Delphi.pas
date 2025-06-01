unit Factory.CodeGenerator.Delphi;

interface

uses
  System.Json, System.SysUtils, System.StrUtils , System.Classes,
  System.Generics.Collections,
  Dialogs, Forms, Factory.ICodeGenerator, Winapi.Windows;

type
  TComponentField = record
    Name, CompType: string;
  end;

  TDelphiGenerator = class(TInterfacedObject, ICodeGenerator)
  private
    ComponentFields: TArray<TComponentField>;
    procedure CollectComponentFields(const CompJson: TJSONObject);
    function GenerateComponent(const CompJson: TJSONObject; const Indent: string = '  '): string;
  public
    function GenerateCode(const Json: TJSONObject; const Indent: string = '  '): string;
  end;

implementation

{ TDelphiGenerator }

function TDelphiGenerator.GenerateComponent(const CompJson: TJSONObject; const Indent: string = '  '): string;
var
  CompType, CompName, PropName: string;
  Children: TJSONArray;
  I: Integer;
begin
  CompType := CompJson.GetValue<string>('Type', '');
  CompName := CompJson.GetValue<string>('Name', '');
  Result := Indent + 'object ' + CompName + ': ' + CompType + sLineBreak;

  // Propriedades básicas
  for var Pair in CompJson do
  begin
    PropName := Pair.JsonString.Value;

    // Ignorar propriedades de controle
    if (PropName = 'Type') or (PropName = 'Name') or (PropName = 'Children') then
      Continue;

    // Ajuste de propriedades específicas para Caption
    if ((CompType = 'TLabel') or (CompType = 'TButton') or (CompType = 'TPanel') or (CompType = 'TTabSheet')) and (PropName = 'Text') then
      PropName := 'Caption';

    // Propriedades numéricas (sem aspas)
    if Pair.JsonValue is TJSONNumber then
      Result := Result + Indent + '  ' + PropName + ' = ' + Pair.JsonValue.Value + sLineBreak
    // Propriedades booleanas (True/False)
    else if Pair.JsonValue is TJSONBool then
      if TJSONBool(Pair.JsonValue).AsBoolean then
        Result := Result + Indent + '  ' + PropName + ' = True' + sLineBreak
      else
        Result := Result + Indent + '  ' + PropName + ' = False' + sLineBreak
    // Propriedades string (sempre aspas simples)
    else if Pair.JsonValue is TJSONString then
      Result := Result + Indent + '  ' + PropName + ' = ''' + Pair.JsonValue.Value + '''' + sLineBreak;
  end;

  // Children
  if CompJson.TryGetValue('Children', Children) then
    for I := 0 to Children.Count - 1 do
      Result := Result + GenerateComponent(Children.Items[I] as TJSONObject, Indent + '  ');

  Result := Result + Indent + 'end' + sLineBreak;
end;

procedure TDelphiGenerator.CollectComponentFields(const CompJson: TJSONObject);
var
  CompType, CompName: string;
  Children: TJSONArray;
  I: Integer;
begin
  CompType := CompJson.GetValue<string>('Type', '');
  CompName := CompJson.GetValue<string>('Name', '');
  if (CompType <> '') and (CompName <> '') and (CompType <> 'TForm') then
  begin
    SetLength(ComponentFields, Length(ComponentFields) + 1);
    ComponentFields[High(ComponentFields)].Name := CompName;
    ComponentFields[High(ComponentFields)].CompType := CompType;
  end;
  if CompJson.TryGetValue('Children', Children) then
    for I := 0 to Children.Count - 1 do
      CollectComponentFields(Children.Items[I] as TJSONObject);
end;

function TDelphiGenerator.GenerateCode(const Json: TJSONObject; const Indent: string = '  '): string;
var
  FormName, DfmText, PasText, FieldsBlock: string;
  Children: TJSONArray;
  I,Width,Height: Integer;
begin
  // 1. Ler o nome do form
  FormName := Json.GetValue<string>('Name', 'TForm1');
  Width := Json.GetValue<integer>('Width', 300)
  Height:= Json.GetValue<integer>('Height', 200)
  // 2. Gerar o texto do .dfm
  DfmText := 'object ' + FormName + ': ' + Json.GetValue<string>('Type', 'TForm') + sLineBreak;
             '  Left = 0' + sLineBreak +
             '  Top = 0' + sLineBreak +
             '  Width = ' + Width  + sLineBreak +
             '  Height = ' + Height  + sLineBreak +
             '  Caption = ''' + Caption + '''' + sLineBreak +
             '  ClientWidth = ' + Width + sLineBreak +
             '  ClientHeight = ' + Height + sLineBreak;  
  // Propriedades do form
  for var Pair in Json do
  begin
    if (Pair.JsonString.Value <> 'Type') and
       (Pair.JsonString.Value <> 'Name') and
       (Pair.JsonString.Value <> 'Children') then
    begin
      if Pair.JsonValue is TJSONString then
        DfmText := DfmText + '  ' + Pair.JsonString.Value + ' = ''' + Pair.JsonValue.Value + '''' + sLineBreak
      else if Pair.JsonValue is TJSONNumber then
        DfmText := DfmText + '  ' + Pair.JsonString.Value + ' = ' + Pair.JsonValue.Value + sLineBreak
      else if Pair.JsonValue is TJSONBool then
        DfmText := DfmText + '  ' + Pair.JsonString.Value + ' = ' + LowerCase(Pair.JsonValue.Value) + sLineBreak;
    end;
  end;
  // Children
  if Json.TryGetValue('Children', Children) and (Children is TJSONArray) then
    for I := 0 to Children.Count - 1 do
      DfmText := DfmText + GenerateComponent(Children.Items[I] as TJSONObject);

  DfmText := DfmText + 'end' + sLineBreak;

  // 3. Gerar o texto do .pas com todos os componentes declarados
  SetLength(ComponentFields, 0);
  if Json.TryGetValue('Children', Children) and (Children is TJSONArray) then
    for I := 0 to Children.Count - 1 do
      CollectComponentFields(Children.Items[I] as TJSONObject);

  FieldsBlock := '';
  for var F in ComponentFields do
    FieldsBlock := FieldsBlock + '    ' + F.Name + ': ' + F.CompType + ';' + sLineBreak;

  PasText := 'unit ' + FormName + ';' + sLineBreak +
             'interface' + sLineBreak +
             'uses  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,'+ sLineBreak +
	           'Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls; '+ sLineBreak +
             'type' + sLineBreak +
             '  ' + 'T'+FormName + ' = class(TForm)' + sLineBreak +
             FieldsBlock +
             '  end;' + sLineBreak +
             'var ' + FormName + ': ' + 'T'+FormName + ';' + sLineBreak +
             'implementation' + sLineBreak +
             '{$R *.dfm} '  + sLineBreak +
             'end.';

  var DelphiFile:= TSaveDialog.Create(nil);
  var SaveFile:= TStringList.Create;

  SaveFile.Append(DfmText);
  try
    DelphiFile.Filter:= 'Delphi Form Module | *.dfm';
    if DelphiFile.Execute(Application.Handle) then
      if not String(DelphiFile.FileName).Trim.Equals('') then
        SaveFile.SaveToFile(DelphiFile.FileName);

    SaveFile.Clear;

    if Application.MessageBox('Do you want save the .pas file ?','Question', MB_ICONQUESTION + MB_YESNO) = IDYES then
    begin
      DelphiFile.Filter:= 'Pascal Source File | *.pas';
      SaveFile.Append(PasText);
      if DelphiFile.Execute(Application.Handle) then
        if not String(DelphiFile.FileName).Trim.Equals('') then
          SaveFile.SaveToFile(DelphiFile.FileName);
    end;
  finally
    DelphiFile.Free;
    SaveFile.Free;
  end;

  Result := DfmText + sLineBreak + '---' + sLineBreak + PasText;
end;

end.
