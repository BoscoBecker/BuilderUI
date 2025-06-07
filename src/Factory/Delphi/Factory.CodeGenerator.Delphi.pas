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

    // NOVO: Tratar Position
    if (PropName = 'Position') and (Pair.JsonValue is TJSONObject) then
    begin
      var PosObj := Pair.JsonValue as TJSONObject;
      var X := PosObj.GetValue<Integer>('X', 0);
      var Y := PosObj.GetValue<Integer>('Y', 0);
      Result := Result + Indent + '  Left = ' + X.ToString + sLineBreak;
      Result := Result + Indent + '  Top = ' + Y.ToString + sLineBreak;
      Continue;
    end;

    // Ajuste de propriedades específicas para Caption
    if ((CompType = 'TLabel') or (CompType = 'TButton') or (CompType = 'TPanel') or (CompType = 'TTabSheet')) then
    begin
      // Se vier 'Text', converte para 'Caption'
      if PropName = 'Text' then
        PropName := 'Caption';
      // Se vier 'Caption', mantém
    end;

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
    begin
      if ((CompType = 'TShape') and (PropName = 'Shape')) or 
         ((CompType = 'TBitBtn') and (PropName = 'Kind')) then
        Result := Result + Indent + '  ' + PropName + ' = ' + Pair.JsonValue.Value +  sLineBreak
      else        
        Result := Result + Indent + '  ' + PropName + ' = ''' + Pair.JsonValue.Value + '''' + sLineBreak;
    end
    // Propriedades array (Items, Lines, etc.)
    else if Pair.JsonValue is TJSONArray then
    begin
      var Arr := Pair.JsonValue as TJSONArray;
      if (PropName = 'Items') or (PropName = 'Lines') then
      begin
        Result := Result + Indent + '  ' + PropName + '.Strings = (' + sLineBreak;
        for var J := 0 to Arr.Count - 1 do
          Result := Result + Indent + '    ''' + Arr.Items[J].Value + '''' + sLineBreak;
        Result := Result + Indent + '  )' + sLineBreak;
      end;
    end;
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
  FormType,PasText, FieldsBlock: string;
  Children: TJSONArray;

begin
  // 1. Ler o nome do form
  var FormName := Json.GetValue<string>('Name');
  var Caption := Json.GetValue<string>('Caption');
  var Width := Json.GetValue<integer>('Width');
  var Height:= Json.GetValue<integer>('Height');


  Json.TryGetValue<string>('Type',FormType);
  if FormType.Trim.Equals('') then Exit;

  // 2. Gerar o texto do .dfm
  var DfmText := 'object ' + FormName+'_' + ': ' + 'T'+FormName+ sLineBreak +
                 '  Left = 0'  +sLineBreak +
                 '  Top =  0'   +sLineBreak +
                 '  Width = ' + Width.ToString + sLineBreak +
                 '  Height = ' + Height.ToString + sLineBreak +
                 '  Caption = ' + QuotedStr(Caption) + sLineBreak +
                 '  ClientWidth = ' + Width.ToString + sLineBreak +
                 '  ClientHeight = ' + Height.ToString + sLineBreak;

  // Children
  if Json.TryGetValue('Children', Children) and (Children is TJSONArray) then
    for var I := 0 to Children.Count - 1 do
      DfmText := DfmText + GenerateComponent(Children.Items[I] as TJSONObject);

  DfmText := DfmText + 'end' + sLineBreak;

  // 3. Gerar o texto do .pas com todos os componentes declarados
  SetLength(ComponentFields, 0);
  if Json.TryGetValue('Children', Children) and (Children is TJSONArray) then
    for var I := 0 to Children.Count - 1 do
      CollectComponentFields(Children.Items[I] as TJSONObject);

  FieldsBlock := '';
  for var F in ComponentFields do
    FieldsBlock := FieldsBlock + '    ' + F.Name + ': ' + F.CompType + ';' + sLineBreak;

  PasText := 'unit ' + FormName + ';' + sLineBreak + #13#10+

             'interface' + sLineBreak + #13#10+

             'uses  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,'+ sLineBreak +
	           '      Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.Grids, '+ sLineBreak +
             '      Data.DB, Vcl.DBGrids; '+ sLineBreak +  #13#10 + #13#10+

             'type' + sLineBreak +
             '  ' + 'T'+FormName + ' = class(TForm)' + sLineBreak +
              FieldsBlock + '  end;' + sLineBreak  + #13#10+ #13#10 +

             'var '+ #13#10 + '   '+FormName+'_' + ': ' + 'T'+FormName + ';' + sLineBreak +  #13#10+

             'implementation' + sLineBreak + #13#10+

             '{$R *.dfm} '  + sLineBreak +   #13#10+

             'end.';

  var DelphiFile:= TSaveDialog.Create(nil);
  var SaveFile:= TStringList.Create;

  SaveFile.Append(DfmText);
  try
    DelphiFile.Filter:= 'Delphi Form Module | *.dfm';
    DelphiFile.FileName:= FormName +'.dfm';
    if DelphiFile.Execute(Application.Handle) then
      if not String(DelphiFile.FileName).Trim.Equals('') then
        SaveFile.SaveToFile(DelphiFile.FileName);

    SaveFile.Clear;

    DelphiFile.Filter:= 'Pascal Source File | *.pas';
    DelphiFile.FileName:= FormName +'.pas';
    SaveFile.Append(PasText);
    if DelphiFile.Execute(Application.Handle) then
      if not String(DelphiFile.FileName).Trim.Equals('') then
        SaveFile.SaveToFile(DelphiFile.FileName);
  finally
    DelphiFile.Free;
    SaveFile.Free;
  end;

  Result := DfmText + sLineBreak + '---' + sLineBreak + PasText;
end;

end.
