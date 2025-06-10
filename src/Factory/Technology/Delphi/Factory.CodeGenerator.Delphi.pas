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

  TDelphiGenerator = class(TInterfacedObject,ICodeGenerator)
  private
    ComponentFields: TArray<TComponentField>;
    FGUIText: string;
    FCOdeText: string;
    function GenerateComponent(const CompJson: TJSONObject; const Indent: string = '  '): string;
    function GenerateDfmText(const Json: TJSONObject ): string;
    function GeneratePasText(const Json: TJSONObject ): string;
    procedure CollectComponentFields(const CompJson: TJSONObject);
    procedure SetGUIText(const Value: string);
    procedure SetCodeText(const Value: string);
  public
    function GetGUIText: string;
    function GetCodeText: string;
    function GenerateCode(const Json: TJSONObject; const Indent: string = '  '): string;
    function FindFormByName(Json: TJSONObject; const AName: string): TJSONObject;
    property GUIText: string read FGUIText write SetGUIText;
    property CodeText: string read FCOdeText write SetCodeText;
  end;

implementation


function TDelphiGenerator.GenerateComponent(const CompJson: TJSONObject; const Indent: string = '  '): string;
var
  CompType, CompName, PropName: string;
  Children: TJSONArray;
  I: Integer;
begin
  CompType := CompJson.GetValue<string>('Type', '');
  CompName := CompJson.GetValue<string>('Name', '');
  Result := Indent + 'object ' + CompName + ': ' + CompType + sLineBreak;

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
      Result := Result + Indent + '  Left = ' + X.ToString + sLineBreak;
      Result := Result + Indent + '  Top = ' + Y.ToString + sLineBreak;
      Continue;
    end;

    if ((CompType = 'TLabel') or (CompType = 'TButton') or (CompType = 'TPanel') or (CompType = 'TTabSheet')) then
    begin
      if PropName = 'Text' then
        PropName := 'Caption';
    end;

    if Pair.JsonValue is TJSONNumber then
      Result := Result + Indent + '  ' + PropName + ' = ' + Pair.JsonValue.Value + sLineBreak
    else if Pair.JsonValue is TJSONBool then
      if TJSONBool(Pair.JsonValue).AsBoolean then
        Result := Result + Indent + '  ' + PropName + ' = True' + sLineBreak
      else
        Result := Result + Indent + '  ' + PropName + ' = False' + sLineBreak
    else if Pair.JsonValue is TJSONString then
    begin
      if ((CompType = 'TShape') and (PropName = 'Shape')) or 
         ((CompType = 'TBitBtn') and (PropName = 'Kind')) then
        Result := Result + Indent + '  ' + PropName + ' = ' + Pair.JsonValue.Value +  sLineBreak
      else        
        Result := Result + Indent + '  ' + PropName + ' = ''' + Pair.JsonValue.Value + '''' + sLineBreak;
    end
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

  if CompJson.TryGetValue('Children', Children) then
    for I := 0 to Children.Count - 1 do
      Result := Result + GenerateComponent(Children.Items[I] as TJSONObject, Indent + '  ');

  Result := Result + Indent + 'end' + sLineBreak;
end;

function TDelphiGenerator.GenerateDfmText(const Json: TJSONObject): string;
begin
  var FormType: string;
  Json.TryGetValue<string>('Type',FormType);
  if FormType.Trim.Equals('') then Exit;

  var Children: TJSONArray;
  var FormName := Json.GetValue<string>('Name');
  var Caption := Json.GetValue<string>('Caption');
  var Width := Json.GetValue<integer>('Width');
  var Height:= Json.GetValue<integer>('Height');

  /// Unit.dfm
  var GUIText := 'object ' + 'U'+FormName + ': ' + 'T'+FormName+ sLineBreak +
                 '  Left = 0'  +sLineBreak +
                 '  Top =  0'   +sLineBreak +
                 '  Width = ' + Width.ToString + sLineBreak +
                 '  Height = ' + Height.ToString + sLineBreak +
                 '  Caption = ' + QuotedStr(Caption) + sLineBreak +
                 '  ClientWidth = ' + Width.ToString + sLineBreak +
                 '  ClientHeight = ' + Height.ToString + sLineBreak;

  if Json.TryGetValue('Children', Children) and (Children is TJSONArray) then
    for var I := 0 to Children.Count - 1 do
      GUIText := GUIText + GenerateComponent(Children.Items[I] as TJSONObject);

  GUIText := GUIText + 'end' + sLineBreak;
  result:= GUIText;
end;

function TDelphiGenerator.GeneratePasText(const Json: TJSONObject): string;
begin
  var Children: TJSONArray;
  var FormName := Json.GetValue<string>('Name');

  SetLength(ComponentFields, 0);
  if Json.TryGetValue('Children', Children) and (Children is TJSONArray) then
    for var I := 0 to Children.Count - 1 do
      CollectComponentFields(Children.Items[I] as TJSONObject);

  var FieldsBlock := '';
  for var F in ComponentFields do
    FieldsBlock := FieldsBlock + '    ' + F.Name + ': ' + F.CompType + ';' + sLineBreak;

  /// Unit.pas
  CodeText := 'unit ' + 'U'+FormName + ';' + sLineBreak + #13#10+

             'interface' + sLineBreak + #13#10+

             'uses  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,'+ sLineBreak +
	           '      Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.Grids, '+ sLineBreak +
             '      Data.DB, Vcl.DBGrids; '+ sLineBreak +  #13#10 + #13#10+

             'type' + sLineBreak +
             '  ' + 'T'+FormName + ' = class(TForm)' + sLineBreak +
              FieldsBlock + '  end;' + sLineBreak  + #13#10+ #13#10 +

             'var '+ #13#10 + '   '+FormName + ': ' + 'T'+FormName + ';' + sLineBreak +  #13#10+

             'implementation' + sLineBreak + #13#10+

             '{$R *.dfm} '  + sLineBreak +   #13#10+

             'end.';
  result:= CodeText;
end;

procedure TDelphiGenerator.CollectComponentFields(const CompJson: TJSONObject);
var
  Children: TJSONArray;
  CompType, CompName: string;
begin
  CompType := CompJson.GetValue<string>('Type', '');
  CompName := CompJson.GetValue<string>('Name', '');

  if (CompType <> '') and (CompName <> '') and
     (CompType <> 'TForm') and
     (not CompType.StartsWith('TJSON')) and
     (not CompType.StartsWith('TArray')) and
     (not CompType.StartsWith('TString')) then
  begin
    SetLength(ComponentFields, Length(ComponentFields) + 1);
    ComponentFields[High(ComponentFields)].Name := CompName;
    ComponentFields[High(ComponentFields)].CompType := CompType;
  end;

  if CompJson.TryGetValue('Children', Children) then
    for var I := 0 to Children.Count - 1 do
      CollectComponentFields(Children.Items[I] as TJSONObject);
end;

function TDelphiGenerator.FindFormByName(Json: TJSONObject; const AName: string): TJSONObject;
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
      FormObj := FormsArray.Items[I] as TJSONObject;
      if SameText(FormObj.GetValue<string>('Name', ''), AName) then
        Exit(FormObj);
    end;
  end;
end;

function TDelphiGenerator.GenerateCode(const Json: TJSONObject; const Indent: string = '  '): string;
begin
  SetGUIText(GenerateDfmText(Json));
  SetCodeText(GeneratePasText(Json));
  Result := FGUIText + sLineBreak + ' ; ' + sLineBreak + FCOdeText;
end;

function TDelphiGenerator.GetGUIText: string;
begin
  result:= FGUIText;
end;

function TDelphiGenerator.GetCodeText: string;
begin
  result:= FCOdeText;
end;

procedure TDelphiGenerator.SetGUIText(const Value: string);
begin
  FGUIText := Value;
end;

procedure TDelphiGenerator.SetCodeText(const Value: string);
begin
  FCOdeText := Value;
end;


end.
