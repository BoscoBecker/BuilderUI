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

unit Factory.CodeGenerator.Lazarus;

interface

uses
  System.Json, System.SysUtils, System.StrUtils , System.Classes,
  System.Generics.Collections,
  Dialogs, Forms, Factory.ICodeGenerator, Winapi.Windows,

  Util.JSONValidator;

type
  TComponentField = record
    Name, CompType: string;
  end;

  TLazarusGenerator = class(TInterfacedObject,ICodeGenerator)
  private
    ComponentFields: TArray<TComponentField>;
    FGUIText: string;
    FCodeText: string;
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
    property CodeText: string read FCodeText write SetCodeText;
  end;

implementation


function TLazarusGenerator.GenerateComponent(const CompJson: TJSONObject; const Indent: string = '  '): string;
begin
  var Children: TJSONArray;
  var Invalids: TArray<string>;
  var CompType := CompJson.GetValue<string>('Type', '');
  var CompName := CompJson.GetValue<string>('Name', '');

  if not Util.JSONValidator.TJSONHelper.ValidateBuilderUIPattern(CompJson.ToJSON, Invalids) then
  begin
    Result := Indent + '// Erro: invalid propertys ' + string.Join(', ', Invalids) + sLineBreak;
    Exit;
  end;

  Result := Indent + 'object ' + CompName + ': ' + CompType + sLineBreak;

  for var Pair in CompJson do
  begin
    var PropName := Pair.JsonString.Value;

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

    if ((CompType = 'TLabel') or (CompType = 'TButton') or (CompType = 'TTabSheet')) then
    begin
      if PropName = 'Text' then
        PropName := 'Caption';
      if (CompType = 'TLabel') and (PropName = 'FontSize') then
        PropName := 'Font.Size';
      if (CompType = 'TLabel') and (PropName = 'FontStyle') then
        PropName := 'Font.Style';
      if (CompType = 'TLabel') and (PropName = 'FontColor') then
        PropName := 'Font.Color';
    end;

    if (CompType = 'TEdit') then
    begin
      if PropName = 'Caption' then
        PropName := 'Text';
    end;

    if ((PropName = 'Color') or PropName.EndsWith('.Color')) and (Pair.JsonValue is TJSONString) then
    begin
      var ColorValue := Pair.JsonValue.Value;
      if ColorValue.StartsWith('#') then
      begin
        var Hex := Copy(ColorValue, 2, 6);
        if Length(Hex) = 6 then
          ColorValue := '$00' + Copy(Hex,5,2) + Copy(Hex,3,2) + Copy(Hex,1,2);
      end;
      Result := Result + Indent + '  ' + PropName + ' = ' + ColorValue + sLineBreak;
      Continue;
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

    if SameText(CompType, 'TPageControl') then
    begin
      var CompText := '  object ' + CompName + ': TPageControl' + sLineBreak;
      var Pages: TJSONArray;
      if CompJson.TryGetValue('Pages', Pages) then
      begin
        for var I := 0 to Pages.Count - 1 do
        begin
          var Page := Pages.Items[I] as TJSONObject;
          var PageName := Page.GetValue<string>('Name', 'TabSheet' + I.ToString);
          var PageCaption := Page.GetValue<string>('Caption', 'Tab ' + I.ToString);

          CompText := CompText +
            '    object ' + PageName + ': TTabSheet' + sLineBreak +
            '      Caption = ' + QuotedStr(PageCaption) + sLineBreak;

          var ChildrenTabs: TJSONArray;
          if Page.TryGetValue('Children', ChildrenTabs) then
            for var J := 0 to ChildrenTabs.Count - 1 do
              CompText := CompText + GenerateComponent(ChildrenTabs.Items[J] as TJSONObject);

          CompText := CompText + '    end' + sLineBreak;
        end;
      end;
      CompText := CompText + '  end' + sLineBreak;
      Exit(CompText);
    end;
  end;

  if CompJson.TryGetValue('Children', Children) then
    for var I := 0 to Children.Count - 1 do
      Result := Result + GenerateComponent(Children.Items[I] as TJSONObject, Indent + '  ');

  Result := Result + Indent + 'end' + sLineBreak;
end;

function TLazarusGenerator.GenerateDfmText(const Json: TJSONObject): string;
begin
  var CompText, CompType, pages, FormType: string;

  Json.TryGetValue<string>('Type',FormType);
  if FormType.Trim.Equals('') then Exit;

  var Children: TJSONArray;
  var FormName := Json.GetValue<string>('Name');
  var Caption := Json.GetValue<string>('Caption');
  var Width := Json.GetValue<integer>('Width');
  var Height:= Json.GetValue<integer>('Height');

  /// Unit.lfm
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

function TLazarusGenerator.GeneratePasText(const Json: TJSONObject): string;
begin
  var FormType: string;
  Json.TryGetValue<string>('Type',FormType);
  if FormType.Trim.Equals('') then Exit;

  var Children: TJSONArray;
  var FormName := Json.GetValue<string>('Name');

  SetLength(ComponentFields, 0);
  if Json.TryGetValue('Children', Children) and (Children is TJSONArray) then
    for var I := 0 to Children.Count - 1 do
      CollectComponentFields(Children.Items[I] as TJSONObject);

  var ChildrenPages: TJSONArray;
  if Json.TryGetValue('Pages', ChildrenPages) and (ChildrenPages is TJSONArray) then
    for var I := 0 to ChildrenPages.Count - 1 do
      CollectComponentFields(ChildrenPages.Items[I] as TJSONObject);

  var FieldsBlock := '';
  for var F in ComponentFields do
    FieldsBlock := FieldsBlock + '    ' + F.Name + ': ' + F.CompType + ';' + sLineBreak;

  /// Unit.pas
  CodeText := 'unit ' + 'U'+FormName + ';' + sLineBreak + #13#10+

              'interface' + sLineBreak + #13#10+

              'uses  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, DBGrids, DB;' + sLineBreak +  #13#10+ #13#10+

              'type' + sLineBreak +
              '  ' + 'T'+FormName + ' = class(TForm)' + sLineBreak +
               FieldsBlock + '  end;' + sLineBreak  + #13#10+ #13#10 +

              'var '+ #13#10 + '   '+FormName + ': ' + 'T'+FormName + ';' + sLineBreak +  #13#10+

              'implementation' + sLineBreak + #13#10+

              '{$R *.lfm} '  + sLineBreak +   #13#10+

              'end.';
    result:= CodeText;
end;

procedure TLazarusGenerator.CollectComponentFields(const CompJson: TJSONObject);
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

  var Pages: TJSONArray;
  var PageObj: TJSONObject;
  if CompJson.TryGetValue('Pages', Pages) and (Pages is TJSONArray) then
  for var I := 0 to Pages.Count - 1 do
  begin
    PageObj := Pages.Items[I] as TJSONObject;

    if not PageObj.TryGetValue<string>('Type',CompType) then
      PageObj.AddPair('Type', 'TTabSheet');

    CollectComponentFields(PageObj);

    if PageObj.TryGetValue('Children', Children) and (Children is TJSONArray) then
      for var J := 0 to Children.Count - 1 do
        CollectComponentFields(Children.Items[J] as TJSONObject);
  end;
end;

function TLazarusGenerator.FindFormByName(Json: TJSONObject; const AName: string): TJSONObject;
begin
  Result := nil;
  var FormObj:= Json;
  var FormsArray: TJSONArray;
  if Json.TryGetValue<TJSONArray>('Forms', FormsArray) then
  begin
    for var I := 0 to FormsArray.Count - 1 do
    begin
      FormObj := FormsArray.Items[I] as TJSONObject;
      if SameText(FormObj.GetValue<string>('Name', ''), AName) then
        Exit(FormObj);
    end;
  end else
  begin
    if SameText(FormObj.GetValue<string>('Name', ''), AName) then
      Exit(FormObj);
  end;
end;

function TLazarusGenerator.GenerateCode(const Json: TJSONObject; const Indent: string = '  '): string;
begin
  SetGUIText(GenerateDfmText(Json));
  SetCodeText(GeneratePasText(Json));
  Result := FGUIText + sLineBreak + ' ; ' + sLineBreak + FCodeText;
end;

function TLazarusGenerator.GetGUIText: string;
begin
  result:= FGUIText;
end;

function TLazarusGenerator.GetCodeText: string;
begin
  result:= FCodeText;
end;

procedure TLazarusGenerator.SetGUIText(const Value: string);
begin
  FGUIText := Value;
end;

procedure TLazarusGenerator.SetCodeText(const Value: string);
begin
  FCodeText := Value;
end;


end.
