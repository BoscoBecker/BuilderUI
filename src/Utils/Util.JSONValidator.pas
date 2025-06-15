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

unit Util.JSONValidator;

interface

uses System.Json, System.Generics.Collections,System.SysUtils;

type
  TJSONHelper = class
    public class function ValidateBuilderUIPattern(const AJSON: string; out InvalidProps: TArray<string>): Boolean; static;
    public class function ValidateJSON(const AJSON: string; out ErrorMessage: string): Boolean; static;
  end;

implementation

class function TJSONHelper.ValidateBuilderUIPattern(const AJSON: string; out InvalidProps: TArray<string>): Boolean;
type
  TPropSet = TDictionary<string, Boolean>;
var
  AllowedProps: TDictionary<string, TPropSet>;
  InvalidList: TList<string>;

  procedure AddProps(const CompType: string; const Props: array of string);
  var
    SetProps: TPropSet;
    Prop: string;
  begin
    SetProps := TPropSet.Create;
    for Prop in Props do
      SetProps.Add(Prop, True);
    AllowedProps.Add(CompType, SetProps);
  end;

  procedure CheckObject(Obj: TJSONObject; const ParentType: string);
  var
    CompType: string;
    Allowed: TPropSet;
    Pair: TJSONPair;
    Key: string;
  begin
    if not Obj.TryGetValue<string>('Type', CompType) then
      CompType := ParentType; // fallback for nested objects

    if not AllowedProps.TryGetValue(CompType, Allowed) then
      Allowed := nil;

    for Pair in Obj do 
    begin
      Key := Pair.JsonString.Value;
      if (Allowed = nil) or (not Allowed.ContainsKey(Key)) then
        InvalidList.Add(Format('BuilderUI - Invalid property "%s" in component type "%s"', [Key, CompType]));

      // Recursively check children/pages/tabs if present
      if (Key = 'Children') or (Key = 'Pages') or (Key = 'Tabs') then
      begin
        if Pair.JsonValue is TJSONArray then
          for var Item in TJSONArray(Pair.JsonValue) do
            if Item is TJSONObject then
              CheckObject(TJSONObject(Item), CompType); 
      end;
    end;
  end;

begin
  Result := True;
  SetLength(InvalidProps, 0);
  InvalidList := TList<string>.Create;
  AllowedProps := TDictionary<string, TPropSet>.Create;

  var FormArr: TJSONArray;
  var Root := TJSONObject.ParseJSONValue(AJSON);
  try
    // Define allowed properties for each component type
    AddProps('TForm',    ['Type','Name','Caption','Width','Height','Align','Children','BorderStyle','Position']);
    AddProps('TPanel',   ['Type','Name','Left','Top','Width','Height','Color','Caption','Align','Children','Position']);
    AddProps('TLabel',   ['Type','Name','Caption','Text','Position','Width','Height','FontSize','FontStyle','FontColor','WordWrap']);
    AddProps('TEdit',    ['Type','Name','Text','Caption','Position','Width','Height','ReadOnly','PasswordChar','FontStyle']);
    AddProps('TButton',  ['Type','Name','Caption','Position','Width','Height']);
    AddProps('TSpeedButton', ['Type','Name','Caption','Position','Width','Height']);
    AddProps('TComboBox',['Type','Name','Items','Position','Width','Height','Lines','Text']);
    AddProps('TListBox',['Type','Name','Items','Position','Width','Height','Lines','Text']);
    AddProps('TDateTimePicker',['Type','Name','Position','Width','Height']);
    AddProps('TPageControl',['Type','Name','Align','Width','Height','Pages','Tabs','Position','Children','Caption']);
    AddProps('TTabSheet',['Type','Name','Caption','Children']);
    AddProps('TImage',    ['Type','Name','Width','Height','Align','Position']);
    AddProps('TStringGrid',    ['Type','Name','Width','Height','Position','ColCount','RowCount']);
    AddProps('TDBGrid',    ['Type','Name','Width','Height','Position']);
    AddProps('TCheckBox',  ['Type','Name','Caption','Position','Width','Height']);
    AddProps('TRadioButton',  ['Type','Name','Caption','Position','Width','Height']);
    AddProps('TShape',  ['Type','Name','Caption','Position','Width','Height','Shape']);
    AddProps('TBitBtn',  ['Type','Name','Caption','Position','Width','Height','Kind']);
    AddProps('TGroupBox',  ['Type','Name','Caption','Position','Width','Height']);
    AddProps('TRadioGroup',  ['Type','Name','Caption','Position','Width','Height','Items']);
    AddProps('TMemo',  ['Type','Name','Caption','Position','Width','Height','Lines','Text']);



    if not Assigned(Root) then Exit;
    if Root is TJSONObject then
    begin
      if TJSONObject(Root).TryGetValue<TJSONArray>('Forms', FormArr) then
      begin
        for var Item in FormArr do
          if Item is TJSONObject then
            CheckObject(TJSONObject(Item), '');
      end
      else
        CheckObject(TJSONObject(Root), '');
    end;

    if InvalidList.Count > 0 then
    begin
      InvalidProps := InvalidList.ToArray;
      Result := False;
    end;
  finally
    for var SetProps in AllowedProps.Values do
      SetProps.Free;
    AllowedProps.Free;
    InvalidList.Free;
    Root.Free;
  end;
end;

class function TJSONHelper.ValidateJSON(const AJSON: string; out ErrorMessage: string): Boolean;
var
  JSONValue: TJSONValue;

  function IsIntegerValue(const Value: TJSONValue): Boolean;
  var
    Dummy: Integer;
  begin
    Result := Value is TJSONNumber;
    if not Result and (Value is TJSONString) then
      Result := TryStrToInt(Value.Value, Dummy);
  end;

  function CheckComponentTypes(Obj: TJSONObject): Boolean;
  var
    Pair: TJSONPair;
    CompType: string;
  begin
    Result := True;
    if not Obj.TryGetValue<string>('Type', CompType) then
      CompType := '';

    for Pair in Obj do
    begin
      if (Pair.JsonString.Value = 'Width') or (Pair.JsonString.Value = 'Height') then
      begin
        if not IsIntegerValue(Pair.JsonValue) then
        begin
          ErrorMessage := Format('Property "%s" in component "%s" must be integer, but got "%s"', [
            Pair.JsonString.Value, Obj.GetValue<string>('Name', ''), Pair.JsonValue.Value]);
          Exit(False);
        end;
      end;
      // Recursivo para Children
      if (Pair.JsonString.Value = 'Children') and (Pair.JsonValue is TJSONArray) then
        for var Item in TJSONArray(Pair.JsonValue) do
          if (Item is TJSONObject) and not CheckComponentTypes(TJSONObject(Item)) then
            Exit(False);
    end;
  end;

begin
  ErrorMessage := '';
  Result := False;

  if AJSON.Trim.IsEmpty then
  begin
    ErrorMessage := 'Empty JSON string';
    Exit;
  end;

  JSONValue := TJSONObject.ParseJSONValue(AJSON);
  try
    if not Assigned(JSONValue) then
    begin
      ErrorMessage := 'Invalid JSON format';
      Exit;
    end;

    if JSONValue is TJSONObject then
    begin
      if not CheckComponentTypes(TJSONObject(JSONValue)) then
        Exit;
    end;

    Result := True;
  finally
    JSONValue.Free;
  end;
end;

end.
