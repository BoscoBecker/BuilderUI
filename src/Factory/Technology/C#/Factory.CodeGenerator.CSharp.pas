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
    function DelphiToWinFormsType(const DelphiType: string): string;
  public
    function GenerateCode(const Json: TJSONObject; const Indent: string = '    '): string;
    function GenerateDesignerCode(const Json: TJSONObject; const Indent: string = '    '): string;
    function FindFormByName(Json: TJSONObject; const AName: string): TJSONObject;
    function GetGUIText: string;
    function GetCodeText: string;
    property GUIText: string read GetGUIText write SetGUIText;
    property CodeText: string read GetCodeText write SetCodeText;
  end;

implementation


function TCsharp.FindFormByName(Json: TJSONObject; const AName: string): TJSONObject;
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

function TCsharp.GenerateComponent(const CompJson: TJSONObject; const Indent: string): string;
var
  CompType, CompName, PropName, Line: string;
  Children, Pages, ChildrenTabs: TJSONArray;
  I, J: Integer;
begin
  CompType := CompJson.GetValue<string>('Type', '');
  CompName := CompJson.GetValue<string>('Name', '');
  Line := '';

  if (CompType = '') or (CompName = '') then  Exit('');

  if SameText(CompType, 'TPageControl') then
  begin

    var PosObj := CompJson.GetValue<TJSONObject>('Position', nil);
    if Assigned(PosObj) then
    begin
      var X := PosObj.GetValue<Integer>('X', 0);
      var Y := PosObj.GetValue<Integer>('Y', 0);
      Line := Line + Indent + 'this.' + CompName + '.Location = new Point(' + X.ToString + ', ' + Y.ToString + ');' + sLineBreak;
    end;

    var Width := CompJson.GetValue<Integer>('Width', 500);
    var Height := CompJson.GetValue<Integer>('Height',550);
    Line := Line + Indent + 'this.' + CompName + '.Size = new Size(' + Width.ToString + ', ' + Height.ToString + ');' + sLineBreak;

    if CompJson.TryGetValue('Pages', Pages) then
    begin
      for I := 0 to Pages.Count - 1 do
      begin
        var Page := Pages.Items[I] as TJSONObject;
        var PageName := Page.GetValue<string>('Name', CompName + '_Tab' + I.ToString);
        var PageCaption := Page.GetValue<string>('Caption', 'Tab ' + I.ToString);

        Line := Line + Indent + 'var ' + PageName + ' = new TabPage("' + PageCaption + '");' + sLineBreak;
        Line := Line + Indent + 'this.' + CompName + '.TabPages.Add(' + PageName + ');' + sLineBreak;

        if Page.TryGetValue('Children', ChildrenTabs) then
        begin
          for J := 0 to ChildrenTabs.Count - 1 do
          begin
            var ChildJson := ChildrenTabs.Items[J] as TJSONObject;
            var ChildCode := GenerateComponent(ChildJson, Indent + '  ');
            var ChildName := ChildJson.GetValue<string>('Name', '');
            Line := Line + ChildCode;
            Line := Line + Indent + '  ' + PageName + '.Controls.Add(this.' + ChildName + ');' + sLineBreak;
          end;
        end;
      end;
    end;

    Line := Line + Indent + 'this.Controls.Add(this.' + CompName + ');' + sLineBreak;
    Exit(Line);
  end;

  Line := Indent + 'this.' + CompName + ' = new ' + DelphiToWinFormsType(CompType) + '();' + sLineBreak;

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
  end;

  Line := Line + Indent + 'this.Controls.Add(this.' + CompName + ');' + sLineBreak;
  if CompJson.TryGetValue('Children', Children) then
    for I := 0 to Children.Count - 1 do
      Line := Line + GenerateComponent(Children.Items[I] as TJSONObject, Indent);

  Result := Line;
end;

function TCsharp.GenerateCode(const Json: TJSONObject; const Indent: string): string;
begin
  var FormName := Json.GetValue<string>('Name', 'MyForm');
  var Caption := Json.GetValue<string>('Caption', FormName);
  var Width := Json.GetValue<Integer>('Width', 800);
  var Height := Json.GetValue<Integer>('Height', 600);
  var Children: TJSONArray;
  var InitComponents: string;

  if Json.TryGetValue('Children', Children) then
  begin
    for var I := 0 to Children.Count - 1 do
    begin
      var CompJson := Children.Items[I] as TJSONObject;
      InitComponents := InitComponents + GenerateComponent(CompJson, Indent + '    ');
    end;
  end;

  // C# WinForms  .cs
  var CodeText :=
    'public partial class ' + FormName + ' : Form' + sLineBreak +
    '{' + sLineBreak +
    Indent + 'public ' + FormName + '()' + sLineBreak +
    Indent + '{' + sLineBreak +
    Indent + '    InitializeComponent();' + sLineBreak +
    Indent + '    this.Text = "' + Caption + '";' + sLineBreak +
    Indent + '    this.Width = ' + Width.ToString + ';' + sLineBreak +
    Indent + '    this.Height = ' + Height.ToString + ';' + sLineBreak +
    Indent + '}' + sLineBreak +
    '}';
  SetCodeText(CodeText);

  Result := FGUIText;
end;

function TCsharp.GenerateDesignerCode(const Json: TJSONObject; const Indent: string): string;
var
  FormName: string;
  Children: TJSONArray;
  Fields, InitCode: string;
  FormWidth, FormHeight: Integer;

  procedure ProcessComponent(const CompJson: TJSONObject; const Indent, ParentName: string);
  begin
    var Children: TJSONArray;
    var CompType := CompJson.GetValue<string>('Type', '');
    var CompName := CompJson.GetValue<string>('Name', '');
    if (CompType = '') or (CompName = '') then Exit;

    Fields := Fields + Indent + 'private ' + DelphiToWinFormsType(CompType) + ' ' + CompName + ';' + sLineBreak;
    InitCode := InitCode + Indent + 'this.' + CompName + ' = new ' + DelphiToWinFormsType(CompType) + '();' + sLineBreak;

    for var Pair in CompJson do
    begin
      var PropName := Pair.JsonString.Value;
      if (PropName = 'Type') or (PropName = 'Name') or (PropName = 'Children') then Continue;

      if (PropName = 'Position') and (Pair.JsonValue is TJSONObject) then
      begin
        var PosObj := Pair.JsonValue as TJSONObject;
        var X := PosObj.GetValue<Integer>('X', 0);
        var Y := PosObj.GetValue<Integer>('Y', 0);
        InitCode := InitCode + Indent + 'this.' + CompName + '.Location = new System.Drawing.Point(' + X.ToString + ', ' + Y.ToString + ');' + sLineBreak;
        Continue;
      end;

      if (PropName = 'Width') or (PropName = 'Height') then
      begin
        InitCode := InitCode + Indent + 'this.' + CompName + '.' + PropName + ' = ' + Pair.JsonValue.Value + ';' + sLineBreak;
        Continue;
      end;

      if (PropName = 'Caption') or (PropName = 'Text') then
      begin
        InitCode := InitCode + Indent + 'this.' + CompName + '.Text = "' + Pair.JsonValue.Value + '";' + sLineBreak;
        Continue;
      end;
    end;

    if SameText(CompType, 'TPageControl') and CompJson.TryGetValue('Pages', Children) then
    begin
      for var I := 0 to Children.Count - 1 do
      begin
        var Page := Children.Items[I] as TJSONObject;
        var PageName := Page.GetValue<string>('Name', CompName + '_Tab' + I.ToString);
        var PageCaption := Page.GetValue<string>('Caption', 'Tab ' + I.ToString);

        Fields := Fields +  'private TabPage ' + PageName + ';' + sLineBreak;
        InitCode := InitCode +  'this.' + PageName + ' = new TabPage("' + PageCaption + '");' + sLineBreak;
        InitCode := InitCode +  'this.' + CompName + '.TabPages.Add(this.' + PageName + ');' + sLineBreak;

        var TabChildren: TJSONArray;
        if Page.TryGetValue('Children', TabChildren) then
        begin
          for var J := 0 to TabChildren.Count - 1 do
          begin
            var Child := TabChildren.Items[J] as TJSONObject;
            ProcessComponent(Child, Indent + '    ', 'this.' + PageName);
          end;
        end;
      end;
    end;

    if not SameText(CompType, 'TTabSheet') then
      InitCode := InitCode + Indent + ParentName + '.Controls.Add(this.' + CompName + ');' + sLineBreak;

    if CompJson.TryGetValue('Children', Children) then
    begin
      for var I := 0 to Children.Count - 1 do
      begin
        var Child := Children.Items[I] as TJSONObject;
        ProcessComponent(Child, Indent + '    ', 'this.' + CompName);
      end;
    end;
  end;

begin
  FormName := Json.GetValue<string>('Name', 'MyForm');
  FormWidth := Json.GetValue<Integer>('Width', 800);
  FormHeight := Json.GetValue<Integer>('Height', 600);

  Fields := '';
  InitCode := '';
  if Json.TryGetValue('Children', Children) and (Children is TJSONArray) then
  begin
    for var I := 0 to Children.Count - 1 do
      ProcessComponent(Children.Items[I] as TJSONObject, Indent + '    ', 'this');
  end;

  Result :=
    'partial class ' + FormName + sLineBreak +
    '{' + sLineBreak +
    Indent + '    /// <summary>' + sLineBreak +
    Indent + '    /// Required designer variable.' + sLineBreak +
    Indent + '    /// </summary>' + sLineBreak +
    Indent + '    private System.ComponentModel.IContainer components = null;' + sLineBreak + sLineBreak +
    Fields + sLineBreak +
    Indent + '    private void InitializeComponent()' + sLineBreak +
    Indent + '    {' + sLineBreak +
             'this.ClientSize = new System.Drawing.Size(' + FormWidth.ToString + ', ' + FormHeight.ToString + ');' + sLineBreak +
    InitCode +
    Indent + '    }' + sLineBreak +
    '}';

  SetGUIText(Result);
end;

function TCsharp.DelphiToWinFormsType(const DelphiType: string): string;
begin
  if SameText(DelphiType, 'TEdit') then Exit('TextBox');
  if SameText(DelphiType, 'TImage') then Exit('PictureBox');
  if SameText(DelphiType, 'TLabel') then Exit('Label');
  if SameText(DelphiType, 'TButton') then Exit('Button');
  if SameText(DelphiType, 'TPanel') then Exit('Panel');
  if SameText(DelphiType, 'TComboBox') then Exit('ComboBox');
  if SameText(DelphiType, 'TCheckBox') then Exit('CheckBox');
  if SameText(DelphiType, 'TListBox') then Exit('ListBox');
  if SameText(DelphiType, 'TDateTimePicker') then Exit('DateTimePicker');
  if SameText(DelphiType, 'TBitBtn') then Exit('Button');
  if SameText(DelphiType, 'TSpeedButton') then Exit('Button');
  if SameText(DelphiType, 'TMemo') then Exit('TextBox');
  if SameText(DelphiType, 'TDBGrid') then Exit('DataGridView');
  if SameText(DelphiType, 'TStringGrid') then Exit('DataGridView');
  if SameText(DelphiType, 'TGroupBox') then Exit('GroupBox');
  if SameText(DelphiType, 'TRadioGroup') then Exit('RadioButton');
  if SameText(DelphiType, 'TabSheet') then Exit('TabPage');
  if SameText(DelphiType, 'TPageControl') then Exit('TabControl');

  Result := DelphiType;
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
