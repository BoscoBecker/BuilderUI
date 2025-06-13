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

  if (CompType = '') or (CompName = '') then Exit('');

  Line := Indent + '' + CompName + ' = new ' + DelphiToWinFormsType(CompType)+ '();' + sLineBreak;

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
  Line := Line + Indent + 'Controls.Add(this.' + CompName + ');' + sLineBreak;
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

  procedure ProcessComponent(const CompJson: TJSONObject; const Indent: string);
  begin
    var Children: TJSONArray;
    var CompType := CompJson.GetValue<string>('Type', '');
    var CompName := CompJson.GetValue<string>('Name', '');
    if (CompType = '') or (CompName = '') then Exit;

    Fields := Fields + Indent + 'private ' + DelphiToWinFormsType(CompType) + ' ' + CompName + ';' + sLineBreak;
    InitCode := InitCode + Indent + CompName + ' = new ' + DelphiToWinFormsType(CompType)+ '();' + sLineBreak;

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

    InitCode := InitCode + Indent + 'Controls.Add(this.' + CompName + ');' + sLineBreak;

    if CompJson.TryGetValue('Children', Children) then
      for var I := 0 to Children.Count - 1 do
        ProcessComponent(Children.Items[I] as TJSONObject, Indent);
  end;

begin
  FormName := Json.GetValue<string>('Name', 'MyForm');
  Fields := '';
  InitCode := '';
  if Json.TryGetValue('Children', Children) and (Children is TJSONArray) then
    for var I := 0 to Children.Count - 1 do
      ProcessComponent(Children.Items[I] as TJSONObject, Indent + '    ');

  // C# WinForms  Designer.cs
  Result := 'partial class ' + FormName + sLineBreak +
  '{' + sLineBreak +
  Indent + '///  Required designer variable.' + sLineBreak + #13#10+
  Indent + 'private System.ComponentModel.IContainer components = null; '+ sLineBreak  + #13#10+
  Fields + sLineBreak +
  Indent + '    private void InitializeComponent()' + sLineBreak +
  Indent + '    {' + sLineBreak +
  InitCode +
  Indent + '    }' + sLineBreak +
  '}' ;

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
