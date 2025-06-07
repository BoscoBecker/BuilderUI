unit Builder.UIBuilderEngine;

interface

uses
  System.JSON, System.Classes,
  System.Types, System.SysUtils, System.StrUtils,
  System.Generics.Collections,
  Forms,

  Vcl.Controls, Vcl.StdCtrls,Vcl.DBGrids, Vcl.Grids, Vcl.ComCtrls,
  Vcl.ExtCtrls, Windows, Messages, Vcl.Buttons, Dialogs,

  Core.IUIBuilder, Adapter.TreeViewAdapter, Vcl.Graphics;

type
  TUIBuilderEngine = class(TInterfacedObject, IUIBuilder)
  public
    function CreateFormFromJson(AOwner: TComponent; Json: TJSONObject): TForm;
    function CreateControlFromJson(AOwner: TComponent; AParent: TWinControl; Json: TJSONObject): TControl;
    procedure ControlClick(Sender: TObject);
    procedure ControlDblClickImage(Sender: TObject);
    procedure SetControlEvent(Ctrl: TControl);
    procedure SetControlCaption(Ctrl: TControl; const Text: string);
    procedure SetControlWidth(Ctrl: TControl; const Text: string);
    procedure SetControlHeight(Ctrl: TControl; const Text: string);
    function SetControlColor(const Text: string): TColor;
    function SetControlType(const AOwner: TComponent;Ctrl: TControl; const CtrlType: string):TControl;
    function SetControlAling(const Text: string): TAlign;
    function SetControlBevelCut(const Text: string): TBevelCut;
    function SetControlBorderStyle(const Text: string): TBorderStyle;
    destructor Destroy; override;
  end;

implementation

procedure TUIBuilderEngine.ControlClick(Sender: TObject);

begin
//  if Length(TControl(Sender).Name) <=0 then Exit;
//  try
//    if TTreeViewAdapter.FTreeView.CanFocus then
//      TTreeViewAdapter.FTreeView.SetFocus;
//    TTreeViewAdapter.FindComponentInTreeView(TControl(Sender).Name);
//    TTreeViewAdapter.FTreeView.OnClick(Sender);
//  except
//  end;
end;

procedure TUIBuilderEngine.ControlDblClickImage(Sender: TObject);
begin
  var Open := TOpenDialog.Create(nil);
  try
    Open.Filter:= 'Images (*.PNG)|*.JPG|*.JPEG';
    if Open.Execute(Application.Handle) then
      if not String(open.FileName).trim.Equals('') then
        TImage(Sender).Picture.LoadFromFile(open.FileName);
  finally
    Open.Free;
  end;
end;

function TUIBuilderEngine.CreateControlFromJson(AOwner: TComponent; AParent: TWinControl; Json: TJSONObject): TControl;
var
  CtrlType : string;
  Ctrl: TControl;
  PositionObj: TJSONObject;
  ChildrenArr: TJSONArray;
  ChildJson: TJSONObject;
  FontStyles: TJSONArray;
  Style: TFontStyles;
  ColorStr,CaptionStr: string;
begin
  Result := nil;
  Ctrl:= nil;
  if Trim(Json.GetValue<string>('Type', '')).Equals('') then Exit;

  CtrlType := Json.GetValue<string>('Type');
  Ctrl:= SetControlType(AOwner, Ctrl, Json.GetValue<string>('Type', ''));
  Ctrl.Name := Json.GetValue<string>('Name', '');
  Ctrl.Visible:= False;

  Ctrl.Parent := AParent;
  SetControlEvent(Ctrl);
  var CaptionOrText := Json.GetValue<string>('Caption', '');
  if CaptionOrText = '' then
    CaptionOrText := Json.GetValue<string>('Text', '');
  SetControlCaption(Ctrl, CaptionOrText);
  SetControlWidth(Ctrl,Json.GetValue<string>('Width','100'));
  SetControlHeight(Ctrl,Json.GetValue<string>('Height', '50'));

   if Json.TryGetValue<TJSONObject>('Position', PositionObj) then
   begin
     Ctrl.Left := Round(PositionObj.GetValue<Single>('X', -100));
     Ctrl.Top := Round(PositionObj.GetValue<Single>('Y', -100));
   end;

   if Ctrl is TPanel then
   begin
     TPanel(Ctrl).Align:= SetControlAling(Json.GetValue<string>('Align', 'alNone'));
     TPanel(Ctrl).BevelOuter:= SetControlBevelCut(Json.GetValue<string>('BevelOuter', 'bvRaised'));
     TPanel(Ctrl).BorderStyle:= SetControlBorderStyle(Json.GetValue<string>('BorderStyle', 'bsNone'));
     TPanel(Ctrl).Color:= SetControlColor(Json.GetValue<string>('Color', ColorStr));
   end;

   var items:= Json.GetValue('Items');
   if (items <> nil) and (items is TJSONArray) then
   begin
     if Ctrl is TComboBox then
     begin
       var ItemsArray := TJSONArray(items);
       try
         for var ItemValue in ItemsArray do
           TComboBox(Ctrl).Items.Add(ItemValue.Value);
       finally
         ItemsArray:= Nil;
       end;
     end;

     if Ctrl is TListBox then
     begin
       var itemsArray := TJSONArray(items);
       try
         TListBox(Ctrl).items.Clear;
         for var ItemValue in ItemsArray do
           TListBox(Ctrl).Items.Add(ItemValue.Value);
       finally
         itemsArray := Nil;
       end;
     end;
   end;

   var lines:= Json.GetValue('Lines');
   if (lines <> nil) and (lines is TJSONArray) then
   begin
     if Ctrl is TMemo then
     begin
       var linesArray := TJSONArray(lines);
       TMemo(Ctrl).lines.Clear;
       try
         for var ItemValue in linesArray do
           TMemo(Ctrl).Lines.Add(ItemValue.Value);
       finally
         linesArray:= nil;
       end;
     end;
   end;

  if (Ctrl is TLabel) and Json.TryGetValue<TJSONArray>('FontStyle', FontStyles) then
  begin
    Style := [];
    for var i := 0 to FontStyles.Count - 1 do
    begin
      var StyleStr := FontStyles.Items[i].Value;
      if SameText(StyleStr, 'Bold') then Include(Style, fsBold)
      else if SameText(StyleStr, 'Italic') then Include(Style, fsItalic)
      else if SameText(StyleStr, 'Underline') then Include(Style, fsUnderline)
      else if SameText(StyleStr, 'StrikeOut') then Include(Style, fsStrikeOut);
    end;
    TLabel(Ctrl).Font.Style := Style;
  end;

  if (Ctrl is TPageControl) and Json.TryGetValue<TJSONArray>('Pages', ChildrenArr) then
  begin
    for var I := 0 to ChildrenArr.Count - 1 do
    begin
      ChildJson := ChildrenArr.Items[I] as TJSONObject;
      var Tab := TTabSheet.Create(AOwner);
      Tab.PageControl := TPageControl(Ctrl);
      Tab.Caption := ChildJson.GetValue<string>('Caption', ' No Title ');
      Tab.Name := Ctrl.Name;
      Tab.Parent := TPageControl(Ctrl);

      var TabChildren: TJSONArray;
      if ChildJson.TryGetValue<TJSONArray>('Children', TabChildren) then
      begin
        for var J := 0 to TabChildren.Count - 1 do
          CreateControlFromJson(AOwner, Tab, TabChildren.Items[J] as TJSONObject);
      end;
    end;
  end;


  if Json.TryGetValue<TJSONArray>('Children', ChildrenArr) or Json.TryGetValue<TJSONArray>('children', ChildrenArr) then
  begin
    for var I := 0 to ChildrenArr.Count - 1 do
    begin
      ChildJson := ChildrenArr.Items[I] as TJSONObject;
      CreateControlFromJson(AOwner, TWinControl(Ctrl), ChildJson);
    end;
  end;
  Ctrl.Visible:= True;
  Result := Ctrl;
end;

function TUIBuilderEngine.CreateFormFromJson(AOwner: TComponent;   Json: TJSONObject): TForm;
var
  Form: TForm;
  ControlsArray: TJSONArray;
  ControlJson: TJSONObject;
begin
  Form:= TForm.Create(AOwner);
  Form.Position:= poDefault;
  Form.FormStyle:= fsMDIChild;
  Form.BorderIcons := Form.BorderIcons - [biMaximize];
  Form.BorderStyle:= bsSizeable;
  Form.Name:= Json.GetValue<string>('Name');
  Form.Caption:= Json.GetValue<string>('Caption','');
  Form.Height:= Json.GetValue<integer>('Height',650);
  Form.Width:= Json.GetValue<integer>('Width',500);
  Form.OnClick:= ControlClick;
  if Json.TryGetValue<TJSONArray>('Children', ControlsArray) then
  begin
    for var newarray in ControlsArray do
    begin
      ControlJson := newarray as TJSONObject;
      CreateControlFromJson(Form, Form, ControlJson);
    end;
  end;
  Form.Show;
  Form.BringToFront;
  Result:= Form;
end;


destructor TUIBuilderEngine.Destroy;
begin
  inherited;
end;

procedure TUIBuilderEngine.SetControlEvent(Ctrl: TControl);
begin
  if Ctrl is TForm then
    TForm(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TLabel then
    TLabel(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TButton then
    TButton(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TBitBtn then
    TBitBtn(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TSpeedButton then
    TSpeedButton(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TEdit then
    TEdit(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TPanel then
    TPanel(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TComboBox then
    TComboBox(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TCheckBox then
    TCheckBox(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TStringGrid then
    TStringGrid(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TDBGrid then
    TDBGrid(Ctrl).OnEnter:= ControlClick
  else
  if Ctrl is TMemo then
    TMemo(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TListBox then
    TListBox(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TDateTimePicker then
    TDateTimePicker(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TRadioGroup then
    TRadioGroup(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TCheckBox then
    TCheckBox(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TRadioButton then
    TRadioButton(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TGroupBox then
    TGroupBox(Ctrl).OnClick := ControlClick
  else
  if Ctrl is TShape then
    TShape(Ctrl).OnMouseEnter := ControlClick
  else
  if Ctrl is TImage then
    TImage(Ctrl).OnDblClick := ControlDblClickImage
end;


function TUIBuilderEngine.SetControlAling(const Text: string): TAlign;
begin
  result:= alNone;
  if Text = 'alTop' then
    result:= alTop
  else if Text = 'alBottom' then
    result := alBottom
  else if Text = 'alLeft' then
    result := alLeft
  else if Text = 'alRight' then
    result := alRight
  else if Text = 'alClient' then
    result := alClient
  else if Text = 'alNone' then
    result := alNone;
end;

function TUIBuilderEngine.SetControlBevelCut(const Text: string): TBevelCut;
begin
  result:= bvRaised;
  if Text = 'bvNone' then
    result := bvNone
  else if Text = 'bvLowered' then
    result := bvLowered
  else if Text = 'bvRaised' then
    result := bvRaised
  else if Text = 'bvSpace' then
    result := bvSpace;
end;

function TUIBuilderEngine.SetControlBorderStyle(const Text: string): TBorderStyle;
begin
  result:= bsNone;
  if not Text.Trim.Equals('bsNone') then
  result:= bsSingle;
end;

procedure TUIBuilderEngine.SetControlCaption(Ctrl: TControl; const Text: string);
begin
  if Trim(Text) = '' then Exit;

  if Ctrl is TLabel then
    TLabel(Ctrl).Caption := Text
  else if Ctrl is TButton then
    TButton(Ctrl).Caption := Text
  else if Ctrl is TBitBtn then
    TBitBtn(Ctrl).Caption := Text
  else if Ctrl is TSpeedButton then
    TSpeedButton(Ctrl).Caption := Text
  else if Ctrl is TRadioButton then
    TRadioButton(Ctrl).Caption := Text
  else if Ctrl is TCheckBox then
    TCheckBox(Ctrl).Caption := Text
  else if Ctrl is TRadioGroup then
    TRadioGroup(Ctrl).Caption := Text
  else if Ctrl is TPanel then
    TPanel(Ctrl).Caption := Text
  else if Ctrl is TGroupBox then
    TGroupBox(Ctrl).Caption := Text
  else if Ctrl is TEdit then
    TEdit(Ctrl).Text := Text
  else if Ctrl is TMemo then
    TMemo(Ctrl).Text := Text;
end;

function TUIBuilderEngine.SetControlColor(const Text: string): TColor;
var
  ColorStr: string;
  ColorValue: integer;
begin
  Result := clNone;
  ColorStr := Trim(Text);
  if ColorStr = '' then Exit;

  if ColorStr[1] = '#' then
  begin
    // #RRGGBB
    if Length(ColorStr) = 7 then
    begin
      ColorValue := RGB(
        StrToInt('$' + Copy(ColorStr, 2, 2)),
        StrToInt('$' + Copy(ColorStr, 4, 2)),
        StrToInt('$' + Copy(ColorStr, 6, 2))
      );
      Result := ColorValue;
      Exit;
    end
    // #RGB
    else if Length(ColorStr) = 4 then
    begin
      ColorValue := RGB(
        StrToInt('$' + StringOfChar(ColorStr[2], 2)),
        StrToInt('$' + StringOfChar(ColorStr[3], 2)),
        StrToInt('$' + StringOfChar(ColorStr[4], 2))
      );
      Result := ColorValue;
      Exit;
    end;
  end
  // Delphi color name (clRed, clBlue, etc.)
  else if SameText(Copy(ColorStr, 1, 2), 'cl') then
  begin
    Result := StringToColor(ColorStr);
    Exit;
  end
  // decimal (0-16777215)
  else if TryStrToInt(ColorStr, ColorValue) then
  begin
    Result := ColorValue;
    Exit;
  end;
end;

function TUIBuilderEngine.SetControlType(const AOwner: TComponent; Ctrl: TControl; const CtrlType: string) : TControl;
begin
  if CtrlType = 'TLabel' then
    Ctrl := TLabel.Create(AOwner)
  else if CtrlType = 'TButton' then
    Ctrl := TButton.Create(AOwner)
  else if CtrlType = 'TBitBtn' then
    Ctrl := TBitBtn.Create(AOwner)
  else if CtrlType = 'TSpeedButton' then
    Ctrl := TSpeedButton.Create(AOwner)
  else if CtrlType = 'TEdit' then
    Ctrl := TEdit.Create(AOwner)
  else if CtrlType = 'TPanel' then
    Ctrl := TPanel.Create(AOwner)
  else if CtrlType = 'TShape' then
    Ctrl := TShape.Create(AOwner)
  else if CtrlType = 'TComboBox' then
    Ctrl := TComboBox.Create(AOwner)
  else if CtrlType = 'TCheckBox' then
    Ctrl := TCheckBox.Create(AOwner)
  else if CtrlType = 'TStringGrid' then
    Ctrl := TStringGrid.Create(AOwner)
  else if CtrlType = 'TDBGrid' then
    Ctrl := TDBGrid.Create(AOwner)
  else if CtrlType = 'TMemo' then
    Ctrl := TMemo.Create(AOwner)
  else if CtrlType = 'TListBox' then
    Ctrl := TListBox.Create(AOwner)
  else if CtrlType = 'TDateTimePicker' then
    Ctrl := TDateTimePicker.Create(AOwner)
  else if CtrlType = 'TRadioGroup' then
    Ctrl := TRadioGroup.Create(AOwner)
  else if CtrlType = 'TCheckBox' then
    Ctrl := TCheckBox.Create(AOwner)
  else if CtrlType = 'TRadioButton' then
    Ctrl := TRadioButton.Create(AOwner)
  else if CtrlType = 'TGroupBox' then
    Ctrl := TGroupBox.Create(AOwner)
  else if CtrlType = 'TPageControl' then
    Ctrl := TPageControl.Create(AOwner)
  else if CtrlType = 'TTabSheet' then
    Ctrl := TTabSheet.Create(AOwner)
  else if CtrlType = 'TImage' then
    Ctrl := TImage.Create(AOwner);
  Result:= Ctrl;
end;

procedure TUIBuilderEngine.SetControlWidth(Ctrl: TControl; const Text: string);
begin
  Ctrl.Width := StrToIntDef(Text,0);
end;

procedure TUIBuilderEngine.SetControlHeight(Ctrl: TControl; const Text: string);
begin
  Ctrl.Height:= StrToIntDef(Text,0);

end;


end.

