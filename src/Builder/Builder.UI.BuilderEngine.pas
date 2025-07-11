﻿{}
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

unit Builder.UI.BuilderEngine;

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
  private
    procedure ControlClick(Sender: TObject);
    procedure ControlDblClickImage(Sender: TObject);
    procedure SetControlEvent(Ctrl: TControl);
    procedure SetControlCaption(Ctrl: TControl; const Text: string);
    procedure SetControlWidth(Ctrl: TControl; const Text: string);
    procedure SetControlHeight(Ctrl: TControl; const Text: string);
    function CreateControlFromJson(AOwner: TComponent; AParent: TWinControl; Json: TJSONObject): TControl;
    function SetControlColor(const Text: string): TColor;
    function SetControlType(const AOwner: TComponent;Ctrl: TControl; const CtrlType: string):TControl;
    function SetControlAling(const Text: string): TAlign;
    function SetControlBevelCut(const Text: string): TBevelCut;
    function SetControlBorderStyle(const Text: string): TBorderStyle;
  public
    destructor Destroy; override;
    function CreateFormFromJson(AOwner: TComponent; Json: TJSONObject): TForm;
  end;

implementation

procedure TUIBuilderEngine.ControlClick(Sender: TObject);

begin
  if Length(TControl(Sender).Name) <=0 then Exit;
  if TTreeViewAdapter.FTreeView.CanFocus then
    TTreeViewAdapter.FTreeView.SetFocus;
  TTreeViewAdapter.FindComponentInTreeView(TControl(Sender).Name);
  TTreeViewAdapter.FindRootFormNode(TControl(Sender).Name);
  TTreeViewAdapter.FTreeView.OnClick(Sender);
end;

procedure TUIBuilderEngine.ControlDblClickImage(Sender: TObject);
begin
  var Open := TOpenDialog.Create(nil);
  try
    Open.Filter:= 'Images (*.png)| *.png';
    if Open.Execute(Application.Handle) then
      if not String(open.FileName).trim.Equals('') then
        TImage(Sender).Picture.LoadFromFile(open.FileName);
  finally
    Open.Free;
  end;
end;

function TUIBuilderEngine.CreateControlFromJson(AOwner: TComponent; AParent: TWinControl; Json: TJSONObject): TControl;
var
  Ctrl: TControl;
  PositionObj: TJSONObject;
  ChildrenArr: TJSONArray;
  ChildJson: TJSONObject;
  FontStyles: TJSONArray;
  Style: TFontStyles;
  ColorStr: string;
begin
  Result := nil;
  Ctrl:= nil;
  if Trim(Json.GetValue<string>('Type', '')).Equals('') then Exit;
  var CtrlType := Json.GetValue<string>('Type');
  Ctrl:= SetControlType(AOwner, Ctrl, Json.GetValue<string>('Type', ''));
  Ctrl.Name := Json.GetValue<string>('Name','');
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


  if Ctrl is TImage then
  begin
    TImage(Ctrl).Center:= True;
    TImage(Ctrl).BringToFront;
    TImage(Ctrl).ShowHint:= true;
    TImage(Ctrl).Hint:= 'Double-click to load Image';
  end;

  if Ctrl is TPanel then
  begin
    TPanel(Ctrl).Align:= SetControlAling(Json.GetValue<string>('Align', 'alNone'));
    TPanel(Ctrl).BevelOuter:= SetControlBevelCut(Json.GetValue<string>('BevelOuter', 'bvRaised'));
    TPanel(Ctrl).BorderStyle:= SetControlBorderStyle(Json.GetValue<string>('BorderStyle', 'bsNone'));
    TPanel(Ctrl).Color:= SetControlColor(Json.GetValue<string>('Color', ColorStr));
    TPanel(Ctrl).ParentColor := False;
    TPanel(Ctrl).ParentBackground := False;
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

  var FontColor: String;
  if (Ctrl is TLabel) and Json.TryGetValue<string>('FontColor', FontColor) then
    TLabel(Ctrl).Font.Color:= SetControlColor(Json.GetValue<string>('FontColor', FontColor));
  var ColorLabel: String;
  if (Ctrl is TLabel) and Json.TryGetValue<string>('Color', ColorLabel) then
    TLabel(Ctrl).Font.Color:= SetControlColor(Json.GetValue<string>('Color', ColorLabel));
  var FontSize: integer;
  if (Ctrl is TLabel) and Json.TryGetValue<integer>('FontSize', FontSize) then
    TLabel(Ctrl).Font.Size := Json.GetValue<integer>('FontSize', FontSize);

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
      Tab.Name := Format('%s%s', [Ctrl.Name, Random(9999999).ToString]);
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
begin
  var ControlsArray: TJSONArray;
  var Form:= TForm.Create(AOwner);
  Form.Position:= poDefault;
  Form.FormStyle:= fsMDIChild;
  Form.Align:= alNone;
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
      var ControlJson := newarray as TJSONObject;
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
  begin
    TImage(Ctrl).OnClick := ControlClick;
    TImage(Ctrl).OnDblClick := ControlDblClickImage;
  end;
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
begin
  Result := clbtnface;
  var ColorValue: integer;
  var ColorStr := Trim(Text);
  if ColorStr = '' then Exit;
  if ColorStr[1] = '#' then
  begin
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
  else if SameText(Copy(ColorStr, 1, 2), 'cl') then
  begin
    Result := StringToColor(ColorStr);
    Exit;
  end
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

