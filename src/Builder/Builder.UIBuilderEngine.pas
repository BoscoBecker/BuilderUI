unit Builder.UIBuilderEngine;

interface

uses
  System.JSON, System.Classes,
  System.Types, System.SysUtils, Forms,

  Vcl.Controls, Vcl.StdCtrls,Vcl.DBGrids, Vcl.Grids, Vcl.ComCtrls,
  Vcl.ExtCtrls, Windows, Messages, Vcl.Buttons, Dialogs,

  Core.IUIBuilder, Adapter.TreeViewAdapter;

type
  TUIBuilderEngine = class(TInterfacedObject, IUIBuilder)
  private
    FDragControl: TControl;
    FDragOffset: TPoint;
  public
    function CreateFormFromJson(AOwner: TComponent; Json: TJSONObject): TForm;
    function CreateControlFromJson(AOwner: TComponent; AParent: TWinControl; Json: TJSONObject): TControl;
    procedure ControlClick(Sender: TObject);
    procedure CtrlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CtrlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure CtrlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  end;

implementation

uses
  Vcl.Graphics;

procedure TUIBuilderEngine.ControlClick(Sender: TObject);
begin
 var ComponentName:= TControl(Sender).Name;
  if Length(ComponentName) <=0 then Exit;

  if TTreeViewAdapter.FTreeView.CanFocus then
    TTreeViewAdapter.FTreeView.SetFocus;
  TTreeViewAdapter.FindComponentInTreeView(TControl(Sender).Name);
  TTreeViewAdapter.FTreeView.OnClick(Sender);
end;

function TUIBuilderEngine.CreateControlFromJson(AOwner: TComponent; AParent: TWinControl; Json: TJSONObject): TControl;
var
  CtrlType, PropText: string;
  Ctrl: TControl;
  PositionObj: TJSONObject;
  ChildrenArr: TJSONArray;
  ChildJson: TJSONObject;
  WidthValue, HeightValue: Integer;
  FontStyles: TJSONArray;
  Style: TFontStyles;
  AlignStr,BevelStr,ColorStr: string;
  ColorValue: integer;
begin
  Result := nil;
  CtrlType := Json.GetValue<string>('Type', '');
  if CtrlType = '' then Exit;

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
  else Exit;

  if Json.TryGetValue<string>('Name', PropText) or Json.TryGetValue<string>('name', PropText) then
    Ctrl.Name := PropText;

  Ctrl.Parent := AParent;

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
  else if Ctrl is TGroupBox then
    TGroupBox(Ctrl).OnClick := ControlClick
  else if Ctrl is TShape then
    TShape(Ctrl).OnMouseEnter := ControlClick;


//  Ctrl.OnMouseDown := CtrlMouseDown;
//  Ctrl.OnMouseMove := CtrlMouseMove;
//  Ctrl.OnMouseUp := CtrlMouseUp;

  if Json.TryGetValue<string>('Text', PropText) or Json.TryGetValue<string>('Caption', PropText) then
  begin
    if Ctrl is TLabel then
      TLabel(Ctrl).Caption := PropText
    else if Ctrl is TButton then
      TButton(Ctrl).Caption := PropText
    else if Ctrl is TEdit then
      TEdit(Ctrl).Text := PropText
    else if Ctrl is TRadioButton then
      TRadioButton(Ctrl).Caption := PropText
    else if Ctrl is TCheckBox then
      TCheckBox(Ctrl).Caption := PropText
    else if Ctrl is TRadioGroup then
      TRadioGroup(Ctrl).Caption := PropText;
  end;

    if Json.TryGetValue<TJSONObject>('Position', PositionObj) or Json.TryGetValue<TJSONObject>('position', PositionObj) then
    begin
      Ctrl.Left := Round(PositionObj.GetValue<Single>('X', 0));
      Ctrl.Top := Round(PositionObj.GetValue<Single>('Y', 0));
    end;

    if Json.TryGetValue<string>('Align', AlignStr) then
    begin
      if AlignStr = 'Top' then
        TPanel(Ctrl).Align := alTop
      else if AlignStr = 'Bottom' then
        TPanel(Ctrl).Align := alBottom
      else if AlignStr = 'Left' then
        TPanel(Ctrl).Align := alLeft
      else if AlignStr = 'Right' then
        TPanel(Ctrl).Align := alRight
      else if AlignStr = 'Client' then
        TPanel(Ctrl).Align := alClient
      else if AlignStr = 'None' then
        TPanel(Ctrl).Align := alNone;
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


  // Propriedade BevelOuter
  if Json.TryGetValue<string>('BevelOuter', BevelStr) then
  begin
    if BevelStr = 'bvNone' then
      TPanel(Ctrl).BevelOuter := bvNone
    else if BevelStr = 'bvLowered' then
      TPanel(Ctrl).BevelOuter := bvLowered
    else if BevelStr = 'bvRaised' then
      TPanel(Ctrl).BevelOuter := bvRaised
    else if BevelStr = 'bvSpace' then
      TPanel(Ctrl).BevelOuter := bvSpace;
  end;

  // Propriedade Color (suporte para múltiplos formatos)
  if Json.TryGetValue<string>('Color', ColorStr) then
  begin
    if (ColorStr <> '') then
    begin
      // Formato hexadecimal #RRGGBB ou #RGB
      if (ColorStr[1] = '#') then
      begin
        if Length(ColorStr) = 7 then  // Formato #RRGGBB
        begin
          ColorValue := RGB(
            StrToInt('$' + Copy(ColorStr, 2, 2)),  // Vermelho
            StrToInt('$' + Copy(ColorStr, 4, 2)),  // Verde
            StrToInt('$' + Copy(ColorStr, 6, 2))   // Azul
          );
        end
        else if Length(ColorStr) = 4 then  // Formato #RGB (curto)
        begin
          ColorValue := RGB(
            StrToInt('$' + StringOfChar(ColorStr[2], 2)),  // Vermelho
            StrToInt('$' + StringOfChar(ColorStr[3], 2)),  // Verde
            StrToInt('$' + StringOfChar(ColorStr[4], 2))   // Azul
          );
        end;
      end
      // Suporte para cores nomeadas do Delphi (clRed, clBlue, etc.)
      else if SameText(Copy(ColorStr, 1, 2), 'cl') then
      begin
        ColorValue := StringToColor(ColorStr);
      end
      // Suporte para cores em formato decimal (0-16777215)
      else if TryStrToInt(ColorStr, Integer(ColorValue)) then
      begin
        // Já convertido pelo TryStrToInt
      end;

      // Aplica a cor ao controle
      if Ctrl is TPanel then
        TPanel(Ctrl).Color := ColorValue
      else if Ctrl is TLabel then
        TLabel(Ctrl).Color := ColorValue;
    end;
  end;


  if Json.TryGetValue<Integer>('Width', WidthValue) or Json.TryGetValue<Integer>('width', WidthValue) then
    Ctrl.Width := WidthValue;

  if Json.TryGetValue<Integer>('Height', HeightValue) or Json.TryGetValue<Integer>('height', HeightValue) then
    Ctrl.Height := HeightValue;

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

  if Json.TryGetValue<TJSONArray>('Children', ChildrenArr) or Json.TryGetValue<TJSONArray>('children', ChildrenArr) then
  begin
    for var I := 0 to ChildrenArr.Count - 1 do
    begin
      ChildJson := ChildrenArr.Items[I] as TJSONObject;
      CreateControlFromJson(AOwner, TWinControl(Ctrl), ChildJson);
    end;
  end;

  Result := Ctrl;
end;

function TUIBuilderEngine.CreateFormFromJson(AOwner: TComponent;   Json: TJSONObject): TForm;
var
  Form: TForm;
  FormName, CaptionText: string;
  ControlsArray: TJSONArray;
  ControlJson: TJSONObject;
begin
  Form := TForm.Create(AOwner);
  Form.Position := poDefault;
  Form.BorderStyle := bsSizeable;

  if Json.TryGetValue<string>('Name', FormName) then
    Form.Name := FormName;

  if Json.TryGetValue<string>('Caption', CaptionText) then
    Form.Caption := CaptionText;

  if Json.TryGetValue<TJSONArray>('Children', ControlsArray) then
  begin
    for var I := 0 to ControlsArray.Count - 1 do
    begin
      ControlJson := ControlsArray.Items[I] as TJSONObject;
      CreateControlFromJson(Form, Form, ControlJson);
    end;
  end;

  // Exibir como MDIChild (opcional)
  if Application.MainForm.FormStyle = fsMDIForm then
    Form.FormStyle := fsMDIChild;
  Form.Show;

  Result := Form;
end;

procedure TUIBuilderEngine.CtrlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (Sender is TControl) then
  begin
    FDragControl := TControl(Sender);
    FDragOffset := Point(X, Y);
    FDragControl.BringToFront;
  end;
end;

procedure TUIBuilderEngine.CtrlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FDragControl) and (ssLeft in Shift) then
  begin
    FDragControl.Left := FDragControl.Left + (X - FDragOffset.X);
    FDragControl.Top := FDragControl.Top + (Y - FDragOffset.Y);
  end;
end;

procedure TUIBuilderEngine.CtrlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FDragControl := nil;
end;

end.

