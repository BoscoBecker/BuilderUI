unit Strategy.IUIControlStrategy;

interface

uses
  FMX.Controls, System.JSON, System.Classes, System.Generics.Collections;

type
  IUIControlStrategy = interface
    ['{F97E0B1E-548F-4B1B-8A4B-A1F50DE3CB2A}']
    procedure ApplyProperties(Ctrl: TControl; Json: TJSONObject);
  end;

  function GetStrategyForControl(Ctrl: TControl): IUIControlStrategy;

implementation

uses
  FMX.StdCtrls, FMX.Edit, System.SysUtils, FMX.Graphics, System.UITypes;

type
  TLabelStrategy = class(TInterfacedObject, IUIControlStrategy)
    procedure ApplyProperties(Ctrl: TControl; Json: TJSONObject);
  end;

  TButtonStrategy = class(TInterfacedObject, IUIControlStrategy)
    procedure ApplyProperties(Ctrl: TControl; Json: TJSONObject);
  end;

  TEditStrategy = class(TInterfacedObject, IUIControlStrategy)
    procedure ApplyProperties(Ctrl: TControl; Json: TJSONObject);
  end;

  var StrategyMap: TDictionary<TClass, IUIControlStrategy>;


procedure TLabelStrategy.ApplyProperties(Ctrl: TControl; Json: TJSONObject);
var
  S: string;
  FontSize: Integer;
  FontStyles: TJSONArray;
  Style: TFontStyles;
begin
  if Json.TryGetValue<string>('Text', S) then
    TLabel(Ctrl).Text := S;

  if Json.TryGetValue<string>('FontSize', S) then
    TLabel(Ctrl).Font.Size := StrToIntDef(S, 12);

  if Json.TryGetValue<TJSONArray>('FontStyle', FontStyles) then
  begin
    Style := [];
    for var i := 0 to FontStyles.Count - 1 do
    begin
      var StyleStr := FontStyles.Items[i].Value;
      if SameText(StyleStr, 'Bold') then
        Include(Style, TFontStyle.fsBold)
      else if SameText(StyleStr, 'Italic') then
        Include(Style, TFontStyle.fsItalic)
      else if SameText(StyleStr, 'Underline') then
        Include(Style, TFontStyle.fsUnderline)
      else if SameText(StyleStr, 'StrikeOut') then
        Include(Style, TFontStyle.fsStrikeOut);
    end;
    TLabel(Ctrl).Font.Style := Style;
  end;
end;

procedure TButtonStrategy.ApplyProperties(Ctrl: TControl; Json: TJSONObject);
begin
  var S: string;
  if Json.TryGetValue<string>('Text', S) then
    TButton(Ctrl).Text := S;
end;

procedure TEditStrategy.ApplyProperties(Ctrl: TControl; Json: TJSONObject);
begin
  var S: string;
  if Json.TryGetValue<string>('Text', S) then
    TEdit(Ctrl).Text := S;
end;

function GetStrategyForControl(Ctrl: TControl): IUIControlStrategy;
var
  StrategyFactory: IUIControlStrategy;
begin
  if Assigned(StrategyMap) and StrategyMap.TryGetValue(Ctrl.ClassType, StrategyFactory) then
    Result := StrategyFactory
  else
    Result := nil;
end;

initialization
  StrategyMap := TDictionary<TClass, IUIControlStrategy>.Create;
  StrategyMap.Add(TLabel, TLabelStrategy.Create);
  StrategyMap.Add(TButton, TButtonStrategy.Create);
  StrategyMap.Add(TEdit, TEditStrategy.Create);

finalization
  StrategyMap.Free;


end.

