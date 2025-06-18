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

unit View.Builder.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Generics.Collections, System.JSON, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Buttons, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.Threading, StrUtils,Clipbrd,TypInfo,  SynEditTypes,

  Adapter.TreeViewAdapter,
  Builder.UI.BuilderEngine,Builder.UI.UserPreferences,
  System.Skia, Vcl.Skia, System.Types, System.UITypes,
  Vcl.Imaging.pngimage, Vcl.WinXCtrls,

  Util.Json, Util.JSONValidator, System.Math, System.ImageList, Vcl.ImgList,
  View.Export.Forms, View.Menu.Context.Windows, View.Window.Json, SynEdit,
  SynEditHighlighter, SynHighlighterJSON, Enum.Utils, Service.JsonFile,

  Service.Zoom, Service.Forms.Manager, Service.Skia.Draw, Service.Json.Validation,
  Service.Component, Service.Component.Search,Service.Component.Manager.Highlighter, Service.StatusBar.Manager,
  RTTI, Vcl.Grids, Vcl.ValEdit, Vcl.Menus;


type
  TOrigRect = record
    Left, Top, Width, Height: Integer;
  end;

  TFormBuilderMain = class(TForm,IPreferenceObserver)
    StatusBarBottom: TStatusBar;
    PanelRenderJson: TPanel;
    SplitterRight: TSplitter;
    SplitViewMain: TSplitView;
    PanelMainSettings: TPanel;
    ImgSettings: TImage;
    PanelTreeComponents: TPanel;
    ImageTreeComponents: TImage;
    PanelMain: TPanel;
    ImageMain: TImage;
    PanelLoad: TPanel;
    ImageRenderJson: TImage;
    PanelOpenTemplate: TPanel;
    ImageOpenTemplate: TImage;
    PanelTopRender: TPanel;
    ImageCloseRender: TImage;
    SkLabelInfoRenderJson: TSkLabel;
    SplitterLeft: TSplitter;
    PanelBottomInfo: TPanel;
    PanelToolPalette: TPanel;
    ImageSelectMode: TImage;
    ImageArrangeWindows: TImage;
    ImageBackground: TImage;
    ImageTreeView: TImage;
    ImageShare: TImage;
    ImageOptions: TImage;
    PanelSettings: TPanel;
    SkLabelSettings: TSkLabel;
    SkPaintBoxToolPalette: TSkPaintBox;
    PanelTop: TPanel;
    SkLabelTittleMain: TSkLabel;
    PanelToolJsonRender: TPanel;
    ImageCopyToClipboard: TImage;
    ImageLoadJson: TImage;
    ImageSaveJson: TImage;
    ImagePrettyJson: TImage;
    ImageHighSize: TImage;
    SplitterBottomTool: TSplitter;
    SkLabelTreeJson: TSkLabel;
    SkLabelLoadTemplate: TSkLabel;
    SkLabelRender: TSkLabel;
    SkPaintBackground: TSkPaintBox;
    PanelTree: TPanel;
    PanelExpandTree: TPanel;
    ImageExpand: TImage;
    ImageColapse: TImage;
    PanelTopExplorer: TPanel;
    PanelSearchComponents: TPanel;
    SearchBoxComponents: TSearchBox;
    TreeViewExplorer: TTreeView;
    SkPaintBoxExplorer: TSkPaintBox;
    ImageCloseExplorer: TImage;
    ImageFilterExplorer: TImage;
    SkLabelExplorer: TSkLabel;
    SkPaintBoxRenderJson: TSkPaintBox;
    ImageClearJson: TImage;
    ImageZoomIn: TImage;
    ImageZoomOut: TImage;
    PanelInfoValidation: TPanel;
    PanelInfoRenderToForms: TPanel;
    ImageErro: TImage;
    ImageOk: TImage;
    LabelInfoJson: TLabel;
    LabelBottomInfo: TLabel;
    ImageInfoBottom: TImage;
    PopupMenuOptions: TPopupMenu;
    CopyText: TMenuItem;
    Cut: TMenuItem;
    Paste: TMenuItem;
    SynJSON: TSynJSONSyn;
    SelectAll1: TMenuItem;
    ImageRenderJsonToolPalette: TImage;
    PageControlRenderExplorer: TPageControl;
    TabSheetRender: TTabSheet;
    Memo: TSynEdit;
    TabSheetProperties: TTabSheet;
    ValueListEditorRenderJson: TValueListEditor;
    procedure FormCreate(Sender: TObject);
    procedure ImgSettingsClick(Sender: TObject);
    procedure ImageTreeComponentsClick(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure ImageRenderJsonClick(Sender: TObject);
    procedure ImageCloseRenderClick(Sender: TObject);
    procedure SkPaintBackgroundDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure SearchBoxComponentsChange(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure TreeViewExplorerClick(Sender: TObject);
    procedure ImageExpandClick(Sender: TObject);
    procedure ImageColapseClick(Sender: TObject);
    procedure ImageFilterExplorerClick(Sender: TObject);
    procedure SkPaintBoxToolPaletteDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure FormShow(Sender: TObject);
    procedure SkPaintBackgroundMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImageRenderClick(Sender: TObject);
    procedure ImageSelectModeClick(Sender: TObject);
    procedure ImageBackgroundClick(Sender: TObject);
    procedure ImageClearJsonClick(Sender: TObject);
    procedure ImageLoadJsonClick(Sender: TObject);
    procedure ImageSaveJsonClick(Sender: TObject);
    procedure ImageCopyToClipboardClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImageCloseExplorerClick(Sender: TObject);
    procedure SkPaintBoxExplorerDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure SkPaintBoxRenderJsonDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure ImageTreeViewClick(Sender: TObject);
    procedure ImageArrangeWindowsClick(Sender: TObject);
    procedure ImageShareClick(Sender: TObject);
    procedure ImagePrettyJsonClick(Sender: TObject);
    procedure ImageZoomInClick(Sender: TObject);
    procedure ImageZoomOutClick(Sender: TObject);
    procedure ImageHighSizeClick(Sender: TObject);
    procedure ImageOkClick(Sender: TObject);
    procedure CopyTextClick(Sender: TObject);
    procedure CutClick(Sender: TObject);
    procedure PasteClick(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure ImageOptionsClick(Sender: TObject);
    procedure ImageRenderJsonToolPaletteClick(Sender: TObject);
  private
    FBuilderBackground: TBuilderBackground;
    FTreeViewAdapter: TTreeViewAdapter;
    FJsonStructure : TJSONObject;
    FBuilder: TUIBuilderEngine;
    FSelectedComponent: String;
    FSelectedShape: TShape;
    FPaint: ISkPaint;
    FZoomService: TZoomService;
    FFormManager: TFormCreatedManager;
    FOrigRectsForms: array of TOrigRect;
    FHighlighter: TShapeHighlighter;
    FStatusBarManager: TStatusBarManager;
    FRuler: boolean;
    procedure RenderJson(const Atext : string);
    procedure ValidateAndProcessJSON(const AJSON: string);
    procedure BuildStatusBar;
    procedure SetSelectedComponent(const Value: String);
    procedure SetBuilderBackground(const Value: TBuilderBackground);
    procedure SetRuler(const value :boolean);
    procedure ZoomIn;
    procedure ZoomOut;
    procedure ApplyZoomToCreatedForms;
    procedure HandleTreeSelection;
    procedure LoadFlatJsonObjectToValueListEditor(const JsonObj: TJSONObject; const Editor: TValueListEditor);
    procedure PrettyJson;
  public
    destructor Destroy; override;
    procedure LoadComponentPropertiesToValueListEditor( AComponent: TComponent; AEditor: TValueListEditor);
    procedure OnPreferenceChanged(const Key, Value: string);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property SelectedComponent: String read FSelectedComponent write SetSelectedComponent;
    property BuilderBackground: TBuilderBackground read FBuilderBackground write SetBuilderBackground;
    property Ruler: boolean  read FRuler write SetRuler;
  end;

var
  FormBuilderMain: TFormBuilderMain;

implementation

{$R *.dfm}

procedure TFormBuilderMain.FormCanResize(Sender: TObject; var NewWidth,  NewHeight: Integer; var Resize: Boolean);
begin
  PanelToolPalette.Left := (ClientWidth - PanelToolPalette.Width) div 2;
  PanelToolPalette.Top := ClientHeight - PanelToolPalette.Height -60;
end;

procedure TFormBuilderMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;

procedure TFormBuilderMain.FormCreate(Sender: TObject);

  function FindFormForComponent(AComponent: TComponent): TForm;
  var
    I: Integer;
    Form: TForm;
  begin
    Result := nil;
    for I := 0 to FFormManager.Forms.Count - 1 do
    begin
      Form := FFormManager.Forms[I];
      if Form.FindComponent(AComponent.Name) = AComponent then
        Exit(Form);
    end;
  end;

begin
  FFormManager := TFormCreatedManager.Create;
  FZoomService := TZoomService.Create;
  FHighlighter:= TShapeHighlighter.Create;
  FStatusBarManager:= TStatusBarManager.Create(StatusBarBottom);

  TUserPreferences.Instance.Create;
  SetBuilderBackground(TUserPreferences.Instance.GetBackgroundEnum);
  SetRuler(TUserPreferences.Instance.GetRulerEnabled);
end;

procedure TFormBuilderMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FStatusBarManager.SetPosition(X, Y);
end;

procedure TFormBuilderMain.FormShow(Sender: TObject);
begin
  BuildStatusBar;
end;

procedure TFormBuilderMain.HandleTreeSelection;
begin
  var Node := TreeViewExplorer.Selected;
  if not Assigned(Node) then Exit;

  var RealName := Node.Text.Trim;
  if RealName.StartsWith('Name: ') then
  begin
    RealName := RealName.Substring(6);
    if RealName.IsEmpty then Exit;
    SetSelectedComponent(RealName);
    var C := TComponentSearchService.FindComponentByName(SkPaintBackground, RealName);

    if Assigned(C) and (C is TControl) then
    begin
      if TWinControl(C) is TForm then
      begin
        if Assigned(FHighlighter) then
          FHighlighter.Hide;
        TWinControl(C).BringToFront;
      end
      else
      begin
        if Assigned(FHighlighter) then
          FHighlighter.Highlight(TControl(C), Self);
      end;
    end
    else if Assigned(FHighlighter) then
      FHighlighter.Hide;
   PageControlRenderExplorer.ActivePage:= TabSheetProperties;
   var ComponentJson := Util.JSON.TJSONHelper.FindComponentJsonByName(FJsonStructure, C.Name);
   if Assigned(ComponentJson) then
     LoadFlatJsonObjectToValueListEditor(ComponentJson, ValueListEditor1);
  end
  else if Assigned(FHighlighter) then
    FHighlighter.Hide;
end;

procedure TFormBuilderMain.ImageRenderJsonToolPaletteClick(Sender: TObject);
begin
  PanelRenderJson.Visible:= not PanelRenderJson.Visible;
  SplitterRight.Visible:= not SplitterRight.Visible;
end;

procedure TFormBuilderMain.ImageArrangeWindowsClick(Sender: TObject);
begin
  var FormContext:= TFormContextWindows.Create(nil,FFormManager.Forms);
  try
    FormContext.ShowModal;
  finally
    FormContext.Free;
  end;
end;

procedure TFormBuilderMain.ImageCloseRenderClick(Sender: TObject);
begin
  PanelRenderJson.Visible:= not PanelRenderJson.Visible;
  SplitterRight.Visible:= not SplitterRight.Visible;
end;

procedure TFormBuilderMain.ImageBackgroundClick(Sender: TObject);
begin
  if FBuilderBackground = bClear then
  begin
    FBuilderBackground := bGrid;
    TUserPreferences.Instance.BackgroundStyle := 'bGrid';
    SkPaintBackground.Width:= SkPaintBackground.Width + 1;
  end  else
  begin
    TUserPreferences.Instance.BackgroundStyle := 'bClear';
    FBuilderBackground := bCLear;
    SkPaintBackground.Width:= SkPaintBackground.Width - 1;
  end;

  TUserPreferences.Instance.SavePreferences;
end;

procedure TFormBuilderMain.ImageTreeViewClick(Sender: TObject);
begin
  PanelTree.Visible:= not PanelTree.Visible;
end;

procedure TFormBuilderMain.ImageShareClick(Sender: TObject);
begin
  var ExportForm:= TFormExports.Create(nil);
  try
    FTreeViewAdapter.CopyRootNodes(TreeViewExplorer,ExportForm.TreeViewForms);
    ExportForm.SetJsonData(FJsonStructure);
    ExportForm.ShowModal;
  finally
    ExportForm.Free;
  end;
end;

procedure TFormBuilderMain.ImageHighSizeClick(Sender: TObject);
begin
  FormJson := TFormJson.Create(Self);
  try
    FormJson.SetJson(Memo.Lines.Text);
    FormJson.ShowModal;
  finally
    Memo.Lines.Clear;
    Memo.Lines.Text:=  FormJson.Json;
    FormJson.Free;
  end;
end;

procedure TFormBuilderMain.ImageZoomInClick(Sender: TObject);
begin
  ZoomIn;
end;

procedure TFormBuilderMain.ImageCloseExplorerClick(Sender: TObject);
begin
  PanelTree.Visible:= not PanelTree.Visible;
end;

procedure TFormBuilderMain.ImageZoomOutClick(Sender: TObject);
begin
  ZoomOut;
end;

procedure TFormBuilderMain.ImageFilterExplorerClick(Sender: TObject);
begin
  PanelSearchComponents.Visible:= not PanelSearchComponents.Visible;
end;

procedure TFormBuilderMain.ImageCopyToClipboardClick(Sender: TObject);
begin
  TJsonFileService.CopyJsonToClipboard(Memo.Lines.Text);
end;

procedure TFormBuilderMain.ImageLoadJsonClick(Sender: TObject);
begin
  var Json: string;
  if TJsonFileService.OpenJsonFromFile(Json) then
    Memo.Lines.Text := Json;
end;

procedure TFormBuilderMain.ImageSelectModeClick(Sender: TObject);
begin
  if Screen.Cursor = crHandPoint then
    Screen.Cursor:= crDefault
  else
    Screen.Cursor:= crHandPoint;
end;

procedure TFormBuilderMain.ImageSaveJsonClick(Sender: TObject);
begin
  TJsonFileService.SaveJsonToFile(Memo.Lines.Text);
end;

procedure TFormBuilderMain.ImagePrettyJsonClick(Sender: TObject);
begin
  PrettyJson;
end;

procedure TFormBuilderMain.ImageClearJsonClick(Sender: TObject);
begin
  Memo.Text:= '';
end;

procedure TFormBuilderMain.ImageRenderClick(Sender: TObject);
begin
  RenderJson(Memo.Lines.text);
end;

procedure TFormBuilderMain.ImageRenderJsonClick(Sender: TObject);
begin
  PanelRenderJson.Visible:= not PanelRenderJson.Visible;
  SplitterRight.Visible:= not SplitterRight.Visible;
end;

procedure TFormBuilderMain.ImgSettingsClick(Sender: TObject);
begin
  if not SplitViewmain.Opened then
    PanelSettings.Visible:= True
  else
    PanelSettings.Visible:= False;

  SplitViewmain.Opened := not SplitViewmain.Opened;
  SplitterLeft.Visible:= True;
end;

procedure TFormBuilderMain.LoadComponentPropertiesToValueListEditor( AComponent: TComponent; AEditor: TValueListEditor);
var
  ctx: TRttiContext;
  typ: TRttiType;
  prop: TRttiProperty;
  value: TValue;
begin
  AEditor.Strings.Clear;

  ctx := TRttiContext.Create;
  try
    typ := ctx.GetType(AComponent.ClassType);

    for prop in typ.GetProperties do
    begin
      if prop.IsReadable and (prop.Visibility in [mvPublished]) then
      begin
        value := prop.GetValue(AComponent);
        AEditor.InsertRow(prop.Name, value.ToString, True);
      end;
    end;

  finally
    ctx.Free;
  end;
end;

procedure TFormBuilderMain.ImageTreeComponentsClick(Sender: TObject);
begin
  PanelTree.Visible:= not PanelTree.Visible;
end;

procedure TFormBuilderMain.ImageColapseClick(Sender: TObject);
begin
  TreeViewExplorer.Colapse;
end;

procedure TFormBuilderMain.ImageExpandClick(Sender: TObject);
begin
  TreeViewExplorer.Expand;
end;

procedure TFormBuilderMain.ImageOkClick(Sender: TObject);
begin
  RenderJson(memo.text);
  FStatusBarManager.SetComponentCount(TComponentService.CountComponents(SkPaintBackground).ToString);
end;

procedure TFormBuilderMain.ImageOptionsClick(Sender: TObject);
begin
  SkPaintBackground.Width:= SkPaintBackground.Width - 1;
  if FRuler then
    TUserPreferences.Instance.Ruler:= False
  else
    TUserPreferences.Instance.Ruler:= True;

  SetRuler(TUserPreferences.Instance.Ruler);
  TUserPreferences.Instance.SavePreferences;
  SkPaintBackground.Width:= SkPaintBackground.Width + 1;
end;

procedure TFormBuilderMain.MemoChange(Sender: TObject);
begin
  ValidateAndProcessJSON(Memo.Lines.Text);
end;

procedure TFormBuilderMain.RenderJson(const Atext : string);
begin
  if Atext.Trim.Equals('') then Exit;
  var MyForm: TForm;
  var FormsArray: TJSONArray;
  var FormJson: TJSONObject;
  var Node: TTreeNode;

  FFormManager.CloseAll;
  TreeViewExplorer.Items.Clear;

  if Assigned(FJsonStructure) then FreeAndNil(FJsonStructure);
  FJsonStructure := TJSONObject.ParseJSONValue(Atext) as TJSONObject;
  FTreeViewAdapter.FTreeView := TreeViewExplorer;
  FTreeViewAdapter.FCreatedForm := FFormManager.Forms;
  try
    try
      if FJsonStructure.TryGetValue<TJSONArray>('Forms', FormsArray) then
      begin
        for var I := 0 to FormsArray.Count - 1 do
        begin
          FormJson := FormsArray.Items[I] as TJSONObject;
          MyForm := FBuilder.CreateFormFromJson(SkPaintBackground, FormJson);
          Node := TreeViewExplorer.Items.Add(nil, FormJson.GetValue<string>('Name'));
          Node.Data := MyForm;
          FTreeViewAdapter.AddJSONToTreeView(FormJson,Node,'root',TreeViewExplorer);

          MyForm.OnClose := FTreeViewAdapter.CloseFormsTreeview;
          FFormManager.AddForm(MyForm);
          MyForm.Show;

          SetLength(FOrigRectsForms, FFormManager.Forms.Count);
          FOrigRectsForms[FFormManager.Forms.Count-1].Left   := MyForm.Left;
          FOrigRectsForms[FFormManager.Forms.Count-1].Top    := MyForm.Top;
          FOrigRectsForms[FFormManager.Forms.Count-1].Width  := MyForm.Width;
          FOrigRectsForms[FFormManager.Forms.Count-1].Height := MyForm.Height;
        end;
      end else
      begin
        Node := TreeViewExplorer.Items.Add(nil, FJsonStructure.GetValue<string>('Name'));
        MyForm := FBuilder.CreateFormFromJson(SkPaintBackground, FJsonStructure);
        Node.Data := MyForm;
        FTreeViewAdapter.AddJSONToTreeView(FJsonStructure,Node,'root',TreeViewExplorer);

        MyForm.OnClose := FTreeViewAdapter.CloseFormsTreeview;
        FFormManager.AddForm(MyForm);
        MyForm.Show;
      end;
    except on E: Exception do
      begin
        FFormManager.CloseAll;
      end;
    end;
  finally
    Memo.CaretXY := BufferCoord(1, 1);
    Memo.TopLine := 1;
  end;
end;

procedure TFormBuilderMain.SearchBoxComponentsChange(Sender: TObject);
begin
  if String(SearchBoxComponents.Text).Equals('') then Exit;
    if  FSelectedShape <> nil then
      FSelectedShape.Visible:= False;
  FTreeViewAdapter.FindComponentInTreeView(SearchBoxComponents.Text);
end;

procedure TFormBuilderMain.SetBuilderBackground(const Value: TBuilderBackground);
begin
  FBuilderBackground := Value;
end;

procedure TFormBuilderMain.SetRuler(const value: boolean);
begin
  FRuler:= value;
end;

procedure TFormBuilderMain.SetSelectedComponent(const Value: String);
begin
  FSelectedComponent := Value;
  FStatusBarManager.SetSelected(FSelectedComponent);
end;

procedure TFormBuilderMain.SkPaintBackgroundDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  TSkiaDrawService.DrawBackground(ACanvas, ADest, AOpacity, FBuilderBackground, FPaint, FRuler, FRuler);
  TSkiaDrawService.DrawRulers(ACanvas, ADest, 1, FRuler, FRuler, FRuler);
end;

procedure TFormBuilderMain.SkPaintBackgroundMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FStatusBarManager.SetPosition(X, Y);
end;

procedure TFormBuilderMain.SkPaintBoxToolPaletteDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  TSkiaDrawService.DrawGradientBox(ACanvas, ADest, AOpacity, FPaint);
end;

procedure TFormBuilderMain.SkPaintBoxExplorerDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  TSkiaDrawService.DrawExplorerBorder(ACanvas, ADest, AOpacity, FPaint);
end;

procedure TFormBuilderMain.SkPaintBoxRenderJsonDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  TSkiaDrawService.DrawBoxRenderJsonBorder(ACanvas, ADest, AOpacity, FPaint);
end;

procedure TFormBuilderMain.TreeViewExplorerClick(Sender: TObject);
begin
  HandleTreeSelection;
end;


procedure TFormBuilderMain.ValidateAndProcessJSON(const AJSON: string);
begin
  var Result := TBuilderUIValidatorService.Validate(AJSON);
  if not Result.IsValid then
  begin
    LabelInfoJson.Caption := Result.ErrorMessage;
    ImageOk.Visible := False;
    ImageErro.Visible := True;
    FFormManager.CloseAll;
    TreeViewExplorer.Items.Clear;
    Exit;
  end;

  LabelInfoJson.Caption := 'Valid JSON';
  ImageOk.Visible := True;
  ImageErro.Visible := False;
end;

procedure TFormBuilderMain.LoadFlatJsonObjectToValueListEditor(
  const JsonObj: TJSONObject;
  const Editor: TValueListEditor);
var
  Pair: TJSONPair;
begin
  Editor.Strings.BeginUpdate;
  try
    Editor.Strings.Clear;
    for Pair in JsonObj do
    begin
      if not (Pair.JsonValue is TJSONObject) and
         not (Pair.JsonValue is TJSONArray) then
      begin
        Editor.InsertRow(Pair.JsonString.Value, Pair.JsonValue.Value, True);
      end;
    end;
  finally
    Editor.Strings.EndUpdate;
  end;
end;

procedure TFormBuilderMain.ZoomIn;
begin
  FZoomService.ZoomIn;
  FStatusBarManager.SetZoom(Round(FZoomService.GetZoom * 100));
  ApplyZoomToCreatedForms;
  SkPaintBackground.Invalidate;
end;

procedure TFormBuilderMain.ZoomOut;
begin
  FZoomService.ZoomOut;
  FStatusBarManager.SetZoom(Round(FZoomService.GetZoom * 100));
  ApplyZoomToCreatedForms;
  SkPaintBackground.Invalidate;
end;

procedure TFormBuilderMain.ApplyZoomToCreatedForms;
begin
  if Length(FOrigRectsForms) <> FFormManager.Forms.Count then Exit;
  for var I := 0 to FFormManager.Forms.Count - 1 do
  begin
    FFormManager.Forms[I].Left   := Round(FOrigRectsForms[I].Left * FZoomService.GetZoom);
    FFormManager.Forms[I].Top    := Round(FOrigRectsForms[I].Top * FZoomService.GetZoom);
    FFormManager.Forms[I].Width  := Round(FOrigRectsForms[I].Width * FZoomService.GetZoom);
    FFormManager.Forms[I].Height := Round(FOrigRectsForms[I].Height * FZoomService.GetZoom);
  end;
end;

procedure TFormBuilderMain.AfterConstruction;
begin
  inherited;
  TUserPreferences.Instance.AddObserver(Self);
end;

procedure TFormBuilderMain.BeforeDestruction;
begin
  TUserPreferences.Instance.RemoveObserver(Self);
  inherited;
end;

procedure TFormBuilderMain.OnPreferenceChanged(const Key, Value: string);
var
  DisplayValue: string;
  function ProcessRuler(KeyRuler, ValueRuler: string) : string;
  begin
    if SameText(KeyRuler, 'Ruler') then
    begin
      if SameText(ValueRuler, '-1') then
        DisplayValue := 'true'
      else
        DisplayValue := 'false'
    end
    else
      DisplayValue := value;
    result:= DisplayValue;
  end;
begin
  ProcessRuler(Key, Value);
  LabelBottomInfo.Caption := Format('Updated preference: %s : %s at date: %s',
             [key, DisplayValue, FormatDateTime('yyyy/mm/dd hh:nn:ss', Now)]);
end;

procedure TFormBuilderMain.PasteClick(Sender: TObject);
begin
  TJsonFileService.PasteJsonToClipboard(Memo);
end;

procedure TFormBuilderMain.CopyTextClick(Sender: TObject);
begin
  TJsonFileService.CopyJsonToClipboard(Memo.text);
end;

procedure TFormBuilderMain.CutClick(Sender: TObject);
begin
  TJsonFileService.CutJsonToClipboard(Memo)
end;

procedure TFormBuilderMain.SelectAll1Click(Sender: TObject);
begin
  TJsonFileService.SelectAll(Memo);
end;
procedure TFormBuilderMain.PrettyJson;
begin
  var erromessage:='';
  if not Memo.lines.Text.Trim.Equals('') then
    if TJSONHelper.ValidateJSON(Memo.lines.Text, erromessage ) then
      Memo.lines.Text:= Util.Json.TJSONHelper.BeautifyJSON(Memo.lines.Text);
end;


procedure TFormBuilderMain.BuildStatusBar;
begin
  StatusBarBottom.Panels[0].Text:= 'X: 0 Y: 0';
  StatusBarBottom.Panels[1].Text := 'Selected: none';
  StatusBarBottom.Panels[2].Text := 'Component count: 0';
  StatusBarBottom.Panels[3].Text := 'Date: ' +FormatDateTime('YYYY/MM/DD',now());
  StatusBarBottom.Panels[4].Text := 'Zoom: 100%';
  StatusBarBottom.Panels[5].Text := 'Mode: Mouse';
  StatusBarBottom.Panels[6].Text := 'Project: (None)';
  StatusBarBottom.Panels[7].Text := '';
end;

destructor TFormBuilderMain.Destroy;
begin
  TUserPreferences.Instance.Destroy;
  if FSelectedShape <> nil then
    FSelectedShape.Free;
  if FTreeViewAdapter <> nil then
    FTreeViewAdapter.Free;
  if FStatusBarManager <> nil then
    FStatusBarManager.Free;
  if FJsonStructure <> nil then
    FJsonStructure.Free;
  if FFormManager <> nil then
    FFormManager.Free;
  if FZoomService <> nil then
    FZoomService.Free;
  if FHighlighter <> nil then
    FHighlighter.Free;
  if FBuilder <> nil then
    FBuilder.Free;
  FPaint:= nil;
  inherited;
end;

end.

