unit View.Builder.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Generics.Collections, System.JSON, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Buttons, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.Threading, StrUtils,Clipbrd,

  Adapter.TreeViewAdapter,
  Builder.UIBuilderEngine,
  System.Skia, Vcl.Skia, System.Types, System.UITypes,
  Vcl.Imaging.pngimage, Vcl.WinXCtrls,

  Util.Json, System.Math, System.ImageList, Vcl.ImgList,
  View.Export.Forms, View.Menu.Context.Windows, View.Window.Json, SynEdit,
  SynEditHighlighter, SynHighlighterJSON;

type  TBuilderBackground = ( bClear, bGrid);

type
  TOrigRect = record
    Left, Top, Width, Height: Integer;
  end;

  TFormBuilderMain = class(TForm)
    StatusBarBottom: TStatusBar;
    PanelRenderJson: TPanel;
    SplitterRight: TSplitter;
    SplitView1: TSplitView;
    Panel6: TPanel;
    ImgSettings: TImage;
    Panel9: TPanel;
    Image9: TImage;
    Panel10: TPanel;
    Image10: TImage;
    Panel7: TPanel;
    ImageRenderJson: TImage;
    Panel12: TPanel;
    ImageOpenTemplate: TImage;
    Panel13: TPanel;
    Image12: TImage;
    SkLabel3: TSkLabel;
    SplitterLeft: TSplitter;
    Panel2: TPanel;
    PanelToolPalette: TPanel;
    Image6: TImage;
    Image11: TImage;
    Image13: TImage;
    Image14: TImage;
    Image15: TImage;
    Image16: TImage;
    PanelSettings: TPanel;
    SkLabelSettings: TSkLabel;
    SkPaintBox1: TSkPaintBox;
    Panel1: TPanel;
    SkLabel1: TSkLabel;
    Panel4: TPanel;
    LabelInfoJson: TLabel;
    Panel8: TPanel;
    Image3: TImage;
    Image5: TImage;
    Image7: TImage;
    Image8: TImage;
    Image17: TImage;
    Splitter3: TSplitter;
    SkLabel4: TSkLabel;
    SkLabel5: TSkLabel;
    SkLabel6: TSkLabel;
    SkPaintBackground: TSkPaintBox;
    PanelTree: TPanel;
    Panel11: TPanel;
    ImageExpand: TImage;
    ImageColapse: TImage;
    ActivityIndicatorExplorer: TActivityIndicator;
    Panel3: TPanel;
    PanelSearchComponents: TPanel;
    SearchBoxComponents: TSearchBox;
    TreeViewExplorer: TTreeView;
    SkPaintBox2: TSkPaintBox;
    Image1: TImage;
    Image2: TImage;
    SkLabel2: TSkLabel;
    SkPaintBox3: TSkPaintBox;
    Image4: TImage;
    Image19: TImage;
    Image20: TImage;
    Memo: TMemo;
    Panel5: TPanel;
    ImageErro: TImage;
    ImageOk: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ImgSettingsClick(Sender: TObject);
    procedure Image9Click(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure ImageRenderJsonClick(Sender: TObject);
    procedure Image12Click(Sender: TObject);
    procedure SkPaintBackgroundDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure SearchBoxComponentsChange(Sender: TObject);
    procedure PanelToolPaletteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelToolPaletteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelToolPaletteMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure TreeViewExplorerClick(Sender: TObject);
    procedure ImageExpandClick(Sender: TObject);
    procedure ImageColapseClick(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure SkPaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure SkPaintBackgroundMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImageRenderClick(Sender: TObject);
    procedure ButtonRunJsonClick(Sender: TObject);
    procedure Image6Click(Sender: TObject);
    procedure Image13Click(Sender: TObject);
    procedure Image4Click(Sender: TObject);
    procedure Image5Click(Sender: TObject);
    procedure Image7Click(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Image1Click(Sender: TObject);
    procedure SkPaintBox2Draw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure SkPaintBox3Draw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure Image14Click(Sender: TObject);
    procedure Image11Click(Sender: TObject);
    procedure Image15Click(Sender: TObject);
    procedure Image8Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Image19Click(Sender: TObject);
    procedure Image20Click(Sender: TObject);
    procedure Image17Click(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
    procedure ImageOkClick(Sender: TObject);
  private
    FBuilderBackground: TBuilderBackground;
    FCreatedForms: TObjectList<TForm>;
    FTreeViewAdapter: TTreeViewAdapter;
    FJsonStructure : TJSONObject;
    FBuilder: TUIBuilderEngine;
    FSelectedComponent: String;
    FSelecionadoShape: TShape;
    FDragging: Boolean;
    FDragOffset: TPoint;
    FPaint: ISkPaint;
    FZoom: Single;
    OrigRectsForms: array of TOrigRect;
    procedure RenderJson(const Atext : string);
    procedure CloseFormsCreated;
    procedure ValidateAndProcessJSON(const AJSON: string);
    procedure BuildStatusBar;
    procedure SetSelectedComponent(const Value: String);
    procedure SetBuilderBackground(const Value: TBuilderBackground);
    procedure ZoomIn;
    procedure ZoomOut;
    procedure ApplyZoomToCreatedForms;
  public
    destructor Destroy; override;
    property SelectedComponent: String read FSelectedComponent write SetSelectedComponent;
    property BuilderBackground: TBuilderBackground read FBuilderBackground write SetBuilderBackground;
  end;

var
  FormBuilderMain: TFormBuilderMain;

implementation

{$R *.dfm}

procedure TFormBuilderMain.BuildStatusBar;
begin
  StatusBarBottom.Panels[0].Text:= 'X: 0 Y: 0';
  StatusBarBottom.Panels[1].Text := 'Selected: Nenhum';
  StatusBarBottom.Panels[2].Text := 'Size: 0x0';
  StatusBarBottom.Panels[3].Text := 'Date: ' +FormatDateTime('YYYY/MM/DD',now());
  StatusBarBottom.Panels[4].Text := 'Zoom: 100%';
  StatusBarBottom.Panels[5].Text := 'Mode: Mouse';
  StatusBarBottom.Panels[6].Text := 'Project: (None)';
  StatusBarBottom.Panels[7].Text := '';
end;

procedure TFormBuilderMain.ButtonRunJsonClick(Sender: TObject);
begin
  RenderJson(Memo.Text);
end;

procedure TFormBuilderMain.CloseFormsCreated;
begin
  for var I := FCreatedForms.Count - 1 downto 0 do
  begin
    if Assigned(FCreatedForms[I]) then
    begin
      try
        FCreatedForms[I].Close;
        FCreatedForms[I].Free;
        FCreatedForms.Remove(FCreatedForms[I]);
      except
        FCreatedForms.Remove(FCreatedForms[I]);
      end;
    end;
  end;
  FCreatedForms.Clear;
end;

destructor TFormBuilderMain.Destroy;
begin
  if FSelecionadoShape <> nil then
    FSelecionadoShape.Free;
  if FTreeViewAdapter <> nil then
    FTreeViewAdapter.Free;
  if FJsonStructure <> nil then
    FJsonStructure.Free;
  if FCreatedForms <> nil then
    FCreatedForms.Free;
  if FBuilder <> nil then
    FBuilder.Free;
  FPaint:= nil;
  inherited;
end;

procedure TFormBuilderMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if not FDragging then
  begin
    PanelToolPalette.Left := (ClientWidth - PanelToolPalette.Width) div 2;
    PanelToolPalette.Top := ClientHeight - PanelToolPalette.Height -60;
  end;
end;

procedure TFormBuilderMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= TCloseAction.caFree;
end;

procedure TFormBuilderMain.FormCreate(Sender: TObject);
begin
  FCreatedForms := TObjectList<TForm>.Create(False);
  SetBuilderBackground(bClear);
  FZoom := 1.0;
end;

procedure TFormBuilderMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  StatusBarBottom.Panels[0].Text := Format('X: %d Y: %d', [X, Y]);
end;

procedure TFormBuilderMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    ZoomIn
  else
    ZoomOut;
  Handled := True;
end;

procedure TFormBuilderMain.FormShow(Sender: TObject);
begin
  BuildStatusBar();
end;

procedure TFormBuilderMain.Image11Click(Sender: TObject);
begin
  var FormContext:= TFormContextWindows.Create(nil,FCreatedForms);
  try
    FormContext.ShowModal;
  finally
    FormContext.Free;
  end;
end;

procedure TFormBuilderMain.Image12Click(Sender: TObject);
begin
  PanelRenderJson.Visible:= not PanelRenderJson.Visible;
  SplitterRight.Visible:= not SplitterRight.Visible;
end;

procedure TFormBuilderMain.Image13Click(Sender: TObject);
begin
  if FBuilderBackground = bClear then
  begin
    FBuilderBackground := bGrid;
    SkPaintBackground.Width:= SkPaintBackground.Width +1;
  end  else
  begin
    FBuilderBackground := bCLear;
    SkPaintBackground.Width:= SkPaintBackground.Width - 1;
  end;
end;

procedure TFormBuilderMain.Image14Click(Sender: TObject);
begin
  PanelTree.Visible:= not PanelTree.Visible;
end;

procedure TFormBuilderMain.Image15Click(Sender: TObject);
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

procedure TFormBuilderMain.Image17Click(Sender: TObject);
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

procedure TFormBuilderMain.Image19Click(Sender: TObject);
begin
  ZoomIn;
  SkPaintBackground.Width:= SkPaintBackground.Width + 1;
end;

procedure TFormBuilderMain.Image1Click(Sender: TObject);
begin
  PanelTree.Visible:= not PanelTree.Visible;
end;

procedure TFormBuilderMain.Image20Click(Sender: TObject);
begin
  ZoomOut;
  SkPaintBackground.Width:= SkPaintBackground.Width - 1;
end;

procedure TFormBuilderMain.Image2Click(Sender: TObject);
begin
  PanelSearchComponents.Visible:= not PanelSearchComponents.Visible;
end;

procedure TFormBuilderMain.Image3Click(Sender: TObject);
begin
  Clipboard.AsText:= Memo.Lines.Text;
end;

procedure TFormBuilderMain.Image5Click(Sender: TObject);
begin
  var OpenJson := TOpenDialog.Create(nil);
  OpenJson.Filter:= 'JSON Files (*.json)|*.json';
  try
    if OpenJson.Execute(Application.Handle) then
    begin
      if not String(OpenJson.FileName).Trim.Equals('') then
        Memo.Lines.LoadFromFile(OpenJson.FileName);
    end;
  finally
    OpenJson.Free;
  end;
end;

procedure TFormBuilderMain.Image6Click(Sender: TObject);
begin
  if Screen.Cursor = crHandPoint then
    Screen.Cursor:= crDefault
  else
    Screen.Cursor:= crHandPoint;
end;

procedure TFormBuilderMain.Image7Click(Sender: TObject);
begin
  var SaveJSON:= TSaveDialog.Create(nil);
  SaveJSON.Filter:= 'JSON Files (*.json)|*.json';
  try
    if SaveJSON.Execute(Application.Handle) then
    begin
      if not String(SaveJSON.FileName).Trim.Equals('') then
      begin
        Memo.Lines.SaveToFile(SaveJSON.FileName);
      end;
    end;
  finally
    SaveJSON.Free;
  end;
end;

procedure TFormBuilderMain.Image8Click(Sender: TObject);
begin
  var erromessage:='';
  if not Memo.lines.Text.Trim.Equals('') then
    if TJSONHelper.ValidateJSON(Memo.lines.Text, erromessage ) then
      Memo.lines.Text:= TJSONHelper.BeautifyJSON(Memo.lines.Text);
end;

procedure TFormBuilderMain.Image4Click(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

procedure TFormBuilderMain.ImageRenderClick(Sender: TObject);
begin
  RenderJson(Memo.Lines.text);
end;

procedure TFormBuilderMain.ImageRenderJsonClick(Sender: TObject);
begin
  if not SplitView1.Opened then
    PanelRenderJson.Visible:= True
  else
    PanelRenderJson.Visible:= False;

  SplitView1.Opened := not SplitView1.Opened;
  SplitterRight.Visible:= True;
end;

procedure TFormBuilderMain.ImgSettingsClick(Sender: TObject);
begin
  if not SplitView1.Opened then
    PanelSettings.Visible:= True
  else
    PanelSettings.Visible:= False;

  SplitView1.Opened := not SplitView1.Opened;
  SplitterLeft.Visible:= True;
end;

procedure TFormBuilderMain.Image9Click(Sender: TObject);
begin
  PanelTree.Visible:= not PanelTree.Visible;
end;

procedure TFormBuilderMain.ImageColapseClick(Sender: TObject);
begin
  Try
    ActivityIndicatorExplorer.Animate:= True;
    TThread.Synchronize(nil,
    procedure
    begin
      TreeViewExplorer.Colapse;
    end)
  Finally
    ActivityIndicatorExplorer.Animate:= False;
  End;
end;

procedure TFormBuilderMain.ImageExpandClick(Sender: TObject);
begin
  Try
    ActivityIndicatorExplorer.Animate:= True;
    TThread.Synchronize(nil,
    procedure
    begin
      TreeViewExplorer.Expand;
    end)
  Finally
    ActivityIndicatorExplorer.Animate:= False;
  End;
end;

procedure TFormBuilderMain.ImageOkClick(Sender: TObject);
begin
  RenderJson(memo.text);
end;

procedure TFormBuilderMain.MemoChange(Sender: TObject);
begin
  //ButtonRun.Enabled:= false;
  ValidateAndProcessJSON(Memo.Lines.Text);
end;

procedure TFormBuilderMain.PanelToolPaletteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragging := True;
    FDragOffset := Point(X, Y);
    PanelToolPalette.Cursor := crSizeAll;
  end;
end;

procedure TFormBuilderMain.PanelToolPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
begin
  if FDragging then
  begin
    PanelToolPalette.Left := PanelToolPalette.Left + (X - FDragOffset.X);
    PanelToolPalette.Top := PanelToolPalette.Top + (Y - FDragOffset.Y);
  end;
end;

procedure TFormBuilderMain.PanelToolPaletteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragging := False;
    PanelToolPalette.Cursor := crDefault;
  end;
end;

procedure TFormBuilderMain.RenderJson(const Atext : string);
begin
  if (Atext.Trim.Equals('')) or (Length(Atext) < 10) then Exit;

  var MyForm: TForm;
  var FormsArray: TJSONArray;
  var FormJson: TJSONObject;
  var Node: TTreeNode;

  CloseFormsCreated;
  TreeViewExplorer.Items.Clear;

  if Assigned(FJsonStructure) then FreeAndNil(FJsonStructure);
  FJsonStructure := TJSONObject.ParseJSONValue(Atext) as TJSONObject;
  FTreeViewAdapter.FTreeView := TreeViewExplorer;
  FTreeViewAdapter.FCreatedForm := FCreatedForms;
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
          FCreatedForms.Add(MyForm);
          MyForm.Show;

          SetLength(OrigRectsForms, FCreatedForms.Count);
          OrigRectsForms[FCreatedForms.Count-1].Left   := MyForm.Left;
          OrigRectsForms[FCreatedForms.Count-1].Top    := MyForm.Top;
          OrigRectsForms[FCreatedForms.Count-1].Width  := MyForm.Width;
          OrigRectsForms[FCreatedForms.Count-1].Height := MyForm.Height;
        end;
      end else
      begin
        Node := TreeViewExplorer.Items.Add(nil, FJsonStructure.GetValue<string>('Name'));
        MyForm := FBuilder.CreateFormFromJson(SkPaintBackground, FJsonStructure);
        Node.Data := MyForm;
        FTreeViewAdapter.AddJSONToTreeView(FJsonStructure,Node,'root',TreeViewExplorer);

        MyForm.OnClose := FTreeViewAdapter.CloseFormsTreeview;
        FCreatedForms.Add(MyForm);
        MyForm.Show;
      end;
    except on E: Exception do
      begin
        CloseFormsCreated;
      end;
    end;
  finally
  end;
end;

procedure TFormBuilderMain.ButtonRunClick(Sender: TObject);
begin
    RenderJson(memo.text);
end;

procedure TFormBuilderMain.SearchBoxComponentsChange(Sender: TObject);
begin
  if String(SearchBoxComponents.Text).Equals('') then Exit;

  ActivityIndicatorExplorer.Animate:= True;
  try
    if  FSelecionadoShape <> nil then
      FSelecionadoShape.Visible:= False;
    FTreeViewAdapter.FindComponentInTreeView(SearchBoxComponents.Text);
  finally
    ActivityIndicatorExplorer.Animate:= False;
  end;
end;

procedure TFormBuilderMain.SetBuilderBackground(const Value: TBuilderBackground);
begin
  FBuilderBackground := Value;
end;

procedure TFormBuilderMain.SetSelectedComponent(const Value: String);
begin
  FSelectedComponent := Value;
  StatusBarBottom.Panels[1].Text := 'Selected: '+ FSelectedComponent;
end;

procedure TFormBuilderMain.SkPaintBackgroundDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
const
  GridSize = 20;
begin
  ACanvas.Save;
  ACanvas.Scale(FZoom, FZoom); // Aplica o zoom
  case FBuilderBackground of
    bClear: ACanvas.Clear(TAlphaColors.White);
    bGrid:
    begin
      ACanvas.Clear($FFF5F5F5);
      FPaint := TSkPaint.Create;
      FPaint.Style := TSkPaintStyle.Stroke;
      FPaint.Color := $FFDDDDDD;
      FPaint.StrokeWidth := 1;
      var X := ADest.Left;
      while X <= ADest.Right / FZoom do
      begin
        ACanvas.DrawLine(X, ADest.Top, X, ADest.Bottom / FZoom, FPaint);
        X := X + GridSize;
      end;
      var Y := ADest.Top;
      while Y <= ADest.Bottom / FZoom do
      begin
        ACanvas.DrawLine(ADest.Left, Y, ADest.Right / FZoom, Y, FPaint);
        Y := Y + GridSize;
      end;
    end;
  end;
  ACanvas.Restore;
end;

procedure TFormBuilderMain.SkPaintBackgroundMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  StatusBarBottom.Panels[0].Text := Format('X: %d Y: %d', [X, Y]);
end;

procedure TFormBuilderMain.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
   FPaint:= TSkPaint.Create;
   FPaint.Shader := TSkShader.MakeGradientSweep(ADest.CenterPoint,
   [$FFF2F2F2, $FFCCCCCC, $FF999999, $FFCCCCCC, $FFF2F2F2]);
   ACanvas.DrawPaint(FPaint);
end;

procedure TFormBuilderMain.SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := True;
  FDragOffset := Point(Round(X), Round(Y));
end;

procedure TFormBuilderMain.SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
begin
  if FDragging then
  begin
    var NewLeft := SkPaintBox1.Left + Round(X) - FDragOffset.X;
    var NewTop := SkPaintBox1.Top + Round(Y) - FDragOffset.Y;
    SkPaintBox1.SetBounds(NewLeft, NewTop, SkPaintBox1.Width, SkPaintBox1.Height);
  end;
end;

procedure TFormBuilderMain.SkPaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
end;

procedure TFormBuilderMain.SkPaintBox2Draw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  Shader: ISkShader;
  BorderRect: TRectF;
  Center: TPointF;
begin
  FPaint := TSkPaint.Create;
  FPaint.Style := TSkPaintStyle.Stroke;
  FPaint.StrokeWidth := 1;

  Center := PointF(ADest.Left + ADest.Width / 2, ADest.Top + ADest.Height / 2);
  Shader := TSkShader.MakeGradientSweep(Center,[$FFB0B0B0, $FFD0D0D0, $FFFFFFFF, $FFD0D0D0, $FFB0B0B0]);
  FPaint.Shader := Shader;
  BorderRect := ADest;

  InflateRect(BorderRect, -FPaint.StrokeWidth / 2, -FPaint.StrokeWidth / 2);
  ACanvas.DrawRect(BorderRect, FPaint);
end;

procedure TFormBuilderMain.SkPaintBox3Draw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
var
  Shader: ISkShader;
  BorderRect: TRectF;
  Center: TPointF;
begin
  FPaint := TSkPaint.Create;
  FPaint.Style := TSkPaintStyle.Stroke;
  FPaint.StrokeWidth := 1;

  Center := PointF(ADest.Left + ADest.Width / 2, ADest.Top + ADest.Height / 2);
  Shader := TSkShader.MakeGradientSweep(Center,[$FFFFFFFF, $FFD0D0D0, $FFB0B0B0, $FFD0D0D0, $FFFFFFFF]);
  FPaint.Shader := Shader;
  BorderRect := ADest;

  InflateRect(BorderRect, -FPaint.StrokeWidth / 2, -FPaint.StrokeWidth / 2);
  ACanvas.DrawRect(BorderRect, FPaint);
end;

procedure TFormBuilderMain.TreeViewExplorerClick(Sender: TObject);
var
  Node: TTreeNode;
  NomeReal: string;
  C: TComponent;

  function BuscarComponentePorNome(Root: TComponent; const Nome: string): TComponent;

    function BuscaRecursivaComponente(Comp: TComponent): TComponent;
    begin
      Result := nil;
      if SameText(Comp.Name, Nome) then Exit(Comp);
      if Comp is TWinControl then
      begin
        for var I := 0 to TWinControl(Comp).ControlCount - 1 do
        begin
          Result := BuscaRecursivaComponente(TWinControl(Comp).Controls[I]);
          if Assigned(Result) then Exit;
        end;
      end;
      for var I := 0 to Comp.ComponentCount - 1 do
      begin
        Result := BuscaRecursivaComponente(Comp.Components[I]);
        if Assigned(Result) then Exit;
      end;
    end;

  begin
    Result := BuscaRecursivaComponente(Root);
  end;

  procedure CriarMarcador(AOwner: TForm);
  begin
    if FSelecionadoShape <> NIl then
    begin
      if FSelecionadoShape.Parent is TForm then
      begin
        FSelecionadoShape.Free;
        FSelecionadoShape := TShape.Create(AOwner);
        FSelecionadoShape.Parent := AOwner;
        FSelecionadoShape.Visible := False;
      end;
    end else
    if not Assigned(FSelecionadoShape) then
    begin
      FSelecionadoShape := TShape.Create(AOwner);
      FSelecionadoShape.Parent := AOwner;
      FSelecionadoShape.Visible := False;
    end else
    FSelecionadoShape.Parent := AOwner;
  end;

  procedure DestacarComponenteSelecionado(Control: TControl);
  begin
    CriarMarcador(Self);
    FSelecionadoShape.Parent := Control.Parent;
    FSelecionadoShape.SetBounds(
      Control.Left - 2,
      Control.Top - 2,
      Control.Width + 4,
      Control.Height + 4
    );
    if FSelecionadoShape.Pen <> nil then
    begin
      FSelecionadoShape.Brush.Style := bsClear;
      FSelecionadoShape.Pen.Color := clGrayText;
      FSelecionadoShape.Pen.Width := 1;
      FSelecionadoShape.Pen.Mode:= pmMask;
      FSelecionadoShape.Pen.Style := psDot;
      FSelecionadoShape.Visible := True;
      FSelecionadoShape.SendToBack;
    end;
  end;

begin
  try
    if TreeViewExplorer.CanFocus then
      TreeViewExplorer.SetFocus;
    TreeViewExplorer.BringToFront;

    Node := TreeViewExplorer.Selected;
    if Node <> nil then
    begin
      NomeReal := Node.Text;
      if Pos('Name: ', NomeReal) = 1 then
      begin
        if Length(NomeReal.Trim) <= 0 then Exit;

        NomeReal := Copy(NomeReal, 7, Length(NomeReal));
        SetSelectedComponent(NomeReal);
        C := BuscarComponentePorNome(SkPaintBackground, NomeReal);
        if Assigned(C) then
        begin
          if C is TControl then
          begin
            begin
              if TWinControl(C) is TForm then
              begin
                if Assigned(FSelecionadoShape) then
                  TWinControl(C).BringToFront;
                if Assigned(FSelecionadoShape) then
                begin
                  FSelecionadoShape.Visible := False;
                  FSelecionadoShape:= nil;
                end;
              end else
                DestacarComponenteSelecionado(TControl(C));
            end;
          end;
        end;
      end else
      if Assigned(FSelecionadoShape) then
      begin
        FSelecionadoShape.Visible := False;
        FSelecionadoShape:= nil;
      end;
    end;
  finally
  end;
end;

procedure TFormBuilderMain.ValidateAndProcessJSON(const AJSON: string);
begin
  var ErrorMsg: string;
  var DupForms: TArray<TDuplicateInfo>;

  LabelInfoJson.Caption:= 'Analizing json ... ';

  var ValidJson:= TJSONHelper.ValidateJSON(AJSON, ErrorMsg);
  if not (ValidJson) or (AJSON.Trim.Equals('')) then
  begin
    LabelInfoJson.Caption:='Invalid json :'+ErrorMsg ;
    ImageOk.Visible:= False;
    ImageErro.Visible:= True;
    //ButtonRun.Enabled:= False;
    //ButtonRun.Font.Color:= clgray;
    CloseFormsCreated;
    TreeViewExplorer.Items.Clear;
    Exit;
  end;

  var ValidNamesComponents:= TJSONHelper.HasDuplicateNamesPerForm(AJSON, DupForms);
  if ValidNamesComponents then
  begin
    var duplicatenames: string;
    for var Info in DupForms do
      duplicatenames:= duplicatenames + string.Join(', ',  Info.DuplicatedNames);
    LabelInfoJson.Caption:='Invalid json, Duplicate Names : ' +duplicatenames;
    ImageOk.Visible:= False;
    ImageErro.Visible:= True;
    //ButtonRun.Enabled:= False;
    //ButtonRun.Font.Color:= clgray;
    CloseFormsCreated;
    TreeViewExplorer.Items.Clear;
    Exit;
  end;

  LabelInfoJson.Caption:= '      Valid json';
  ImageOk.Visible:= True;
  ImageErro.Visible:= False;
  //ButtonRun.Font.Color:= clblack;
  //ButtonRun.Enabled:= True;
end;

procedure TFormBuilderMain.ZoomIn;
begin
  FZoom := FZoom * 1.1;
  StatusBarBottom.Panels[4].Text := Format('Zoom: %d%%', [Round(FZoom * 100)]);
  ApplyZoomToCreatedForms;
  SkPaintBackground.Invalidate;
end;

procedure TFormBuilderMain.ZoomOut;
begin
  FZoom := FZoom / 1.1;
  StatusBarBottom.Panels[4].Text := Format('Zoom: %d%%', [Round(FZoom * 100)]);
  ApplyZoomToCreatedForms;
  SkPaintBackground.Invalidate;
end;

procedure TFormBuilderMain.ApplyZoomToCreatedForms;
begin
  if Length(OrigRectsForms) <> FCreatedForms.Count then Exit;
  for var I := 0 to FCreatedForms.Count - 1 do
  begin
    FCreatedForms[I].Left   := Round(OrigRectsForms[I].Left * FZoom);
    FCreatedForms[I].Top    := Round(OrigRectsForms[I].Top * FZoom);
    FCreatedForms[I].Width  := Round(OrigRectsForms[I].Width * FZoom);
    FCreatedForms[I].Height := Round(OrigRectsForms[I].Height * FZoom);
  end;
end;

end.

