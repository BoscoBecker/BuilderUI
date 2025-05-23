unit View.Builder.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Generics.Collections, System.JSON, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Buttons, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.Threading, StrUtils,

  Adapter.TreeViewAdapter,
  Builder.UIBuilderEngine, System.Skia, Vcl.Skia, System.Types, System.UITypes,
  Vcl.Imaging.pngimage, Vcl.WinXCtrls,
  Util.Json ;
type
  TTreeViewIndexer = class
  private
    FIndex: TDictionary<string, TTreeNode>;
    FTreeView: TTreeView;
    procedure BuildIndex;
  public
    constructor Create(ATreeView: TTreeView);
    destructor Destroy; override;
    function FindNodeByComponentName(const AName: string): TTreeNode;
    function SelectComponentByName(const AName: string) : String;
    procedure RefreshIndex;
    property TreeView: TTreeView read FTreeView;
  end;

type
  TSkCachedBox = class(TSkCustomControl)
  protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
  end;

type
  TForm2 = class(TForm)
    StatusBar1: TStatusBar;
    PanelRenderJson: TPanel;
    BtnRender: TButton;
    Memo: TMemo;
    Splitter2: TSplitter;
    SplitView1: TSplitView;
    Panel6: TPanel;
    ImgSettings: TImage;
    Panel9: TPanel;
    Image9: TImage;
    Panel10: TPanel;
    Image10: TImage;
    Panel4: TPanel;
    Image1: TImage;
    Panel5: TPanel;
    Image2: TImage;
    Panel7: TPanel;
    Image3: TImage;
    Panel8: TPanel;
    Image4: TImage;
    Panel12: TPanel;
    Image8: TImage;
    lblOpenModel: TLabel;
    lblTreeJson: TLabel;
    lblRenderJson: TLabel;
    PanelTree: TPanel;
    Panel11: TPanel;
    Image5: TImage;
    Image7: TImage;
    SkLabel1: TSkLabel;
    TreeView1: TTreeView;
    PanelValidateJson: TPanel;
    SkLblVerify: TSkLabel;
    Panel13: TPanel;
    Image12: TImage;
    SkLabel3: TSkLabel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Panel1: TPanel;
    PanelSearchComponents: TPanel;
    SearchBoxComponents: TSearchBox;
    ActivityIndicatorSearch: TActivityIndicator;
    SkPaintBackground: TSkPaintBox;
    PanelPaleta: TPanel;
    Image6: TImage;
    Image11: TImage;
    Image13: TImage;
    Image14: TImage;
    Image15: TImage;
    Image16: TImage;
    procedure BtnRenderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SkPaintBox2Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure ImgSettingsClick(Sender: TObject);
    procedure Image9Click(Sender: TObject);
    procedure Image5Click(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure Image12Click(Sender: TObject);
    procedure SkPaintBackgroundDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure Image7Click(Sender: TObject);
    procedure SearchBoxComponentsChange(Sender: TObject);
    procedure SkPaintBackgroundResize(Sender: TObject);
    procedure PanelPaletaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelPaletaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelPaletaMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FBuilder : TUIBuilderEngine;
    FCreatedForms: TObjectList<TForm>;
    FTreeIndexer :TTreeViewIndexer;
    FDragging: Boolean;
    FDragOffset: TPoint;
    procedure RenderJson(const Atext : string);
    procedure CloseFormsCreated;
    procedure AddJSONToTreeView(JSONValue: TJSONValue; ParentNode: TTreeNode; NodeName: string; TreeView: TTreeView );
    procedure ValidateAndProcessJSON(const AJSON: string);

  public
    destructor Destroy; override;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

{ TForm2 }

procedure TForm2.AddJSONToTreeView(JSONValue: TJSONValue; ParentNode: TTreeNode; NodeName: string; TreeView: TTreeView );
var
  JSONObject: TJSONObject;
  JSONArray: TJSONArray;
  NewNode: TTreeNode;
begin
  TreeView.Items.BeginUpdate;
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := TJSONObject(JSONValue);
      NewNode := TreeView1.Items.AddChild(ParentNode, NodeName);

      // Percorre todos os pares (chave-valor) do objeto JSON
      for var JSONPair in JSONObject do
      begin
        AddJSONToTreeView(JSONPair.JSONValue, NewNode, JSONPair.JSONString.Value,TreeView);
      end;
    end
    else if JSONValue is TJSONArray then
    begin
      JSONArray := TJSONArray(JSONValue);
      NewNode := TreeView1.Items.AddChild(ParentNode, NodeName + ' (Array)');

      // Percorre todos os elementos do array
      for var I := 0 to JSONArray.Count - 1 do
      begin
        AddJSONToTreeView(JSONArray.Items[i], NewNode, Format('[%d]', [i]),TreeView);
      end;
    end
    else if JSONValue is TJSONString then
    begin
      TreeView.Items.AddChild(ParentNode, Format('%s: %s', [NodeName, TJSONString(JSONValue).Value]));
    end
    else if JSONValue is TJSONNumber then
    begin
      TreeView.Items.AddChild(ParentNode, Format('%s: %s', [NodeName, TJSONNumber(JSONValue).ToString]));
    end
    else if JSONValue is TJSONBool then
    begin
      TreeView.Items.AddChild(ParentNode, Format('%s: %s', [NodeName, BoolToStr(TJSONBool(JSONValue).AsBoolean, True)]));
    end
    else if JSONValue is TJSONNull then
    begin
      TreeView.Items.AddChild(ParentNode, NodeName + ': null');
    end;
  finally
    TreeView.Items.EndUpdate;
  end;
end;

procedure TForm2.BtnRenderClick(Sender: TObject);
begin
  RenderJson(Memo.Lines.text);
end;

procedure TForm2.CloseFormsCreated;
begin
  for var i := FCreatedForms.Count - 1 downto 0 do
  begin
    FCreatedForms[i].Close;
    Application.ProcessMessages;
  end;

   if not FCreatedForms.IsEmpty then
      FCreatedForms.Clear;
end;

destructor TForm2.Destroy;
begin
  FTreeIndexer.Free;
  FCreatedForms.Free;
  FBuilder.Free;
  inherited;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
   FCreatedForms := TObjectList<TForm>.Create(False);
   FTreeIndexer:= TTreeViewIndexer.Create(TreeView1);
   Self.DoubleBuffered := True;
end;

procedure TForm2.Image12Click(Sender: TObject);
begin
  PanelRenderJson.Visible:= False;
end;

procedure TForm2.Image3Click(Sender: TObject);
begin
  PanelRenderJson.Visible:= True;
end;

procedure TForm2.Image5Click(Sender: TObject);
begin
  PanelTree.Visible:= False;
end;

procedure TForm2.Image7Click(Sender: TObject);
begin
  PanelSearchComponents.Visible:= not PanelSearchComponents.Visible;
end;

procedure TForm2.ImgSettingsClick(Sender: TObject);
begin
  if not SplitView1.Opened then
  begin
    lblRenderJson.Visible:= True;
    lblOpenModel.Visible:= True;
    lblTreeJson.Visible:= True;
  end else
  begin
    lblRenderJson.Visible:= False;
    lblOpenModel.Visible:= False;
    lblTreeJson.Visible:= False;
  end;

  SplitView1.Opened := not SplitView1.Opened;
end;

procedure TForm2.Image9Click(Sender: TObject);
begin
  PanelTree.Visible:= not PanelTree.Visible;
end;

procedure TForm2.MemoChange(Sender: TObject);
begin
  BtnRender.Enabled:= false;
  try
    try
      ValidateAndProcessJSON(Memo.Lines.Text);
    except
      BtnRender.Enabled:= true;
    end;
  finally
    BtnRender.Enabled:= true;
  end;
end;

procedure TForm2.PanelPaletaMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragging := True;
    FDragOffset := Point(X, Y);
    PanelPaleta.Cursor := crSizeAll;
  end;
end;

procedure TForm2.PanelPaletaMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FDragging then
  begin
    PanelPaleta.Left := PanelPaleta.Left + (X - FDragOffset.X);
    PanelPaleta.Top := PanelPaleta.Top + (Y - FDragOffset.Y);
  end;
end;

procedure TForm2.PanelPaletaMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragging := False;
    PanelPaleta.Cursor := crDefault;
  end;
end;

procedure TForm2.RenderJson(const Atext : string);
begin
  var UIControl: TControl;
  var JsonText := Atext;
  var Json := TJSONObject.ParseJSONValue(JsonText) as TJSONObject;
  var MyForm: TForm;
  var FormsArray: TJSONArray;
  var FormJson: TJSONObject;
  var Node: TTreeNode;
  UIControl:= Nil;
  CloseFormsCreated;
  TreeView1.Items.Clear;
  TFormCloseAdapter.TreeView := TreeView1;
  TFormCloseAdapter.FCreatedForm := FCreatedForms;
  try
    if Json.TryGetValue<TJSONArray>('Forms', FormsArray) then
    begin
      for var I := 0 to FormsArray.Count - 1 do
      begin
        FormJson := FormsArray.Items[I] as TJSONObject;
        MyForm := FBuilder.CreateFormFromJson(Application, FormJson);

        Node := TreeView1.Items.Add(nil, FormJson.GetValue<string>('Name'));
        Node.Data := MyForm;
        AddJSONToTreeView(FormJson,Node,'root',TreeView1);

        MyForm.OnClose := TFormCloseAdapter.HandleClose;
        FCreatedForms.Add(MyForm);
        MyForm.Show;
      end;
    end else
    begin
      Node := TreeView1.Items.Add(nil, Json.GetValue<string>('Name'));
      MyForm := FBuilder.CreateFormFromJson(Application, Json);
      Node.Data := MyForm;
      AddJSONToTreeView(Json,Node,'root',TreeView1);

      MyForm.OnClose := TFormCloseAdapter.HandleClose;
      FCreatedForms.Add(MyForm);
      MyForm.Show;
    end;
  finally
    TreeView1.AutoExpand:= True;
    Json.Free;
    FTreeIndexer.RefreshIndex;
  end
end;

procedure TForm2.SearchBoxComponentsChange(Sender: TObject);
begin
  if String(SearchBoxComponents.Text).Equals('') then Exit;
  try
    ActivityIndicatorSearch.Animate:= True;
    TThread.Queue(TThread.CurrentThread,
    procedure
    begin
       FTreeIndexer.SelectComponentByName(SearchBoxComponents.Text);
    end)
  finally
    ActivityIndicatorSearch.Animate:= False;
  end;
end;

procedure TForm2.SkPaintBackgroundDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
const
  GridSize = 20; // espaçamento entre linhas
var
  Paint: ISkPaint;
  X, Y: Single;
begin
  // Cor de fundo
  ACanvas.Clear($FFF5F5F5); // cor clara tipo Delphi Panel

  // Configura o pincel
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.Color := $FFDDDDDD;
  Paint.StrokeWidth := 1;

  // Linhas verticais
  X := ADest.Left;
  while X <= ADest.Right do
  begin
    ACanvas.DrawLine(X, ADest.Top, X, ADest.Bottom, Paint);
    X := X + GridSize;
  end;

  // Linhas horizontais
  Y := ADest.Top;
  while Y <= ADest.Bottom do
  begin
    ACanvas.DrawLine(ADest.Left, Y, ADest.Right, Y, Paint);
    Y := Y + GridSize;
  end;

end;



procedure TForm2.SkPaintBackgroundResize(Sender: TObject);
begin
   SkPaintBackground.Invalidate;
end;

//procedure TForm2.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
//  const ADest: TRectF; const AOpacity: Single);
//var
//  LBorderPaint, LPanelPaint: ISkPaint;
//  LOuterRect, LInnerRect: TRectF;
//  LShader: ISkShader;
//  BorderWidth, CornerRadius: Single;
//begin
//  BorderWidth := 6;         // espessura da borda
//  CornerRadius := 6;        // cantos suavemente arredondados
//
//  // Área total do componente
//  LOuterRect := ADest;
//  InflateRect(LOuterRect, -2, -2); // margens externas para evitar corte
//
//  // Criando gradiente sweep na borda
//  LShader := TSkShader.MakeGradientSweep(
//    PointF(LOuterRect.CenterPoint.X, LOuterRect.CenterPoint.Y),
//    [$FFFCE68D, $FFF7CAA5, $FF2EBBC1, $FFFCE68D]
//  );
//
//  // Pintura da borda
//  LBorderPaint := TSkPaint.Create;
//  //LBorderPaint.IsAntialias := True;
//  LBorderPaint.Shader := LShader;
//  ACanvas.DrawRoundRect(LOuterRect, CornerRadius, CornerRadius, LBorderPaint);
//
//  // Área interna do "painel"
//  LInnerRect := LOuterRect;
//  InflateRect(LInnerRect, -BorderWidth, -BorderWidth);
//
//  // Pintura do conteúdo (como se fosse o fundo do painel)
//  LPanelPaint := TSkPaint.Create;
//  LPanelPaint.Color := $FFF5F5F5; // cor padrão de panel mais clara
//  //LPanelPaint.IsAntialias := True;
//
//  ACanvas.DrawRoundRect(LInnerRect, CornerRadius, CornerRadius, LPanelPaint);
//end;

procedure TForm2.SkPaintBox2Draw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  LBorderPaint, LPanelPaint: ISkPaint;
  LOuterRect, LInnerRect: TRectF;
  LShader: ISkShader;
  BorderWidth, CornerRadius: Single;
begin
  BorderWidth := 6;         // espessura da borda
  CornerRadius := 6;        // cantos suavemente arredondados

  // Área total do componente
  LOuterRect := ADest;
  InflateRect(LOuterRect, -2, -2); // margens externas para evitar corte

  // Criando gradiente sweep na borda
  LShader := TSkShader.MakeGradientSweep(
    PointF(LOuterRect.CenterPoint.X, LOuterRect.CenterPoint.Y),
    [$FFFCE68D, $FFF7CAA5, $FF2EBBC1, $FFFCE68D]
  );

  // Pintura da borda
  LBorderPaint := TSkPaint.Create;
  //LBorderPaint.IsAntialias := True;
  LBorderPaint.Shader := LShader;
  ACanvas.DrawRoundRect(LOuterRect, CornerRadius, CornerRadius, LBorderPaint);

  // Área interna do "painel"
  LInnerRect := LOuterRect;
  InflateRect(LInnerRect, -BorderWidth, -BorderWidth);

  // Pintura do conteúdo (como se fosse o fundo do painel)
  LPanelPaint := TSkPaint.Create;
  LPanelPaint.Color := $FFFFFFFF;; // cor padrão de panel mais clara
  //LPanelPaint.IsAntialias := True;

  ACanvas.DrawRoundRect(LInnerRect, CornerRadius, CornerRadius, LPanelPaint);
end;

procedure TForm2.ValidateAndProcessJSON(const AJSON: string);
begin
    TThread.CreateAnonymousThread(
    procedure
    var
      IsValid: Boolean;
      ErrorMsg: string;
    begin
      SkLblVerify.Caption:= 'Analizing json ... ';
      try
        TThread.Queue(nil,
          procedure
          begin
            IsValid := TJSONHelper.ValidateJSON(AJSON, ErrorMsg);
            if IsValid then
            begin
              SkLblVerify.Caption:= 'Valid (RFC 8259)';
              Self.PanelValidateJson.Color:= clgreen;
              Self.PanelValidateJson.Repaint;
            end else
            begin
              SkLblVerify.Caption:= 'InValid Valid (RFC 8259)';
              Self.PanelValidateJson.Color:= clred;
              Self.PanelValidateJson.repaint;
            end;
          end);
      except
        on E: Exception do
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Erro: ' + E.Message);
            end);
      end;
    end).Start;

  end;

{ TTreeViewIndexer }

procedure TTreeViewIndexer.BuildIndex;
var
  LNode: TTreeNode;
  LComponent: TComponent;
begin
  FIndex.Clear;

  for var i := 0 to FTreeView.Items.Count - 1 do
  begin
    LNode := FTreeView.Items[i];

    // Verifica se o nó está associado a um componente
    if (LNode.Data <> nil) and (TObject(LNode.Data) is TComponent) then
    begin
      LComponent := TComponent(LNode.Data);

      // Indexa pelo nome do componente
      if LComponent.Name <> '' then
        FIndex.AddOrSetValue(LComponent.Name, LNode);
    end;
  end;
end;

constructor TTreeViewIndexer.Create(ATreeView: TTreeView);
begin
  inherited Create;
  FTreeView := ATreeView;
  FIndex := TDictionary<string, TTreeNode>.Create;
  BuildIndex;
end;

destructor TTreeViewIndexer.Destroy;
begin
  FIndex.Free;
  inherited;
end;

function TTreeViewIndexer.FindNodeByComponentName(const AName: string): TTreeNode;
begin
  if not FIndex.TryGetValue(AName.ToLower, Result) then
    Result := nil;
end;

procedure TTreeViewIndexer.RefreshIndex;
begin
//  TTask.Run(procedure
//            begin
//              BuildIndex;
//            end);
  BuildIndex;
end;

function TTreeViewIndexer.SelectComponentByName(const AName: string): string;
var
  LNode: TTreeNode;
begin
  LNode := FindNodeByComponentName(AName);
  if Assigned(LNode) then
  begin
    FTreeView.Selected := LNode;
    LNode.MakeVisible;
    FTreeView.SetFocus;
  end;
end;

{ TSkCachedBox }

procedure TSkCachedBox.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);
var
  Paint: ISkPaint;
  Shader: ISkShader;
  Radius: Single;
begin
  Radius := 10;

  Shader := TSkShader.MakeGradientSweep(
    PointF(ADest.CenterPoint.X, ADest.CenterPoint.Y),
    [$FFAAAAAA, $FFCCCCCC, $FFEFEFEF, $FFCCCCCC, $FFAAAAAA]
  );

  Paint := TSkPaint.Create;
  Paint.Shader := Shader;
  //Paint.IsAntialias := True;

  ACanvas.DrawRoundRect(ADest, Radius, Radius, Paint);
end;

end.
