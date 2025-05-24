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
    Panel7: TPanel;
    ImageRenderJson: TImage;
    Panel12: TPanel;
    ImageOpenTemplate: TImage;
    PanelTree: TPanel;
    Panel11: TPanel;
    ImageExpand: TImage;
    ImageColapse: TImage;
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
    PanelToolPalette: TPanel;
    Image6: TImage;
    Image11: TImage;
    Image13: TImage;
    Image14: TImage;
    Image15: TImage;
    Image16: TImage;
    PanelSettings: TPanel;
    SkLabelSettings: TSkLabel;
    Panel3: TPanel;
    Image1: TImage;
    Image2: TImage;
    SkLabel2: TSkLabel;
    SkPaintBox1: TSkPaintBox;
    procedure BtnRenderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SkPaintBox2Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure ImgSettingsClick(Sender: TObject);
    procedure Image9Click(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure ImageRenderJsonClick(Sender: TObject);
    procedure Image12Click(Sender: TObject);
    procedure SkPaintBackgroundDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure SearchBoxComponentsChange(Sender: TObject);
    procedure SkPaintBackgroundResize(Sender: TObject);
    procedure PanelToolPaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelToolPaletteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelToolPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
    procedure TreeView1Click(Sender: TObject);
    procedure ImageExpandClick(Sender: TObject);
    procedure ImageColapseClick(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SkPaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FBuilder : TUIBuilderEngine;
    FCreatedForms: TObjectList<TForm>;
    FDragging: Boolean;
    FDragOffset: TPoint;
    FGridBackground: ISkPicture;
    FSelecionadoShape: TShape;

    procedure RenderJson(const Atext : string);
    procedure CloseFormsCreated;
    procedure AddJSONToTreeView(JSONValue: TJSONValue; ParentNode: TTreeNode; NodeName: string; TreeView: TTreeView );
    function FindNodeRecursive(ParentNode: TTreeNode; const AText: string): TTreeNode;

    function EncontrarNoPorComponente(TreeView: TTreeView; const NomeComponente: string): TTreeNode;
    procedure PesquisarFormularioDinamico(const ATerm: string );
    procedure ExpandirPais(Node: TTreeNode);
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
  FCreatedForms.Free;
  FBuilder.Free;
  inherited;
end;


function TForm2.EncontrarNoPorComponente(TreeView: TTreeView;const NomeComponente: string): TTreeNode;
begin
  Result := nil;
  // Percorre todos os nós do TreeView
  for var i := 0 to TreeView.Items.Count - 1 do
  begin
    var NodeText := TreeView.Items[i].Text;

    // Remove o prefixo "Name: " se existir
    if NodeText.StartsWith('Name: ') then
      NodeText := Copy(NodeText, 7, Length(NodeText));

    // Verifica se o texto do nó corresponde ao nome do componente
    if SameText(NodeText, NomeComponente) then
    begin
      // Verifica se o nó tem um componente associado (opcional)
      if (TreeView.Items[i].Data <> nil) and (TObject(TreeView.Items[i].Data) is TComponent) then
      begin
        Result := TreeView.Items[i];
        Exit;
      end
      else if TreeView.Items[i].Data = nil then
      begin
        // Se não há data associada, mas o nome bate, retorna mesmo assim
        Result := TreeView.Items[i];
        Exit;
      end;
    end;
  end;
end;


procedure TForm2.ExpandirPais(Node: TTreeNode);
var
  ParentNode: TTreeNode;
begin
  if Node = nil then Exit;

  ParentNode := Node.Parent;
  while ParentNode <> nil do
  begin
    ParentNode.Expand(False);
    ParentNode := ParentNode.Parent;
  end;
end;


function TForm2.FindNodeRecursive(ParentNode: TTreeNode;
  const AText: string): TTreeNode;
var
  Child: TTreeNode;
begin
  Result := nil;

  if SameText(ParentNode.Text, AText) then
    Exit(ParentNode);

  Child := ParentNode.GetFirstChild;
  while Assigned(Child) do
  begin
    Result := FindNodeRecursive(Child, AText);
    if Assigned(Result) then
      Exit;
    Child := Child.GetNextSibling;
  end;
end;

procedure TForm2.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if not FDragging then
  begin
    PanelToolPalette.Left := (ClientWidth - PanelToolPalette.Width) div 2;
    PanelToolPalette.Top := ClientHeight - PanelToolPalette.Height -60;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  Recorder: ISkPictureRecorder;
  Canvas: ISkCanvas;
begin
   FCreatedForms := TObjectList<TForm>.Create(False);
   Self.DoubleBuffered := True;
  Recorder := TSkPictureRecorder.Create;
  Canvas := Recorder.BeginRecording(TRectF.Create(0, 0, SkPaintBackground.Width, SkPaintBackground.Height));

  // Desenha um grid 20x20 por exemplo
  Canvas.Save;
  for var I := 0 to Trunc(SkPaintBackground.Width / 20) do
    Canvas.DrawLine(I * 20, 0, I * 20, SkPaintBackground.Height, TSkPaint.Create);
  for var I := 0 to Trunc(SkPaintBackground.Height / 20) do
    Canvas.DrawLine(0, I * 20, SkPaintBackground.Width, I * 20, TSkPaint.Create);
  Canvas.Restore;

  FGridBackground := Recorder.FinishRecording;
end;

procedure TForm2.FormShow(Sender: TObject);
begin

//  PanelToolPalette.Left := (ClientWidth - PanelToolPalette.Width) div 2;
//  PanelToolPalette.Top := ClientHeight - PanelToolPalette.Height -5;
end;

procedure TForm2.Image12Click(Sender: TObject);
begin
  PanelRenderJson.Visible:= False;
end;

procedure TForm2.Image2Click(Sender: TObject);
begin
  PanelSearchComponents.Visible:= not PanelSearchComponents.Visible;
end;

procedure TForm2.ImageRenderJsonClick(Sender: TObject);
begin
  PanelRenderJson.Visible:= True;
end;

procedure TForm2.ImgSettingsClick(Sender: TObject);
begin
  if not SplitView1.Opened then
    PanelSettings.Visible:= True
  else
    PanelSettings.Visible:= False;
  SplitView1.Opened := not SplitView1.Opened;
end;

procedure TForm2.Image9Click(Sender: TObject);
begin
  PanelTree.Visible:= not PanelTree.Visible;
end;

procedure TForm2.ImageColapseClick(Sender: TObject);
begin
  TreeView1.Items.BeginUpdate;
  try
    for var i := 0 to TreeView1.Items.Count - 1 do
      TreeView1.Items[i].Collapse(True); // True para colapsar recursivamente
  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TForm2.ImageExpandClick(Sender: TObject);
begin
  TreeView1.Items.BeginUpdate;
  try
    for var i := 0 to TreeView1.Items.Count - 1 do
      TreeView1.Items[i].Expand(True); // True para expandir recursivamente
  finally
    TreeView1.Items.EndUpdate;
  end;
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

procedure TForm2.PanelToolPaletteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragging := True;
    FDragOffset := Point(X, Y);
    PanelToolPalette.Cursor := crSizeAll;
  end;
end;

procedure TForm2.PanelToolPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FDragging then
  begin
    PanelToolPalette.Left := PanelToolPalette.Left + (X - FDragOffset.X);
    PanelToolPalette.Top := PanelToolPalette.Top + (Y - FDragOffset.Y);
  end;
end;

procedure TForm2.PanelToolPaletteMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragging := False;
    PanelToolPalette.Cursor := crDefault;
  end;
end;

procedure TForm2.PesquisarFormularioDinamico(const ATerm: string);
var
  NoEncontrado: TTreeNode;
begin
  NoEncontrado:= EncontrarNoPorComponente(TreeView1, ATerm);

  if NoEncontrado <> nil then
  begin
    ExpandirPais(NoEncontrado);
    TreeView1.Selected := NoEncontrado;
    NoEncontrado.MakeVisible;
    TreeView1.SetFocus;
  end;
end;

procedure TForm2.RenderJson(const Atext : string);
begin
  var JsonText := Atext;
  var Json := TJSONObject.ParseJSONValue(JsonText) as TJSONObject;
  var MyForm: TForm;
  var FormsArray: TJSONArray;
  var FormJson: TJSONObject;
  var Node: TTreeNode;
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
        MyForm := FBuilder.CreateFormFromJson(SkPaintBackground, FormJson);

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
      MyForm := FBuilder.CreateFormFromJson(SkPaintBackground, Json);
      Node.Data := MyForm;
      AddJSONToTreeView(Json,Node,'root',TreeView1);

      MyForm.OnClose := TFormCloseAdapter.HandleClose;
      FCreatedForms.Add(MyForm);
      MyForm.Show;
    end;
  finally
    TreeView1.AutoExpand:= True;
    Json.Free;
  end
end;

procedure TForm2.SearchBoxComponentsChange(Sender: TObject);
begin
  if String(SearchBoxComponents.Text).Equals('') then Exit;

  ActivityIndicatorSearch.Animate:= True;
  try
    PesquisarFormularioDinamico(SearchBoxComponents.Text);
  finally
    ActivityIndicatorSearch.Animate:= False;
  end;
end;

procedure TForm2.SkPaintBackgroundDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
const GridSize = 20; // espaçamento entre linhas
var Paint: ISkPaint;
begin
  // Cor de fundo
  ACanvas.Clear($FFF5F5F5); // cor clara tipo Delphi Panel

  // Configura o pincel
  Paint := TSkPaint.Create;
  Paint.Style := TSkPaintStyle.Stroke;
  Paint.Color := $FFDDDDDD;
  Paint.StrokeWidth := 1;

  // Linhas verticais
  var X := ADest.Left;
  while X <= ADest.Right do
  begin
    ACanvas.DrawLine(X, ADest.Top, X, ADest.Bottom, Paint);
    X := X + GridSize;
  end;

  // Linhas horizontais
  var Y := ADest.Top;
  while Y <= ADest.Bottom do
  begin
    ACanvas.DrawLine(ADest.Left, Y, ADest.Right, Y, Paint);
    Y := Y + GridSize;
  end;
end;



procedure TForm2.SkPaintBackgroundResize(Sender: TObject);
begin

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

procedure TForm2.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
begin
  var LPaint: ISkPaint := TSkPaint.Create;
  LPaint.Shader := TSkShader.MakeGradientSweep(ADest.CenterPoint,
  [$FFF2F2F2, $FFCCCCCC, $FF999999, $FFCCCCCC, $FFF2F2F2]);
  ACanvas.DrawPaint(LPaint);
end;

procedure TForm2.SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := True;
  FDragOffset := Point(Round(X), Round(Y));
end;

procedure TForm2.SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  NewLeft, NewTop: Integer;
begin
  if FDragging then
  begin
    NewLeft := SkPaintBox1.Left + Round(X) - FDragOffset.X;
    NewTop := SkPaintBox1.Top + Round(Y) - FDragOffset.Y;
    SkPaintBox1.SetBounds(NewLeft, NewTop, SkPaintBox1.Width, SkPaintBox1.Height);
  end;
end;

procedure TForm2.SkPaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
end;

procedure TForm2.SkPaintBox2Draw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
//var
//  LBorderPaint, LPanelPaint: ISkPaint;
//  LOuterRect, LInnerRect: TRectF;
//  LShader: ISkShader;
//  BorderWidth, CornerRadius: Single;
begin
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
//  LPanelPaint.Color := $FFFFFFFF;; // cor padrão de panel mais clara
//  //LPanelPaint.IsAntialias := True;
//
//  ACanvas.DrawRoundRect(LInnerRect, CornerRadius, CornerRadius, LPanelPaint);
end;

procedure TForm2.TreeView1Click(Sender: TObject);
var
  Node: TTreeNode;
  NomeReal: string;
  C: TComponent;

  function BuscarComponentePorNome(Root: TComponent; const Nome: string): TComponent;

    function BuscaRecursivaComponente(Comp: TComponent): TComponent;
    var
      I: Integer;
    begin
      Result := nil;

      if SameText(Comp.Name, Nome) then
        Exit(Comp);

      if Comp is TWinControl then
      begin
        for I := 0 to TWinControl(Comp).ControlCount - 1 do
        begin
          Result := BuscaRecursivaComponente(TWinControl(Comp).Controls[I]);
          if Assigned(Result) then
            Exit;
        end;
      end;

      for I := 0 to Comp.ComponentCount - 1 do
      begin
        Result := BuscaRecursivaComponente(Comp.Components[I]);
        if Assigned(Result) then
          Exit;
      end;
    end;

  begin
    Result := BuscaRecursivaComponente(Root);
  end;

  procedure CriarMarcador(AOwner: TForm);
  begin
    if not Assigned(FSelecionadoShape) then
    begin
      FSelecionadoShape := TShape.Create(AOwner);
      FSelecionadoShape.Parent := AOwner;
      FSelecionadoShape.Visible := False;
    end;
  end;

  procedure DestacarComponenteSelecionado(Control: TControl);
    begin
      CriarMarcador(Self); // ou o form atual

      FSelecionadoShape.SetBounds(
        Control.Left - 2,
        Control.Top - 2,
        Control.Width + 4,
        Control.Height + 4
      );
      FSelecionadoShape.Parent := Control.Parent;
      FSelecionadoShape.Brush.Style := bsClear;
      FSelecionadoShape.Pen.Color := clGrayText;
      FSelecionadoShape.Pen.Width := 1;
      FSelecionadoShape.Pen.Mode:= pmMask;
      FSelecionadoShape.Pen.Style := psDot;
      FSelecionadoShape.Visible := True;
      FSelecionadoShape.BringToFront;
    end;

begin
  Node := TreeView1.Selected;
  if Node <> nil then
  begin
    NomeReal := Node.Text;
    if Pos('Name: ', NomeReal) = 1 then
    begin
      NomeReal := Copy(NomeReal, 7, Length(NomeReal));
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
              FSelecionadoShape.Visible := False;
            end else
              DestacarComponenteSelecionado(TControl(C));
          end;
        end;
      end;
    end;
  end;
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
            Self.BtnRender.Click();
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


end.
