unit View.Builder.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Generics.Collections, System.JSON, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Buttons, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.Threading, StrUtils,

  Adapter.TreeViewAdapter,
  Builder.UIBuilderEngine,
  System.Skia, Vcl.Skia, System.Types, System.UITypes,
  Vcl.Imaging.pngimage, Vcl.WinXCtrls,

  Util.Json, System.Math, System.ImageList, Vcl.ImgList;

type  TBuilderBackground = ( bClear, bGrid);

type
  TFormBuilderMain = class(TForm)
    StatusBarBottom: TStatusBar;
    PanelRenderJson: TPanel;
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
    Panel13: TPanel;
    Image12: TImage;
    SkLabel3: TSkLabel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    PanelSearchComponents: TPanel;
    SearchBoxComponents: TSearchBox;
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
    Panel1: TPanel;
    SkLabel1: TSkLabel;
    SkPaintBackground: TSkPaintBox;
    Panel4: TPanel;
    LabelInfoJson: TLabel;
    ImageOk: TImage;
    ImageErro: TImage;
    Panel5: TPanel;
    PanelExecuteJson: TPanel;
    ActivityIndicatorExplorer: TActivityIndicator;
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
    procedure TreeView1Click(Sender: TObject);
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
    procedure PanelExecuteJsonClick(Sender: TObject);
    procedure Image6Click(Sender: TObject);
    procedure Image13Click(Sender: TObject);
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
    procedure RenderJson(const Atext : string);
    procedure CloseFormsCreated;
    procedure ValidateAndProcessJSON(const AJSON: string);
    procedure BuildStatusBar;
    procedure SetSelectedComponent(const Value: String);
    procedure SetBuilderBackground(const Value: TBuilderBackground);
  public
    destructor Destroy; override;
    property SelectedComponent: String read FSelectedComponent write SetSelectedComponent;
    property BuilderBackground: TBuilderBackground read FBuilderBackground write SetBuilderBackground;
  end;

var
  FormBuilderMain: TFormBuilderMain;

implementation

{$R *.dfm}

{ TForm2 }

procedure TFormBuilderMain.BuildStatusBar;
begin
  StatusBarBottom.Panels[0].Text:= 'X: 0 Y: 0';                 // Panel 0 - Coordenadas do Mouse
  StatusBarBottom.Panels[1].Text := 'Selecionado: Nenhum';       // Panel 1 - Componente Selecionado
  StatusBarBottom.Panels[2].Text := 'Tamanho: 0x0';              // Panel 2 - Tamanho do componente
  StatusBarBottom.Panels[3].Text := 'Date: ' +FormatDateTime('YYYY/MM/DD',now());             // Panel 3 - Snap/Grid
  StatusBarBottom.Panels[4].Text := 'Zoom: 100%';                // Panel 4 - Zoom
  StatusBarBottom.Panels[5].Text := 'Modo: Seleção';             // Panel 5 - Modo atual
  StatusBarBottom.Panels[6].Text := 'Projeto: (vazio)';          // Panel 6 - Nome do Projeto
  StatusBarBottom.Panels[7].Text := '';                          // Panel 7 - Mensagens temporárias
end;

procedure TFormBuilderMain.ButtonRunJsonClick(Sender: TObject);
begin
  RenderJson(Memo.Text);
end;

procedure TFormBuilderMain.CloseFormsCreated;
begin
  for var I := FCreatedForms.Count - 1 downto 0 do
  begin
    FCreatedForms[i].Close;
    Application.ProcessMessages;
  end;

  if not FCreatedForms.IsEmpty then
    FCreatedForms.Clear;
end;

destructor TFormBuilderMain.Destroy;
begin
  FSelecionadoShape.Free;
  FTreeViewAdapter.Free;
  FJsonStructure.Free;
  FCreatedForms.Free;
  FBuilder.Free;
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

procedure TFormBuilderMain.FormCreate(Sender: TObject);
begin
  FCreatedForms := TObjectList<TForm>.Create(False);
  SetBuilderBackground(bClear);
end;

procedure TFormBuilderMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  StatusBarBottom.Panels[0].Text := Format('X: %d Y: %d', [X, Y]);
end;

procedure TFormBuilderMain.FormShow(Sender: TObject);
begin
  BuildStatusBar();
end;

procedure TFormBuilderMain.Image12Click(Sender: TObject);
begin
  PanelRenderJson.Visible:= False;
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

procedure TFormBuilderMain.Image2Click(Sender: TObject);
begin
  PanelSearchComponents.Visible:= not PanelSearchComponents.Visible;
end;

procedure TFormBuilderMain.Image6Click(Sender: TObject);
begin
  if Screen.Cursor = crHandPoint then
    Screen.Cursor:= crDefault
  else
    Screen.Cursor:= crHandPoint;
end;

procedure TFormBuilderMain.ImageRenderClick(Sender: TObject);
begin
  RenderJson(Memo.Lines.text);
end;

procedure TFormBuilderMain.ImageRenderJsonClick(Sender: TObject);
begin
  PanelRenderJson.Visible:= True;
  Splitter2.Visible:= True;
end;

procedure TFormBuilderMain.ImgSettingsClick(Sender: TObject);
begin
  if not SplitView1.Opened then
    PanelSettings.Visible:= True
  else
    PanelSettings.Visible:= False;
  SplitView1.Opened := not SplitView1.Opened;
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
      TreeView1.Colapse;
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
      TreeView1.Expand;
    end)
  Finally
    ActivityIndicatorExplorer.Animate:= False;
  End;
end;

procedure TFormBuilderMain.MemoChange(Sender: TObject);
begin
  PanelExecuteJson.Enabled:= false;
  ValidateAndProcessJSON(Memo.Lines.Text);
end;

procedure TFormBuilderMain.PanelExecuteJsonClick(Sender: TObject);
begin
  RenderJson(memo.text);
end;

procedure TFormBuilderMain.PanelToolPaletteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragging := True;
    FDragOffset := Point(X, Y);
    PanelToolPalette.Cursor := crSizeAll;
  end;
end;

procedure TFormBuilderMain.PanelToolPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FDragging then
  begin
    PanelToolPalette.Left := PanelToolPalette.Left + (X - FDragOffset.X);
    PanelToolPalette.Top := PanelToolPalette.Top + (Y - FDragOffset.Y);
  end;
end;

procedure TFormBuilderMain.PanelToolPaletteMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
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

  var JsonText := Atext;
  FJsonStructure := TJSONObject.ParseJSONValue(JsonText) as TJSONObject;
  var MyForm: TForm;
  var FormsArray: TJSONArray;
  var FormJson: TJSONObject;
  var Node: TTreeNode;
  CloseFormsCreated;
  TreeView1.Items.Clear;
  FTreeViewAdapter.FTreeView := TreeView1;
  FTreeViewAdapter.FCreatedForm := FCreatedForms;
  try
    try
      if FJsonStructure.TryGetValue<TJSONArray>('Forms', FormsArray) then
      begin
        for var I := 0 to FormsArray.Count - 1 do
        begin
          FormJson := FormsArray.Items[I] as TJSONObject;
          MyForm := FBuilder.CreateFormFromJson(SkPaintBackground, FormJson);

          Node := TreeView1.Items.Add(nil, FormJson.GetValue<string>('Name'));
          Node.Data := MyForm;
          FTreeViewAdapter.AddJSONToTreeView(FormJson,Node,'root',TreeView1);

          MyForm.OnClose := FTreeViewAdapter.CloseFormsTreeview;
          FCreatedForms.Add(MyForm);
          MyForm.Show;
        end;
      end else
      begin
        Node := TreeView1.Items.Add(nil, FJsonStructure.GetValue<string>('Name'));
        MyForm := FBuilder.CreateFormFromJson(SkPaintBackground, FJsonStructure);
        Node.Data := MyForm;
        FTreeViewAdapter.AddJSONToTreeView(FJsonStructure,Node,'root',TreeView1);

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

procedure TFormBuilderMain.SetBuilderBackground(
  const Value: TBuilderBackground);
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
var
  Paint: ISkPaint;
begin
  case FBuilderBackground of
    bClear: ACanvas.Clear(TAlphaColors.White);
    bGrid:
    begin
      ACanvas.DrawRect(RectF(100, 100, 300, 200), TSkPaint.Create(TSkPaintStyle.Stroke));
      ACanvas.Clear($FFF5F5F5);
      Paint := TSkPaint.Create;
      Paint.Style := TSkPaintStyle.Stroke;
      Paint.Color := $FFDDDDDD;
      Paint.StrokeWidth := 1;
      var X := ADest.Left;
      while X <= ADest.Right do
      begin
        ACanvas.DrawLine(X, ADest.Top, X, ADest.Bottom, Paint);
        X := X + GridSize;
      end;
      var Y := ADest.Top;
      while Y <= ADest.Bottom do
      begin
        ACanvas.DrawLine(ADest.Left, Y, ADest.Right, Y, Paint);
        Y := Y + GridSize;
      end;
    end;
  end;
end;


procedure TFormBuilderMain.SkPaintBackgroundMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  StatusBarBottom.Panels[0].Text := Format('X: %d Y: %d', [X, Y]);
end;

procedure TFormBuilderMain.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  var LPaint: ISkPaint := TSkPaint.Create;
      LPaint.Shader := TSkShader.MakeGradientSweep(ADest.CenterPoint,
      [$FFF2F2F2, $FFCCCCCC, $FF999999, $FFCCCCCC, $FFF2F2F2]);
      ACanvas.DrawPaint(LPaint);
end;

procedure TFormBuilderMain.SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := True;
  FDragOffset := Point(Round(X), Round(Y));
end;

procedure TFormBuilderMain.SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
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

procedure TFormBuilderMain.SkPaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
end;

procedure TFormBuilderMain.TreeView1Click(Sender: TObject);
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
    if TreeView1.CanFocus then
      TreeView1.SetFocus;
    TreeView1.BringToFront;

    Node := TreeView1.Selected;
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
  TThread.CreateAnonymousThread(
  procedure
  var
    ErrorMsg: string;
    Duplicates: TArray<string>;
  begin
    LabelInfoJson.Caption:= 'Analizing json ... ';
    try
      if TJSONHelper.ValidateJSON(AJSON, ErrorMsg) and TJSONHelper.HasDuplicateNames(AJSON, Duplicates) then
      begin
        if String(Duplicates) = '' then
        LabelInfoJson.Caption:= 'Invalid json'
        else
          LabelInfoJson.Caption:= 'Invalid json, Has Duplicate Names : ' + string.Join(', ', Duplicates);
        ImageOk.Visible:= False;
        ImageErro.Visible:= True;
        PanelExecuteJson.Enabled:= False;
        PanelExecuteJson.Font.Color:= clgray;
        CloseFormsCreated;
        TreeView1.Items.Clear;
        Exit;
      end else
      begin
        LabelInfoJson.Caption:= 'Is Valid json';
        ImageOk.Visible:= True;
        ImageErro.Visible:= False;
        PanelExecuteJson.Font.Color:= clblack;
        PanelExecuteJson.Enabled:= True;
      end;
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

(************* Copied from AI Chat Window ***************
Tudo bem? 😁 O que posso fazer por você hoje?
*)
