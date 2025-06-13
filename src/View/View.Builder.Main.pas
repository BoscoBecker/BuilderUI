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
  Vcl.ExtCtrls, System.Threading, StrUtils,Clipbrd,

  Adapter.TreeViewAdapter,
  Builder.UIBuilderEngine,
  System.Skia, Vcl.Skia, System.Types, System.UITypes,
  Vcl.Imaging.pngimage, Vcl.WinXCtrls,

  Util.Json, Util.JSONValidator, System.Math, System.ImageList, Vcl.ImgList,
  View.Export.Forms, View.Menu.Context.Windows, View.Window.Json, SynEdit,
  SynEditHighlighter, SynHighlighterJSON,

  Service.Zoom, Service.Forms.Manager, Service.Skia.Draw, Service.JsonFile;

type
  TOrigRect = record
    Left, Top, Width, Height: Integer;
  end;

  TFormBuilderMain = class(TForm)
    StatusBarBottom: TStatusBar;
    PanelRenderJson: TPanel;
    SplitterRight: TSplitter;
    SplitViewMain: TSplitView;
    Panel6: TPanel;
    ImgSettings: TImage;
    PanelTreeComponents: TPanel;
    ImageTreeComponents: TImage;
    Panel10: TPanel;
    Image10: TImage;
    PanelLoad: TPanel;
    ImageRenderJson: TImage;
    PanelOpenTemplate: TPanel;
    ImageOpenTemplate: TImage;
    PanelTopRender: TPanel;
    ImageCloseRender: TImage;
    SkLabel3: TSkLabel;
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
    SkPaintBox1: TSkPaintBox;
    PanelTop: TPanel;
    SkLabel1: TSkLabel;
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
    ActivityIndicatorExplorer: TActivityIndicator;
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
    Memo: TMemo;
    PanelInfoValidation: TPanel;
    PanelInfoRenderToForms: TPanel;
    ImageErro: TImage;
    ImageOk: TImage;
    LabelInfoJson: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ImgSettingsClick(Sender: TObject);
    procedure ImageTreeComponentsClick(Sender: TObject);
    procedure MemoChange(Sender: TObject);
    procedure ImageRenderJsonClick(Sender: TObject);
    procedure ImageCloseRenderClick(Sender: TObject);
    procedure SkPaintBackgroundDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure SearchBoxComponentsChange(Sender: TObject);
    procedure PanelToolPaletteMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelToolPaletteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelToolPaletteMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure TreeViewExplorerClick(Sender: TObject);
    procedure ImageExpandClick(Sender: TObject);
    procedure ImageColapseClick(Sender: TObject);
    procedure ImageFilterExplorerClick(Sender: TObject);
    procedure SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    procedure SkPaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure SkPaintBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SkPaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure SkPaintBackgroundMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImageRenderClick(Sender: TObject);
    procedure ButtonRunJsonClick(Sender: TObject);
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
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ImageZoomInClick(Sender: TObject);
    procedure ImageZoomOutClick(Sender: TObject);
    procedure ImageHighSizeClick(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
    procedure ImageOkClick(Sender: TObject);
  private
    FBuilderBackground: TBuilderBackground;
    FTreeViewAdapter: TTreeViewAdapter;
    FJsonStructure : TJSONObject;
    FBuilder: TUIBuilderEngine;
    FSelectedComponent: String;
    FSelecionadoShape: TShape;
    FDragging: Boolean;
    FDragOffset: TPoint;
    FPaint: ISkPaint;
    FZoomService: TZoomService;
    FFormManager: TFormCreatedManager;
    OrigRectsForms: array of TOrigRect;
    procedure RenderJson(const Atext : string);
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

destructor TFormBuilderMain.Destroy;
begin
  if FSelecionadoShape <> nil then
    FSelecionadoShape.Free;
  if FTreeViewAdapter <> nil then
    FTreeViewAdapter.Free;
  if FJsonStructure <> nil then
    FJsonStructure.Free;
  if FFormManager <> nil then
    FFormManager.Free;
  if FZoomService <> nil then
    FZoomService.Free;
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
  FFormManager := TFormCreatedManager.Create;
  FZoomService := TZoomService.Create;
  SetBuilderBackground(bClear);
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
    SkPaintBackground.Width:= SkPaintBackground.Width + 1;
  end  else
  begin
    FBuilderBackground := bCLear;
    SkPaintBackground.Width:= SkPaintBackground.Width - 1;
  end;
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
  SkPaintBackground.Width:= SkPaintBackground.Width + 1;
end;

procedure TFormBuilderMain.ImageCloseExplorerClick(Sender: TObject);
begin
  PanelTree.Visible:= not PanelTree.Visible;
end;

procedure TFormBuilderMain.ImageZoomOutClick(Sender: TObject);
begin
  ZoomOut;
  SkPaintBackground.Width:= SkPaintBackground.Width - 1;
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
  var erromessage:='';
  if not Memo.lines.Text.Trim.Equals('') then
    if TJSONHelper.ValidateJSON(Memo.lines.Text, erromessage ) then
      Memo.lines.Text:= Util.Json.TJSONHelper.BeautifyJSON(Memo.lines.Text);
end;

procedure TFormBuilderMain.ImageClearJsonClick(Sender: TObject);
begin
  TJsonFileService.ClearMemoJson(Memo);
end;

procedure TFormBuilderMain.ImageRenderClick(Sender: TObject);
begin
  RenderJson(Memo.Lines.text);
end;

procedure TFormBuilderMain.ImageRenderJsonClick(Sender: TObject);
begin
  if not SplitViewmain.Opened then
    PanelRenderJson.Visible:= True
  else
    PanelRenderJson.Visible:= False;

  SplitViewmain.Opened := not SplitViewmain.Opened;
  SplitterRight.Visible:= True;
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

procedure TFormBuilderMain.ImageTreeComponentsClick(Sender: TObject);
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

          SetLength(OrigRectsForms, FFormManager.Forms.Count);
          OrigRectsForms[FFormManager.Forms.Count-1].Left   := MyForm.Left;
          OrigRectsForms[FFormManager.Forms.Count-1].Top    := MyForm.Top;
          OrigRectsForms[FFormManager.Forms.Count-1].Width  := MyForm.Width;
          OrigRectsForms[FFormManager.Forms.Count-1].Height := MyForm.Height;
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
begin
  TSkiaDrawService.DrawBackground(ACanvas, ADest, AOpacity, FBuilderBackground, FPaint);
end;

procedure TFormBuilderMain.SkPaintBackgroundMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  StatusBarBottom.Panels[0].Text := Format('X: %d Y: %d', [X, Y]);
end;

procedure TFormBuilderMain.SkPaintBox1Draw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  TSkiaDrawService.DrawGradientBox(ACanvas, ADest, AOpacity, FPaint);
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

procedure TFormBuilderMain.SkPaintBoxExplorerDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  TSkiaDrawService.DrawExplorerBorder(ACanvas, ADest, AOpacity, FPaint);
end;

procedure TFormBuilderMain.SkPaintBoxRenderJsonDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  TSkiaDrawService.DrawBoxRenderJsonBorder(ACanvas, ADest, AOpacity, FPaint);
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
  try
    var Invalids: TArray<string>;
    var IsValid := TTask.Future<Boolean>(function: Boolean
                                         begin
                                           Result := Util.JSONValidator
                                                           .TJSONHelper
                                                             .ValidateBuilderUIPattern(AJSON, Invalids);
                                         end).Value;
                    if not IsValid then
    begin
      LabelInfoJson.Caption := 'Invalid properties: ' + string.Join(sLineBreak, Invalids);
      ImageOk.Visible := False;
      ImageErro.Visible := True;
      FFormManager.CloseAll;
      TreeViewExplorer.Items.Clear;
      Exit;
    end;

    LabelInfoJson.Caption:= 'Analizing Json ';
    var ValidJson:= TTask.Future<Boolean>(function: Boolean
                                          begin
                                            Result := Util.JSONValidator
                                                            .TJSONHelper
                                                              .ValidateJSON(AJSON, ErrorMsg);
                                          end).Value;
    if not (ValidJson) or (AJSON.Trim.Equals('')) then
    begin
      LabelInfoJson.Caption:='Invalid json :'+ErrorMsg ;
      ImageOk.Visible:= False;
      ImageErro.Visible:= True;
      FFormManager.CloseAll;
      TreeViewExplorer.Items.Clear;
      Exit;
    end;

    var ValidNamesComponents:= TTask.Future<Boolean>(function: Boolean
                                                     begin
                                                       Result:= Util.Json
                                                                      .TJSONHelper
                                                                        .HasDuplicateNamesPerForm(AJSON, DupForms);
                                                     end).Value;
    if ValidNamesComponents then
    begin
      var duplicatenames: string;
      for var Info in DupForms do
        duplicatenames:= duplicatenames + string.Join(', ',  Info.DuplicatedNames);
      LabelInfoJson.Caption:='Invalid json, Duplicate Names : ' +duplicatenames;
      ImageOk.Visible:= False;
      ImageErro.Visible:= True;
      FFormManager.CloseAll;
      TreeViewExplorer.Items.Clear;
      Exit;
    end;

    LabelInfoJson.Caption:= 'Valid json';
    ImageOk.Visible:= True;
    ImageErro.Visible:= False;
  finally
    LabelInfoJson.Repaint;
  end;
end;

procedure TFormBuilderMain.ZoomIn;
begin
  FZoomService.ZoomIn;
  StatusBarBottom.Panels[4].Text := Format('Zoom: %d%%', [Round(FZoomService.GetZoom * 100)]);
  ApplyZoomToCreatedForms;
  SkPaintBackground.Invalidate;
end;

procedure TFormBuilderMain.ZoomOut;
begin
  FZoomService.ZoomOut;
  StatusBarBottom.Panels[4].Text := Format('Zoom: %d%%', [Round(FZoomService.GetZoom * 100)]);
  ApplyZoomToCreatedForms;
  SkPaintBackground.Invalidate;
end;

procedure TFormBuilderMain.ApplyZoomToCreatedForms;
begin
  if Length(OrigRectsForms) <> FFormManager.Forms.Count then Exit;
  for var I := 0 to FFormManager.Forms.Count - 1 do
  begin
    FFormManager.Forms[I].Left   := Round(OrigRectsForms[I].Left * FZoomService.GetZoom);
    FFormManager.Forms[I].Top    := Round(OrigRectsForms[I].Top * FZoomService.GetZoom);
    FFormManager.Forms[I].Width  := Round(OrigRectsForms[I].Width * FZoomService.GetZoom);
    FFormManager.Forms[I].Height := Round(OrigRectsForms[I].Height * FZoomService.GetZoom);
  end;
end;

end.

