unit Service.Component.PropertyExplorer;

interface

uses
  System.Classes, System.JSON, System.Generics.Collections, Data.DB, Dialogs,
  FireDAC.Comp.Client, Vcl.DBGrids, Vcl.StdCtrls,Synedit;

type
  TComponentPropertyExplorer = class
  private
    FDBGrid: TDBGrid;
    FMemTable: TFDMemTable;
    FDataSource: TDataSource;
    FSynEdit: TSynEdit;
    FJsonStructure: TJSONObject;
    FSelectedComponent: string;
    procedure InitializeDataStructure;
    procedure SetJsonStructure(const Value: TJSONObject);
  public
    constructor Create(ADBGrid: TDBGrid; SynEdit: TSynEdit);
    destructor Destroy; override;

    procedure LoadJsonToDBGrid(JsonObj: TJSONObject);
    procedure SaveDBGridToJson(JsonObj: TJSONObject);
    procedure SelectComponent(ComponentName: string);

    procedure DBGridCellClick(Column: TColumn);
    procedure DBGridKeyPress(Sender: TObject; var Key: Char);
    property JsonStructure: TJSONObject read FJsonStructure write SetJsonStructure;
  end;

implementation

uses
  System.SysUtils, Util.JSON;

constructor TComponentPropertyExplorer.Create(ADBGrid: TDBGrid; SynEdit: TSynEdit);
begin
  inherited Create;

  FDBGrid := ADBGrid;
  FSynEdit := SynEdit;

  FMemTable := TFDMemTable.Create(nil);
  FDataSource := TDataSource.Create(nil);

  InitializeDataStructure;

  FDBGrid.DataSource := FDataSource;
  FDBGrid.Options := [dgEditing, dgTitles, dgIndicator, dgColumnResize,
                     dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit];

  FDBGrid.OnKeyPress := DBGridKeyPress;
  FDBGrid.OnCellClick := DBGridCellClick;
end;

destructor TComponentPropertyExplorer.Destroy;
begin
  FreeAndNil(FMemTable);
  FreeAndNil(FDataSource);
  inherited;
end;

procedure TComponentPropertyExplorer.InitializeDataStructure;
begin
  with FMemTable.FieldDefs do
  begin
    Clear;
    Add('Key', ftString, 50);
    Add('Value', ftString, 255);
  end;

  FMemTable.CreateDataSet;
  FMemTable.FieldByName('Key').DisplayWidth :=10;
  FMemTable.FieldByName('Value').DisplayWidth := 50;
  FMemTable.Active := True;
  FDataSource.DataSet := FMemTable;
end;

procedure TComponentPropertyExplorer.LoadJsonToDBGrid(JsonObj: TJSONObject);
begin
  if not Assigned(JsonObj) then Exit;

  FMemTable.DisableControls;
  try
    FMemTable.EmptyDataSet;

    for var I := 0 to JsonObj.Count - 1 do
    begin
      FMemTable.Append;
      FMemTable.FieldByName('Key').AsString := JsonObj.Pairs[I].JsonString.Value;
      FMemTable.FieldByName('Value').AsString := JsonObj.Pairs[I].JsonValue.Value;
      FMemTable.Post;
    end;
  finally
    FMemTable.EnableControls;
  end;
end;

procedure TComponentPropertyExplorer.SaveDBGridToJson(JsonObj: TJSONObject);
begin
  if not Assigned(JsonObj) then Exit;
  try
    FMemTable.DisableControls;
    try
      FMemTable.First;
      while not FMemTable.Eof do
      begin
        var PropName := FMemTable.FieldByName('key').AsString;
        var PropValue := FMemTable.FieldByName('Value').AsString;

        var Pair := JsonObj.Get(PropName);
        if Assigned(Pair) then
        begin
          JsonObj.RemovePair(PropName).Free;
          JsonObj.AddPair(PropName, PropValue);
        end;
        FMemTable.Next;
      end;

      if Assigned(FSynEdit) and Assigned(FJsonStructure) then
        FSynEdit.Lines.Text := FJsonStructure.Format(2);
    finally
      FMemTable.EnableControls;
    end;
  except
    on E: Exception do
      raise Exception.Create('Erro ao salvar JSON: ' + E.Message);
  end;
end;

procedure TComponentPropertyExplorer.SelectComponent(ComponentName: string);
begin
  FSelectedComponent := ComponentName;

  if (ComponentName = '') or (not Assigned(FJsonStructure)) then
  begin
    FMemTable.EmptyDataSet;
    Exit;
  end;

  var ComponentJson := TJSONHelper.FindComponentJsonByName(FJsonStructure, ComponentName);
  if Assigned(ComponentJson) then
    LoadJsonToDBGrid(ComponentJson)
  else
    FMemTable.EmptyDataSet;
end;

procedure TComponentPropertyExplorer.SetJsonStructure(const Value: TJSONObject);
begin
  FJsonStructure := Value;
end;

procedure TComponentPropertyExplorer.DBGridCellClick(Column: TColumn);
begin
///
end;

procedure TComponentPropertyExplorer.DBGridKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
//    if (FSelectedComponent = '') or (not Assigned(FJsonStructure)) then Exit;
//    try
//      var ComponentJson := TJSONHelper.FindComponentJsonByName(FJsonStructure, FSelectedComponent);
//      if not Assigned(ComponentJson) then Exit;
//      SaveDBGridToJson(ComponentJson);
//    except
//      on E: Exception do ShowMessage('Erro ao salvar: ' + E.Message);
//    end;
  end;
end;



end.
