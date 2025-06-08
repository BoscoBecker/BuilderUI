unit Adapter.TreeViewAdapter;

interface

uses  Vcl.Graphics,Vcl.Controls, Vcl.Forms,
      Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
      Vcl.ExtCtrls, Buttons,

      System.Json, System.SysUtils, System.Classes,
      System.Generics.Collections ;

type
  TTreeViewAdapterHelper = class helper for TTreeView
    procedure Expand;
    procedure Colapse;
 end;

type
  TTreeViewAdapter = class
    public class var FTreeView: TTreeView;
    public class var FCreatedForm: TObjectList<TForm>;
    public procedure CloseFormsTreeview(Sender: TObject; var Action: TCloseAction);
    public class procedure AddJSONToTreeView(JSONValue: TJSONValue; ParentNode: TTreeNode; NodeName: string; FTreeView: TTreeView ); static;
    public class procedure ExpandFathers(Node: TTreeNode); static;
    public class procedure FindComponentInTreeView(const Aterm : string); static;
    public class function FindNodeByComponent(TreeView: TTreeView;const ComponentName: string): TTreeNode; static;
    public class procedure AddFormsToTreeView(Forms: TObjectList<TForm>; TreeView: TTreeView); static;
    public procedure CopyRootNodes(SourceTree, TargetTree: TTreeView);
  end;

implementation

class procedure TTreeViewAdapter.AddJSONToTreeView(JSONValue: TJSONValue;   ParentNode: TTreeNode; NodeName: string; FTreeView: TTreeView);
var
  JSONObject: TJSONObject;
  JSONArray: TJSONArray;
  NewNode: TTreeNode;
begin
  FTreeView.Items.BeginUpdate;
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := TJSONObject(JSONValue);
      NewNode := FTreeView.Items.AddChild(ParentNode, NodeName);

      for var JSONPair in JSONObject do
        AddJSONToTreeView(JSONPair.JSONValue, NewNode, JSONPair.JSONString.Value,FTreeView);
    end else if JSONValue is TJSONArray then
    begin
      JSONArray := TJSONArray(JSONValue);
      NewNode := FTreeView.Items.AddChild(ParentNode, NodeName + ' (Array)');

      for var I := 0 to JSONArray.Count - 1 do
        AddJSONToTreeView(JSONArray.Items[i], NewNode, Format('[%d]', [i]),FTreeView);
    end else
    if JSONValue is TJSONString then
      FTreeView.Items.AddChild(ParentNode, Format('%s: %s', [NodeName, TJSONString(JSONValue).Value]))
    else
    if JSONValue is TJSONNumber then
      FTreeView.Items.AddChild(ParentNode, Format('%s: %s', [NodeName, TJSONNumber(JSONValue).ToString]))
    else
    if JSONValue is TJSONBool then
      FTreeView.Items.AddChild(ParentNode, Format('%s: %s', [NodeName, BoolToStr(TJSONBool(JSONValue).AsBoolean, True)]))
    else if JSONValue is TJSONNull then
      FTreeView.Items.AddChild(ParentNode, NodeName + ': null');
  finally
    FTreeView.Items.EndUpdate;
  end;
end;

procedure TTreeViewAdapter.CloseFormsTreeview(Sender: TObject; var Action: TCloseAction);
var
  Node: TTreeNode;
begin
  if not Assigned(FTreeView) or not Assigned(Sender) then Exit;
  if Sender is TForm then
  begin
    TForm(Sender).FormStyle:= fsMDIForm;
    TForm(Sender).Visible:= False;
    TForm(Sender).Top:=  -5000;

    for var I := FTreeView.Items.Count - 1 downto 0 do
    begin
      Node := FTreeView.Items[I];
      if Node.Data = Sender then
      begin
        FTreeView.Items.Delete(Node);
        Action := TCloseAction.caHide;
      end;
    end;
  end;
end;

class procedure TTreeViewAdapter.ExpandFathers(Node: TTreeNode);
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

class procedure TTreeViewAdapter.FindComponentInTreeView(const Aterm: string);
var
  NoEncontrado: TTreeNode;
begin
  NoEncontrado:= FindNodeByComponent(FTreeView, ATerm);

  if NoEncontrado <> nil then
  begin
    ExpandFathers(NoEncontrado);
    FTreeView.Selected := NoEncontrado;
    NoEncontrado.MakeVisible;
    if FTreeView.CanFocus then
      FTreeView.SetFocus;
  end;
end;

class function TTreeViewAdapter.FindNodeByComponent(TreeView: TTreeView;  const ComponentName: string): TTreeNode;
begin
  Result := nil;
  for var I :=  0 to TreeView.Items.Count - 1 do
  begin
    var NodeText := TreeView.Items[i].Text;

    if NodeText.StartsWith('Name: ') then
      NodeText := Copy(NodeText, 7, Length(NodeText));

    if SameText(NodeText, ComponentName) then
    begin
      if (TreeView.Items[i].Data <> nil) and (TObject(TreeView.Items[i].Data) is TComponent) then
      begin
        Result := TreeView.Items[i];
        Exit;
      end else if TreeView.Items[i].Data = nil then
      begin
        Result := TreeView.Items[i];
        Exit;
      end;
    end;
  end;
end;

class procedure TTreeViewAdapter.AddFormsToTreeView(Forms: TObjectList<TForm>; TreeView: TTreeView);
var
  RootNode, FormNode: TTreeNode;
  Form: TForm;
begin
  if not Assigned(TreeView) or not Assigned(Forms) then Exit;

  TreeView.Items.BeginUpdate;
  try
    RootNode := TreeView.Items.Add(nil, 'Forms');
    for Form in Forms do
    begin
      FormNode := TreeView.Items.AddChild(RootNode, Form.Name);
      FormNode.Data := Form;
    end;
    RootNode.Expand(False);
  finally
    TreeView.Items.EndUpdate;
  end;
end;

procedure TTreeViewAdapter.CopyRootNodes(SourceTree, TargetTree: TTreeView);
begin
  TargetTree.Items.BeginUpdate;
  try
    TargetTree.Items.Clear;
    for var I := 0 to SourceTree.Items.Count - 1 do
    begin
      var SourceNode := SourceTree.Items[I];
      if SourceNode.Parent = nil then
      begin
        var NewNode := TargetTree.Items.Add(Nil, SourceNode.Text);
        NewNode.Data := SourceNode.Data;
      end;
    end;
  finally
    TargetTree.Items.EndUpdate;
  end;
end;

procedure TTreeViewAdapterHelper.Colapse;
begin
  self.Items.BeginUpdate;
  try
    for var I := 0 to self.Items.Count - 1 do
      self.Items[i].Collapse(True);
  finally
    self.Items.EndUpdate;
  end;
end;

procedure TTreeViewAdapterHelper.Expand;
begin
  self.Items.BeginUpdate;
  try
    for var I := 0 to self.Items.Count - 1 do
      self.Items[i].Expand(True);
  finally
    self.Items.EndUpdate;
  end;
end;

end.
