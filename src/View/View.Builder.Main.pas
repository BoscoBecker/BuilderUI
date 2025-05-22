unit View.Builder.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Generics.Collections, System.JSON, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Buttons, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls,

  Adapter.TreeViewAdapter,
  Builder.UIBuilderEngine;

type
  TForm2 = class(TForm)
    TreeView1: TTreeView;
    Panel1: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
    Button1: TButton;
    Memo: TMemo;
    Splitter2: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FBuilder : TUIBuilderEngine;
    FCreatedForms: TObjectList<TForm>;
    procedure CloseFormsCreated;
    procedure AddJSONToTreeView(JSONValue: TJSONValue; ParentNode: TTreeNode; NodeName: string; TreeView: TTreeView );
  public
    { Public declarations }
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

procedure TForm2.Button1Click(Sender: TObject);
begin
  var UIControl: TControl;
  var JsonText := Memo.Text;
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
  end
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

procedure TForm2.FormCreate(Sender: TObject);
begin
   FCreatedForms := TObjectList<TForm>.Create(False);
end;


end.
