unit Adapter.TreeViewAdapter;

interface

uses  Vcl.Graphics,Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Buttons, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
      System.Generics.Collections;

type
  TFormCloseAdapter = class
  public
    class var TreeView: TTreeView;
    class var FCreatedForm: TObjectList<TForm>;
    class procedure HandleClose(Sender: TObject; var Action: TCloseAction);
  end;


implementation

{ TFormCloseAdapter }

class procedure TFormCloseAdapter.HandleClose(Sender: TObject; var Action: TCloseAction);
var
  Node: TTreeNode;
begin
  if not Assigned(TreeView) or not Assigned(Sender) then Exit;
  if Sender is TForm then
  begin
    for var I := TreeView.Items.Count - 1 downto 0 do
    begin
      Node := TreeView.Items[I];
      if Node.Data = Sender then
      begin
        TreeView.Items.Delete(Node);
        Action := caFree;
      end;
    end;
  end;
end;


end.
