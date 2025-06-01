unit Util.Form.Arranger;

interface

uses Vcl.Controls, Vcl.Forms, System.Generics.Collections;

type
  TFormArranger = class
    public class function FormsToArray(const AList: TObjectList<TForm>): TArray<TForm>; static;
    public class procedure ArrangeCascade(const Forms: array of TForm); static;
    public class procedure ArrangeStacked(const Forms: array of TForm); static;
    public class procedure ArrangeSideBySide(const Forms: array of TForm); static;
  end;


implementation

class function TFormArranger.FormsToArray(const AList: TObjectList<TForm>): TArray<TForm>;
begin
  Result := AList.ToArray;
end;

class procedure TFormArranger.ArrangeCascade(const Forms: array of TForm);
begin
  var X := 20;
  var Y := 20;
  for var I := 0 to High(Forms) do
  begin
    Forms[I].SetBounds(X, Y, Forms[I].Width, Forms[I].Height);
    Inc(X, 30);
    Inc(Y, 30);
  end;
end;

class procedure TFormArranger.ArrangeStacked(const Forms: array of TForm);
begin
  var Y := 20;
  for var I := 0 to High(Forms) do
  begin
    Forms[I].Left := 20;
    Forms[I].Top := Y;
    Inc(Y, Forms[I].Height + 10);
  end;
end;

class procedure TFormArranger.ArrangeSideBySide(const Forms: array of TForm);
begin
  var X := 20;
  for var I := 0 to High(Forms) do
  begin
    Forms[I].Top := 20;
    Forms[I].Left := X;
    Inc(X, Forms[I].Width + 10);
  end;
end;

end.
