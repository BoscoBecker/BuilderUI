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
