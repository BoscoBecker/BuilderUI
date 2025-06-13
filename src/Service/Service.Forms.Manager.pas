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
unit Service.Forms.Manager;

interface

uses System.Generics.Collections, Vcl.Forms;

type
  TFormCreatedManager = class
  private
    FForms: TObjectList<TForm>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddForm(AForm: TForm);
    procedure CloseAll;
    procedure Clear;
    property Forms: TObjectList<TForm> read FForms;
  end;

implementation

{ TFormCreatedManager }

procedure TFormCreatedManager.AddForm(AForm: TForm);
begin
  FForms.Add(AForm);
end;

procedure TFormCreatedManager.Clear;
begin
  FForms.Clear;
end;

procedure TFormCreatedManager.CloseAll;
begin
  for var I := FForms.Count - 1 downto 0 do
    if Assigned(FForms[I]) then
      try
        FForms[I].Close;
        FForms[I].Free;
      except
      end;
  FForms.Clear;
end;

constructor TFormCreatedManager.Create;
begin
  FForms := TObjectList<TForm>.Create(False);
end;

destructor TFormCreatedManager.Destroy;
begin
  CloseAll;
  FForms.Free;
  inherited;
end;

end.
