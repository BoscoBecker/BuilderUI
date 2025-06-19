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
unit View.Menu.Context.Windows;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, Vcl.Skia,

  Util.Form.Arranger;

type
  TFormContextWindows = class(TForm)
    SkLabelCascade: TSkLabel;
    SkLabelStacked: TSkLabel;
    SkLabelSideBySide: TSkLabel;
    SkLabelTitle: TSkLabel;
    ImageCascade: TImage;
    ImageStacked: TImage;
    ImageSidebySide: TImage;
    procedure ImageCascadeClick(Sender: TObject);
    procedure ImageStackedClick(Sender: TObject);
    procedure ImageSidebySideClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    FForm :TObjectList<TForm>;
    constructor Create(AOwner: TComponent;const Form :TObjectList<TForm>); reintroduce;
    destructor Destroy; override;
  end;

var
  FormContextWindows: TFormContextWindows;

implementation

{$R *.dfm}

constructor TFormContextWindows.Create(AOwner: TComponent;const Form: TObjectList<TForm>);
begin
  inherited Create(AOwner);
  FForm:= Form;
end;

destructor TFormContextWindows.Destroy;
begin
  inherited;
end;

procedure TFormContextWindows.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = VK_ESCAPE then Close;
end;

procedure TFormContextWindows.ImageSidebySideClick(Sender: TObject);
begin
  TFormArranger.ArrangeSideBySide(TFormArranger.FormsToArray(FForm));
end;

procedure TFormContextWindows.ImageStackedClick(Sender: TObject);
begin
  TFormArranger.ArrangeStacked(TFormArranger.FormsToArray(FForm));
end;

procedure TFormContextWindows.ImageCascadeClick(Sender: TObject);
begin
  TFormArranger.ArrangeCascade(TFormArranger.FormsToArray(FForm))
end;

end.
