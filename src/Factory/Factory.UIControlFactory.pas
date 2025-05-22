unit Factory.UIControlFactory;

interface

uses
  FMX.Controls, System.Classes, System.SysUtils;

type
  TUIControlFactory = class
  public
    class function CreateControl(AOwner: TComponent; const AType: string): TControl;
  end;

implementation

uses
  FMX.Layouts, FMX.StdCtrls, FMX.Edit;

class function TUIControlFactory.CreateControl(AOwner: TComponent; const AType: string): TControl;
begin
  if AType = 'TLayout' then
    Result := TLayout.Create(AOwner)
  else if AType = 'TLabel' then
    Result := TLabel.Create(AOwner)
  else if AType = 'TButton' then
    Result := TButton.Create(AOwner)
  else if AType = 'TEdit' then
    Result := TEdit.Create(AOwner)
  else
    raise Exception.CreateFmt('Tipo de controle "%s" não suportado.', [AType]);
end;

end.

