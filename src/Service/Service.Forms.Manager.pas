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
