unit Service.Zoom;

interface

type
  TZoomService = class
  private
    FZoom: Single;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ZoomIn;
    procedure ZoomOut;
    function GetZoom: Single;
    property Zoom: Single read FZoom write FZoom;
  end;

implementation

constructor TZoomService.Create;
begin
  FZoom := 1.0;
end;

destructor TZoomService.destroy;
begin
  inherited;
end;

function TZoomService.GetZoom: Single;
begin
  result:= FZoom;
end;

procedure TZoomService.ZoomIn;
begin
  FZoom := FZoom * 1.1;
end;

procedure TZoomService.ZoomOut;
begin
  FZoom := FZoom / 1.1;
end;

end.
