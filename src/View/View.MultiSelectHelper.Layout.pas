unit View.MultiSelectHelper.Layout;

interface

uses
  System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  FMX.Controls, FMX.Types, FMX.Objects, FMX.Graphics, FMX.StdCtrls,
  FMX.Layouts;

type
  TMultiSelectHelper = class(TComponent)
  private
    FLayout: TLayout;
    FSelectionStart: TPointF;
    FSelectionRect: TRectF;
    FIsSelecting: Boolean;
    FSelectedControls: TList<TControl>;
    FSelectionBox: TRectangle;

    procedure LayoutMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure LayoutMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure LayoutMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure UpdateSelection;
  public
    constructor Create(AOwner: TComponent; ALayout: TLayout); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  System.Math, FMX.Forms;

{ TMultiSelectHelper }

constructor TMultiSelectHelper.Create(AOwner: TComponent; ALayout: TLayout);
begin
  inherited Create(AOwner);

  FLayout := ALayout;
  FSelectedControls := TList<TControl>.Create;

  FSelectionBox := TRectangle.Create(FLayout);
  FSelectionBox.Parent := FLayout;
  FSelectionBox.Fill.Color := TAlphaColorRec.Dodgerblue;
  FSelectionBox.Fill.Kind := TBrushKind.Solid;
  FSelectionBox.Opacity := 0.2;
  FSelectionBox.Stroke.Thickness := 1;
  FSelectionBox.Visible := False;
  FSelectionBox.HitTest := False;

  FLayout.OnMouseDown := LayoutMouseDown;
  FLayout.OnMouseMove := LayoutMouseMove;
  FLayout.OnMouseUp := LayoutMouseUp;
end;

destructor TMultiSelectHelper.Destroy;
begin
  FSelectedControls.Free;
  inherited;
end;

procedure TMultiSelectHelper.LayoutMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in Shift then
  begin
    FSelectionStart := PointF(X, Y);
    FIsSelecting := True;
    FSelectionRect := TRectF.Create(FSelectionStart, FSelectionStart);
    FSelectionBox.SetBounds(FSelectionStart.X, FSelectionStart.Y, 1, 1);
    FSelectionBox.Visible := True;
  end;
end;

procedure TMultiSelectHelper.LayoutMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  R: TRectF;
begin
  if FIsSelecting then
  begin
    R := TRectF.Create(FSelectionStart, PointF(X, Y));
    R.NormalizeRect;
    FSelectionRect := R;
    FSelectionBox.SetBounds(R.Left, R.Top, R.Width, R.Height);
  end;
end;

procedure TMultiSelectHelper.LayoutMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if FIsSelecting then
  begin
    UpdateSelection;
    FIsSelecting := False;
    FSelectionBox.Visible := False;
  end;
end;

procedure TMultiSelectHelper.UpdateSelection;
var
  Ctrl: TControl;
  I: Integer;
begin
  if not (ssCtrl in GetShiftState  ) then
  begin
    // Clear previous selection
    for Ctrl in FSelectedControls do
      Ctrl.Opacity := 1;
    FSelectedControls.Clear;
  end;

  for I := 0 to FLayout.ChildrenCount - 1 do
  begin
    if FLayout.Children[I] is TControl then
    begin
      Ctrl := TControl(FLayout.Children[I]);
      if FSelectionRect.IntersectsWith(Ctrl.BoundsRect) then
      begin
        if not FSelectedControls.Contains(Ctrl) then
        begin
          FSelectedControls.Add(Ctrl);
          Ctrl.Opacity := 0.5; // indicate selection
        end;
      end;
    end;
  end;
end;

end.

