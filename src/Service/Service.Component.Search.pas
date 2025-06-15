unit Service.Component.Search;

interface

uses
  System.Classes;

type
  TComponentSearchService = class
  public
    class function FindComponentByName(Root: TComponent; const Name: string): TComponent;
  end;

implementation

uses
  Vcl.Controls, System.SysUtils;

class function TComponentSearchService.FindComponentByName(Root: TComponent; const Name: string): TComponent;

  function RecursiveFindComponent(Comp: TComponent): TComponent;
  var
    I: Integer;
  begin
    Result := nil;
    if SameText(Comp.Name, Name) then Exit(Comp);

    if Comp is TWinControl then
      for I := 0 to TWinControl(Comp).ControlCount - 1 do
      begin
        Result := RecursiveFindComponent(TWinControl(Comp).Controls[I]);
        if Assigned(Result) then Exit;
      end;

    for I := 0 to Comp.ComponentCount - 1 do
    begin
      Result := RecursiveFindComponent(Comp.Components[I]);
      if Assigned(Result) then Exit;
    end;
  end;

begin
  Result := RecursiveFindComponent(Root);
end;

end.

