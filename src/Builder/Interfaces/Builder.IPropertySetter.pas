unit Builder.IPropertySetter;

interface

uses FMX.Controls;

type
  ICompositeControl = interface
    procedure AddChild(Child: TControl);
    procedure RemoveChild(Child: TControl);
    function GetChildren: TArray<TControl>;
  end;

implementation

end.
