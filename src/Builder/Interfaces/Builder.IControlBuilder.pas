unit Builder.IControlBuilder;

interface

uses FMX.Controls;

type
  IControlBuilder = interface
    function SetName(const Name: string): IControlBuilder;
    function SetPosition(X, Y: Single): IControlBuilder;
    function SetSize(Width, Height: Single): IControlBuilder;
    function SetText(const Text: string): IControlBuilder;
    function SetAlign(const Align: string): IControlBuilder;
    function Build: TControl;
  end;

implementation

end.
