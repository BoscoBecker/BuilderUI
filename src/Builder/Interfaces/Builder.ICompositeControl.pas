unit Builder.ICompositeControl;

interface

uses FMX.Controls;

type
  ICompositeControl = interface
  ['{8E23A885-4106-4201-BB00-8ED8B96B5564}']
  procedure CreateDragableFrame(AParent: TControl);
  end;

implementation

end.
