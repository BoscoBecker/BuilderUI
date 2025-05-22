unit Core.IUIBuilder;

interface

uses
  System.JSON, System.Classes, Forms, Controls;

type
  IUIBuilder = interface
    ['{A119A45D-9133-4F26-89D0-893BE3AA1FEF}']
    function CreateFormFromJson(AOwner: TComponent; Json: TJSONObject): TForm;
    function CreateControlFromJson(AOwner: TComponent; AParent: TWinControl; Json: TJSONObject): TControl;
  end;

implementation

end.
