program BuilderUI;



uses
  Vcl.Forms,
  View.Builder.Main in 'View\View.Builder.Main.pas' {Form2},
  Builder.UIBuilderEngine in 'Builder\Builder.UIBuilderEngine.pas',
  Core.IUIBuilder in 'Core\Core.IUIBuilder.pas',
  Adapter.TreeViewAdapter in 'Adapter\Adapter.TreeViewAdapter.pas',
  Vcl.Themes,
  Vcl.Styles,
  Util.JSON in 'Utils\Util.JSON.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
