program BuilderUI;



uses
  Vcl.Forms,
  View.Builder.Main in 'View\View.Builder.Main.pas' {FormBuilderMain},
  Builder.UIBuilderEngine in 'Builder\Builder.UIBuilderEngine.pas',
  Core.IUIBuilder in 'Core\Core.IUIBuilder.pas',
  Adapter.TreeViewAdapter in 'Adapter\Adapter.TreeViewAdapter.pas',
  Vcl.Themes,
  Vcl.Styles,
  Util.JSON in 'Utils\Util.JSON.pas',
  Factory.ICodeGenerator in 'Factory\Factory.ICodeGenerator.pas',
  View.Menu.Context.Windows in 'View\View.Menu.Context.Windows.pas' {FormContextWindows},
  View.Export.Forms in 'View\View.Export.Forms.pas' {FormExports},
  Util.Form.Arranger in 'Utils\Util.Form.Arranger.pas',
  Strategy.IExport in 'Strategy\Strategy.IExport.pas',
  Factory.CodeGenerator.Delphi in 'Factory\Delphi\Factory.CodeGenerator.Delphi.pas',
  Factory.ICodeGeneratorFactory in 'Factory\Factory.ICodeGeneratorFactory.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormBuilderMain, FormBuilderMain);
  Application.Run;
end.
