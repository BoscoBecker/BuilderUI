program BuilderUI;



uses
  Vcl.Forms,
  View.Builder.Main in 'View\View.Builder.Main.pas' {FormBuilderMain},
  Builder.UI.BuilderEngine in 'Builder\Builder.UI.BuilderEngine.pas',
  Core.IUIBuilder in 'Core\Core.IUIBuilder.pas',
  Adapter.TreeViewAdapter in 'Adapter\Adapter.TreeViewAdapter.pas',
  Vcl.Themes,
  Vcl.Styles,
  Util.JSON in 'Utils\Util.JSON.pas',
  Factory.ICodeGenerator in 'Factory\Interface\Factory.ICodeGenerator.pas',
  View.Menu.Context.Windows in 'View\View.Menu.Context.Windows.pas' {FormContextWindows},
  View.Export.Forms in 'View\View.Export.Forms.pas' {FormExports},
  Util.Form.Arranger in 'Utils\Util.Form.Arranger.pas',
  Factory.CodeGenerator.Delphi in 'Factory\Technology\Delphi\Factory.CodeGenerator.Delphi.pas',
  Factory.CodeGeneratorFactory in 'Factory\Base\Factory.CodeGeneratorFactory.pas',
  Strategy.Export.Delphi in 'Strategy\Technology\Delphi\Strategy.Export.Delphi.pas',
  Strategy.IExport in 'Strategy\Interface\Strategy.IExport.pas',
  Factory.CodeGenerator.CSharp in 'Factory\Technology\C#\Factory.CodeGenerator.CSharp.pas',
  Strategy.Export.CSharp in 'Strategy\Technology\C#\Strategy.Export.CSharp.pas',
  Service.Export in 'Service\Service.Export.pas',
  View.Window.Json in 'View\View.Window.Json.pas' {FormJson},
  Util.JSONValidator in 'Utils\Util.JSONValidator.pas',
  Service.Zoom in 'Service\Service.Zoom.pas',
  Service.Forms.Manager in 'Service\Service.Forms.Manager.pas',
  Service.Skia.Draw in 'Service\Service.Skia.Draw.pas',
  Service.JsonFile in 'Service\Service.JsonFile.pas',
  Builder.UI.UserPreferences in 'Builder\Builder.UI.UserPreferences.pas',
  Enum.Utils in 'Enum\Enum.Utils.pas',
  Factory.CodeGenerator.Lazarus in 'Factory\Technology\Lazarus\Factory.CodeGenerator.Lazarus.pas',
  Strategy.Export.Lazarus in 'Strategy\Technology\Lazarus\Strategy.Export.Lazarus.pas',
  Service.Json.Validation in 'Service\Service.Json.Validation.pas',
  Service.Component.Search in 'Service\Service.Component.Search.pas',
  Service.Component.Manager.Highlighter in 'Service\Service.Component.Manager.Highlighter.pas',
  Service.StatusBar.Manager in 'Service\Service.StatusBar.Manager.pas',
  Service.Component in 'Service\Service.Component.pas',
  Service.Component.PropertyExplorer in 'Service\Service.Component.PropertyExplorer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormBuilderMain, FormBuilderMain);
  Application.Run;
end.
