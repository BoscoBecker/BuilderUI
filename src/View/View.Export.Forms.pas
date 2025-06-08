unit View.Export.Forms;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Json,  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, SYstem.Types, System.Generics.Collections ,Vcl.Skia,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Strategy.Export.Delphi, Vcl.StdCtrls,
  Vcl.WinXCtrls, Vcl.ComCtrls;

type
  TFormExports = class(TForm)
    SkLabelTitle: TSkLabel;
    ImageDelphi: TImage;
    ImageLazarus: TImage;
    ImageCSharp: TImage;
    ImageJava: TImage;
    ImageVB: TImage;
    LabelDelphi: TLabel;
    LabelLazarus: TLabel;
    LabelCSharp: TLabel;
    LabelJava: TLabel;
    LabelVB: TLabel;
    GroupBoxOptionsForms: TGroupBox;
    EditPath: TEdit;
    LabelInfoExportPath: TLabel;
    ButtonSelectFolder: TButton;
    CheckBoxOnlyGUI: TCheckBox;
    RadioButtonDelphi: TRadioButton;
    RadioButtonVisualBasic: TRadioButton;
    RadioButtonLazarus: TRadioButton;
    RadioButtonCSharp: TRadioButton;
    RadioButtonJava: TRadioButton;
    CheckBoxShowFolder: TCheckBox;
    ButtonStartProcess: TButton;
    ActivityIndicatorLoading: TActivityIndicator;
    LabelInfo: TLabel;
    TreeViewForms: TTreeView;
    procedure LabelDelphiMouseEnter(Sender: TObject);
    procedure LabelDelphiMouseLeave(Sender: TObject);
    procedure ButtonStartProcessClick(Sender: TObject);
    procedure ButtonSelectFolderClick(Sender: TObject);
  private
    FJsonData: TJSONObject;
    FForms: TObjectList<TForm>;
    Procedure ExportForms;
    procedure ExportToDelphi(const AName, APath: string );
    procedure ExportToLazarus(const AName, APath: string );
    procedure ExportToCSharp(const AName, APath: string );
    procedure ExportToJava(const AName, APath: string );
    procedure ExportToVisualBasic(const AName, APath: string );
    procedure SelectFolder;
  public
    procedure SetForms(const Value: TObjectList<TForm>);
    procedure SetJsonData(const AValue: TJSONObject);
    property JsonData: TJSONObject read FJsonData write SetJsonData;
    property Forms: TObjectList<TForm> read FForms write SetForms;
  end;

var
  FormExports: TFormExports;

implementation

{$R *.dfm}

{ TFormExports }


uses Factory.ICodeGenerator, Factory.CodeGeneratorFactory, Adapter.TreeViewAdapter;

procedure TFormExports.ButtonSelectFolderClick(Sender: TObject);
begin
  SelectFolder;
end;

procedure TFormExports.ButtonStartProcessClick(Sender: TObject);
begin
  ExportForms;
end;

procedure TFormExports.SelectFolder;
begin
  var Dialog := TFileOpenDialog.Create(Self);
  try
    Dialog.Options := [fdoPickFolders];
    Dialog.Title := 'Select export folder';
    if Dialog.Execute(Application.Handle) then
      EditPath.Text := Dialog.FileName;
  finally
    Dialog.Free;
  end;
end;

procedure TFormExports.ExportForms;
begin
  ActivityIndicatorLoading.Animate := True;
  ActivityIndicatorLoading.Visible := True;
  LabelInfo.Caption := 'Exporting...';
  LabelInfo.Visible := True;
  try
    var Node := TreeViewForms.Items.GetFirstNode;
    while Assigned(Node) do
    begin
      if Node.Checked then
      begin
        var FormName := Node.Text;
        LabelInfo.Caption := 'Exporting ' + FormName + '...';

        Application.ProcessMessages;
        sleep(1000);
        ActivityIndicatorLoading.invalidate;
        Application.ProcessMessages;

        if RadioButtonDelphi.Checked then
          ExportToDelphi(FormName, EditPath.Text)
        else
        if RadioButtonLazarus.Checked then
          ExportToLazarus(FormName, EditPath.Text)
        else
        if RadioButtonCSharp.Checked then
          ExportToCSharp(FormName, EditPath.Text)
        else
        if RadioButtonJava.Checked then
          ExportToJava(FormName, EditPath.Text)
        else
        if RadioButtonVisualBasic.Checked then
          ExportToVisualBasic(FormName, EditPath.Text);
      end;
      Node := Node.getNextSibling;
    end;
    LabelInfo.Caption := 'Export finished!';
  finally
    ActivityIndicatorLoading.Animate := False;
    ActivityIndicatorLoading.Visible := False;
  end;
end;

procedure TFormExports.ExportToCSharp(const AName, APath: string);
begin

end;

procedure TFormExports.ExportToDelphi(const AName, APath: string );
begin
  var Generator:= TCodeGeneratorFactory.CreateGenerator('Delphi');
  var ExportDelphiFiles:= TDelphiExport.Create;
  var obj:= Generator.FindFormByName(FJsonData,AName);
  try
    Generator.GenerateCode(obj);

    ExportDelphiFiles.ExportData(APath + '\'+AName + '.pas', Generator.PasText);
    ExportDelphiFiles.ExportData(APath + '\'+AName + '.dfm', Generator.DfmText);
  finally
    ExportDelphiFiles.Free;
  end;
end;

procedure TFormExports.ExportToJava(const AName, APath: string);
begin

end;

procedure TFormExports.ExportToLazarus(const AName, APath: string);
begin

end;

procedure TFormExports.ExportToVisualBasic(const AName, APath: string);
begin

end;

procedure TFormExports.LabelDelphiMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style:= [fsbold];
end;

procedure TFormExports.LabelDelphiMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style:= [];
end;


procedure TFormExports.SetForms(const Value: TObjectList<TForm>);
begin
  FForms := Value;
end;

procedure TFormExports.SetJsonData(const AValue: TJSONObject);
begin
  FJsonData := AValue;
end;




end.
