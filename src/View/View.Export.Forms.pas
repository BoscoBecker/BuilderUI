unit View.Export.Forms;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Json,  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, SYstem.Types, System.Generics.Collections ,Vcl.Skia,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.WinXCtrls, Vcl.ComCtrls;

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
    procedure EditPathChange(Sender: TObject);
  private
    FJsonData: TJSONObject;
    FForms: TObjectList<TForm>;
    Procedure ExportForms;
    procedure SelectFolder;
    procedure VerifyPath;
    function GetJsonData: TJSONObject;
    function GetSelectedTechnology: string;
  public
    procedure SetForms(const Value: TObjectList<TForm>);
    procedure SetJsonData(const AValue: TJSONObject);
    property JsonData: TJSONObject read GetJsonData write SetJsonData;
    property Forms: TObjectList<TForm> read FForms write SetForms;
  end;

var
  FormExports: TFormExports;

implementation

{$R *.dfm}

uses Factory.ICodeGenerator, Factory.CodeGeneratorFactory, Adapter.TreeViewAdapter, Service.Export, ShellAPI;

procedure TFormExports.ButtonSelectFolderClick(Sender: TObject);
begin
  SelectFolder;
end;

procedure TFormExports.ButtonStartProcessClick(Sender: TObject);
begin
  ExportForms;
end;

procedure TFormExports.EditPathChange(Sender: TObject);
begin
  VerifyPath;
end;

function TFormExports.GetJsonData: TJSONObject;
begin
  result:= FJsonData;
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

procedure TFormExports.VerifyPath;
begin
  ButtonStartProcess.Enabled:= DirectoryExists(EditPath.Text);
end;

procedure TFormExports.SelectFolder;
begin
  var Dialog := TFileOpenDialog.Create(Self);
  try
    Dialog.Options := [fdoPickFolders];
    Dialog.Title := 'Select export folder';

    if Dialog.Execute(Application.Handle) then
      EditPath.Text := Dialog.FileName;

    VerifyPath;
  finally
    Dialog.Free;
  end;
end;

function TFormExports.GetSelectedTechnology: string;
begin
  if RadioButtonDelphi.Checked then
    Result := 'Delphi'
  else if RadioButtonLazarus.Checked then
    Result := 'Lazarus'
  else if RadioButtonCSharp.Checked then
    Result := 'C#'
  else if RadioButtonJava.Checked then
    Result := 'Java'
  else if RadioButtonVisualBasic.Checked then
    Result := 'VisualBasic'
  else
    Result := '';
end;

procedure TFormExports.ExportForms;
begin
  VerifyPath;
  try
    var Technology := GetSelectedTechnology;
    var FormNames := TreeViewForms.GetSelectedFormNames;
    for var FormName in FormNames do
      TExportService.ExportForm(GetJsonData,FormName,EditPath.Text,Technology,CheckBoxOnlyGUI.Checked);
  finally
    if CheckBoxShowFolder.Checked then
      ShellExecute(0, 'open', PChar(EditPath.Text), nil, nil, SW_SHOWNORMAL);
  end;
end;


end.

