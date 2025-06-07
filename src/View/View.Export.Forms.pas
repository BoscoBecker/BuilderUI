unit View.Export.Forms;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Json,  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Skia,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Strategy.Export.Delphi;

type
  TFormExports = class(TForm)
    SkLabel5: TSkLabel;
    SkLabel6: TSkLabel;
    SkLabel7: TSkLabel;
    SkLabelTitle: TSkLabel;
    SkLabel9: TSkLabel;
    SkLabel10: TSkLabel;
    ImageDelphi: TImage;
    ImageLazarus: TImage;
    ImageCSharp: TImage;
    ImageJava: TImage;
    ImageVB: TImage;
    procedure ImageDelphiClick(Sender: TObject);
  private
    FJsonData: TJSONObject;
  public
    procedure SetJsonData(const Value: TJSONObject);
    procedure ExportToDelphi;
    property JsonData: TJSONObject read FJsonData write SetJsonData;
  end;

var
  FormExports: TFormExports;

implementation

{$R *.dfm}

{ TFormExports }


uses Factory.ICodeGenerator, Factory.CodeGeneratorFactory;

procedure TFormExports.ExportToDelphi;
begin
  var ExportFiles:= TDelphiExport.Create;
  try
    var Generator := TCodeGeneratorFactory.CreateGenerator('Delphi');
        Generator.GenerateCode(FJsonData);
    ExportFiles.ExportData(Generator.PasText);
    ExportFiles.ExportData(Generator.DfmText);
  finally
    ExportFiles.Free;
  end;
end;

procedure TFormExports.ImageDelphiClick(Sender: TObject);
begin
  ExportToDelphi;
end;

procedure TFormExports.SetJsonData(const Value: TJSONObject);
begin
  FJsonData := Value;
end;

end.
