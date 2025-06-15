unit Test.Service.Json.Validation;

interface

uses
  DUnitX.TestFramework,
  TestFramework,            // DUnit
  Service.Json.Validation,  // Classe que queremos testar
  System.SysUtils;

type
  [TestFixture]
  TestTBuilderUIValidatorService = class(TTestCase)
  published
    [Test]
    procedure TestValidJSON;
    procedure TestInvalidJSON;
    procedure TestJSONWithDuplicateNames;
  end;

implementation

procedure TestTBuilderUIValidatorService.TestValidJSON;
var
  JSON: string;
  Result: TBuilderUIValidationResult;
begin
  JSON := '{ "Forms": [ { "Type": "TForm", "Name": "MainForm", "Caption": "Test" } ] }';
  Result := TBuilderUIValidatorService.Validate(JSON);
  CheckTrue(Result.IsValid, 'Expected JSON to be valid');
end;

procedure TestTBuilderUIValidatorService.TestInvalidJSON;
var
  JSON: string;
  Result: TBuilderUIValidationResult;
begin
  JSON := '{ invalid json... ';
  Result := TBuilderUIValidatorService.Validate(JSON);
  CheckFalse(Result.IsValid, 'Expected invalid JSON');
end;

procedure TestTBuilderUIValidatorService.TestJSONWithDuplicateNames;
var
  JSON: string;
  Result: TBuilderUIValidationResult;
begin
  JSON := '{ "Forms": [ ' +
          '{ "Type": "TForm", "Name": "Form1", "Children": [ ' +
          '{ "Type": "TEdit", "Name": "Edit1" }, ' +
          '{ "Type": "TEdit", "Name": "Edit1" } ' +
          '] } ] }';

  Result := TBuilderUIValidatorService.Validate(JSON);
  CheckFalse(Result.IsValid, 'Expected failure due to duplicate component names');
end;

initialization
  RegisterTest(TestTBuilderUIValidatorService.Suite);

end.

