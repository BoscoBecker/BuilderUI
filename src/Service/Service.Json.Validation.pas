unit Service.Json.Validation;

interface

uses System.SysUtils, System.Generics.Collections;

type
  TBuilderUIValidationResult = record
    public
      IsValid: Boolean;
      ErrorMessage: string;
      DuplicatedNames: string;
      InvalidProperties: TArray<string>;
      class function Success: TBuilderUIValidationResult; static;
      class function Fail(const Msg: string): TBuilderUIValidationResult; static;
  end;

  TBuilderUIValidatorService = class
  public
    class function Validate(const AJSON: string): TBuilderUIValidationResult;
  end;

implementation

uses Util.JSONValidator, Util.Json, System.StrUtils;

class function TBuilderUIValidationResult.Fail(const Msg: string): TBuilderUIValidationResult;
begin
  Result.IsValid := False;
  Result.ErrorMessage := Msg;
end;

class function TBuilderUIValidationResult.Success: TBuilderUIValidationResult;
begin
  Result.IsValid := True;
end;

class function TBuilderUIValidatorService.Validate(const AJSON: string): TBuilderUIValidationResult;
var
  Invalids: TArray<string>;
  DupForms: TArray<TDuplicateInfo>;
  ErrorMsg: string;
begin
  if not Util.JSONValidator.TJSONHelper.ValidateBuilderUIPattern(AJSON, Invalids) then
    Exit(TBuilderUIValidationResult.Fail('Invalid properties: ' + string.Join(sLineBreak, Invalids)));

  if not Util.JSONValidator.TJSONHelper.ValidateJSON(AJSON, ErrorMsg) or AJSON.Trim.IsEmpty then
    Exit(TBuilderUIValidationResult.Fail('Invalid JSON: ' + ErrorMsg));

  if TJSONHelper.HasDuplicateNamesPerForm(AJSON, DupForms) then
  begin
    var Duplicated := '';
    for var Info in DupForms do
      Duplicated := Duplicated + string.Join(', ', Info.DuplicatedNames);
    Exit(TBuilderUIValidationResult.Fail('Duplicate names: ' + Duplicated));
  end;

  Exit(TBuilderUIValidationResult.Success);
end;

end.

