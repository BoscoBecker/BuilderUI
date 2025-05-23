unit Util.JSON;

interface

uses
  System.JSON, System.SysUtils, System.Classes;

type
  EInvalidJSON = class(Exception);

  IJSONValidatable = interface
    ['{9A5F1D4C-3D72-4A5B-8D1B-3C9E7F2A1D78}']
    function Validate: Boolean;
  end;

  IJSONSerializable = interface
    ['{7B3E8F2A-1C45-4D89-BF7D-6A2C9E1B3D4F}']
    function ToJSON: string;
    procedure FromJSON(const AJSON: string);
  end;

  TJSONHelper = class
  public
    class function IsValidJSON(const AJSON: string): Boolean; static;
    class function BeautifyJSON(const AJSON: string): string; static;
    class function MinifyJSON(const AJSON: string): string; static;
    class function ValidateJSON(const AJSON: string; out ErrorMessage: string): Boolean; static;
  end;

  TJSONWorker<T: class, constructor> = class
  public
    class function FromJSON(const AJSON: string): T; static;
    class function ToJSON(AObject: T): string; static;
    class function ToJSONAndFree(AObject: T): string; static;
  end;

implementation

{ TJSONHelper }

class function TJSONHelper.IsValidJSON(const AJSON: string): Boolean;
var
  JSONValue: TJSONValue;
begin
  Result := False;
  if AJSON.Trim.IsEmpty then Exit;

  JSONValue := TJSONObject.ParseJSONValue(AJSON);
  if Assigned(JSONValue) then
  begin
    JSONValue.Free;
    Result := True;
  end;
end;

class function TJSONHelper.BeautifyJSON(const AJSON: string): string;
var
  JSONValue: TJSONValue;
begin
  if not IsValidJSON(AJSON) then
    raise EInvalidJSON.Create('Invalid JSON input');

  JSONValue := TJSONObject.ParseJSONValue(AJSON);
  try
    Result := JSONValue.Format(2); // Indentação com 2 espaços
  finally
    JSONValue.Free;
  end;
end;

class function TJSONHelper.MinifyJSON(const AJSON: string): string;
var
  JSONValue: TJSONValue;
begin
  if not IsValidJSON(AJSON) then
    raise EInvalidJSON.Create('Invalid JSON input');

  JSONValue := TJSONObject.ParseJSONValue(AJSON);
  try
    Result := JSONValue.ToString;
  finally
    JSONValue.Free;
  end;
end;

class function TJSONHelper.ValidateJSON(const AJSON: string; out ErrorMessage: string): Boolean;
var
  JSONValue: TJSONValue;
begin
  ErrorMessage := '';
  Result := False;

  if AJSON.Trim.IsEmpty then
  begin
    ErrorMessage := 'Empty JSON string';
    Exit;
  end;

  try
    JSONValue := TJSONObject.ParseJSONValue(AJSON);
    if Assigned(JSONValue) then
    begin
      JSONValue.Free;
      Result := True;
    end
    else
    begin
      ErrorMessage := 'Invalid JSON format';
    end;
  except
    on E: Exception do
    begin
      ErrorMessage := E.Message;
    end;
  end;
end;

{ TJSONWorker<T> }


class function TJSONWorker<T>.FromJSON(const AJSON: string): T;
var
  JSONSerializable: IJSONSerializable;
begin
  if not TJSONHelper.IsValidJSON(AJSON) then
    raise EInvalidJSON.Create('Invalid JSON');

  Result := T.Create;
  try
    // Verifica se o objeto implementa IJSONSerializable
    if Supports(Result, IJSONSerializable, JSONSerializable) then
      JSONSerializable.FromJSON(AJSON)
    else
      raise EInvalidJSON.Create(Format('%s does not implement IJSONSerializable', [Result.ClassName]));
  except
    on E: Exception do
    begin
      Result.Free;
      raise;
    end;
  end;
end;


class function TJSONWorker<T>.ToJSON(AObject: T): string;
var
  JSONSerializable: IJSONSerializable;
begin
  if not Assigned(AObject) then
    raise EInvalidJSON.Create('Object is nil');

  if Supports(AObject, IJSONSerializable, JSONSerializable) then
    Result := JSONSerializable.ToJSON
  else
    raise EInvalidJSON.Create(Format('%s does not implement IJSONSerializable', [AObject.ClassName]));
end;

class function TJSONWorker<T>.ToJSONAndFree(AObject: T): string;
begin
  try
    Result := ToJSON(AObject);
  finally
    AObject.Free;
  end;
end;

end.
