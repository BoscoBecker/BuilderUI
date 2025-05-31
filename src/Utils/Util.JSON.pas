unit Util.JSON;

interface

uses
  System.JSON, System.SysUtils, System.Classes, system.Generics.Collections;

type
  TDuplicateInfo = record
    FormName: string;
    DuplicatedNames: TArray<string>;
  end;

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
    public class function IsValidJSON(const AJSON: string): Boolean; static;
    public class function BeautifyJSON(const AJSON: string): string; static;
    public class function MinifyJSON(const AJSON: string): string; static;
    public class function ValidateJSON(const AJSON: string; out ErrorMessage: string): Boolean; static;
    public class function HasNamedComponent(const AJSON, ATargetName: string): Boolean; static;
    public class function HasDuplicateNames(const AJSON: string; out Duplicates: TArray<string>): Boolean; static;
    public class function HasDuplicateNamesPerForm(const AJSON: string; out DuplicateForms: TArray<TDuplicateInfo>): Boolean; static;
    public class procedure TraverseJSON(const JSONValue: TJSONValue;  const ATargetName: string; var Found: Boolean; const NameList: TDictionary<string, Integer>); static;
  end;

  TJSONWorker<T: class, constructor> = class
    public class function FromJSON(const AJSON: string): T; static;
    public class function ToJSON(AObject: T): string; static;
    public class function ToJSONAndFree(AObject: T): string; static;
  end;

implementation

class function TJSONHelper.HasDuplicateNames(const AJSON: string; out Duplicates: TArray<string>): Boolean;
var
  Root: TJSONValue;
  NameDict: TDictionary<string, Integer>;
  DupList: TList<string>;
  procedure InternalTraverse(Value: TJSONValue);
  var
    Obj: TJSONObject;
    Arr: TJSONArray;
    Pair: TJSONPair;
    Item: TJSONValue;
    NameVal: string;
  begin
    if Value is TJSONObject then
    begin
      Obj := TJSONObject(Value);

      if Obj.TryGetValue<string>('Name', NameVal) then
      begin
        if NameDict.ContainsKey(NameVal) then
          NameDict[NameVal] := NameDict[NameVal] + 1
        else
          NameDict.Add(NameVal, 1);
      end;

      for Pair in Obj do
        InternalTraverse(Pair.JsonValue);
    end
    else if Value is TJSONArray then
    begin
      Arr := TJSONArray(Value);
      for Item in Arr do
        InternalTraverse(Item);
    end;
  end;
begin
  Result := False;
  SetLength(Duplicates, 0);
  Root := TJSONObject.ParseJSONValue(AJSON);
  if not Assigned(Root) then
    Exit;

  NameDict := TDictionary<string, Integer>.Create;
  DupList := TList<string>.Create;
  try
    InternalTraverse(Root);

    for var Key in NameDict.Keys do
      if NameDict[Key] > 1 then
        DupList.Add(Key);

    if DupList.Count > 0 then
    begin
      Duplicates := DupList.ToArray;
      Result := True;
    end;
  finally
    Root.Free;
    NameDict.Free;
    DupList.Free;
  end;
end;

class function TJSONHelper.HasDuplicateNamesPerForm(const AJSON: string; out DuplicateForms: TArray<TDuplicateInfo>): Boolean;
var
  Root: TJSONValue;
  FormList: TList<TDuplicateInfo>;

  procedure ProcessForm(AFormObj: TJSONObject);
  var
    NameDict: TDictionary<string, Integer>;
    DupList: TList<string>;

    procedure TraverseChildren(Value: TJSONValue);
    var
      Obj: TJSONObject;
      Pair: TJSONPair;
      Item: TJSONValue;
      NameVal: string;
    begin
      if Value is TJSONObject then
      begin
        Obj := TJSONObject(Value);
        if Obj.TryGetValue<string>('Name', NameVal) then
        begin
          if NameDict.ContainsKey(NameVal) then
            NameDict[NameVal] := NameDict[NameVal] + 1
          else
            NameDict.Add(NameVal, 1);
        end;

        for Pair in Obj do
          if not SameText(Pair.JsonString.Value, 'Name') then
            TraverseChildren(Pair.JsonValue);
      end
      else if Value is TJSONArray then
      begin
        for Item in TJSONArray(Value) do
          TraverseChildren(Item);
      end;
    end;

  var
    FormName: string;
  begin
    NameDict := TDictionary<string, Integer>.Create;
    DupList := TList<string>.Create;
    try
      TraverseChildren(AFormObj);

      for var Key in NameDict.Keys do
        if NameDict[Key] > 1 then
          DupList.Add(Key);

      if DupList.Count > 0 then
      begin
        if AFormObj.TryGetValue<string>('Name', FormName) then
        begin
          var Info: TDuplicateInfo;
          Info.FormName := FormName;
          Info.DuplicatedNames := DupList.ToArray;
          FormList.Add(Info);
        end;
      end;
    finally
      NameDict.Free;
      DupList.Free;
    end;
  end;

  procedure TraverseAll(Value: TJSONValue);
  var
    Obj: TJSONObject;
    Arr: TJSONArray;
    Pair: TJSONPair;
    Item: TJSONValue;
    TypeVal: string;
  begin
    if Value is TJSONObject then
    begin
      Obj := TJSONObject(Value);
      if Obj.TryGetValue<string>('Type', TypeVal) and SameText(TypeVal, 'TForm') then
        ProcessForm(Obj);

      for Pair in Obj do
        TraverseAll(Pair.JsonValue);
    end
    else if Value is TJSONArray then
    begin
      Arr := TJSONArray(Value);
      for Item in Arr do
        TraverseAll(Item);
    end;
  end;

begin
  Result := False;
  SetLength(DuplicateForms, 0);
  Root := TJSONObject.ParseJSONValue(AJSON);
  if not Assigned(Root) then
    Exit;

  FormList := TList<TDuplicateInfo>.Create;
  try
    TraverseAll(Root);
    if FormList.Count > 0 then
    begin
      DuplicateForms := FormList.ToArray;
      Result := True;
    end;
  finally
    Root.Free;
    FormList.Free;
  end;
end;

class function TJSONHelper.HasNamedComponent(const AJSON, ATargetName: string): Boolean;
var
  Root: TJSONValue;
  Found: Boolean;
  DummyDict: TDictionary<string, Integer>;
begin
  Result := False;
  Root := TJSONObject.ParseJSONValue(AJSON);
  DummyDict := TDictionary<string, Integer>.Create;
  try
    if Assigned(Root) then
    begin
      Found := False;
      TraverseJSON(Root, ATargetName, Found, DummyDict);
      Result := Found;
    end;
  finally
    DummyDict.Free;
    Root.Free;
  end;
end;

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

class procedure TJSONHelper.TraverseJSON(const JSONValue: TJSONValue; const ATargetName: string; var Found: Boolean; const NameList: TDictionary<string, Integer>);
var
  Pair: TJSONPair;
  Item: TJSONValue;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  NameValue: string;
begin
  if JSONValue is TJSONObject then
  begin
    JSONObject := JSONValue as TJSONObject;

    if JSONObject.TryGetValue<string>('Name', NameValue) then
    begin
      if SameText(NameValue, ATargetName) then
        Found := True;

      if NameList.ContainsKey(NameValue) then
        NameList[NameValue] := NameList[NameValue] + 1
      else
        NameList.Add(NameValue, 1);
    end;

    for Pair in JSONObject do
    begin
      if Pair.JsonValue is TJSONArray then
      begin
        JSONArray := Pair.JsonValue as TJSONArray;
        for Item in JSONArray do
          TraverseJSON(Item, ATargetName, Found, NameList);
      end
      else if Pair.JsonValue is TJSONObject then
      begin
        TraverseJSON(Pair.JsonValue, ATargetName, Found, NameList);
      end;
    end;
  end
  else if JSONValue is TJSONArray then
  begin
    JSONArray := JSONValue as TJSONArray;
    for Item in JSONArray do
      TraverseJSON(Item, ATargetName, Found, NameList);
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

class function TJSONWorker<T>.FromJSON(const AJSON: string): T;
var
  JSONSerializable: IJSONSerializable;
begin
  if not TJSONHelper.IsValidJSON(AJSON) then
    raise EInvalidJSON.Create('Invalid JSON');

  Result := T.Create;
  try
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
