unit Services.AIService;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient,

  Prompt.Engine;

type
  TAIService = class
  private
    FClient: THttpClient;
    FBaseUrl: string;
  public
    constructor Create(const ABaseUrl: string);
    destructor Destroy; override;
    function GenerateUIJson(const PromptText: string): string;
  end;

implementation

uses
  System.JSON;

constructor TAIService.Create(const ABaseUrl: string);
begin
  FClient := THttpClient.Create;
  FBaseUrl := ABaseUrl;
end;

destructor TAIService.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TAIService.GenerateUIJson(const PromptText: string): string;
var
  RequestBody: TStringStream;
  ResponseStream: TStream;
  StreamReader: TStreamReader;
  Line: string;
  JSON: TJSONObject;
  Chunk: string;
  Done: Boolean;
  ResultBuilder: TStringBuilder;
begin
  ResultBuilder := TStringBuilder.Create;
  var JSONObj := TJSONObject.Create;

  try
    if PromptText.IsEmpty then Exit('');
    try
//      if FClient = nil then
//        FClient := THttpClient.Create;

      FClient.CustomHeaders['Connection'] := 'keep-alive';
      FClient.CustomHeaders['Upgrade-Insecure-Requests'] := '1';
      FClient.CustomHeaders['Content-Type'] := 'application/json';
      FClient.AllowCookies := False;
      FClient.HandleRedirects := False;

      var completePrompt:= Format(SUiBuilderPromptTemplate,[PromptText]);
      var PromptString := TJSONString.Create(completePrompt);  // já faz o escape necessário
      JSONObj.AddPair('model', 'deepseek-r1:1.5b');
      JSONObj.AddPair('prompt', PromptString);
      JSONObj.AddPair('stream', TJSONBool.Create(True));
      RequestBody := TStringStream.Create(JSONObj.ToJSON, TEncoding.UTF8);
      //RequestBody := TStringStream.Create( Format('{"model":"tinyllama","prompt":"%s","stream":true}', [completePrompt]),TEncoding.UTF8);
      try
        RequestBody.Position := 0;
        ResponseStream := FClient.Post('http://localhost:11434/api/generate', RequestBody).ContentStream;

        ResponseStream.Position := 0;

        StreamReader := TStreamReader.Create(ResponseStream, TEncoding.UTF8);
        try
          Done := False;
          while not StreamReader.EndOfStream and (not Done) do
          begin
            Line := StreamReader.ReadLine.Trim;
            if Line.IsEmpty then Continue;

            JSON := TJSONObject.ParseJSONValue(Line) as TJSONObject;
            try
              if Assigned(JSON) then
              begin
                if JSON.TryGetValue<string>('response', Chunk) then
                begin
                  ResultBuilder.Append(Chunk);
                  // Aqui você pode colocar um callback para atualizar UI em tempo real, se quiser
                end;

                if JSON.TryGetValue<Boolean>('done', Done) and Done then
                  Break;
              end;
            finally
              JSON.Free;
            end;
          end;
        finally
          StreamReader.Free;
        end;
      finally
        RequestBody.Free;
      end;
    finally
      //FClient.Free;
    end;

    Result := ResultBuilder.ToString;
  finally
    ResultBuilder.Free;
  end;
end;


end.
