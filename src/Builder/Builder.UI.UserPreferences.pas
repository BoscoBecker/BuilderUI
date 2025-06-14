{}
{ Project: BuilderUI Forms for Windows }
{ A visual form builder for Windows based on Delphi }
{ }
{ Copyright (c) 2024 João Bosco Becker }
{ }
{ Contributors to this file: João Bosco Becker }
{ }
{ You can get the latest version of this file at: }
{ https://github.com/BoscoBecker/BuilderUI }
{ }
{ This library is free software; you can redistribute it and/or modify it }
{ under the terms of the GNU Lesser General Public License as published by the }
{ Free Software Foundation; either version 2.1 of the License, or (at your option) }
{ any later version. }
{ }
{ This library is distributed in the hope that it will be useful, but WITHOUT ANY }
{ WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A }
{ PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details. }
{ }
{ You should have received a copy of the GNU Lesser General Public License along }
{ with this library; if not, write to the Free Software Foundation, Inc., }
{ 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA. }
{ You may also obtain a copy of the license at: }
{ https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html }
{ }
{ João Bosco Becker - https://github.com/BoscoBecker }

unit Builder.UI.UserPreferences;

interface

uses System.JSON, System.Classes, System.SysUtils, System.Generics.Collections,
     System.TypInfo, Enum.Utils;

type
  IPreferenceObserver = interface
    ['{C5B96C5F-7E6E-4A4F-AF50-5327F041B33A}']
    procedure OnPreferenceChanged(const key, value: string);
  end;

type
 TUserPreferences = class
   private
     FObservers: TList<IPreferenceObserver>;
     FBackgroundStyle: string;
     FLastExportPath: string;
     FConfigFile: string;
     class var FInstance: TUserPreferences;
     class constructor Create;
     class destructor Destroy;

     procedure Load;
     procedure Save;
     procedure NotifyObservers(const key, Value : string);
   public
     class function Instance:  TUserPreferences;
     procedure SetLastExportPath(const Value: string);
     procedure SetConfigFile(const Value: string);
     procedure SetBackgroundStyle(const Value: string);
     procedure AddObserver(const Observer: IPreferenceObserver);
     procedure RemoveObserver(const Observer: IPreferenceObserver);
     procedure SavePreferences;
     procedure SetBackgroundEnum(const Value: TBuilderBackground);
     function GetBackgroundEnum: TBuilderBackground;
     function GetLastExportPath: string;

     property BackgroundStyle: string read FBackgroundStyle write SetBackgroundStyle;
     property LastExportPath: string read FLastExportPath write SetLastExportPath;
     property ConfigFile: string read FConfigFile write SetConfigFile;
 end;


implementation

procedure TUserPreferences.AddObserver(const Observer: IPreferenceObserver);
begin
  if not FObservers.Contains(Observer) then
    FObservers.Add(Observer);
end;

class constructor TUserPreferences.Create;
begin
  FInstance:= TUserPreferences.Create;
  FInstance.FConfigFile:= ExtractFilePath(ParamStr(0)) + 'BuilderUISettings.json';
  FInstance.FObservers:= TList<IPreferenceObserver>.Create;
  FInstance.Load;
end;

class destructor TUserPreferences.Destroy;
begin
  if FInstance.FObservers <> nil then
    FInstance.FObservers.Free;
end;

class function TUserPreferences.Instance: TUserPreferences;
begin
  Result := FInstance;
end;

procedure TUserPreferences.Load;
begin
  if not FileExists(FConfigFile) then Exit;

  var JSONString := TStringList.Create;
  try
    JSONString.LoadFromFile(FConfigFile);
    var JSON := TJSONObject.ParseJSONValue(JSONString.Text) as TJSONObject;
    try
      FBackgroundStyle := JSON.GetValue<string>('BackgroundStyle', 'bClear');
      FLastExportPath := JSON.GetValue<string>('LastExportPath', '');
    finally
      JSON.Free;
    end;
  finally
    JSONString.Free;
  end;
end;

procedure TUserPreferences.NotifyObservers(const key, Value: string);
begin
  for var Obs in FObservers do
    Obs.OnPreferenceChanged(Key, Value);
end;

procedure TUserPreferences.RemoveObserver(const Observer: IPreferenceObserver);
begin
  FObservers.Remove(Observer);
end;

procedure TUserPreferences.Save;
begin
  var JSON := TJSONObject.Create;
  try
    JSON.AddPair('BackgroundStyle', FBackgroundStyle);
    JSON.AddPair('LastExportPath', FLastExportPath);
    var JSONString := TStringList.Create;
    try
      JSONString.Text := JSON.ToJSON;
      JSONString.SaveToFile(FConfigFile);
    finally
      JSONString.Free;
    end;
  finally
    JSON.Free;
  end;
end;

procedure TUserPreferences.SavePreferences;
begin
  Save;
end;

procedure TUserPreferences.SetBackgroundEnum(const Value: TBuilderBackground);
begin
  FBackgroundStyle := GetEnumName(TypeInfo(TBuilderBackground), Ord(Value));
end;

function TUserPreferences.GetBackgroundEnum: TBuilderBackground;
begin
  Result := TBuilderBackground(GetEnumValue(TypeInfo(TBuilderBackground), FBackgroundStyle));
end;

function TUserPreferences.GetLastExportPath: string;
begin
  Result := FLastExportPath;
end;

procedure TUserPreferences.SetBackgroundStyle(const Value: string);
begin
  if FBackgroundStyle <> Value then
  begin
    FBackgroundStyle := Value;
    NotifyObservers('BackgroundStyle', Value);
  end;
end;

procedure TUserPreferences.SetConfigFile(const Value: string);
begin
  FConfigFile := Value;
end;

procedure TUserPreferences.SetLastExportPath(const Value: string);
begin
  if FLastExportPath <> Value then
  begin
    FLastExportPath := Value;
    NotifyObservers('LastExportPath', Value);
  end;
end;

end.

