{}
{ Project: BuilderUI Forms for Windows }
{ A visual form builder for Windows based on Delphi }
{ }
{ Copyright (c) 2024 Jo�o Bosco Becker }
{ }
{ Contributors to this file: Jo�o Bosco Becker }
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
{ Jo�o Bosco Becker - https://github.com/BoscoBecker }

unit Service.Component;

interface

uses System.Classes, Vcl.Controls, System.SysUtils;

type
  TComponentService = class
  public
    class function CountComponents(Root: TComponent): Integer;
  end;

implementation

class function TComponentService.CountComponents(Root: TComponent): Integer;
begin
  Result := 0;
  for var I := 0 to Root.ComponentCount - 1 do
  begin
    Inc(Result);
    Result := Result + CountComponents(Root.Components[I]);
  end;
end;

end.
