unit Helper.ScrollStyleHelper;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Controls, Vcl.Forms,Vcl.ComCtrls,Vcl.StdCtrls ;

type
  TScrollBarStyleHelper = class
  public
    class procedure RoundScrollBar(AControl: TWinControl; Radius: Integer = 8);
    procedure ConfigureModernScrollBar(ScrollBar: TScrollBar);
  end;

implementation

uses
  Winapi.CommCtrl;

{ TScrollBarStyleHelper }

procedure TScrollBarStyleHelper.ConfigureModernScrollBar(ScrollBar: TScrollBar);
begin
 if not Assigned(ScrollBar) then Exit;

  // Configurações básicas de aparência
  ScrollBar.brush.Color := $00F5F5F5;          // Cor de fundo (cinza claro)
  //ScrollBar.brush.ParentColor := False;        // Impede herança de cor do parent
  ScrollBar.StyleElements := [];         // Desabilita estilos do sistema

  // Configura o thumb (a parte móvel da scrollbar)
//  if CheckWin32Version(6, 0) then        // Verifica se o OS suporta (Vista+)
//    ScrollBar.ThumbColor := $00404040;   // Cor do thumb (cinza escuro)

  // Configurações de tamanho e comportamento
  if ScrollBar.Kind = sbHorizontal then
    ScrollBar.Height := 10               // Scrollbar horizontal mais fina
  else
    ScrollBar.Width := 10;               // Scrollbar vertical mais fina

  // Comportamento moderno
  ScrollBar.PageSize := 0;               // Remove o efeito de "página"
  //ScrollBar.Smooth := True;              // Scroll suave
  ScrollBar.DoubleBuffered := True;      // Reduz flickering
end;

class procedure TScrollBarStyleHelper.RoundScrollBar(AControl: TWinControl; Radius: Integer);
var
  ScrollInfo: TScrollInfo;
  ScrollBar: TScrollBar;
begin
  // Verifica scrollbar vertical
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;

  if GetScrollInfo(AControl.Handle, SB_VERT, ScrollInfo) then
  begin
    ScrollBar := TScrollBar.Create(nil);
    try
      ScrollBar.Kind := sbVertical;
      ConfigureModernScrollBar(ScrollBar);
      // Aqui você aplicaria as configurações ao controle nativo
      // (esta parte requer mais trabalho para substituir completamente)
    finally
      ScrollBar.Free;
    end;
  end;
end;

end.

