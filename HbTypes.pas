unit HbTypes;

interface
uses
  MyStr,
  SysUtils, Controls, Classes;

type
  THBTipoEntrada = (teRightToLeft, teLeftToRight, teText, teDate);
  THBAlinhamento = (taEsquerda, taDireita, taCentro);
  THBInsertState = (isInsert, isOverwrite);
  THBTipoChave = (tcNenhuma, tcPesquisa, tcPrincipal, tcSugerida);
  THBComportamento = (cPadrao, cInvertido, cSempreHabilitado, cSempreDesabilitado, cSempreReadOnly, cEspecial);
  THBTiposPermissao = (tpIncluir, tpAlterar, tpExcluir);
  TValidateEvent = procedure(Sender: TObject; var SaidaValida: Boolean) of object;
  THbProgressEvent = procedure (n: Longint; Mensagem: String) of object;

  TStrArray = array of String;

  TReduceCallbackAsString = function(Acumulador, Item: String): String;
  TMapCallbackAsString = function(Item: String; Indice: Integer; Lista: TStrArray): String;

  THbStringList = class(TStringList)
  public
    function Implode(Separador: String): String;
    procedure AppendSemRepeticao(Valor: String);
  end;

const
  VK_A = 65;
  VK_B = 66;
  VK_C = 67;
  VK_D = 68;
  VK_E = 69;
  VK_F = 70;
  VK_G = 71;
  VK_H = 72;
  VK_I = 73;
  VK_J = 74;
  VK_K = 75;
  VK_L = 76;
  VK_M = 77;
  VK_N = 78;
  VK_O = 79;
  VK_P = 80;
  VK_Q = 81;
  VK_R = 82;
  VK_S = 83;
  VK_T = 84;
  VK_U = 85;
  VK_V = 86;
  VK_W = 87;
  VK_X = 88;
  VK_Y = 89;
  VK_Z = 90;

  HBFormatoData: String = 'dd/mm/yyyy';
  HBMatrizCursor: array of TCursor = nil;
  CinzaClaro = $00E3E3E3;
  VerdeClaro = $00E1FFF0;
  AmareloClaro = $00EEFFFF;
  NEW_LINE = #13#10;

  ShChaveParamLocal = '\Software\Shadow\Comum';
  ShParPreviewFont  = 'PreviewFont';

implementation


{ THbStringList }

function THbStringList.Implode(Separador: String): String;
var
  i: Integer;
begin
  Result := '';
  if Count < 1 then
    Exit;
  for i := 0 to Count-1 do
    Result := Result + IIfStr(Result <> '', Separador, '') + Strings[i];
end;


procedure THbStringList.AppendSemRepeticao(Valor: String);
begin
  if IndexOf(Valor) < 0 then
    Append(Valor);
end;


end.
