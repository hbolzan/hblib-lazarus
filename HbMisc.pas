(* HBMisc.PAS
 ;
 ; Miscelanea
 ; Diversos componentes e funções
 ;
*)
unit HBMisc;

interface

uses
  StrUtils,
  SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs;

function Extenso(i: Longint): String;
function ValorExtenso(r: Real): String;
function ProximaQuebra(const s: String; Inicio: Integer): String;

{ funções para problemas de geometria analítica }
function SegTamanho(x0, y0, x1, y1: Real): Real;
procedure SegPonto(x0, y0, x1, y1, d: Real; var x2, y2: Real);
procedure SegParalelo(x0, y0, x1, y1, d: Real; var xp0, yp0, xp1, yp1: Real);
procedure SegPontoMedio(x0, y0, x1, y1: Real; var xm, ym: Real);

// validações
function CPFValido (const CPF: String): Boolean;

implementation

type
  Str12 = String[12];

const
  Milhao = 1000000;
  Bilhao = 1000000000;

  Unidades: array[1..19] of Str12 = ('UM', 'DOIS', 'TRES', 'QUATRO',
                                     'CINCO', 'SEIS', 'SETE', 'OITO',
                                     'NOVE', 'DEZ', 'ONZE', 'DOZE',
                                     'TREZE', 'CATORZE', 'QUINZE',
                                     'DEZESSEIS', 'DEZESSETE', 'DEZOITO',
                                     'DEZENOVE');

  Dezenas: array[1..10] of Str12 = ('DEZ', 'VINTE', 'TRINTA',
                                    'QUARENTA', 'CINQÜENTA', 'SESSENTA',
                                    'SETENTA', 'OITENTA', 'NOVENTA', 'CEM');

  Centenas:array[1..10] of Str12 = ('CENTO', 'DUZENTOS', 'TREZENTOS',
                                    'QUATROCENTOS', 'QUINHENTOS',
                                    'SEISCENTOS', 'SETECENTOS',
                                    'OITOCENTOS', 'NOVECENTOS', 'MIL');

  Milhoes: array[1..2] of Str12 = ('MILHAO', 'MILHOES');
  Bilhoes: array[1..2] of Str12 = ('BILHAO', 'BILHOES');




(* Extenso
 ;
 ; retorna o extenso de
 ; um número inteiro
 ;
*)
function Extenso(i: Longint): String;
var
  b, m, mm, c, d, u: Longint;
  bStr, mStr, mmStr, cStr, dStr, uStr: String;

begin
  { decompõe em bilhões, milhões, milhares, }
  { centenas, dezenas e unidades }
  b := i div Bilhao;
  mm := (i-b*Bilhao) div Milhao;
  m := (i-b*Bilhao - mm*Milhao) div 1000;
  c := (i-b*Bilhao - mm*Milhao - m*1000) div 100;
  d := (i-b*Bilhao - mm*Milhao - m*1000 - c*100) div 10;
  u := (i-b*Bilhao - mm*Milhao - m*1000 - c*100 - d*10);

  { inicializa as strings }
  bStr := ''; mmStr := ''; mStr := '';
  cStr := ''; dStr := ''; uStr := '';

  { para cada componente maior que zero }
  { entra em recursão para gerar o fragmento }
  { do extenso para aquele componente }

  if b > 0 then begin
    bStr := Extenso(b);
    if b > 1 then bStr := bStr+' '+Bilhoes[1]
    else bStr := bStr+' '+Bilhoes[2];
  end;

  if mm > 0 then begin
    mmStr := Extenso(mm);
    if mm > 1 then mmStr := mmStr+' '+Milhoes[2]
    else mmStr := mmStr+' '+Milhoes[1];
  end;

  if m > 0 then mStr := Extenso(m) + ' MIL';

  if c > 0 then begin
    if c > 1 then cStr := Centenas[c] else
    if c = 1 then begin
      if (d > 0) or (u > 0) then cStr := Centenas[c]
      else cStr := Dezenas[10];
    end;
  end;

  if d > 0 then begin
    if (d > 1) or (u = 0) then dStr := Dezenas[d];
    if (d > 1) and (u > 0) then uStr := Unidades[u] else
    if (d = 1) and (u > 0) then dStr := Unidades[10*d+u];
  end
  else if (d = 0) and (u > 0) then uStr := Unidades[u];

  Result := bStr;
  if (b > 0) and ((mm+m+c+d+u) > 0)
  then Result := Result+' E '
  else if (b > 0) and ((mm+m+c+d+u) = 0)
  then Result := Result+' DE';

  Result := Result+mmStr;
  if (mm > 0) and ((m+c+d+u) > 0) then Result := Result+' E ' else
  if (mm > 0) and ((m+c+d+u) = 0) then Result := Result+' DE';

  Result := Result+mStr;
  if (m > 0) and ((c+d+u) > 0) then Result := Result+' E ';

  Result := Result+cStr;
  if (c > 0) and ((d+u) > 0) then Result := Result+' E ';

  Result := Result+dStr;
  if (d > 1) and (u > 0) then Result := Result+' E ';

  Result := Result+uStr;
end;



(* ValorExtenso
 ;
 ; retorna o valor de r
 ; acompanhados de reais e centavos
 ;
*)
function ValorExtenso(r: Real): String;
var
  Reais,
  Centavos: Longint;
  n: Real;
begin
  Reais := Trunc(r);
  n := r-Reais;
  Centavos := Round((r-Reais)*100);

  Result := '';
  if Reais > 0 then begin
    Result := Extenso(Reais);
    if Reais > 1
    then Result := Result + ' REAIS'
    else Result := Result + ' REAL';
    if Centavos > 0 then Result := Result+' E ';
  end;

  if Centavos > 0 then begin
    Result := Result+Extenso(Centavos)+' CENTAVO';
    if Centavos > 1 then Result := Result+'S';
  end;
end;

(* ProximaQuebra
 ;
 ; retorna o texto até
 ; a próxima quebra a partir
 ; da posição Início
 ;
*)
function ProximaQuebra(const s: String; Inicio: Integer): String;
var
  t: String;
  i, l: Integer;
begin
  { considera os seguintes caracteres para quebra: }
  { .,;-+|/|}
  t := '';
  i := Inicio;
  l := Length(s);
  repeat
    t := t+s[i];
    Inc(i);
  until (i > l) or (s[i] in [' ', '.', ',', ';', '-', '+', '|', '/']);
  Result := t;
end;



(* um pouquinho de geometria analítica *)
(* SegTamanho
 ;
 ; retorna o tamanho do segmento
 ; que vai de x0, y0 a x1, y1
 ;
*)
function SegTamanho(x0, y0, x1, y1: Real): Real;
var
  DeltaX,
  DeltaY: Real;
begin
  DeltaX := Abs(x1-x0);
  DeltaY := Abs(y1-y0);

  { a soma dos quadrados dos catetos }
  { é igual ao quadrado da hipotenusa }
  { DeltaX^2 + DeltaY^2 = Result^2 }
  { => Result = Sqrt(DeltaX^2 + DeltaY^2) }
  Result := Sqrt(Sqr(DeltaX) + Sqr(DeltaY));
end;


(* SegPonto
 ;
 ; dado um segmento x0, y0 -> x1, y1
 ; e uma distancia d
 ; determinar o ponto x2, y2 que define
 ; o segmento x0, y0 -> x2, y2 de tamanho d
 ; na mesma reta que x0, y0 -> x1, y1
 ;
*)
procedure SegPonto(x0, y0, x1, y1, d: Real; var x2, y2: Real);
var
  DeltaX,
  DeltaY,
  d0: Real;
begin
  DeltaX := x1-x0;
  DeltaY := y1-y0;
  d0 := SegTamanho(x0, y0, x1, y1);
  x2 := x0+(DeltaX/d0)*d;
  y2 := y0+(DeltaY/d0)*d;
end;


(* SegParalelo
 ;
 ; encontra o segmento paralelo
 ; a x0, y0 -> x1, y1 a uma distancia
 ; perpendicular d
 ;
*)
procedure SegParalelo(x0, y0, x1, y1, d: Real; var xp0, yp0, xp1, yp1: Real);
var
  DeltaX, DeltaY,
  DeltaPx, DeltaPy,
  l: Real;
begin
  { DeltaPx = d*(DeltaY/l) }
  { DeltaPy = d*(DeltaX/l) }

  { determinar o tamanho do segmento }
  l := SegTamanho(x0, y0, x1, y1);

  { calcular os Deltas do segmento origem }
  DeltaX := x1-x0;
  DeltaY := y1-y0;

  { calcular os Deltas do segmento destino }
  DeltaPx := -d*(DeltaY/l);
  DeltaPy := d*(DeltaX/l);

  { calcular os pontos que definem o segmento }
  xp0 := x0+DeltaPx;
  yp0 := y0+DeltaPy;
  xp1 := x1+DeltaPx;
  yp1 := y1+DeltaPy;
end;



(* SegPontoMedio
 ;
 ; retorna o ponto médio
 ; do segmento x0, y0 -> x1, y1
 ;
*)
procedure SegPontoMedio(x0, y0, x1, y1: Real; var xm, ym: Real);
begin
  xm := (x1-x0)/2+x0;
  ym := (y1-y0)/2+y0;
end;


(** SomenteNumeros
 *
 * remove caracteres não numéricos
 *
 *)
function SomenteNumeros (Expressao: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Expressao) do
    if Expressao[i] in ['0'..'9'] then
      Result := Result + Expressao[i];
end;

(** GetDigitoModulo11
 *
 * calcula um dígito módulo 11
 *
 *)
function GetDigitoModulo11 (Expressao: String): Integer;
var
  i, Len: Integer;

begin
  Result := 0;
  Len := Length(Expressao);
  for i := 1 to Len do
    Result := Result + (StrToInt(Expressao[i])*(Len+2-i));

  Result := 11 - Result mod 11;
  if (Result = 10) or (Result = 11) then
    Result := 0;
end;

(** CPFDigitosVerificadores
 *
 * calcula dígitos verificadores para um número de CPF
 *
 *)
function CPFDigitosVerificadores (const CPF: String): String;
var
  Expressao: String;
begin
  Expressao := Copy (CPF, 1, 9);
  Expressao := Expressao + IntToStr(GetDigitoModulo11(Expressao));
  Expressao := Expressao + IntToStr(GetDigitoModulo11(Expressao));
  Result := Copy (Expressao, 10, 2);
end;

(** CPFFajuto
 *
 * identifica tentativas fáceis de fajutar o CPF
 *
 *)
function CPFFajuto (const CPF: String): Boolean;
begin

  Result := AnsiIndexStr(CPF,
    [
      '00000000000', '11111111111', '22222222222', '33333333333', '44444444444',
      '55555555555', '66666666666', '77777777777', '88888888888', '99999999999'
    ]
  ) >= 0;
  
end;


(** CPFValido
 *
 * faz validação de CPF
 *
 *)
function CPFValido (const CPF: String): Boolean;
var
  CPFLimpo: String;
begin
  // default false
  Result := False;

  // remove separadores
  CPFLimpo := SomenteNumeros(CPF);

  // CPF sem sepradores deve ter 11 caracteres
  // e evita enganação barata
  if (Length(CPFLimpo) <> 11) or CPFFajuto(CPFLimpo) then
    Exit;

  // calcula os digitos verificadores
  Result := CPFDigitosVerificadores(CPFLimpo) = Copy (CPFLimpo, 10, 2);
end;



end.

