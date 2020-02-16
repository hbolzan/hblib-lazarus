unit MyStr;
interface

uses
  SysUtils, Classes, Graphics, Dialogs;

type
  TMyStrArray = array of String;

function Transform(n: Real; FPicture: String): String;
function RTrim(s: String): String;
function LTrim(s: String): String;
function AllTrim(s: String): String;
function Replicate(c: Char; n: Integer): String;
function Left(s: String; n: Integer): String;
function Right(s: String; n: Integer): String;
function PadL(s: String; n: Integer; c_preenche: Char): String;
function PadC(s: String; n: Integer; c_preenche: Char): String;
function PadR(s: String; n: Integer; c_preenche: Char): String;
function IncrementoAlfa(s: String; TamanhoMaximo: Integer): String;
function InteiroDe(s: String): Longint;
function RealDe(const S: String): Real;
function BooleanoDe(s: String): Boolean;
function BoolToStr(l: Boolean): String;
function Cripto(s, Senha: String): String;
function DeCripto(s, Senha: String): String;
function Split(sep, s: String): TMyStrArray;
function ValorDoElemento(Lista: TMyStrArray; Indice: Integer): String;
function Explode(sep, s: String): TMyStrArray;
function Implode(Cola: String; Lista: TMyStrArray): String;
function ValorExisteNaLista(Valor: string; Lista: TMyStrArray): Boolean;
procedure Desmembr(texto: String; separador: Char; lista: TStringList);
function Substituir(s, Origem, Destino: String): String;
function SubstituirDelimitado (Texto, Busca, Subst: String): String;
function ReplaceMultiplo (Texto: String; Antigos, Novos: array of String): String;
function ProximaOcorrencia(s: String; c: Char; Inicio: Integer): Integer;
function FontToStr(AFont: TFont; SemCor: Boolean = False): String;
procedure StrToFont(s: String; AFont: TFont; SemCor: Boolean = False); 
procedure QuebrarEmLinhas(n: Integer; s: String; Lista: TStringList);
function GetQuebraEmLinhas(s: String; n: Integer): TMyStrArray;
function TimeToSeconds(Horario: TDateTime): Longint;
function TimeToSecondsR(Horario: TDateTime): Real;
function SecondsToTime(Segundos: Longint): TDateTime;
function IIfStr(BoolExpr: Boolean; RetTrue, RetFalse: String): String;
function IIf(BoolExpr: Boolean; RetTrue, RetFalse: Longint): Longint; overload;
function IIf(BoolExpr: Boolean; RetTrue, RetFalse: Real): Real; overload;
function IIf(BoolExpr: Boolean; RetTrue, RetFalse: String): String; overload;
function IIf(BoolExpr: Boolean; RetTrue, RetFalse: TMsgDlgBtn): TMsgDlgBtn; overload;
function ExprOrDefault(Expr, Default: String): String;

// function IIf(BoolExpr: Boolean; RetTrue, RetFalse: TDateTime): TDateTime; overload;

function AddBarra(Caminho: String): String;
function DelBarra(Caminho: String): String;
function L2Bin(Valor: Longint): String;
function Bin2L(s: String): Longint;
function HexToInt(sHex: String): Longint;
function IntToBin(i: Integer): String;
function HbIsInt(s: String): Boolean;
function PreencherPosicao(nPos: Integer; sOrigem, sInsert: String): String;
function Capitalize(s: String): String;
function CapitalizeTexto(Texto: String): String;
function Mac2DecStr(MACAddr: String): String;
function RightPos(Substr: string; S: string): Integer;
function LeftPos(Substr: string; S: string): Integer;
function MensagemMsSQLFiltrada(s: String): String;
function MensagemPostgresFiltrada(s: String): String;
procedure SubstituirMacro(Nome, Valor: String; Lista: TStrings);
function RemoverAcentos(Str: String): String;
function PosicaoFechamento (S, DelimAbre,
  DelimFecha: String; Inicio: Integer=1): Integer;
function RemoverComentariosSql (Sql: String): String;
function StartsWith(const S, Search: String): Boolean;
function EndsWith(const S, Search: String): Boolean;
function IsNumeric(const S: String): Boolean;
function ValueBetween(const Value, Lower, Upper: Real): Boolean;
function EscapeSQL(const Sql: String): String;
function PosExCaseInsensitive(const Procurado, Texto: String;
  Offset: Cardinal = 1): Integer;
function PosicaoDaUltimaOcorrencia(const Procurado, Texto: String;
  CaseSensitive: Boolean = False): Integer;
function PosicaoDoPrimeiroSeparador(const Texto: String): Integer;
function PosicaoDoUltimoSeparador(const Texto: String): Integer;
function GetPrimeiraPalavra(const Texto: String): String;
function FloatToSql(Valor: Real): String;
function InMyStrArray(Valor: String; UmArray: TMyStrArray): Boolean;
procedure AppendToMyStrArray(Valor: String; var UmArray: TMyStrArray);
function GetItemDaListaDelimitada(UmaListaDelimitada, UmDelimitador: String;
  UmItem: Integer): String;
function getVariantDateAsString(varDate: Variant): String;
function StrToVarDate(StrDate: String): Variant;
function RemoverMascara(Valor: String): String;
function ConcatenarComSeparador(Destino, Adicionar, Separador: String): String;


implementation

uses StrUtils, Variants;

const
  MatrizHex = '0123456789ABCDEF';

var
  UnidadeTDateTime: Double;

(* Transform
 ;
 ; funciona como o transform do Clipper
 ; mas sÛ para n˙meros
 ;
*)
function Transform(n: Real; FPicture: String): String;
var
  i, l:     Integer;
  t, s:     String;
  p, w, d:  Integer;
begin
  s := '';
  w := Length(FPicture);

  { calcular o n˙mero de casas      }
  { decimais definidas pela m·scara }
  p := Pos(',', FPicture);
  if p = 0 then d := 0
  else d := Length(FPicture)-p;

  { converte o valor para string }
  Str(n:w:d, t);
  t := AllTrim(t);

  { monta o texto default }
  for i := 1 to w do begin
    if FPicture[i] = '9'
    then s := s+'0'
    else s := s+FPicture[i];
  end;

  { agora preenche com o texto }
  l := Length(t);
  i := Length(FPicture);
  if l > 0 then repeat
    while (i > 0) and ((s[i] = ',') or (s[i] = '.')) do Dec(i);
    while (l > 0) and not ((t[l] >= '0') and (t[l] <= '9')) do Dec(l);
    if (i > 0) and (l > 0) then s[i] := t[l];
    Dec(i);
    Dec(l);
  until (i = 0) or (l = 0);

  { d· um trim nos zeros ‡ esquerda }
  if d <= 0 then d := 1 else d := d+2;
  while (Length(s) > d) and (s[ 1] in ['0', '.']) do Delete( s, 1, 1);

  Result := s;
end;

function RTrim(s: String): String;
begin
  { espaÁos ‡ direita  }
  while (Length(s) > 0) and (s[Length(s)] = ' ') do Delete( s, Length(s), 1);
  Result := s;
end;

function LTrim(s: String): String;
begin
  { espaÁos ‡ esquerda }
  while (Length(s) > 0) and (s[ 1] = ' ') do Delete( s, 1, 1);
  Result := s;
end;

(* AllTrim
 ;
 ; IdÍntica ‡ funÁ„o homÙnima do Clipper
 ; Poda os espaÁos em branco ‡ direita
 ; e a esquerda
 ;
*)
function AllTrim( s: String ): String;
begin
  { espaÁos ‡ esquerda }
  while (Length(s) > 0) and (s[ 1] = ' ') do Delete( s, 1, 1);

  { espaÁos ‡ direita  }
  while (Length(s) > 0) and (s[Length(s)] = ' ') do Delete( s, Length(s), 1);

  AllTrim := s;
end;

(* Left, Right
 ;
 ; Adivinha...
 ;
*)
function Left(s: String; n: Integer): String;
begin
  Result := Copy(s, 1, n);
end;

function Right(s: String; n: Integer): String;
var
  l: Integer;

begin
  l := Length(s);
  if n >= l then Result := s else
  Result := Copy(s, l-n+1, n);
end;


(* Replicate
 ;
 ; igual ‡ do Clipper
 ;
*)
function Replicate(c: Char; n: Integer): String;
var
  i: Integer;
  s: String;

begin
  if n < 1 then Result := ''
  else begin
    s := '';
    for i := 1 to n do s := s+c;
    Result := s;
  end;
end;



(* Pad
 ;
 ; serve para PadL, PadC e PadR
 ;
*)
function Pad(s: String; n: Integer; c_preenche, c_tipo: Char): String;
var
  s1, s2:       String;
  l1, l2:       Integer;

begin
  s1 := AllTrim(s);
  l1 := Length(s1);
  l2 := n-l1;

  if l2 = 0 then s2 := s1
  else if l2 < 0 then begin
    { preenchimento ‡ esquerda significa alinhamento ‡ direita e vice-versa }
    if c_tipo = 'L' then s2 := Copy(s1, -l2+1, n) else
    if c_tipo = 'C' then s2 := Copy(s1, (-l2+2) div 2, n)
    else s2 := Copy(s1, 1, n);
  end else case c_tipo of
    'L': s2 := Replicate(c_preenche, l2)+s1;
    'R': s2 := s1+Replicate(c_preenche, l2);
    'C': begin
           if l2 < 2 then
             s2 := c_preenche+s1
           else
             s2 := Replicate(c_preenche, l2 div 2)+s1
                  +Replicate(c_preenche, l2-(l2 div 2));
           end;
  end;
  Pad := s2;
end;


(* PadL, PadC e PadR
 ;
 ; funÁıes de preenchimento
 ; iguais ‡s do Clipper
 ;
*)
function PadL(s: String; n: Integer; c_preenche: Char): String;
begin
  Result := Pad(s, n, c_preenche, 'L');
end;

function PadC(s: String; n: Integer; c_preenche: Char): String;
begin
  Result := Pad(s, n, c_preenche, 'C');
end;

function PadR(s: String; n: Integer; c_preenche: Char): String;
begin
  Result := Pad(s, n, c_preenche, 'R');
end;


(* Substituir
 ;
 ; todas as ocorrencias de Origem
 ; na string s s„o substituidas por Destino
 ;
*)
function Substituir(s, Origem, Destino: String): String;
begin
  Result := StringReplace(s, Origem, Destino, [rfReplaceAll, rfIgnoreCase]);
end;


(** SubstituirDelimitado
 *
 * substitui somente delimitados
 * considera como delimitador
 * qualquer caracter diferente de letras, n˙meros e underline
 *
 *)
function SubstituirDelimitado (Texto, Busca, Subst: String): String;
var
  dAntesOk, dDepoisOk: Boolean;
  n, l, lt, i: Integer;
  sRet, sTexto, sBusca: String;
begin
  sBusca := AnsiLowerCase(Busca);
  sTexto := Texto;

  // se n„o tem busca ou est· substituindo pela mesma coisa, ignora
  if (Length(sBusca) = 0) or (sBusca = AnsiLowerCase (Subst)) then
    Exit;

  l := Length (sBusca);
  sRet := '';
  repeat
    lt := Length (sTexto);
    n := Pos (sBusca, AnsiLowerCase (sTexto));

    if n > 0 then
    begin
      // verifica se tem delimitadores
      // antes
      if n = 1 then
        dAntesOk := True
      else
        dAntesOk := not (sTexto[n-1] in ['a'..'z', 'A'..'Z', '0'..'9', '_']);

      // depois
      if n+l > lt then
        dDepoisOk := True
      else
        dDepoisOk := not (sTexto[n+l] in ['a'..'z', 'A'..'Z', '0'..'9', '_']);

      if dAntesOk and dDepoisOk then
      // 1234567890
      // xxxbusca_yy
      begin
        sRet := sRet+Copy (sTexto, 1, n-1)+Subst;
        sTexto := Copy (sTexto, n+l, lt);

      end else
      begin
        sRet := sRet+Copy (sTexto, 1, n+l-1);
        sTexto := Copy (sTexto, n+l, lt);

        // adiciona atÈ o fim da palavra
        lt := Length (sTexto);
        n := 1;
        repeat
          sRet := sRet+sTexto[n];
          n := n+1;
        until (n = lt) or (not (sTexto[n] in ['a'..'z', 'A'..'Z', '0'..'9', '_']));
        sTexto := Copy (sTexto, n, lt);
      end;

    end else
      sRet := sRet+sTexto;

  until n = 0;

  Result := sRet;
end;


function ReplaceMultiplo(Texto: String; Antigos, Novos: array of String): String;
var
  i, i_novo, len_antigos, len_novos: Integer;
begin
  Result := Texto;

  len_antigos := Length(antigos);
  len_novos := Length(novos);
  if (len_antigos < 1) or (len_novos < 1) then begin
    Exit;
  end;

  for i := 0 to len_antigos - 1 do begin
    if len_novos < (i+1) then begin
      i_novo := len_novos-1
    end else begin
      i_novo := i;
    end;

    Result := Substituir(Result, antigos[i], novos[i_novo]);
  end;
end;


(* RealDe
 ;
 ; converte s para um valor Real
 ;
*)
function RealDe(const S:  String): Real;
var
  PosicaoOndeOcorreuErro: Integer;
  Resultado: Real;
  ValorStr: String;
begin
  if Pos(',', S) > 0 then
    ValorStr := Trim(Substituir(Substituir(S, '.', ''), ',', '.'))
  else
    ValorStr := Trim(S);
  Val(ValorStr, Resultado, PosicaoOndeOcorreuErro);
  Result := IIf(PosicaoOndeOcorreuErro > 0, 0, Resultado);
end;

(* InteiroDe
 ;
 ; converte s para um valor
 ; de tipo inteiro
 ;
*)
function InteiroDe(s: String): Longint;
begin
  Result := Round(RealDe(s));
end;

(* BooleanoDe
 ;
 ; converte a string
 ; para True ou False
 ;
*)
function BooleanoDe(s: String): Boolean;
begin
  Result := StrToBool(s);
end;

function BoolToStr(l: Boolean): String;
begin
  if l then Result := 'TRUE' else Result := 'FALSE';
end;


(* Split
 ;
 ; igual ‡ funÁ„o split do Perl
 ;
*)
function Split(sep, s: String): TMyStrArray;
var
  n, l, ls, t: Integer;
  a: TMyStrArray;
begin
  a := nil;
  Result := a;

  // HBB - 2011-08-22
  // se s for uma string vazia, retorna vazio
  if s = '' then
    Exit;

  l := 0;
  t := Length(sep);
  ls := Length(s);
  n := Pos(sep, s);
  while n > 0
  do begin
    SetLength(a, l+1);
    a[l] := Copy(s, 1, n-1); // atribui a prÛxima entrada da lista ao elemento atual
    s := Copy(s, n+t, ls);   // e poda esta ponta de s
    n := Pos(sep, s);
    Inc(l);
  end;

  (*
  // verifica a sobra
  if s <> ''
  then begin
    SetLength(a, l+1);
    a[l] := s;
  end;
  *)

  // adiciona a sobra
  SetLength(a, l+1);
  a[l] := s;

  // retorna a
  Result := a;
end;

function ValorDoElemento(Lista: TMyStrArray; Indice: Integer): String;
begin
  Result := '';
  if ( Length(Lista) >= Indice + 1 ) then begin
    Result := Lista[Indice];
  end;
end;

function Explode(sep, s: String): TMyStrArray;
begin
  Result := Split(sep, s);
end;

function Implode(Cola: String; Lista: TMyStrArray): String;
var
  i, l: Integer;
begin
  Result := '';
  l := Length(Lista);
  if l < 1 then
    Exit;

  for i := 0 to l-1 do
    Result := Result + IIfStr(Result <> '', Cola, '') + Lista[i];
end;


function ValorExisteNaLista(Valor: string; Lista: TMyStrArray): Boolean;
var
  Indice: Integer;
begin
  Result := False;
  for Indice := 0 to Length(Lista)-1 do
    if SameText(Valor, Lista[Indice]) then
    begin
      Result := True;
      Exit;
    end;
end;


(* Desmembr
 ;
 ; procedure de desmembramento de strings
 ; armazena o resultado na string list
 ; recebida como parametro
 ;
*)
procedure Desmembr( texto: String; separador: Char; lista: TStringList);
var
  i, l: Integer;
  s:    String;

begin
  { antes de mais nada, limpa a lista }
  lista.Clear;
  s := '';
  l := Length(texto);

  if (l > 0) then begin
    for i := 1 to l do begin
      if (texto[i] <> separador) then
        s := s+texto[i]
      else begin
        lista.Add(s);
        s := '';
      end;
    end;
    { como n„o h· separador apÛs o ˙ltimo item,
      ele ainda est· armazenado em S
    }
    lista.Add(s);
  end;
end;


(* ProximoChar
 ;
 ; Determina o prÛximo caractere
 ; alfa ou numÈrico
 ;
*)
function ProximoChar(const c: Char; var Virou: Boolean): Char;
var
  p:  Char;
begin
  p := c;
  Virou := False;

  { se for espaÁo, vira para 1 }
  if p = ' ' then p := '1'
  else
  { verifica se È A..Z }
  if p in ['A'..'Z'] then begin
    Inc(p);
    if p > 'Z' then begin
      p := 'A';
      Virou := True;
    end;
  end else

  { verifica se È a..z }
  if p in ['a'..'z'] then begin
    Inc(p);
    if p > 'z' then begin
      p := 'a';
      Virou := True;
    end;
  end else

  { verifica se È 0..9 }
  if p in ['0'..'9'] then begin
    Inc(p);
    if p > '9' then begin
      p := '0';
      Virou := True;
    end;

  { se n„o for nenhum dos anteriores           }
  { considera como separador (como / . - etc.) }
  { ent„o n„o muda o caracter mas retorna      }
  { virou = True para forÁar o incremento do   }
  { prÛximo dÌgito ‡ esquerda                  }
  end else Virou := True;
  Result := p;
end;

(* IncrementoAlfa
 ;
 ; retorna o valor incrementado
 ; para uma seq¸Íncia alfanumÈrica
 ;
 ; o paramentro TamanhoMaximo sÛ
 ; È relevante se "s" vier vazio
 ;
*)
function IncrementoAlfa(s: String; TamanhoMaximo: Integer): String;
var
  Virou:    Boolean;
  i:        Integer;
begin
  Result := '';
  if (s = '') then begin
    Exit;
  end;
  s := PadL(s, TamanhoMaximo, ' ');
  Virou := True;
  i := Length(AllTrim(s));
  if i = 0 then
    s := Replicate('0', TamanhoMaximo-1)+'1'
  else begin
    i := Length(s);
    while (i > 0) and Virou do begin
      s[i] := ProximoChar(s[i], Virou);
      Dec(i);
    end;
  end;
  Result := Trim(s);
end;


(* Cripto
 ;
 ; criptografa a string s com Senha
 ;
*)
function Cripto(s, Senha: String): String;
var
  i, j, l: Integer;
  nSenha: Integer;
  b: Integer;
  r: String;
begin
  r := '';
  j := 0;
  l := Length(Senha);
  if l = 0 then r := s;

  if (l > 0) and (Length(s) > 0) then
  for i := 1 to Length(s) do begin
    Inc(j); if j > l then j := 1;
    nSenha := Ord(Senha[j]);
    b := Ord(s[i]);
    Inc(b, nSenha);
    r := r+Chr(b);
  end;
  Result := r;
end;


function DeCripto(s, Senha: String): String;
var
  i, j, l: Integer;
  nSenha: Integer;
  b: Integer;
  r: String;
begin
  r := '';
  j := 0;
  l := Length(Senha);
  if l = 0 then r := s;

  if (l > 0) and (Length(s) > 0) then
  for i := 1 to Length(s) do begin
    Inc(j); if j > l then j := 1;
    nSenha := Ord(Senha[j]);
    b := Ord(s[i]);
    Dec(b, nSenha);
    r := r+Chr(b);
  end;
  Result := r;
end;

(* ProximaOcorrencia
 ;
 ; retorna a prÛxima ocorrencia
 ; do caracter c na string s
 ; a partir da posiÁ„o Inicio
 ;
*)
function ProximaOcorrencia(s: String; c: Char; Inicio: Integer): Integer;
var
  i, l, n: Integer;
begin
  i := Inicio;
  l := Length(s);
  n := 0;

  while (i < l) and (n = 0) do begin
    Inc(i);
    if s[i] = c then n := i;
  end;

  Result := n;
end;


function FontToStr(AFont: TFont; SemCor: Boolean = False): String;
var
  s: String;
begin
  if not SemCor then
    s := IntToStr(AFont.Color)+';'
  else
    s := '';
  s := s + AFont.Name+';'+IntToStr(AFont.Size)+';[';
  if fsBold in AFont.Style then s := s+'fsBold,';
  if fsItalic in AFont.Style then s := s+'fsItalic,';
  if fsUnderline in AFont.Style then s := s+'fsUnderline,';
  if fsStrikeOut in AFont.Style then s := s+'fsStrikeOut,';
  if Copy(s, Length(s), 1) = ',' then s := Copy(s, 1, Length(s)-1);
  s := s+']';
  Result := s;
end;

procedure StrToFont(s: String; AFont: TFont; SemCor: Boolean = False); 
var
  i, Indice: Integer;
  Lista: TSTringList;
  sStyle: String;
begin
  Lista := TStringList.Create;
  try
    Indice := -1;
    Desmembr(s, ';', Lista);
    if (Lista.Count >= 4) or (SemCor and (Lista.Count >= 3)) then
    begin
      if not SemCor then
      begin
        AFont.Color := StrToInt(Lista[0]);
        Indice := 0;
      end;            
      AFont.Name := Lista[Indice+1];
      AFont.Size := StrToInt(Lista[Indice+2]);
      AFont.Style := [];
      sStyle := Lista[Indice+3];
      sStyle := Copy(sStyle, 2, Length(sStyle)-2); { remove os [] }

      Desmembr(sStyle, ',', Lista);
      if Lista.Count > 0
      then for i := 0 to Lista.Count-1 do begin
        if Lista[i] = 'fsBold' then AFont.Style := AFont.Style+[fsBold] else
        if Lista[i] = 'fsItalic' then AFont.Style := AFont.Style+[fsItalic] else
        if Lista[i] = 'fsUnderline' then AFont.Style := AFont.Style+[fsUnderline] else
        if Lista[i] = 'fsStrikeOut' then AFont.Style := AFont.Style+[fsStrikeOut];
      end;
    end;
  finally
    Lista.Free;
  end;
end;



(* ExtrairProximaPalavra
 ;
 ; retorna a prÛxima palavra em s
 ; e devolve s sem ela
 ;
*)
function ExtrairProximaPalavra(var s: String): String;
var
  c: Char;
  i, l: Integer;
begin
  Result := '';
  s := Trim(s);
  if s <> '' then begin
    i := 1;
    l := Length(s);
    repeat
      c := s[i];
      Result := Result+c;
      Inc(i);
    until (i > l) or (c in [' ', ',', '.', '-', '/']);
    if i > l then s := '' else s := Copy(s, i, l);
  end;
end;




(* GetProximaPalavra
 ;
 ; retorna a prÛxima palavra em s
 ;
*)
function GetProximaPalavra(s: String): String;
var
  c: Char;
  i, l: Integer;
begin
  Result := '';
  // s := Trim(s);
  if s <> '' then begin
    i := 1;
    l := Length(s);
    repeat
      c := s[i];
      Result := Result+c;
      Inc(i);
    until (i > l) or (c in [' ', ',', '.', '-', '/']);

    // verfica se tem espaÁos depois do separador e adiciona 
    while (i <= l) and (s[i] in [' ', ',', '.', '-', '/'])
    do begin
      Result := Result+s[i];
      Inc(i);
    end;
  end;
end;

(* QuebrarEmLinhas
 ;
 ; quebra s em linhas com largura m·xima n
 ;
*)
procedure QuebrarEmLinhas(n: Integer; s: String; Lista: TStringList);
var
  Excedeu: Boolean;
  Linha, Proxima: String;
begin
  Lista.Clear;
  if n > 0
  then repeat
    Excedeu := False;
    Linha := '';
    repeat
      Proxima := GetProximaPalavra(s);
      if Length(Proxima) > n then Proxima := Copy(Proxima, 1, n);

      // se a palavra cabe na linha
      // adiciona e poda s
      if Length(Linha+Proxima) <= n
      then begin
        Linha := Linha+Proxima;
        s := Copy(s, Length(Proxima)+1, Length(s));
      end
      else Excedeu := True;
    until Excedeu or (s = '');
    if Linha <> '' then Lista.Add(Linha);
  until (s = '');
end;

function GetQuebraEmLinhas(s: String; n: Integer): TMyStrArray;
var
  i: Integer;
  a: TMyStrArray;
  Lista: TStringList;
begin
  a := nil;
  Lista := TStringList.Create;
  try
    QuebrarEmLinhas(n, s, Lista);
    if Lista.Count > 0
    then begin
      SetLength(a, Lista.Count);
      for i := 0 to Lista.Count-1
      do a[i] := Lista[i]; 
    end;
  finally
    Lista.Free;
    Result := a;
  end;
end;

function TimeToSeconds(Horario: TDateTime): Longint;
begin
  Result := Trunc(Horario/UnidadeTDateTime);
end;

function TimeToSecondsR(Horario: TDateTime): Real;
begin
  Result := Horario/UnidadeTDateTime;
end;

function SecondsToTime(Segundos: Longint): TDateTime;
begin
  Result := Segundos*UnidadeTDateTime;
end;


function IIfStr(BoolExpr: Boolean; RetTrue, RetFalse: String): String;
begin
  if BoolExpr then Result := RetTrue else Result := RetFalse;
end;

function IIf(BoolExpr: Boolean; RetTrue, RetFalse: Longint): Longint; overload;
begin
  if BoolExpr then Result := RetTrue else Result := RetFalse;
end;

function IIf(BoolExpr: Boolean; RetTrue, RetFalse: Real): Real; overload;
begin
  if BoolExpr then Result := RetTrue else Result := RetFalse;
end;

function IIf(BoolExpr: Boolean; RetTrue, RetFalse: String): String; overload;
begin
  if BoolExpr then Result := RetTrue else Result := RetFalse;
end;

function IIf(BoolExpr: Boolean; RetTrue, RetFalse: TMsgDlgBtn): TMsgDlgBtn; overload;
begin
  if BoolExpr then Result := RetTrue else Result := RetFalse;
end;

function ExprOrDefault(Expr, Default: String): String;
begin
  Result := IIfStr(Trim(Expr) = '', Default, Expr);
end;

{
function IIf(BoolExpr: Boolean; RetTrue, RetFalse: TDateTime): TDateTime; overload;
begin
  if BoolExpr then Result := RetTrue else Result := RetFalse;
end;
}

function AddBarra(Caminho: String): String;
begin
  Result := IncludeTrailingPathDelimiter(Caminho);
end;


function DelBarra(Caminho: String): String;
begin
  Result := ExcludeTrailingPathDelimiter(Caminho);
end;

(*
function IntToBin (Valor: Longint; Digitos: Integer): String;
begin
  Result := StringOfChar('0', Digitos) ;
  while Valor > 0 do
  begin
    if (Valor and 1) = 1 then
      Result[Digits] := '1';
    Dec(Digitos);
    Valor := Valor shr 1;
  end;
end;
*)


function L2Bin(Valor: Longint): String;
var
  i,
  b1, b2, b3, b4: Integer;
begin
  i := Valor;
  b1 := i mod 255;
  b2 := (i div 255) mod 255;
  b3 := ((i div 255) div 255) mod 255;
  b4 := ((i div 255) div 255) div 255;
  Result := Chr(b1)+Chr(b2)+Chr(b3)+Chr(b4);
end;

function Bin2L(s: String): Longint;
var
  i, n, l: Integer;
begin
  Result := 0;
  l := Length(s);
  if l > 0 then
    for i := 1 to l do begin
      n := l-i;
      if Copy(s, i, 1) = '1'
      then Result := Result+Trunc(Exp(Ln(2) * n)); // Exp(Ln(r1) * r2); { r1 elevado a r2 }
    end;
end;

function HexToInt(sHex: String): Longint;
var
  i, n, Base: Integer;
begin
  Result := 0;
  if Length(sHex) > 0
  then begin
    Base := 1;
    for i := Length(sHex) downto 1
    do begin
      n := Pos(Copy(sHex, i, 1), MatrizHex)-1;
      Result := Result+n*Base;
      Base := Base*16;
    end;
  end;
end;

function IntToBin(i: Integer): String;
var
  n: Integer;
  s: String;
begin
  n := 1;
  s := '';
  while n <= i do begin
    if (n and i) > 0
    then s := s+'1'
    else s := s+'0';
    n := n*2;
  end;
  if i <= 0 then s := '0';

  Result := '';
  for n := Length(s) downto 1
  do Result := Result+s[n];
end;

function HbIsInt(s: String): Boolean;
var
  i: Integer;
begin
  Result := True;
  if Length(s) > 0
  then for i := 1 to Length(s)
  do if not (s[i] in ['0'..'9'])
  then begin
    Result := False;
    Break;
  end;
end;

function PreencherPosicao(nPos: Integer; sOrigem, sInsert: String): String;
var
  l: Integer;
begin
  l := Length(sInsert);
  Delete(sOrigem, nPos, l);
  Insert(sInsert, sOrigem, nPos);
  Result := sOrigem;
end;

function Capitalize(s: String): String;
begin
  Result := '';
  if Length (s) < 1 then
    Exit;
  
  s := AnsiLowerCase(s);
  s[1] := UpCase(s[1]);
  Result := s;
end;

function CapitalizeTexto(Texto: String): String;
var
  i: integer;
  c: string;
begin
  if Length(Texto) < 1 then Exit;
  Texto := Trim(Texto);
  Texto := Capitalize(Texto);
  for i:=2 to Length(Texto) do
  begin
    if (Texto[I] = ' ') or (Texto[I] = '(')  or (Texto[I] = '.') then
    begin
      try
        if (Texto[I+2] <> ' ') then
          begin
            c := UpperCase(Texto[I+1]);
            Texto[I+1] := c[1];
          end;
      except

      end;
    end;
  end;
  result := Texto;
end;

function Mac2DecStr(MACAddr: String): String;
var
  i: Integer;
  s: String;
  GruposMAC: TMyStrArray;
begin
  Result := '';
  GruposMAC := Split(':', MACAddr);
  if Length(GruposMAC) > 0
  then begin
    for i := 0 to Length(GruposMAC)-1
    do begin
      s := IntToStr(HexToInt(GruposMAC[i]));
      Result := Result+IIfStr(i > 0, ' ', '')+s;
    end;
  end;
end;

function RightPos(Substr: string; S: string): Integer;
var
  i, ls, lss: Integer;
begin
  Result := 0;
  ls  := Length(S);
  lss := Length(Substr);
  if (ls > 0) and (ls >= lss) then
    for i := ls downto 1 do
    begin
      if Copy(s, i, lss) = Substr then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function LeftPos(Substr: string; S: string): Integer;
var
  i, ls, lss: Integer;
begin
  Result := 0;
  ls  := Length(S);
  lss := Length(Substr);
  if (ls > 0) and (ls >= lss) then
    for i := 1 to ls do
    begin
      if Copy(s, i, lss) = Substr then
      begin
        Result := i;
        Break;
      end;
    end;
end;

(* MensagemMsSQLFiltrada
 ;
 ; filtra uma mensagem de erro do SQL Server
 ; para exibir somente a parte "legÌvel"
 ;
*)
function MensagemMsSQLFiltrada(s: String): String;
var
  n: Integer;
begin
  n := RightPos(']', s);
  if n > 0 then
    Result := Copy(s, n+1, Length(s))
  else
    Result := s;
end;

(* MensagemPostgresFiltrada
 ;
 ; a mensagem do postgres comeÁa sempre
 ; com 'Key violation.' <quebra de linha> 'ERROR: '
 ;
*)
function MensagemPostgresFiltrada(s: String): String;
var
  i: Integer;
begin
  i := Pos('ERROR:', s);
  if i < 1 then
    i := Pos(':', s)
  else
    i := i+6;
    
  Result := Trim(Copy(s, i+1, Length(s)));
end;


procedure SubstituirMacro(Nome, Valor: String; Lista: TStrings);
var
  i: Integer;
begin
  if Lista.Count > 0 then
    for i := 0 to Lista.Count-1 do
      Lista[i] := Substituir(Lista[i], '&'+Nome, Valor); 
end;

function RemoverAcentos(Str: String): String;
const
  ComAcento = '‡‚ÍÓÙ˚„ı·ÈËÌÏÛÚ˙˘Á¸¿¬ Œ‘€√’¡…»ÕÃ”“⁄Ÿ«‹¥`~"êÄß#%&*@∫™∞Æ•Ò?Üßﬂµ±£ÆœÀÿ÷ÂÊÎÔˆ§£¶úå';
  SemAcento = 'aaeiouaoaeeiioouucuAAEIOUAOAEEIIOOUUCU                                         ';
var
  x : Integer;
begin
  for x := 1 to Length(Str) do
  begin
      if Pos(str[x],ComAcento)<>0 Then
      begin
        str[x] := SemAcento[Pos(Str[x],ComAcento)];
      end;//if..then
  end;//for..do
  result := str;
end;


function PosicaoFechamento (S, DelimAbre,
  DelimFecha: String; Inicio: Integer=1): Integer;
var
  posAbre, Contagem,
  i, ls, lss: Integer;
  c: String;
begin
  Result := -1;
  ls  := Length(S);
  if ls < Inicio then
    Exit;

  // procura o primeiro delimitador
  posAbre := 0;
  for i := Inicio to ls do
  begin
    if Copy(S, i, 1) = DelimAbre then
    begin
      posAbre := i;
      Break;
    end;
  end;

  // se n„o encontrou o delimitador de abertura,
  // retorna com erro
  if posAbre = 0 then
    Exit;

  // a partir desse ponto,
  // conta aberturas e fechamentos
  Contagem := 1;
  for i := posAbre+1 to ls do
  begin
    c := Copy(S, i, 1);
    if c = DelimAbre then
      Contagem := Contagem+1
    else if c = DelimFecha then
      Contagem := Contagem-1;

    if Contagem = 0 then
    begin
      Result := i;
      Exit;
    end;

  end;
end;

function RemoverComentariosSql (Sql: String): String;
var
  i, n, nAbre, nFecha: Integer;
  Lista: TStringList;
begin
  // primeiro remove os coment·rios /* */
  repeat
    nAbre := Pos ('/*', Sql);
    nFecha := Pos ('*/', Sql);
    if nAbre > 0 then
      Delete (Sql, nAbre, nFecha-nAbre+2);
  until nAbre = 0;

  Lista := TStringList.Create;
  try
    Lista.Text := Sql;
    for i := 0 to Lista.Count-1 do
    begin
      n := Pos ('--', Lista [i]);
      if n > 0 then
        Lista [i] := Copy (Lista [i], 1, n-1);
    end;
    Result := Lista.Text;
  finally
    Lista.Free;
  end;
end;


(** StartsWith
 *
 * verifica se S comeÁa com Search
 * case insensitive
 *
 *)
function StartsWith(const S, Search: String): Boolean;
var
  Verif: String;
begin
  Verif := Copy(S, 1, Length(Search));
  Result := CompareText(Search, Verif) = 0;
end;


function EndsWith(const S, Search: String): Boolean;
var
  LSearch, LS: Integer;
begin
  Result := False;
  LSearch := Length(Search);
  LS := Length(S);
  if ( LSearch > LS ) then begin
    Exit;
  end;
  Result := CompareText(Search, Copy(S, LS-LSearch+1, LS)) = 0;
end;


function IsNumeric(const S: String): Boolean;
begin
  Result := True;
  try
    StrToFloat (S);
  except
    Result := False;
  end;
end;


function ValueBetween(const Value, Lower, Upper: Real): Boolean;
begin
  Result := (Value >= Lower) and (Value <= Upper); 
end;


function EscapeSQL(const Sql: String): String;
begin
  Result := StringReplace(StringReplace(Sql, '''', '''''', [rfReplaceAll]), '\', '\\', [rfReplaceAll]);
end;


function PosExCaseInsensitive(const Procurado, Texto: String;
  Offset: Cardinal = 1): Integer;
begin
  Result := PosEx(AnsiUpperCase(Procurado), AnsiUpperCase(Texto), Offset);
end;


function PosicaoDaUltimaOcorrencia(const Procurado, Texto: String;
  CaseSensitive: Boolean = False): Integer;
var
  PosicaoAnterior: Integer;
begin
  PosicaoAnterior := 0;
  repeat
    if CaseSensitive then
      Result := PosEx(Procurado, Texto, PosicaoAnterior+1)
    else
      Result := PosExCaseInsensitive(Procurado, Texto, PosicaoAnterior+1);
    if Result > 0 then
      PosicaoAnterior := Result;
  until
    Result = 0;
  Result := PosicaoAnterior;
end;


function PosicaoDoPrimeiroSeparador(const Texto: String): Integer;
var
  Indice: Integer;
begin
  Result := 0;
  if Texto = '' then
    Exit;
  for Indice := 1 to Length(Texto) do
    if Texto[Indice] in [#32..#47] then
    begin
      Result := Indice;
      Exit;
    end;
end;


function PosicaoDoUltimoSeparador(const Texto: String): Integer;
var
  Indice: Integer;
begin
  Result := 0;
  if Texto = '' then
    Exit;
  for Indice := Length(Texto) downto 1 do
    if Texto[Indice] in [#32..#47] then
    begin
      Result := Indice;
      Exit;
    end;
end;


function GetPrimeiraPalavra(const Texto: String): String;
begin
  Result := LeftStr(Texto, Pos(' ', Texto)-1);
end;

function FloatToSql(Valor: Real): String;
begin
  Result := Substituir(FloatToStr(Valor), ',', '.');
end;


function InMyStrArray(Valor: String; UmArray: TMyStrArray): Boolean;
var
  Indice: Integer;
begin
  Result := True;
  for Indice := 0 to Length(UmArray)-1 do
    if SameText(Valor, Trim(UmArray[Indice])) then
      Exit;
  Result := False;
end;


procedure AppendToMyStrArray(Valor: String; var UmArray: TMyStrArray);
var
  Len: Integer;
begin
  Len := Length(UmArray);
  SetLength(UmArray, Len+1);
  UmArray[Len] := Valor;
end;


function GetItemDaListaDelimitada(UmaListaDelimitada, UmDelimitador: String;
  UmItem: Integer): String;
var
  Lista: TMyStrArray;
begin
  Result := '';
  Lista := Explode(UmDelimitador, UmaListaDelimitada);
  if Length(Lista) > UmItem then
    Result := Lista[UmItem];
end;


function getVariantDateAsString(varDate: Variant): String;
begin
  Result := '';
  if ( not VarIsNull(varDate) ) then begin
    Result := FormatDateTime('dd/mm/yyyy', VarToDateTime(varDate));
  end;
end;


function StrToVarDate(StrDate: String): Variant;
begin
  Result := Null;
  if ( Trim(StrDate) <> '' ) then begin
    Result := StrToDate(StrDate);
  end;
end;

function RemoverMascara(Valor: String): String;
begin
  Result := Substituir(Substituir(Substituir(Substituir(Substituir(
    Substituir(Valor, '.', ''), '-', ''), '/', ''), ',', ''), '(', ''), ')', '');
end;

function ConcatenarComSeparador(Destino, Adicionar, Separador: String): String;
begin
  Result := Destino + IIfStr((Destino = '') or (Adicionar = ''), '', Separador) + Adicionar;
end;


var
  lt, lf: Integer;

initialization

  UnidadeTDateTime := 1/86400; { 1/(24*3600) }

  // extende as listas de convers„o para StrToBool (em SysUtils)
  lt := Length(TrueBoolStrs);
  lf := Length(FalseBoolStrs);
  SetLength(TrueBoolStrs, lt+6);
  SetLength(FalseBoolStrs, lf+6);

  TrueBoolStrs[lt]   := 'True';
  TrueBoolStrs[lt+1] := 'T';
  TrueBoolStrs[lt+2] := 'Verdadeiro';
  TrueBoolStrs[lt+3] := 'V';
  TrueBoolStrs[lt+4] := 'Sim';
  TrueBoolStrs[lt+5] := 'S';

  FalseBoolStrs[lf]  := 'False';
  FalseBoolStrs[lf+1]:= 'F';
  FalseBoolStrs[lf+2]:= 'Falso';
  FalseBoolStrs[lf+3]:= 'Nao';
  FalseBoolStrs[lf+4]:= 'N„o';
  FalseBoolStrs[lf+5]:= 'N';

end.

