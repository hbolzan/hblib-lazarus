(* MyMath
 ;
 ; Funções de análise de expressões
 ;
*)

unit MyMath;

interface
uses
  MyStr,
  Math, Classes, SysUtils;

type
  PReal = ^Real;

  TVar = record
    Nome:  String;
    Valor: Real;
  end;
  TMatrizVar = array of TVar;

  TMyMath = Class(TObject)
  private
    MatrizVar: TMatrizVar;

    function IsNumber(s: String): Boolean;
    function GetValor(s: String; ListaValores: TList): Real;
    function Elevar(s1, s2: String; ListaValores: TList): Real;
    function Multiplicar(s1, s2: String; ListaValores: TList): Real;
    function Dividir(s1, s2: String; ListaValores: TList): Real;
    function ProximoOperando(var expr: String): String;
    function ProximoOperador(var expr: String): Char;
    function Operar(r: Real; operador: Char; operando: String; ListaValores: TList): Real;
    function IndiceDeVar(NomeVar: String): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetVar(NomeVar: String; Valor: Real);
    procedure ClearVars;
    function Calc(expr: String): Real;
  end;



function Elevado(r1, r2: Real): Real;
function Arredondado(Valor: Real; Casas: Byte): Real;
function TaxaDia(txMes: Real): Real;
function TaxaMes(txDia: Real): Real;
function ValorPresente(Valor, txDia: Real; NumDias: Word): Real;
function ValorPresenteAnt(Valor, txDia: Real; NumDias: Word): Real;
function ValorParcela(NumParcelas, Periodo: Integer;
                      ValorPresente, txDia: Real;
                      Entrada: Boolean): Real;
function GetMultiploDe(QuantidadeBase, QuantidadeAtual: Real): Real;
function IsMultiploDe(Base, Valor: Real): Boolean;


implementation

(* Elevado
 ;
 ; retorna r1 elevado a r2
 ;
*)
function Elevado(r1, r2: Real): Real;
begin                                          
  Result := Exp(Ln(r1) * r2); { r1 elevado a r2 }
end;


(* Arredondado
 ;
 ; arredonda valor
 ; no número de casas decimais
 ; especificado
 ;
*)
function Arredondado(Valor: Real; Casas: Byte): Real;
var
  Inteiro,
  Decimal: Real;
  XCasas: Real;
begin
  XCasas := Elevado(10, Casas);
  Inteiro := Int(Valor);
  Decimal := Frac(Valor);
  Decimal := Round(Decimal*XCasas)/XCasas;
  Result := Inteiro+Decimal;
end;


constructor TMyMath.Create;
begin
  MatrizVar := nil;
end;

destructor TMyMath.Destroy;
begin
  MatrizVar := nil;
  Inherited Destroy;
end;


function TMyMath.IndiceDeVar(NomeVar: String): Integer;
var
  i, l: Integer;
begin
  Result := -1;
  NomeVar := UpperCase(Trim(NomeVar));
  l := Length(MatrizVar);
  if l > 0
  then for i := 0 to l-1
  do begin
    if NomeVar = MatrizVar[i].Nome
    then begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TMyMath.SetVar(NomeVar: String; Valor: Real);
var
  n: Integer;
begin
  NomeVar := UpperCase(Trim(NomeVar));
  n := IndiceDeVar(NomeVar);
  if n < 0
  then begin
    n := Length(MatrizVar);
    SetLength(MatrizVar, n+1);
    MatrizVar[n].Nome := NomeVar;
  end;
  MatrizVar[n].Valor := Valor;
end;

procedure TMyMath.ClearVars;
begin
  MatrizVar := nil;
end;

(* IsNumber
 ;
 ; retorna True se a expressão
 ; só contiver dígitos numéricos
 ;
*)
function TMyMath.IsNumber(s: String): Boolean;
var
  i: Integer;
  l: Boolean;
begin
  l := True;
  if Length(s) > 0 then for i := 1 to Length(s) do
  if not (s[i] in ['0'..'9', '.']) then l := False;
  Result := l;
end;


(* GetValor
 ;
 ; retorna o valor de s
 ;
*)
function TMyMath.GetValor(s: String; ListaValores: TList): Real;
var
  r: Real;
  i: Integer;
begin
  { se for uma string vazia, retorna zero }
  if Length(s) < 1 then Result := 0

  { se for Pi, retorna 3.1416             }
  else if s = 'PI' then Result := 3.1416

  { se for uma constante numérica, retorna o valor }
  else if IsNumber(s) then begin
    Result := RealDe(s);
  end

  { se começar com #, retorna um item da lista local }
  else if s[1] = '#' then begin
    Delete(s, 1, 1);
    i := StrToInt(s);
    if (i > 0) and (i <= ListaValores.Count)
    then Result := Real(ListaValores.Items[i-1]^)
    else Result := 0;
  end

  { se for uma variável, pesquisa na lista global }
  else begin
    i := IndiceDeVar(s);
    if i < 0
    then Result := 0
    else Result := MatrizVar[i].Valor; 
  end;
end;

function TMyMath.Elevar(s1, s2: String; ListaValores: TList): Real;
var
  r1, r2: Real;
begin
  r1 := GetValor(s1, ListaValores);
  r2 := GetValor(s2, ListaValores);
  Result := Exp(Ln(r1) * r2); { r1 elevado a r2 }
end;

function TMyMath.Multiplicar(s1, s2: String; ListaValores: TList): Real;
var
  r1, r2: Real;
begin
  r1 := GetValor(s1, ListaValores);
  r2 := GetValor(s2, ListaValores);
  Result := r1*r2;
end;

function TMyMath.Dividir(s1, s2: String; ListaValores: TList): Real;
var
  r1, r2: Real;
begin
  r1 := GetValor(s1, ListaValores);
  r2 := GetValor(s2, ListaValores);
  Result := r1/r2;
end;

function TMyMath.ProximoOperando(var expr: String): String;
var
  s: String;
begin
  s := '';
  while (Length(expr) > 0) and (Pos(expr[1], ' -+*/^') = 0)
  do begin
    s := s+expr[1];
    Delete(expr, 1, 1);
  end;
  Result := s;
end;

function TMyMath.ProximoOperador(var expr: String): Char;
var
  c:    Char;
begin
  if Length(expr) > 0 then begin
    repeat
      c := expr[1];
      Delete(expr, 1, 1);
    until (Length(expr) = 0) or (Pos(c, '-+*/^') > 0);
    if Pos(c, '-+*/^') = 0 then c := '+';
  end else c := '+';
  Result := c;
end;

function TMyMath.Operar(r: Real; operador: Char; operando: String; ListaValores: TList): Real;
var
  r2: Real;
begin
  r2 := GetValor(operando, ListaValores);
  case operador of
    '+': Result := r+r2;
    '-': Result := r-r2;
    '*': Result := r*r2;
    '/': Result := r/r2;
    '^': Result := Exp(r2 * Ln(r)); { r elevado a r2 }
  else Result := 0;
  end;
end;




(* Calc
 ;
 ; faz o agrupamento
 ; das sub-expressões
 ; e calcula cada uma
 ;
*)
function TMyMath.Calc(expr: String): Real;
var
  i, l, n:      Integer;
  c:            Char;
  s, s1, s2:    String;
  r:            Real;
  ListaValores: TList;
begin
  ListaValores := TList.Create;
  expr := UpperCase(expr);

  { 1. resolver o que estiver entre parentesis }
  while Pos('(', expr) > 0 do begin
    s := '';
    n := 0;
    i := Pos('(', expr);
    repeat
      if expr[i] = '(' then Inc(n) else
      if expr[i] = ')' then Dec(n);
      s := s+expr[i];
      Delete(expr, i, 1);
    until (n=0) or (i > Length(expr));

    { por dúvida das vias verifica se }
    { tem mesmo os parentesis         }
    if s[1] = '(' then Delete(s, 1, 1);
    if s[Length(s)] = ')' then Delete(s, Length(s), 1);

    ListaValores.Add(New(PReal));
    Real(ListaValores.Items[ListaValores.Count-1]^) := Calc(s);

    { insere um identificador de resultado local }
    if i > Length(expr) then expr := expr+'#'+IntToStr(ListaValores.Count)
    else Insert('#'+IntToStr(ListaValores.Count), expr, i);
  end;

  { agora resolve os grupos de expressões  }
  { pela ordem de precedência              }
  { potenciação                            }
  while (Pos('^', expr) > 0) do begin
    s := ''; s1 := ''; s2 := '';
    n := 1;
    i := Pos('^', expr);
    { procura os operandos }
    Dec(i);
    while (i > 0) and (Pos(expr[i], ' +-/*^') = 0) do Dec(i);
    Inc(i);
    while n < 3 do begin
      if (Length(expr) < i) or (Pos(expr[i], ' +-/*^') > 0) then begin
        if n = 1 then begin
          s1 := s;
          Delete(expr, i, 1);
        end else s2 := s;
        s := '';
        Inc(n);
      end else begin
        s := s+expr[i];
        Delete(expr, i, 1);
      end;
    end;
    r := Elevar(s1, s2, ListaValores);
    ListaValores.Add(New(PReal));
    Real(ListaValores.Items[ListaValores.Count-1]^) := r;

    { insere um identificador de resultado local }
    if i > Length(expr) then expr := expr+'#'+IntToStr(ListaValores.Count)
    else Insert('#'+IntToStr(ListaValores.Count), expr, i);
  end;

  { multiplicação                          }
  while (Pos('*', expr) > 0) do begin
    s := ''; s1 := ''; s2 := '';
    n := 1;
    i := Pos('*', expr);
    { procura os operandos }
    Dec(i);
    while (i > 0) and (Pos(expr[i], ' +-/*^') = 0) do Dec(i);
    Inc(i);
    while n < 3 do begin
      if (Length(expr) < i) or (Pos(expr[i], ' +-/*^') > 0) then begin
        if n = 1 then begin
          s1 := s;
          Delete(expr, i, 1);
        end else s2 := s;
        s := '';
        Inc(n);
      end else begin
        s := s+expr[i];
        Delete(expr, i, 1);
      end;
    end;
    r := Multiplicar(s1, s2, ListaValores);
    ListaValores.Add(New(PReal));
    Real(ListaValores.Items[ListaValores.Count-1]^) := r;

    { insere um identificador de resultado local }
    if i > Length(expr) then expr := expr+'#'+IntToStr(ListaValores.Count)
    else Insert('#'+IntToStr(ListaValores.Count), expr, i);
  end;

  { divisão                                }
  while (Pos('/', expr) > 0) do begin
    s := ''; s1 := ''; s2 := '';
    n := 1;
    i := Pos('/', expr);
    { procura os operandos }
    Dec(i);
    while (i > 0) and (Pos(expr[i], ' +-/*^') = 0) do Dec(i);
    Inc(i);
    while n < 3 do begin
      if (Length(expr) < i) or (Pos(expr[i], ' +-/*^') > 0) then begin
        if n = 1 then begin
          s1 := s;
          Delete(expr, i, 1);
        end else s2 := s;
        s := '';
        Inc(n);
      end else begin
        s := s+expr[i];
        Delete(expr, i, 1);
      end;
    end;
    r := Dividir(s1, s2, ListaValores);
    ListaValores.Add(New(PReal));
    Real(ListaValores.Items[ListaValores.Count-1]^) := r;

    { insere um identificador de resultado local }
    if i > Length(expr) then expr := expr+'#'+IntToStr(ListaValores.Count)
    else Insert('#'+IntToStr(ListaValores.Count), expr, i);
  end;

  { agora o que sobrou é soma e subtração }
  s1 := ProximoOperando(expr);
  r := GetValor(s1, ListaValores);
  while (Pos('+', expr) > 0) or (Pos('-', expr) > 0)
  do begin
    c  := ProximoOperador(expr);
    s2 := ProximoOperando(expr);
    r := Operar(r, c, s2, ListaValores);
  end;
  ListaValores.Free;
  Result := r;
end;

// funções financeiras
function TaxaDia(txMes: Real): Real;
begin
  Result := (Elevado(1+txMes/100, 1/30)-1)*100;
end;

function TaxaMes(txDia: Real): Real;
begin
  Result := (Elevado(1+txDia/100, 30)-1)*100;
end;

function ValorPresente(Valor, txDia: Real; NumDias: Word): Real;
begin
  Result := Valor/Elevado(1+txDia/100, NumDias);
end;

function ValorPresenteAnt(Valor, txDia: Real; NumDias: Word): Real;
begin
  Result := Valor*(2-Elevado(1+txDia/100, NumDias));
end;


(* ValorParcela
 ;
 ; calcula valor de parcelamento
 ; periodo deve ser passado em numero de dias
 ;
*)
function ValorParcela(NumParcelas, Periodo: Integer;
                      ValorPresente, txDia: Real;
                      Entrada: Boolean): Real;
var
  i: Integer;
  txSoma, txPeriodo: Real;
begin
  { calcula a taxa do período }
  txPeriodo := Elevado(1+txDia/100, Periodo);

  txSoma := 0;
  for i := 1 to NumParcelas do
  txSoma := txSoma+Elevado(txPeriodo, NumParcelas-i);

  { sem entrada }
  if not Entrada then
  Result := ValorPresente*Elevado(txPeriodo, NumParcelas)/txSoma

  { com entrada }
  else
  Result := ValorPresente*Elevado(txPeriodo, NumParcelas-1)/txSoma;
end;

function GetMultiploDe(QuantidadeBase, QuantidadeAtual: Real): Real;
begin
  if QuantidadeBase = 0 then
    Result := QuantidadeAtual
  else
    Result := Ceil(QuantidadeAtual/QuantidadeBase)*QuantidadeBase;
end;


function IsMultiploDe(Base, Valor: Real): Boolean;
begin
  Result := ( Ceil(Valor/Base)*Base = Valor );
end;


end.


