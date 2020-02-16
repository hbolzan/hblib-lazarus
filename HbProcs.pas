unit HbProcs;

interface

uses
  MyDlg, HbTypes, MyStr, HbMisc,
  DB, Registry,
  SysUtils, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;


function FecharTodos: Boolean;
procedure FecharTodosOsForms;
function HbAcharFormName(Nome: String): TForm; overload;
function HbAcharFormName(Proprietario: TComponent; Nome: String): TForm; overload;
procedure GetUserCompany(var Usuario, Empresa: String);
function HbExisteFormulario(Titulo: String): TForm;
function HbCGCValido(sCGC: String): Boolean;
function HbCPFValido(sCPF: String): Boolean;
procedure PushCursor(ACursor: TCursor);
procedure PopCursor;
function HbModulo11(ANumero: Integer): Integer;
procedure LevantarExcecaoSe(Condicao: Boolean; Mensagem: String);
procedure LevantarExcecao(Mensagem: String; Controle: TWinControl);
procedure LevantarExcecaoCondicional(Condicao: Boolean; MensagemDeErro: String;
  Controle: TWinControl); overload;
procedure LevantarExcecaoCondicional(Mensagem: String; Controle: TWinControl); overload;
function ReplaceMultiplo (s: String; antigos, novos: array of String): String;
function InterpretarParametrosDeLinhaDeComando: TStringList;
function ExistemArquivos(Caminho, Mascara: String): Boolean;
procedure VincularUmFormAUmWinControl(UmForm: TForm; UmWinControl: TWinControl);
function GetFileContents(FilePath: String): String;
procedure SaveFileContents(FilePath, Content: String);
function StringToHex(S: String): String;
function HexToString(Hex: String): String;
function MapString(Callback: TMapCallbackAsString; Lista: TStrArray): TStrArray;
function ReduceString(Callback: TReduceCallbackAsString; Lista: array of String; ValorInicial: String = ''): String;


implementation

(* FecharTodos
 ;
 ; fecha todos os formulários
 ; do sistema, exceto o formulário principal
 ;
*)
function FecharTodos: Boolean;
var
  n, i, l: Integer;
  Ok: Boolean;
  MatrizForms: array of TForm;
begin
  // preenche a matriz com todos os forms instanciados
  for i := 0 to Application.ComponentCount-1 do
  begin
    if (Application.Components[i] is TForm) and
      (TForm(Application.Components[i]).Tag <> 666) and
      (TForm(Application.Components[i]) <> Application.MainForm) then
    begin
      l := Length(MatrizForms);
      SetLength(MatrizForms, l+1);
      MatrizForms[l] := TForm(Application.Components[i]);
    end;
  end;

  // agora libera todos os forms
  for i := 0 to Length(MatrizForms)-1 do
    MatrizForms[i].Close;
  Result := True;
end;

procedure FecharTodosOsForms;
begin
  FecharTodos;
end;

procedure GetUserCompany(var Usuario, Empresa: String);
var
  reg: TRegIniFile;
begin
  // RootKey default é HKEY_CURRENT_USER
  reg := TRegIniFile.create('SOFTWARE\MICROSOFT\MS SETUP (ACME)\');
  try
    Usuario := reg.ReadString('USER INFO','DefName','');
    Empresa := reg.ReadString('USER INFO','DefCompany','');
  finally
    reg.free;
  end;
end;

(* HbExisteFormulario
 ;
 ; pesquisa em Application
 ; um formulário com Caption = Titulo
 ;
*)
function HbExisteFormulario(Titulo: String): TForm;
var
  i, j, n: Integer;
begin
  i := 0;
  j := -1;
  n := Application.ComponentCount;
  repeat
    if i < n then begin
      if (Application.Components[i] is TForm)
      and (TForm(Application.Components[i]).Caption = Titulo)
      then j := i;
    end;
    Inc(i);
  until (j >= 0) or (i >= n);
  if j < 0
  then Result := nil
  else Result := TForm(Application.Components[j]);
end;


function HbAcharFormName(Nome: String): TForm;
var
  i: Integer;
  Componente: TComponent;
begin
  Componente := Application.FindComponent(Nome);
  if (Componente = nil) or (not (Componente is TForm)) then
    Result := nil
  else
    Result := TForm(Componente);

  Exit;
end;

function HbAcharFormName(Proprietario: TComponent; Nome: String): TForm;
var
  i: Integer;
begin
  Result := nil;
  Nome := Trim(UpperCase(Nome));
  for i := 0 to Proprietario.ComponentCount-1 do
  begin
    if (Proprietario.Components[i] is TForm)
    and (Trim(UpperCase(TForm(Proprietario.Components[i]).Name)) = Nome)
    then begin
      Result := TForm(Proprietario.Components[i]);
      Break;
    end;
  end;
end;

procedure PushCursor(ACursor: TCursor);
var
  l: Integer;
begin
  l := Length(HBMatrizCursor);
  SetLength(HBMatrizCursor, l+1);
  HBMatrizCursor[l] := Screen.Cursor;
  Screen.Cursor := ACursor;
end;

procedure PopCursor;
var
  l: Integer;
begin
  l := Length(HBMatrizCursor);
  if l > 0
  then begin
    Screen.Cursor := HBMatrizCursor[l-1];
    HBMatrizCursor := Copy(HBMatrizCursor, 0, l-1);
  end;
end;

function HbCPFValido(sCPF: String): Boolean;
var
  num: String[11];
  n1,n2,n3,n4,n5,n6,n7,n8,n9: integer;
  d1,d2: integer;
  digitado, calculado: string;
begin
  { limpa os caracteres da máscara }
  sCPF := Substituir(sCPF, '.', '');
  sCPF := Substituir(sCPF, '-', '');
  num := sCPF;

  n1:=StrToInt(num[1]);
  n2:=StrToInt(num[2]);
  n3:=StrToInt(num[3]);
  n4:=StrToInt(num[4]);
  n5:=StrToInt(num[5]);
  n6:=StrToInt(num[6]);
  n7:=StrToInt(num[7]);
  n8:=StrToInt(num[8]);
  n9:=StrToInt(num[9]);

  d1:=n9*2+n8*3+n7*4+n6*5+n5*6+n4*7+n3*8+n2*9+n1*10;
  d1:=11-(d1 mod 11);

  if d1>=10 then d1:=0;

  d2:=d1*2+n9*3+n8*4+n7*5+n6*6+n5*7+n4*8+n3*9+n2*10+n1*11;
  d2:=11-(d2 mod 11);

  if d2>=10 then d2:=0;

  calculado:=inttostr(d1)+inttostr(d2);
  digitado:=num[10]+num[11];

  if calculado=digitado
  then Result := True
  else Result := False;
end;


function HbCGCValido(sCGC: String): Boolean;
var
  num: String[14];
  n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12: integer;
  d1,d2: integer;
  digitado, calculado: string;
begin
  { limpa os caracteres da máscara }
  sCGC := Substituir(sCGC, '.', '');
  sCGC := Substituir(sCGC, '/', '');
  sCGC := Substituir(sCGC, '-', '');
  num := sCGC;

  n1:=StrToInt(num[1]);
  n2:=StrToInt(num[2]);
  n3:=StrToInt(num[3]);
  n4:=StrToInt(num[4]);
  n5:=StrToInt(num[5]);
  n6:=StrToInt(num[6]);
  n7:=StrToInt(num[7]);
  n8:=StrToInt(num[8]);
  n9:=StrToInt(num[9]);
  n10:=StrToInt(num[10]);
  n11:=StrToInt(num[11]);
  n12:=StrToInt(num[12]);

  d1:=n12*2+n11*3+n10*4+n9*5+n8*6+n7*7+n6*8+n5*9+n4*2+n3*3+n2*4+n1*5;
  d1:=11-(d1 mod 11);
  if d1>=10 then d1:=0;

  d2:=d1*2+n12*3+n11*4+n10*5+n9*6+n8*7+n7*8+n6*9+n5*2+n4*3+n3*4+n2*5+n1*6;
  d2:=11-(d2 mod 11);
  if d2>=10 then d2:=0;

  calculado:=inttostr(d1)+inttostr(d2);
  digitado:=num[13]+num[14];
  if calculado=digitado
  then Result := True
  else begin
    Result := False;
    // MONITOR
    // ShowMessage('Digito verificador deveria ser '+calculado);
  end;
end;


function HbModulo11(ANumero: Integer): Integer;
var
  i, d, Soma, Peso, Resto: Integer;
  s, sD: String;
begin
  // algoritmo módulo 11 p/ cálculo do dígito verificador
  s := IntToStr(ANumero);
  Soma := 0;
  Peso := 9;
  for i := 11 downto 1
  do begin
    Soma := Soma+InteiroDe(s[i])*Peso;
    Dec(Peso);
    if Peso < 2 then Peso := 9;
  end;

  Resto := Soma mod 11;
  if Resto < 10 then
    d := Resto
  else
    d := -1;
    
  if d < 0 then
    sD := '0'
  else
    sD := IntToStr(d);

  // retorna tipo + numero seq + dígito
  Result := StrToInt(sD);
end;


procedure LevantarExcecaoSe(Condicao: Boolean; Mensagem: String);
begin
  if not Condicao then begin
    Exit;
  end;

  if Mensagem <> '' then begin
    raise Exception.Create(Mensagem);
  end else begin
    Abort;
  end;
end;


(** LevantarExcecao
 *
 * manda o foco para o controle
 * antes de levantar a exceção
 *
 *)
procedure LevantarExcecao(Mensagem: String; Controle: TWinControl);
begin
  if (Assigned(Controle)) and Controle.Visible then
    Controle.SetFocus;
  if Mensagem <> '' then
    raise Exception.Create (Mensagem)
  else
    Abort;
end;


procedure LevantarExcecaoCondicional(Mensagem: String; Controle: TWinControl); overload;
begin
  if HbMessageDlg('Atenção', Mensagem, mtWarning, [mbYes, mbNo]) = mrYes then
    Exit;
  LevantarExcecao('', Controle);
end;

procedure LevantarExcecaoCondicional(Condicao: Boolean; MensagemDeErro: String;
  Controle: TWinControl); overload;
begin
  if Condicao then
    LevantarExcecao(MensagemDeErro, Controle);
end;

(** ReplaceMultiplo
 *
 * replace em arrays
 * o segundo array pode ser menor que o primeiro
 * se o segundo array for menor que o primeiro
 * fixa no último elemento quando chegar nele
 *
 *)
function ReplaceMultiplo (s: String; antigos, novos: array of String): String;
var
  i, i_novo, len_antigos, len_novos: Integer;
begin
  Result := s;

  len_antigos := Length(antigos);
  len_novos := Length(novos);
  if (len_antigos < 1) or (len_novos < 1) then
    Exit;

  for i := 0 to len_antigos - 1 do
  begin
    if len_novos < (i+1) then
      i_novo := len_novos-1
    else
      i_novo := i;

    Result := StringReplace (Result, antigos[i], novos[i_novo],
      [rfReplaceAll, rfIgnoreCase]);
  end;
end;


function InterpretarParametrosDeLinhaDeComando: TStringList;
var
  i, n: Integer;
  Token, Parametro: String;
  ParametrosCmd: TStringList;
begin
  i := 1;
  n := ParamCount;
  ParametrosCmd := TStringList.Create; 
  if n > 0 then
  repeat
    Token := ParamStr(i);
    if Copy(Token, 1, 1) = '-' then
    begin
      Parametro := LowerCase(Trim(Copy(Token, 2, Length(Token))));
      // verifica se existe próximo parametro e se ele não começa com '-'
      if (ParamCount > i) and
        (Copy(ParamStr(i+1), 1, 1) <> '-') then
      begin
        // se existir, incrementa i para avançar o ponteiro de parâmetros
        // e monta o par 'parametro=valor'
        Inc(i);
        ParametrosCmd.Add(Parametro+'='+ParamStr(i));
      end else
        ParametrosCmd.Add(Parametro);
    end;
    Inc(i);
  until i > n;
  Result := ParametrosCmd;
end;


function ExistemArquivos(Caminho, Mascara: String): Boolean;
var
  SearchRec: TSearchRec;
  Atributos: Integer;
begin
  Atributos := faArchive+faAnyFile;
  Result := FindFirst(AddBarra(Caminho)+Mascara, Atributos, SearchRec) = 0;
  SysUtils.FindClose(SearchRec);
end;


procedure VincularUmFormAUmWinControl(UmForm: TForm; UmWinControl: TWinControl);
begin
  UmForm.Parent := UmWinControl;
  UmForm.Position := poDesigned;
  UmForm.WindowState := wsMaximized;
  UmForm.Align := alClient;
  UmForm.BorderStyle := bsNone;
  UmForm.Visible := True;
end;


function GetFileContents(FilePath: String): String;
var
  Lista: TStringList;
begin
  Lista := TStringList.Create;
  try
    Lista.LoadFromFile(FilePath);
    Result := Lista.Text;
  finally
    Lista.Free;
  end;
end;


procedure SaveFileContents(FilePath, Content: String);
var
  Lista: TStringList;
begin
  Lista := TStringList.Create;
  try
    Lista.Text := Content;
    Lista.SaveToFile(FilePath);
  finally
    Lista.Free;
  end;
end;


function StringToHex(S: String): String;
var
  Indice: Integer;
begin
  Result := '';
  for Indice := 1 to Length(S) do begin
    Result := Result + IntToHex(Ord(S[Indice]), 2);
  end;
end;

function HexToString(Hex: String): String;
var
  Indice: Integer;
begin
  Result := '';
  for Indice := 1 to Length(Hex) div 2 do begin
    Result := Result + Chr(StrToInt('$' + Copy(Hex, (Indice-1)*2+1, 2)));
  end;
end;

function MapString(Callback: TMapCallbackAsString; Lista: TStrArray): TStrArray;
var
  Indice, Len: Integer;
begin
  SetLength(Result, Length(Lista));
  for Indice := 0 to Length(Lista) do begin
    Result[Indice] := Callback(Lista[Indice], Indice, Lista);
  end;
end;

function ReduceString(Callback: TReduceCallbackAsString;
  Lista: array of String; ValorInicial: String): String;
var
  Indice: Integer;
begin
  Result := ValorInicial;
  for Indice := 0 to Length(Lista) do begin
    Result := Callback(Result, Lista[Indice]);
  end;
end;


end.

