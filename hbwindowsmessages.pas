unit HbWindowsMessages;

{$mode objfpc}{$H+}

interface

uses
  messages, windows,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  TCopyDataStruct = packed record
    dwData: DWORD; //up to 32 bits of data to be passed to the receiving application
    cbData: DWORD; //the size, in bytes, of the data pointed to by the lpData member
    lpData: Pointer; //Points to data to be passed to the receiving application. This member can be nil.
  end;

  TWMCopyData = packed record
    Msg: Cardinal;
    From: HWND;//Handle of the Window that passed the data
    CopyDataStruct: PCopyDataStruct; //data passed
    Result: Longint;//Use it to send a value back to the "Sender"
  end;

  TMensagemRecebidaEvent = procedure(Sender: TObject; Mensagem: String) of object;
  TErrorEvent = procedure(Sender: TObject; ErrorCode: Integer; ErrorMsg: String) of object;


  { TFormHbWindowsMsg }

  TFormHbWindowsMsg = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FMensagemRecebida: String;
    FOnMensagemRecebida: TMensagemRecebidaEvent;
    FOnError: TErrorEvent;
    procedure SetMensagemRecebida(UmaMensagemRecebida: String);
    procedure EmitirErro(Codigo: Integer; Mensagem: String);
  public
    { public declarations }
    class function Instance(WindowName: String = ''): TFormHbWindowsMsg;
    class procedure DropInstance;

    procedure EnviarMensagem(Destino, Mensagem: String);

    property OnMensagemRecebida: TMensagemRecebidaEvent read FOnMensagemRecebida write FOnMensagemRecebida;
    property OnError: TErrorEvent read FOnError write FOnError;
    property MensagemRecebida: String read FMensagemRecebida write SetMensagemRecebida;
  end;

const
    HBWM_ERROR_NOT_FOUND = 1;

implementation

{$R *.lfm}


{ TFormHbWindowsMsg }


var
  PrevWndProc: WNDPROC;
  FInstance: TFormHbWindowsMsg;

class function TFormHbWindowsMsg.Instance(WindowName: String): TFormHbWindowsMsg;
begin
  if not Assigned(FInstance) then begin
    FInstance := TFormHbWindowsMsg.Create(Nil);
  end;
  if WindowName <> '' then begin
    FInstance.Caption := WindowName;
  end;
  Result := FInstance;
end;

class procedure TFormHbWindowsMsg.DropInstance;
begin
  if Assigned(FInstance) then begin
    FreeAndNil(FInstance);
  end;
end;

function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam;
  lParam: LParam): LRESULT; stdcall;
var
  pMyData: CopyDataStruct;
  ms: TMemoryStream;
begin
  if uMsg = WM_COPYDATA then begin {Process it}
     pMyData := PCopyDataStruct(lParam)^;
     TFormHbWindowsMsg.Instance.MensagemRecebida := PChar(pMyData.lpData);
     Exit;
  end;
  Result := CallWindowProc(PrevWndProc,Ahwnd,uMsg,WParam,LParam);
end;

procedure TFormHbWindowsMsg.FormCreate(Sender: TObject);
begin
  FOnMensagemRecebida := Nil;
  FOnError := Nil;
  PrevWndProc := Windows.WNDPROC(SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PtrInt(@WndCallback)));
end;

procedure TFormHbWindowsMsg.EnviarMensagem(Destino, Mensagem: String);
var
  h: HWND;
  Data: TCopyDataStruct;
begin
  Data.dwData := 0; //use it to identify the message contents
  Data.cbData := 1 + Length(Mensagem) ;
  Data.lpData := PChar(Mensagem) ;

  h := FindWindow(nil, PChar(Destino));
  if IsWindow(h) then begin
    SendMessage(h, WM_COPYDATA, Integer(Handle), Integer(@Data));
  end else begin
    EmitirErro(HBWM_ERROR_NOT_FOUND, 'Janela destino não encontrada');
  end;
end;

procedure TFormHbWindowsMsg.EmitirErro(Codigo: Integer; Mensagem: String);
begin
  if Assigned(FOnError) then begin
    FOnError(Self, Codigo, Mensagem);
  end;
end;

procedure TFormHbWindowsMsg.SetMensagemRecebida(UmaMensagemRecebida: String);
begin
  FMensagemRecebida := UmaMensagemRecebida;
  if Assigned(FOnMensagemRecebida) then begin
    FOnMensagemRecebida(Self, FMensagemRecebida);
  end;
end;

initialization
  FInstance := Nil;

finalization
  TFormHbWindowsMsg.DropInstance;

end.

