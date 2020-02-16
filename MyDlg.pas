unit MyDlg;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, Dialogs, Forms, StdCtrls;


function HbCustomMessageDlg(const Titulo, Msg: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; Captions: array of String): Word;
function HbMessageDlg(const Titulo, Msg: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): Word;


implementation


const ButtonsCaptions: array[0..11] of String =  ('Sim', 'Não', 'OK', 'Cancelar', 'Desistir', 'Repetir', 'Ignorar', 'Todos',
  'Tudo Não', 'Tudo Sim', 'Ajuda', 'Fechar');

// mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose

function HbCustomMessageDlg(const Titulo, Msg: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; Captions: array of String): Word;
var
  aMsgDlg: TForm;
  i: Integer;
  dlgButton: TCustomButton;
  CaptionIndex: Integer;
begin
  { Create the Dialog }
  aMsgDlg := CreateMessageDialog(Msg, DlgType, Buttons);
  aMsgDlg.Caption := Titulo;

  captionIndex := 0;
  { Loop through Objects in Dialog }
  for i := 0 to aMsgDlg.ComponentCount - 1 do begin
   { If the object is of type TButton, then }
    if (aMsgDlg.Components[i] is TCustomButton) then begin
      dlgButton := TCustomButton(aMsgDlg.Components[i]);
      if CaptionIndex > High(Captions) then begin
        Break;
      end;
      { Give a new caption from our Captions array}
      dlgButton.Caption := Captions[CaptionIndex];
      Inc(CaptionIndex);
    end;
  end;
  Result := aMsgDlg.ShowModal;
end;

function HbMessageDlg(const Titulo, Msg: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons): Word;
begin
  Result := HbCustomMessageDlg(Titulo, Msg, DlgType, Buttons, ButtonsCaptions);
end;

end.

