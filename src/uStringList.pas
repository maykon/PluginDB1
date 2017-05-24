unit uStringList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TfStringList = class(TForm)
    mmValores: TMemo;
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  private
    procedure SetTextoLista(Value: string);
    procedure NumerarLinhas;
  public
    property TextoLista: string write SetTextoLista;
  end;

var
  fStringList: TfStringList;

implementation

{$R *.DFM}

procedure TfStringList.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfStringList.NumerarLinhas;
var
  nContador: integer;
  sLinha: string;
begin
  for nContador := 0 to Pred(mmValores.Lines.Count) do
  begin
    sLinha := Format('%d) %s', [nContador + 1, mmValores.Lines[nContador]]);
    mmValores.Lines[nContador] := sLinha;
  end;
end;

procedure TfStringList.SetTextoLista(Value: string);
var
  sTextoFormatado: string;
begin
  sTextoFormatado := StringReplace(Value, '#$D#$A', #13, [rfReplaceAll]);
  sTextoFormatado := StringReplace(sTextoFormatado, #39, EmptyStr, [rfReplaceAll]);
  mmValores.Lines.Text := sTextoFormatado;

  NumerarLinhas;
end;

end.

