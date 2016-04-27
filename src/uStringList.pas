unit uStringList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TfStringList = class(TForm)
    mmValores: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  private
    procedure CarregarStringList;
  end;

var
  fStringList: TfStringList;

implementation

uses
  uConstantes;

{$R *.DFM}

procedure TfStringList.CarregarStringList;
var
  nTentativas: smallint;
begin
  nTentativas := 0;
  repeat
    Sleep(150);
    Inc(nTentativas);
  until FileExists(sPATH_ARQUIVO_LISTA) or (nTentativas = nNUMERO_TENTATIVAS_LEITURA);

  if nTentativas = nNUMERO_TENTATIVAS_LEITURA then
  begin
    MessageDlg('Erro ao carregar os dados. Tente novamente!', mtWarning, [mbOK], 0);
    Exit;
  end;

  mmValores.Lines.LoadFromFile(sPATH_ARQUIVO_LISTA);
end;

procedure TfStringList.FormCreate(Sender: TObject);
begin
  CarregarStringList;
end;

procedure TfStringList.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Close;
  end;
end;

end.

