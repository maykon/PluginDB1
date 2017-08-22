unit uCompilacao;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, CheckLst, Buttons, uFuncoes;

type
  TfCompilacao = class(TForm)
    CheckListBoxProjetos: TCheckListBox;
    PanelBotoes: TPanel;
    BitBtnCompilar: TBitBtn;
    PanelPesquisa: TPanel;
    EditPesquisa: TEdit;
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure EditPesquisaChange(Sender: TObject);
    procedure EditPesquisaKeyPress(Sender: TObject; var Key: char);
  private
    FoFuncoes: TFuncoes;
    procedure ExibirProjetosCarregados;
  public
    property Funcoes: TFuncoes write FoFuncoes;
    function PegarUltimoProjetoMarcado: string;
    function PegarProjetosSelecionados: string;
  end;

var
  fCompilacao: TfCompilacao;

implementation

uses
  ToolsAPI;

{$R *.DFM}

procedure TfCompilacao.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfCompilacao.ExibirProjetosCarregados;
var
  slProjetos: TStringList;
  nContador: smallint;
begin
  slProjetos := TStringList.Create;
  try
    slProjetos.CommaText := FoFuncoes.PegarProjetosCarregados;

    for nContador := 0 to Pred(slProjetos.Count) do
      CheckListBoxProjetos.Items.Add(slProjetos[nContador]);
  finally
    FreeAndNil(slProjetos);
  end;
end;

function TfCompilacao.PegarUltimoProjetoMarcado: string;
var
  nContador: byte;
begin
  result := EmptyStr;
  for nContador := 0 to Pred(CheckListBoxProjetos.Items.Count) do
  begin
    if CheckListBoxProjetos.Checked[nContador] then
      result := CheckListBoxProjetos.Items[nContador];
  end;
end;

procedure TfCompilacao.FormShow(Sender: TObject);
begin
  ExibirProjetosCarregados;
end;

function TfCompilacao.PegarProjetosSelecionados: string;
var
  nContador: byte;
  slProjetos: TStringList;
begin
  slProjetos := TStringList.Create;
  try
    for nContador := 0 to Pred(CheckListBoxProjetos.Items.Count) do
    begin
      if CheckListBoxProjetos.Checked[nContador] then
        slProjetos.Add(CheckListBoxProjetos.Items[nContador]);
    end;
    result := slProjetos.CommaText;
  finally
    FreeAndNil(slProjetos);
  end;
end;

procedure TfCompilacao.EditPesquisaChange(Sender: TObject);
var
  nContador: smallint;
begin
  if Trim(EditPesquisa.Text) = EmptyStr then
  begin
    CheckListBoxProjetos.ItemIndex := 0;
    Exit;
  end;

  for nContador := 0 to Pred(CheckListBoxProjetos.Items.Count) do
  begin
    // X é usado como um workaround na busca
    if Pos(LowerCase(EditPesquisa.Text),
      LowerCase('X' + CheckListBoxProjetos.Items[nContador])) > 0 then
    begin
      CheckListBoxProjetos.ItemIndex := nContador;
      Break;
    end;
  end;
end;

procedure TfCompilacao.EditPesquisaKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #32 then
  begin
    Key := #0;

    if CheckListBoxProjetos.ItemIndex < 0 then
      Exit;

    CheckListBoxProjetos.Checked[CheckListBoxProjetos.ItemIndex] :=
      not CheckListBoxProjetos.Checked[CheckListBoxProjetos.ItemIndex];
  end;
end;

end.

