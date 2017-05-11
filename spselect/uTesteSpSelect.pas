unit uTesteSpSelect;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, DB, DBClient, Grids, DBGrids, ComCtrls, SConnect, Menus;

type
  TfTesteSpSelect = class(TForm)
    PageControl: TPageControl;
    TabSheetParametros: TTabSheet;
    TabSheetDados: TTabSheet;
    TabSheetSQL: TTabSheet;
    DBGridDados: TDBGrid;
    LabelClasseConjuntoDados: TLabel;
    EditClasseConjuntoDados: TEdit;
    LabelNomeSpSelect: TLabel;
    EditNomeSpSelect: TEdit;
    cdsParametros: TClientDataSet;
    dsParametros: TDataSource;
    BitBtnExecutarTeste: TBitBtn;
    cdsDados: TClientDataSet;
    dsDados: TDataSource;
    DBGridParametros: TDBGrid;
    cdsParametrosNome: TStringField;
    cdsParametrosValor: TStringField;
    MemoSQL: TMemo;
    SpeedButtonAdicionar: TSpeedButton;
    SpeedButtonRemover: TSpeedButton;
    PopupMenu: TPopupMenu;
    MenuItemSalvarDados: TMenuItem;
    SaveDialog: TSaveDialog;
    LabelStatus: TLabel;
    procedure DBGridParametrosKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure BitBtnExecutarTesteClick(Sender: TObject);
    procedure SpeedButtonAdicionarClick(Sender: TObject);
    procedure SpeedButtonRemoverClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MenuItemSalvarDadosClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FoConexao: TSocketConnection;
    FvSpDB: variant;

    function ConectarServidor: boolean;
    function PegarParametros: olevariant;
    function PegarClasseConjuntoDados: string;
    function VerificarPodeExecutarTeste: boolean;
    procedure AtualizarStatus(const psMensagem: string);
    procedure ExecutarTeste;
    procedure GravarParametrosUtilizados;
    procedure LerUltimosParametrosUtilizados;
    procedure LimparTela;
  end;

var
  fTesteSpSelect: TfTesteSpSelect;

implementation

uses
  IniFiles, uConstantes;

{$R *.DFM}

function TfTesteSpSelect.ConectarServidor: boolean;
var
  sErroConexao: string;
begin
  FoConexao := TSocketConnection.Create(nil); //PC_OK

  try
    AtualizarStatus('Conectando-se ao servidor...');

    FoConexao.ServerName := 'fpgServidor.fpgServidorDM';
    FoConexao.Address := '127.0.0.1';
    FoConexao.ServerGUID := '{42A9E80A-670D-459D-B20E-E41F9B3AD880}';
    FoConexao.Connected := True;

    AtualizarStatus('Realizando Login...');

    FvSpDB := FoConexao.AppServer.spDB;
    FvSpDB.LoginAT('90', '', '0', sErroConexao);

    result := True;
  except
    on E: Exception do
    begin
      result := False;
      AtualizarStatus('Erro na conexão com o servidor.');
      MessageDlg('Houve um erro na conexão com o servidor.' + #13 + E.Message + #13 +
        sErroConexao, mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TfTesteSpSelect.DBGridParametrosKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    cdsParametros.Delete;

  if Key = VK_INSERT then
    cdsParametros.Append;
end;

procedure TfTesteSpSelect.ExecutarTeste;
var
  sClasseConjuntoDados: string;
  sNomeSpSelect: string;
  vParametros: olevariant;
  vResultadoDados: olevariant;
  sResultadoSQL: string;
begin
  if not ConectarServidor then
    Exit;

  sClasseConjuntoDados := PegarClasseConjuntoDados;
  sNomeSpSelect := Trim(EditNomeSpSelect.Text);

  try
    vParametros := PegarParametros;

    GravarParametrosUtilizados;

    AtualizarStatus('Executando a consulta...');

    FvSpDB.fpgTesteSpSelect.ExecutarTesteSpSelect(sClasseConjuntoDados,
      sNomeSpSelect, vParametros, {out} vResultadoDados, {out} sResultadoSQL);

    cdsDados.Data := vResultadoDados;
    MemoSQL.Lines.Text := sResultadoSQL;

    MessageDlg('Consulta executada com sucesso!', mtInformation, [mbOK], 0);
    PageControl.ActivePage := TabSheetDados;
  except
    on E: Exception do
    begin
      AtualizarStatus('Erro na execução do método.');
      MessageDlg('Houve um erro na execução do método.' + #13 + E.Message, mtWarning, [mbOK], 0);
    end;
  end;
end;

function TfTesteSpSelect.VerificarPodeExecutarTeste: boolean;
begin
  result := False;

  if Trim(EditClasseConjuntoDados.Text) = EmptyStr then
  begin
    MessageDlg('Preencha a classe do conjunto de dados.', mtWarning, [mbOK], 0);
    EditClasseConjuntoDados.SetFocus;
    Exit;
  end;

  if Trim(EditNomeSpSelect.Text) = EmptyStr then
  begin
    MessageDlg('Preencha o nome do spSelect.', mtWarning, [mbOK], 0);
    EditNomeSpSelect.SetFocus;
    Exit;
  end;

  result := True;
end;

procedure TfTesteSpSelect.BitBtnExecutarTesteClick(Sender: TObject);
begin
  if not VerificarPodeExecutarTeste then
    Exit;

  LabelStatus.Visible := True;
  cdsDados.Close;
  MemoSQL.Lines.Clear;
  ExecutarTeste;

  FvSpDB := null;
  LabelStatus.Visible := False;
  FreeAndNil(FoConexao); //PC_OK
end;

procedure TfTesteSpSelect.SpeedButtonAdicionarClick(Sender: TObject);
begin
  cdsParametros.Append;
end;

procedure TfTesteSpSelect.SpeedButtonRemoverClick(Sender: TObject);
begin
  cdsParametros.Delete;
end;

procedure TfTesteSpSelect.LimparTela;
begin
  EditClasseConjuntoDados.Clear;
  EditNomeSpSelect.Clear;
  cdsDados.Close;
  MemoSQL.Lines.Clear;
  cdsParametros.EmptyDataSet;

  PageControl.ActivePage := TabSheetParametros;
  EditClasseConjuntoDados.SetFocus;
end;

procedure TfTesteSpSelect.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_F5 then
    LimparTela;
end;

procedure TfTesteSpSelect.MenuItemSalvarDadosClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    cdsDados.SaveToFile(SaveDialog.FileName);
end;

procedure TfTesteSpSelect.PopupMenuPopup(Sender: TObject);
begin
  if (not cdsDados.Active) or (cdsDados.IsEmpty) then
    Abort;
end;

function TfTesteSpSelect.PegarParametros: olevariant;
begin
  result := null;

  if not cdsParametros.IsEmpty then
    result := cdsParametros.Data;
end;

procedure TfTesteSpSelect.AtualizarStatus(const psMensagem: string);
begin
  LabelStatus.Caption := psMensagem;
  Application.ProcessMessages;
end;

procedure TfTesteSpSelect.FormShow(Sender: TObject);
begin
  PageControl.ActivePage := TabSheetParametros;
  LerUltimosParametrosUtilizados;
end;

procedure TfTesteSpSelect.LerUltimosParametrosUtilizados;
var
  oArquivoINI: TIniFile;
begin
  oArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI);
  try
    //jcf:format=off
    EditClasseConjuntoDados.Text := oArquivoINI.ReadString('TesteSpSelect', 'ClasseConjuntoDados', EmptyStr);
    EditNomeSpSelect.Text := oArquivoINI.ReadString('TesteSpSelect', 'NomeSpSelect', EmptyStr);
    //jcf:format=on

    if FileExists(sPATH_PARAMS_SELECT) then
      cdsParametros.LoadFromFile(sPATH_PARAMS_SELECT);
  finally
    FreeAndNil(oArquivoINI);
  end;
end;

procedure TfTesteSpSelect.GravarParametrosUtilizados;
var
  oArquivoINI: TIniFile;
begin
  oArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI);
  try
    oArquivoINI.WriteString('TesteSpSelect', 'ClasseConjuntoDados', EditClasseConjuntoDados.Text);
    oArquivoINI.WriteString('TesteSpSelect', 'NomeSpSelect', EditNomeSpSelect.Text);

    if not cdsParametros.IsEmpty then
      cdsParametros.SaveToFile(sPATH_PARAMS_SELECT);
  finally
    FreeAndNil(oArquivoINI);
  end;
end;

function TfTesteSpSelect.PegarClasseConjuntoDados: string;
begin
  result := Trim(EditClasseConjuntoDados.Text);

  // adiciona o "T" antes do nome da classe, caso não exista
  if Copy(result, 0, 1) <> 'T' then
    result := 'T' + result;

  // remove o sufixo "Serv" do nome
  result := StringReplace(result, 'Serv', EmptyStr, [rfReplaceAll]);
end;

end.

