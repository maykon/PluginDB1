unit uVisualizadorDataSet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DB,
  DBClient, DBGrids, StdCtrls, Buttons, CheckLst, Menus, Grids;

type
  TfVisualizadorDataSet = class(TForm)
    grdDados: TDBGrid;
    DataSource: TDataSource;
    ClientDataSet: TClientDataSet;
    edtFiltro: TEdit;
    chkFiltroAtivado: TCheckBox;
    lbQuantidade: TLabel;
    clCampos: TCheckListBox;
    btnTodos: TBitBtn;
    btnNenhum: TBitBtn;
    PopupMenu: TPopupMenu;
    PopupMenuCopiar: TMenuItem;
    PopupMenuExcluir: TMenuItem;
    lbFiltro: TLabel;
    chkAjustarTamanhoColunas: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure chkFiltroAtivadoClick(Sender: TObject);
    procedure clCamposClickCheck(Sender: TObject);
    procedure edtFiltroChange(Sender: TObject);
    procedure edtFiltroKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure grdDadosTitleClick(Column: TColumn);
    procedure btnNenhumClick(Sender: TObject);
    procedure btnTodosClick(Sender: TObject);
    procedure PopupMenuCopiarClick(Sender: TObject);
    procedure PopupMenuExcluirClick(Sender: TObject);
    procedure ClientDataSetBeforeInsert(DataSet: TDataSet);
    procedure chkAjustarTamanhoColunasClick(Sender: TObject);
  private
    FaTamanhoMaximo: array of smallint;

    procedure MouseScroll(var Msg: TMsg; var Handled: boolean);
    procedure AjustarTamanhoColunas;
    procedure CalcularTamanhoColunas;
    procedure CarregarArquivoDados;
    procedure CarregarCampos;
    procedure CarregarFiltro;
    procedure ContarRegistros;
    procedure MarcarTodosRegistros(const pbMarcar: boolean);
  public
    procedure CarregarDadosDataSet;
  end;

var
  fVisualizadorDataSet: TfVisualizadorDataSet;

implementation

uses
  ClipBrd, uConstantes;

{$R *.DFM}

{ TfVisualizadorDataSet }

procedure TfVisualizadorDataSet.FormCreate(Sender: TObject);
begin
  Application.OnMessage := MouseScroll;
  CarregarDadosDataSet;
end;

procedure TfVisualizadorDataSet.AjustarTamanhoColunas;
var
  nCont: smallint;
  nTamanho: integer;
begin
  for nCont := 0 to Pred(grdDados.Columns.Count) do
  begin
    if chkAjustarTamanhoColunas.Checked then
      nTamanho := FaTamanhoMaximo[nCont]
    else
      nTamanho := grdDados.Columns[nCont].Field.Size;

    if nTamanho > 0 then
    begin
      grdDados.Columns[nCont].Width := nTamanho;
    end;
  end;
end;

procedure TfVisualizadorDataSet.CalcularTamanhoColunas;
var
  nTamanho, nCont: smallint;
  sDisplayText: string;
begin
  SetLength(FaTamanhoMaximo, ClientDataSet.FieldCount);

  for nCont := 0 to grdDados.Columns.Count - 1 do
  begin
    FaTamanhoMaximo[nCont] := Canvas.TextWidth(ClientDataSet.Fields[nCont].DisplayLabel) +
      nBORDA_DBGRID;
  end;

  ClientDataSet.DisableControls;
  ClientDataSet.First;
  while not ClientDataSet.EOF do
  begin
    for nCont := 0 to grdDados.Columns.Count - 1 do
    begin
      sDisplayText := grdDados.Columns[nCont].Field.DisplayText;
      nTamanho := Canvas.TextWidth(Trim(sDisplayText)) + nBORDA_DBGRID;
      if nTamanho > FaTamanhoMaximo[nCont] then
      begin
        FaTamanhoMaximo[nCont] := nTamanho;
      end;
    end;
    ClientDataSet.Next;
  end;
  ClientDataSet.First;
  ClientDataSet.EnableControls;
end;

procedure TfVisualizadorDataSet.CarregarArquivoDados;
var
  nTentativas: smallint;
begin
  //jcf:format=off
  nTentativas := 0;
  repeat
    Sleep(150);
    Inc(nTentativas);
  until FileExists(sPATH_ARQUIVO_DADOS) or (nTentativas = nNUMERO_TENTATIVAS_LEITURA);

  if nTentativas = nNUMERO_TENTATIVAS_LEITURA then
  begin
    MessageDlg('Não foi possível carregar os dados. Tente novamente!', mtWarning, [mbOK], 0);
    Application.Terminate;
    Exit;
  end;

  try
    ClientDataSet.LoadFromFile(sPATH_ARQUIVO_DADOS);
    ClientDataSet.First;
    ContarRegistros;
  except
    On E:Exception do
    begin
      ClientDataSet.Close;
      
      MessageDlg('Não foi possível carregar os dados. Possíveis causas:' + #13 + #13 +
        '- O DataSet está nil;' + #13 +
        '- O DataSet está fechado;' + #13 +
        '- O DataSet selecionado está em modo de inserção/edição.' + #13 +
        'Erro: ' + E.Message,
        mtWarning, [mbOK], 0);

      Application.Terminate;
    end;
  end;
  //jcf:format=on
end;

procedure TfVisualizadorDataSet.CarregarCampos;
var
  nCont: smallint;
begin
  for nCont := 0 to ClientDataSet.Fields.Count - 1 do
  begin
    clCampos.Items.Add(ClientDataSet.Fields[nCont].FieldName);
    clCampos.Checked[nCont] := True;
  end;
end;

procedure TfVisualizadorDataSet.CarregarDadosDataSet;
begin
  CarregarArquivoDados;
  CarregarCampos;
  CarregarFiltro;
end;

procedure TfVisualizadorDataSet.CarregarFiltro;
var
  slFiltro: TStringList;
  sFiltro: string;
begin
  if not FileExists(sPATH_ARQUIVO_FILTRO) then
  begin
    Exit;
  end;

  if not ClientDataSet.Active then
  begin
    Exit;
  end;

  slFiltro := TStringList.Create;
  try
    slFiltro.LoadFromFile(sPATH_ARQUIVO_FILTRO);
    sFiltro := Copy(slFiltro[0], 2, Length(slFiltro[0]) - 2);

    if sFiltro = EmptyStr then
    begin
      Exit;
    end;

    edtFiltro.Text := sFiltro;
    chkFiltroAtivado.Checked := True;
  finally
    FreeAndNil(slFiltro);
  end;
end;

procedure TfVisualizadorDataSet.chkFiltroAtivadoClick(Sender: TObject);
begin
  ClientDataSet.Filter := edtFiltro.Text;
  try
    ClientDataSet.Filtered := chkFiltroAtivado.Checked;
  except
    MessageDlg('Filtro inválido!', mtWarning, [mbOK], 0);
    edtFiltro.SetFocus;
    chkFiltroAtivado.Checked := False;
  end;
  ContarRegistros;
end;

procedure TfVisualizadorDataSet.clCamposClickCheck(Sender: TObject);
var
  bHabilitar: boolean;
  sNomeCampo: string;
begin
  bHabilitar := clCampos.Checked[clCampos.ItemIndex];
  sNomeCampo := clCampos.Items[clCampos.ItemIndex];
  ClientDataSet.FieldByName(sNomeCampo).Visible := bHabilitar;

  if bHabilitar then
  begin
    AjustarTamanhoColunas;
  end;
end;

procedure TfVisualizadorDataSet.ContarRegistros;
begin
  lbQuantidade.Caption := Format('%d registro(s)', [ClientDataSet.RecordCount]);
end;

procedure TfVisualizadorDataSet.edtFiltroChange(Sender: TObject);
begin
  ClientDataSet.Filtered := False;
  chkFiltroAtivado.Checked := False;
end;

procedure TfVisualizadorDataSet.edtFiltroKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    chkFiltroAtivado.Checked := not chkFiltroAtivado.Checked;
  end;
end;

procedure TfVisualizadorDataSet.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Application.OnMessage := nil;
  Action := caFree;
end;

procedure TfVisualizadorDataSet.grdDadosTitleClick(Column: TColumn);
var
  sIndexName: string;
  oOrdenacao: TIndexOptions;
  nCont: smallint;
begin
  for nCont := 0 to grdDados.Columns.Count - 1 do
  begin
    grdDados.Columns[nCont].Title.Font.Style := [];
  end;

  ClientDataSet.IndexDefs.Clear;

  if ClientDataSet.IndexName = Column.FieldName + sINDEX_ASC then
  begin
    sIndexName := Column.FieldName + sINDEX_DESC;
    oOrdenacao := [ixDescending];
  end
  else
  begin
    sIndexName := Column.FieldName + sINDEX_ASC;
    oOrdenacao := [];
  end;

  ClientDataSet.AddIndex(sIndexName, Column.FieldName, oOrdenacao);
  Column.Title.Font.Style := [fsBold];
  ClientDataSet.IndexName := sIndexName;
  ClientDataSet.First;
end;

procedure TfVisualizadorDataSet.MarcarTodosRegistros(const pbMarcar: boolean);
var
  nCont: smallint;
  sNomeCampo: string;
begin
  for nCont := 0 to clCampos.Items.Count - 1 do
  begin
    clCampos.Checked[nCont] := pbMarcar;
    sNomeCampo := clCampos.Items[nCont];
    ClientDataSet.FieldByName(sNomeCampo).Visible := pbMarcar;
  end;
end;

procedure TfVisualizadorDataSet.MouseScroll(var Msg: TMsg; var Handled: boolean);
var
  nCont: smallint;
begin
  if Msg.message = WM_MOUSEWHEEL then
  begin
    Msg.message := WM_KEYDOWN;
    Msg.lParam := 0;
    nCont := HiWord(Msg.wParam);

    if nCont > 0 then
      Msg.wParam := VK_UP
    else
      Msg.wParam := VK_DOWN;

    Handled := False;
  end;
end;

procedure TfVisualizadorDataSet.btnNenhumClick(Sender: TObject);
begin
  MarcarTodosRegistros(False);
end;

procedure TfVisualizadorDataSet.btnTodosClick(Sender: TObject);
begin
  MarcarTodosRegistros(True);
  AjustarTamanhoColunas;
end;

procedure TfVisualizadorDataSet.PopupMenuCopiarClick(Sender: TObject);
var
  sColuna: string;
begin
  sColuna := grdDados.SelectedField.FieldName;
  Clipboard.AsText := ClientDataSet.FieldByName(sColuna).AsString;
end;

procedure TfVisualizadorDataSet.PopupMenuExcluirClick(Sender: TObject);
begin
  ClientDataSet.Delete;
  ContarRegistros;
end;

procedure TfVisualizadorDataSet.ClientDataSetBeforeInsert(DataSet: TDataSet);
begin
  Abort;
end;

procedure TfVisualizadorDataSet.chkAjustarTamanhoColunasClick(Sender: TObject);
begin
  if chkAjustarTamanhoColunas.Checked then
  begin
    CalcularTamanhoColunas;
  end;

  AjustarTamanhoColunas;
end;

end.

