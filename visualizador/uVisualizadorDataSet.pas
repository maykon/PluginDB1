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
    lbIndices: TLabel;
    edtIndices: TEdit;
    chkIndicesAtivado: TCheckBox;
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
    procedure edtIndicesKeyPress(Sender: TObject; var Key: char);
    procedure edtIndicesChange(Sender: TObject);
    procedure chkIndicesAtivadoClick(Sender: TObject);
    procedure ClientDataSetAfterScroll(DataSet: TDataSet);
  private
    FaTamanhoMaximo: array of smallint;

    procedure MouseScroll(var Msg: TMsg; var Handled: boolean);
    procedure AjustarTamanhoColunas;
    procedure CalcularTamanhoColunas;
    procedure CarregarArquivoDados;
    procedure CarregarCampos;
    procedure CarregarFiltro;
    procedure CarregarIndices;
    procedure CarregarClasse;
    procedure AtualizarContadorRegistros;
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
      grdDados.Columns[nCont].Width := nTamanho;
  end;
end;

procedure TfVisualizadorDataSet.CalcularTamanhoColunas;
var
  nTamanho, nCont: smallint;
  sDisplayText: string;
begin
  SetLength(FaTamanhoMaximo, ClientDataSet.FieldCount);

  for nCont := 0 to Pred(grdDados.Columns.Count) do
  begin
    FaTamanhoMaximo[nCont] := Canvas.TextWidth(ClientDataSet.Fields[nCont].DisplayLabel) +
      nBORDA_DBGRID;
  end;

  ClientDataSet.DisableControls;
  ClientDataSet.First;
  while not ClientDataSet.EOF do
  begin
    for nCont := 0 to Pred(grdDados.Columns.Count) do
    begin
      sDisplayText := grdDados.Columns[nCont].Field.DisplayText;
      nTamanho := Canvas.TextWidth(Trim(sDisplayText)) + nBORDA_DBGRID;

      if nTamanho > FaTamanhoMaximo[nCont] then
        FaTamanhoMaximo[nCont] := nTamanho;
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
    AtualizarContadorRegistros;
  except
    On E:Exception do
    begin
      ClientDataSet.Close;
      MessageDlg('Não foi possível carregar os dados. Erro: ' + E.Message, mtWarning, [mbOK], 0);
      Application.Terminate;
    end;
  end;
  //jcf:format=on                                 
end;

procedure TfVisualizadorDataSet.CarregarCampos;
var
  nCont: smallint;
begin
  for nCont := 0 to Pred(ClientDataSet.Fields.Count) do
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
  CarregarIndices;
  CarregarClasse;
end;

procedure TfVisualizadorDataSet.CarregarFiltro;
var
  slFiltro: TStringList;
  sFiltro: string;
begin
  if not FileExists(sPATH_ARQUIVO_FILTRO) then
    Exit;

  if not ClientDataSet.Active then
    Exit;

  slFiltro := TStringList.Create;
  try
    slFiltro.LoadFromFile(sPATH_ARQUIVO_FILTRO);
    sFiltro := Copy(slFiltro[0], 2, Length(slFiltro[0]) - 2);

    if sFiltro = EmptyStr then
      Exit;

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
    chkFiltroAtivado.Font.Style := [];
    if chkFiltroAtivado.Checked then
      chkFiltroAtivado.Font.Style := [fsBold];

    ClientDataSet.Filtered := chkFiltroAtivado.Checked;
  except
    MessageDlg('Filtro inválido!', mtWarning, [mbOK], 0);

    if edtFiltro.CanFocus then
      edtFiltro.SetFocus;

    chkFiltroAtivado.Checked := False;
  end;
  AtualizarContadorRegistros;
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
    AjustarTamanhoColunas;
end;

procedure TfVisualizadorDataSet.AtualizarContadorRegistros;
begin
  lbQuantidade.Caption := Format('Contador: %d / %d', [ClientDataSet.RecNo,
    ClientDataSet.RecordCount]);
end;

procedure TfVisualizadorDataSet.edtFiltroChange(Sender: TObject);
begin
  ClientDataSet.Filtered := False;
  chkFiltroAtivado.Checked := False;
end;

procedure TfVisualizadorDataSet.edtFiltroKeyPress(Sender: TObject; var Key: char);
begin
  if Trim(edtFiltro.Text) = EmptyStr then
  begin
    chkFiltroAtivado.Checked := False;
    Exit;
  end;

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
  for nCont := 0 to Pred(grdDados.Columns.Count) do
    grdDados.Columns[nCont].Title.Font.Style := [];

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
  for nCont := 0 to Pred(clCampos.Items.Count) do
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
  AtualizarContadorRegistros;
end;

procedure TfVisualizadorDataSet.ClientDataSetBeforeInsert(DataSet: TDataSet);
begin
  Abort;
end;

procedure TfVisualizadorDataSet.chkAjustarTamanhoColunasClick(Sender: TObject);
begin
  if chkAjustarTamanhoColunas.Checked then
    CalcularTamanhoColunas;

  AjustarTamanhoColunas;
end;

procedure TfVisualizadorDataSet.CarregarIndices;
var
  slIndices: TStringList;
  sIndices: string;
begin
  if not FileExists(sPATH_ARQUIVO_INDICES) then
    Exit;

  if not ClientDataSet.Active then
    Exit;

  slIndices := TStringList.Create;
  try
    slIndices.LoadFromFile(sPATH_ARQUIVO_INDICES);
    sIndices := Copy(slIndices[0], 2, Length(slIndices[0]) - 2);

    if sIndices = EmptyStr then
      Exit;

    edtIndices.Text := sIndices;
    chkIndicesAtivado.Checked := True;
  finally
    FreeAndNil(slIndices);
  end;
end;

procedure TfVisualizadorDataSet.edtIndicesKeyPress(Sender: TObject; var Key: char);
begin
  if Trim(edtIndices.Text) = EmptyStr then
  begin
    chkIndicesAtivado.Checked := False;
    Exit;
  end;

  if Key = #13 then
  begin
    Key := #0;
    chkIndicesAtivado.Checked := not chkIndicesAtivado.Checked;
  end;
end;

procedure TfVisualizadorDataSet.edtIndicesChange(Sender: TObject);
begin
  ClientDataSet.IndexFieldNames := EmptyStr;
  chkIndicesAtivado.Checked := False;
end;

procedure TfVisualizadorDataSet.chkIndicesAtivadoClick(Sender: TObject);
begin
  try
    ClientDataSet.IndexFieldNames := EmptyStr;
    chkIndicesAtivado.Font.Style := [];
    if chkIndicesAtivado.Checked then
    begin
      chkIndicesAtivado.Font.Style := [fsBold];
      ClientDataSet.IndexFieldNames := Trim(edtIndices.Text);
    end;
  except
    MessageDlg('Índice inválido!', mtWarning, [mbOK], 0);

    if edtIndices.CanFocus then
      edtIndices.SetFocus;

    chkIndicesAtivado.Checked := False;
  end;
end;

procedure TfVisualizadorDataSet.CarregarClasse;
var
  slNome: TStringList;
  sNome: string;
begin
  if not FileExists(sPATH_ARQUIVO_CLASSE) then
    Exit;

  if not ClientDataSet.Active then
    Exit;

  slNome := TStringList.Create;
  try
    slNome.LoadFromFile(sPATH_ARQUIVO_CLASSE);
    sNome := Copy(slNome[0], 2, Length(slNome[0]) - 2);

    if sNome = EmptyStr then
      Exit;

    Self.Caption := sNome + ' - Visualizador de DataSet';
    Application.Title := sNome + ' - Visualizador de DataSet';
  finally
    FreeAndNil(slNome);
  end;
end;

procedure TfVisualizadorDataSet.ClientDataSetAfterScroll(DataSet: TDataSet);
begin
  AtualizarContadorRegistros;
end;

end.

