unit uVisualizadorDataSet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DB,
  DBClient, DBGrids, StdCtrls, Buttons, CheckLst, Menus, Grids;

type
  TfVisualizadorDataSet = class(TForm)
    btnCarregar: TBitBtn;
    btnNenhum: TBitBtn;
    btnTodos: TBitBtn;
    chkAjustarTamanhoColunas: TCheckBox;
    chkFiltroAtivado: TCheckBox;
    chkIndicesAtivado: TCheckBox;
    clCampos: TCheckListBox;
    ClientDataSet: TClientDataSet;
    DataSource: TDataSource;
    edtFiltro: TEdit;
    edtIndices: TEdit;
    edtPesquisaCampos: TEdit;
    grdDados: TDBGrid;
    lbFiltro: TLabel;
    lbIndices: TLabel;
    lbQuantidade: TLabel;
    PopupMenu: TPopupMenu;
    PopupMenuCopiar: TMenuItem;
    PopupMenuExcluir: TMenuItem;
    procedure btnCarregarClick(Sender: TObject);
    procedure btnNenhumClick(Sender: TObject);
    procedure btnTodosClick(Sender: TObject);
    procedure chkAjustarTamanhoColunasClick(Sender: TObject);
    procedure chkFiltroAtivadoClick(Sender: TObject);
    procedure chkIndicesAtivadoClick(Sender: TObject);
    procedure clCamposClickCheck(Sender: TObject);
    procedure ClientDataSetAfterScroll(DataSet: TDataSet);
    procedure ClientDataSetBeforeDelete(DataSet: TDataSet);
    procedure ClientDataSetBeforeInsert(DataSet: TDataSet);
    procedure edtFiltroChange(Sender: TObject);
    procedure edtFiltroKeyPress(Sender: TObject; var Key: char);
    procedure edtIndicesChange(Sender: TObject);
    procedure edtIndicesKeyPress(Sender: TObject; var Key: char);
    procedure edtPesquisaCamposChange(Sender: TObject);
    procedure edtPesquisaCamposKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure edtPesquisaCamposKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure grdDadosTitleClick(Column: TColumn);
    procedure PopupMenuCopiarClick(Sender: TObject);
    procedure PopupMenuExcluirClick(Sender: TObject);
  private
    FaTamanhoMaximo: array of smallint;

    procedure AjustarTamanhoColunas;
    procedure CalcularTamanhoColunas;
    procedure CarregarArquivoDados(const psNomeArquivo: string);
    procedure CarregarCampos;
    procedure CarregarParametrosDataSet;
    procedure ControlarVisibilidadeCampo;
    procedure AtualizarContadorRegistros;
    procedure MarcarTodosRegistros(const pbMarcar: boolean);
    procedure Aviso(const psMensagem: string);
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
  if ParamStr(1) = '/auto' then
    CarregarDadosDataSet;
end;

procedure TfVisualizadorDataSet.AjustarTamanhoColunas;
var
  nTamanho, nContador: smallint;
begin
  if not ClientDataSet.Active then
    Exit;

  for nContador := 0 to Pred(grdDados.Columns.Count) do
  begin
    if chkAjustarTamanhoColunas.Checked then
      nTamanho := FaTamanhoMaximo[nContador]
    else
      nTamanho := grdDados.Columns[nContador].Field.Size;

    if nTamanho > 0 then
      grdDados.Columns[nContador].Width := nTamanho;
  end;
end;

procedure TfVisualizadorDataSet.CalcularTamanhoColunas;
var
  nTamanho, nContador: smallint;
  sDisplayText: string;
begin
  if not ClientDataSet.Active then
    Exit;

  SetLength(FaTamanhoMaximo, ClientDataSet.FieldCount);

  for nContador := 0 to Pred(grdDados.Columns.Count) do
  begin
    FaTamanhoMaximo[nContador] := Canvas.TextWidth(ClientDataSet.Fields[nContador].DisplayLabel) +
      nBORDA_DBGRID;
  end;

  ClientDataSet.DisableControls;
  ClientDataSet.First;
  while not ClientDataSet.EOF do
  begin
    for nContador := 0 to Pred(grdDados.Columns.Count) do
    begin
      sDisplayText := grdDados.Columns[nContador].Field.DisplayText;
      nTamanho := Canvas.TextWidth(Trim(sDisplayText)) + nBORDA_DBGRID;

      if nTamanho > FaTamanhoMaximo[nContador] then
        FaTamanhoMaximo[nContador] := nTamanho;
    end;
    ClientDataSet.Next;
  end;
  ClientDataSet.First;
  ClientDataSet.EnableControls;
end;

procedure TfVisualizadorDataSet.CarregarArquivoDados(const psNomeArquivo: string);
var
  nTentativas: smallint;
begin
  //jcf:format=off
  nTentativas := 0;
  repeat
    Sleep(150);
    Inc(nTentativas);
  until FileExists(psNomeArquivo) or (nTentativas = nNUMERO_TENTATIVAS_LEITURA);

  if nTentativas = nNUMERO_TENTATIVAS_LEITURA then
  begin
    Aviso('Não foi possível carregar os dados. Tente novamente!');
    Close;
    Exit;
  end;

  try
    ClientDataSet.Close;
    ClientDataSet.LoadFromFile(psNomeArquivo);
    AtualizarContadorRegistros;
  except
    On E:Exception do
    begin
      ClientDataSet.Close;
      Aviso('Não foi possível carregar os dados. Erro: ' + E.Message);
      Application.Terminate;
    end;
  end;
  //jcf:format=on                                 
end;

procedure TfVisualizadorDataSet.CarregarCampos;
var
  nContador: smallint;
begin
  for nContador := 0 to Pred(ClientDataSet.Fields.Count) do
  begin
    clCampos.Items.Add(ClientDataSet.Fields[nContador].FieldName);
    clCampos.Checked[nContador] := True;
  end;
end;

procedure TfVisualizadorDataSet.CarregarDadosDataSet;
begin
  CarregarArquivoDados(sPATH_ARQUIVO_DADOS);
  CarregarCampos;

  CarregarParametrosDataSet;
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
    Aviso('Filtro inválido!');

    if edtFiltro.CanFocus then
      edtFiltro.SetFocus;

    chkFiltroAtivado.Checked := False;
  end;
  AtualizarContadorRegistros;
end;

procedure TfVisualizadorDataSet.clCamposClickCheck(Sender: TObject);
begin
  ControlarVisibilidadeCampo;
end;

procedure TfVisualizadorDataSet.AtualizarContadorRegistros;
begin
  if not ClientDataSet.Active then
    Exit;

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
  nContador: smallint;
begin
  if not ClientDataSet.Active then
    Exit;

  for nContador := 0 to Pred(grdDados.Columns.Count) do
    grdDados.Columns[nContador].Title.Font.Style := [];

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
  nContador: smallint;
  sNomeCampo: string;
begin
  for nContador := 0 to Pred(clCampos.Items.Count) do
  begin
    clCampos.Checked[nContador] := pbMarcar;
    sNomeCampo := clCampos.Items[nContador];
    ClientDataSet.FieldByName(sNomeCampo).Visible := pbMarcar;
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
begin
  Clipboard.AsText := ClientDataSet.FieldByName(grdDados.SelectedField.FieldName).AsString;
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
    Aviso('Índice inválido!');

    if edtIndices.CanFocus then
      edtIndices.SetFocus;

    chkIndicesAtivado.Checked := False;
  end;
end;

procedure TfVisualizadorDataSet.ClientDataSetAfterScroll(DataSet: TDataSet);
begin
  AtualizarContadorRegistros;
end;

procedure TfVisualizadorDataSet.CarregarParametrosDataSet;
var
  slParametros: TStringList;
  sFiltro: string;
  sIndices: string;
  sNomeClasse: string;
begin
  if not FileExists(sPATH_PROP_DATASET) then
    Exit;

  if not ClientDataSet.Active then
    Exit;

  slParametros := TStringList.Create;
  try
    slParametros.LoadFromFile(sPATH_PROP_DATASET);

    sFiltro := Copy(slParametros[0], 2, Length(slParametros[0]) - 2);
    sIndices := Copy(slParametros[1], 2, Length(slParametros[1]) - 2);
    sNomeClasse := Copy(slParametros[2], 2, Length(slParametros[2]) - 2);

    if Trim(sFiltro) <> EmptyStr then
    begin
      edtFiltro.Text := sFiltro;
      chkFiltroAtivado.Checked := True;
    end;

    if Trim(sIndices) <> EmptyStr then
    begin
      edtIndices.Text := sIndices;
      chkIndicesAtivado.Checked := True;
    end;

    if Trim(sNomeClasse) <> EmptyStr then
    begin
      Self.Caption := sNomeClasse + ' - Visualizador de DataSet';
      Application.Title := sNomeClasse + ' - Visualizador de DataSet';
    end;

    ClientDataSet.First;
  finally
    FreeAndNil(slParametros);
  end;
end;

procedure TfVisualizadorDataSet.btnCarregarClick(Sender: TObject);
var
  oOpenDialog: TOpenDialog;
begin
  oOpenDialog := TOpenDialog.Create(Self);
  try
    if not oOpenDialog.Execute then
      Exit;

    clCampos.Clear;
    edtFiltro.Clear;
    edtIndices.Clear;
    CarregarArquivoDados(oOpenDialog.FileName);
    CarregarCampos;
  finally
    FreeAndNil(oOpenDialog);
  end;
end;

procedure TfVisualizadorDataSet.edtPesquisaCamposChange(Sender: TObject);
var
  nContador: smallint;
begin
  if Trim(edtPesquisaCampos.Text) = EmptyStr then
  begin
    clCampos.ItemIndex := 0;
    Exit;
  end;

  for nContador := 0 to Pred(clCampos.Items.Count) do
  begin
    // X é usado como um workaround na busca
    if Pos(LowerCase(edtPesquisaCampos.Text), LowerCase('X' + clCampos.Items[nContador])) > 0 then
    begin
      clCampos.ItemIndex := nContador;

      if clCampos.Checked[nContador] then
        grdDados.SelectedField := ClientDataSet.FieldByName(clCampos.Items[nContador]);

      Break;
    end;
  end;
end;

procedure TfVisualizadorDataSet.edtPesquisaCamposKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #32 then
  begin
    Key := #0;

    if clCampos.ItemIndex < 0 then
      Exit;

    clCampos.Checked[clCampos.ItemIndex] := not clCampos.Checked[clCampos.ItemIndex];
    ControlarVisibilidadeCampo;
  end;
end;

procedure TfVisualizadorDataSet.ControlarVisibilidadeCampo;
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

procedure TfVisualizadorDataSet.ClientDataSetBeforeDelete(DataSet: TDataSet);
begin
  if ClientDataSet.IsEmpty then
    Abort;
end;

procedure TfVisualizadorDataSet.edtPesquisaCamposKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Key = VK_DOWN then
  begin
    Key := 0;

    if clCampos.ItemIndex < Pred(clCampos.Items.Count) then
      clCampos.ItemIndex := clCampos.ItemIndex + 1;
  end;

  if Key = VK_UP then
  begin
    Key := 0;

    if clCampos.ItemIndex > 0 then
      clCampos.ItemIndex := clCampos.ItemIndex - 1;
  end;
end;

procedure TfVisualizadorDataSet.Aviso(const psMensagem: string);
begin
  MessageDlg(psMensagem, mtWarning, [mbOK], 0);
end;

end.

