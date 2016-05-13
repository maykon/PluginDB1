unit uFuncoes;

interface

uses
  ToolsAPI, uAguarde, Classes, Menus, uToolsAPIUtils;

type
  TTipoSistema = (tsPG, tsSG, tsPJ);

  TFuncoes = class
  private
    FoToolsAPIUtils: TToolsAPIUTils;
    FenTipoSistema: TTipoSistema;

    function PegarDiretorioBin: string;
    function SalvarArquivoDataSet(const psNomeDataSet: string): boolean;
    function VerificarArquivoExisteNoDiretorioBin(const psNomeArquivo: string): boolean;
    function ValidarTextoSelecionado(const psTexto: string): boolean;
    procedure CarregarArquivoDataSet;
    procedure ExcluirArquivo(const psNomeArquivo: string);
    procedure PegarSistemaPadrao;
    procedure SalvarFiltroDataSet(const psNomeDataSet: string);
  public
    constructor Create;
    destructor Destroy; override;

    // Setter
    procedure SetTipoSistema(Value: TTipoSistema);

    // ferramentas internas
    procedure AbrirServidor(Sender: TObject);
    procedure AbrirAplicacao(Sender: TObject);
    procedure AbrirDiretorioBin(Sender: TObject);
    procedure AbrirSPCfg(Sender: TObject);
    procedure AbrirItemRTC(Sender: TObject);

    // ferramentas externas
    procedure AbrirVisualizaDTS(Sender: TObject);
    procedure AbrirSPMonitor(Sender: TObject);
    procedure AbrirSPMonitor3(Sender: TObject);
    procedure AbrirSelectSQL(Sender: TObject);
    procedure AbrirSqlDbx(Sender: TObject);
    procedure AbrirWinSpy(Sender: TObject);
    procedure ConsultarRansack(Sender: TObject);

    // operações com DataSet e StringList
    procedure ProcessarDataSet(const psNomeDataSet: string);
    procedure VisualizarDataSet(Sender: TObject);
    procedure VisualizarDataSetManual(Sender: TObject);
    procedure LerStringList(Sender: TObject);
    procedure GravarSistemaArquivoINI;

    // atalhos
    procedure ConfigurarAtalhos;

    property TipoSistema: TTipoSistema read FenTipoSistema write SetTipoSistema;
  end;

implementation

uses
  Forms, IniFiles, TypInfo, SysUtils, ShellAPI, Windows, Dialogs, uConstantes,
  uStringList, uConfigurarAtalhos;

{ TFuncoes }

constructor TFuncoes.Create;
begin
  inherited;

  FoToolsAPIUtils := TToolsAPIUtils.Create; //PC_OK
  PegarSistemaPadrao;
end;

destructor TFuncoes.Destroy;
begin
  FreeAndNil(FoToolsAPIUtils); //PC_OK

  inherited;
end;

function TFuncoes.PegarDiretorioBin: string;
var
  sDiretorio: string;
  nPosicaoPastaSRC: integer;
  sParteDiretorioSRC: string;
begin
  sDiretorio := UpperCase(FoToolsAPIUtils.PegarDiretorioProjetoAtivo);
  nPosicaoPastaSRC := Pos('SRC', sDiretorio);
  sParteDiretorioSRC := Copy(sDiretorio, nPosicaoPastaSRC, Length(sDiretorio));
  result := StringReplace(sDiretorio, sParteDiretorioSRC, 'bin\', [rfReplaceAll]);
end;

procedure TFuncoes.VisualizarDataSet(Sender: TObject);
var
  sNomeDataSet: string;
begin
  sNomeDataSet := FoToolsAPIUtils.PegarTextoSelecionado;
  ProcessarDataSet(sNomeDataSet);
end;

procedure TFuncoes.CarregarArquivoDataSet;
begin
  if not FileExists(sPATH_VISUALIZADOR) then
  begin
    MessageDlg(Format('O visualizador de DataSets não está disponível no caminho: %s',
      [ExtractFilePath(sPATH_VISUALIZADOR)]), mtWarning, [mbOK], 0);
    Exit;
  end;

  FoToolsAPIUtils.AbrirArquivo(sPATH_VISUALIZADOR, EmptyStr);
end;

function TFuncoes.SalvarArquivoDataSet(const psNomeDataSet: string): boolean;
var
  sExpressao: string;
  sResultado: string;
  oThread: IOTAThread;
  oRetorno: TOTAEvaluateResult;
begin
  oThread := FoToolsAPIUtils.PegarThreadAtual;
  try
    if not Assigned(oThread) then
    begin
      Exit;
    end;
    sExpressao := Format('%s.SaveToFile(''%s'')', [psNomeDataSet, sPATH_ARQUIVO_DADOS]);
    oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);
    result := oRetorno in [erOK, erDeferred];
  finally
    FreeAndNil(oThread); //PC_OK
  end;
end;

procedure TFuncoes.SalvarFiltroDataSet(const psNomeDataSet: string);
var
  oThread: IOTAThread;
  oRetorno: TOTAEvaluateResult;
  sExpressao: string;
  sResultado: string;
  slFiltro: TStringList;
begin
  oThread := FoToolsAPIUtils.PegarThreadAtual;
  sResultado := EmptyStr;
  try
    if not Assigned(oThread) then
    begin
      Exit;
    end;

    sExpressao := Format('%s.Filter', [psNomeDataSet]);
    oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);
  finally
    FreeAndNil(oThread); //PC_OK
  end;

  if oRetorno = erError then
  begin
    Exit;
  end;

  if Trim(StringReplace(sResultado, '''', '', [rfReplaceAll])) = EmptyStr then
  begin
    Exit;
  end;

  slFiltro := TStringList.Create;
  try
    slFiltro.Add(sResultado);
    slFiltro.SaveToFile(Format('%s', [sPATH_ARQUIVO_FILTRO]));
  finally
    FreeAndNil(slFiltro);
  end;
end;

procedure TFuncoes.VisualizarDataSetManual(Sender: TObject);
var
  sNomeDataSet: string;
begin
  sNomeDataSet := 'Ex: fpgProcessoParte.esajParte';

  if not InputQuery('Informar o DataSet', 'Informe o nome do DataSet:', sNomeDataSet) then
  begin
    Exit;
  end;

  ProcessarDataSet(sNomeDataSet);
end;

procedure TFuncoes.GravarSistemaArquivoINI;
var
  oArquivoINI: TIniFile;
  sValor: string;
begin
  oArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI);
  try
    sValor := GetEnumName(TypeInfo(TTipoSistema), integer(FenTipoSistema));
    oArquivoINI.WriteString('Parametros', 'Sistema', sValor);
  finally
    FreeAndNil(oArquivoINI);
  end;
end;

procedure TFuncoes.PegarSistemaPadrao;
var
  oArquivoINI: TIniFile;
  sNomeSistema: string;
begin
  oArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI);
  try
    sNomeSistema := oArquivoINI.ReadString('Parametros', 'Sistema', EmptyStr);

    if sNomeSistema = EmptyStr then
    begin
      FenTipoSistema := tsPG;
      GravarSistemaArquivoINI;
      Exit;
    end;

    FenTipoSistema := TTipoSistema(GetEnumValue(TypeInfo(TTipoSistema), sNomeSistema));
  finally
    FreeAndNil(oArquivoINI);
  end;
end;

function TFuncoes.VerificarArquivoExisteNoDiretorioBin(const psNomeArquivo: string): boolean;
begin
  result := FileExists(Format('%s%s', [PegarDiretorioBin, psNomeArquivo]));

  if not result then
  begin
    MessageDlg(Format(sMENSAGEM_ARQUIVO_NAO_ENCONTRADO, [psNomeArquivo]), mtWarning, [mbOK], 0);
  end;
end;

procedure TFuncoes.ProcessarDataSet(const psNomeDataSet: string);
var
  oFormAguarde: TfAguarde;
begin
  if Trim(psNomeDataSet) = EmptyStr then
  begin
    Exit;
  end;

  oFormAguarde := TfAguarde.Create(nil);
  oFormAguarde.Show;
  Application.ProcessMessages;

  try
    ExcluirArquivo(sPATH_ARQUIVO_DADOS);
    ExcluirArquivo(sPATH_ARQUIVO_FILTRO);
    SalvarFiltroDataSet(psNomeDataSet);
    if SalvarArquivoDataSet(psNomeDataSet) then
    begin
      CarregarArquivoDataSet;
    end;
  finally
    oFormAguarde.Close;
    FreeAndNil(oFormAguarde);
  end;
end;

procedure TFuncoes.SetTipoSistema(Value: TTipoSistema);
begin
  FenTipoSistema := Value;
  GravarSistemaArquivoINI;
end;

procedure TFuncoes.LerStringList(Sender: TObject);
var
  sExpressao: string;
  sResultado: string;
  sTextoSelecionado: string;
  fStringList: TfStringList;
  oThread: IOTAThread;
  oRetorno: TOTAEvaluateResult;
begin
  oThread := FoToolsAPIUtils.PegarThreadAtual;
  if not Assigned(oThread) then
  begin
    Exit;
  end;

  sTextoSelecionado := FoToolsAPIUtils.PegarTextoSelecionado;
  if Trim(sTextoSelecionado) = EmptyStr then
  begin
    Exit;
  end;

  ExcluirArquivo(sPATH_ARQUIVO_LISTA);
  sExpressao := Format('%s.SaveToFile(''%s'')', [sTextoSelecionado, sPATH_ARQUIVO_LISTA]);
  oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);

  if not (oRetorno in [erOK, erDeferred]) then
  begin
    Exit;
  end;

  fStringList := TfStringList.Create(nil);
  try
    fStringList.ShowModal;
  finally
    FreeAndNil(fStringList);
  end;
end;

procedure TFuncoes.ConfigurarAtalhos;
var
  fConfigurarAtalhos: TfConfigurarAtalhos;
begin
  fConfigurarAtalhos := TfConfigurarAtalhos.Create(nil);
  try
    fConfigurarAtalhos.ShowModal;
  finally
    FreeAndNil(fConfigurarAtalhos);
  end;
end;

procedure TFuncoes.ExcluirArquivo(const psNomeArquivo: string);
begin
  if not FileExists(psNomeArquivo) then
  begin
    Exit;
  end;

  if not DeleteFile(PChar(psNomeArquivo)) then
  begin
    MessageDlg('Ocorreu um erro ao excluir os arquivos antigos :(', mtWarning, [mbOK], 0);
    Exit;
  end;

  repeat
    Sleep(150);
  until not FileExists(psNomeArquivo);
end;

procedure TFuncoes.AbrirServidor(Sender: TObject);
var
  sNomeServidor: string;
begin
  case FenTipoSistema of
    tsPG: sNomeServidor := sNOME_SERVIDOR_PG;
    tsSG: sNomeServidor := sNOME_SERVIDOR_SG;
    tsPJ: sNomeServidor := sNOME_SERVIDOR_PJ;
  end;

  if not VerificarArquivoExisteNoDiretorioBin(sNomeServidor) then
  begin
    Exit;
  end;

  FoToolsAPIUtils.AbrirArquivo(PegarDiretorioBin, sNomeServidor);
end;

procedure TFuncoes.AbrirAplicacao(Sender: TObject);
var
  sNomeAplicacao: string;
begin
  case FenTipoSistema of
    tsPG: sNomeAplicacao := sNOME_APLICACAO_PG;
    tsSG: sNomeAplicacao := sNOME_APLICACAO_SG;
    tsPJ: sNomeAplicacao := sNOME_APLICACAO_PJ;
  end;

  if not (VerificarArquivoExisteNoDiretorioBin(sNomeAplicacao)) then
  begin
    Exit;
  end;

  FoToolsAPIUtils.AbrirArquivo(PegarDiretorioBin, sNomeAplicacao);
end;

procedure TFuncoes.AbrirDiretorioBin(Sender: TObject);
begin
  ShellExecute(0, nil, PChar(PegarDiretorioBin), '', '', SW_SHOW);
end;

procedure TFuncoes.AbrirSPCfg(Sender: TObject);
var
  sArquivo: string;
begin
  sArquivo := Format('%s%s', [PegarDiretorioBin, sNOME_ARQUIVO_CONFIG]);
  ShellExecute(0, 'open', PChar(sArquivo), '', '', SW_SHOWNORMAL);
end;

procedure TFuncoes.AbrirItemRTC(Sender: TObject);
var
  sTexto: string;
  sURL: string;
begin
  sTexto := FoToolsAPIUtils.PegarTextoSelecionado;

  if not ValidarTextoSelecionado(sTexto) then
  begin
    sTexto := EmptyStr;
  end;

  if not InputQuery('Digite o item do RTC', 'Item do RTC:', sTexto) then
  begin
    Exit;
  end;

  if Trim(sTexto) = EmptyStr then
  begin
    Exit;
  end;

  sURL := Format(sURL_RTC, [sTexto]);
  ShellExecute(0, 'open', PChar(sURL), '', '', SW_SHOWNORMAL);
end;

procedure TFuncoes.AbrirVisualizaDTS(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_VISUALIZA_DTS, EmptyStr);
end;

procedure TFuncoes.AbrirSPMonitor(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_SP_MONITOR, EmptyStr);
end;

procedure TFuncoes.AbrirSPMonitor3(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_SP_MONITOR3, EmptyStr);
end;

procedure TFuncoes.AbrirSelectSQL(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_SELECT_SQL, EmptyStr);
end;

procedure TFuncoes.AbrirSqlDbx(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_SQLDBX, EmptyStr);
end;

procedure TFuncoes.AbrirWinSpy(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_WINSPY, EmptyStr);
end;

function TFuncoes.ValidarTextoSelecionado(const psTexto: string): boolean;
var
  nNumero: integer;
begin
  result := False;

  if Length(psTexto) > nTAMANHO_MAXIMO_ITEM_RTC then
  begin
    Exit;
  end;

  nNumero := StrToIntDef(psTexto, 0);
  if nNumero = 0 then
  begin
    Exit;
  end;

  result := True;
end;

procedure TFuncoes.ConsultarRansack(Sender: TObject);
var
  sDiretorio: string;
  nPosicaoPastaSRC: integer;
  sParteAposSRC: string;
  sTextoSelecionado: string;
begin
  sTextoSelecionado := FoToolsAPIUtils.PegarTextoSelecionado;
  if Trim(sTextoSelecionado) = EmptyStr then
  begin
    Exit;
  end;

  sDiretorio := FoToolsAPIUtils.PegarDiretorioProjetoAtivo;
  nPosicaoPastaSRC := Pos('src', sDiretorio);

  if nPosicaoPastaSRC <= 0 then
  begin
    Exit;
  end;

  sParteAposSRC := Copy(sDiretorio, nPosicaoPastaSRC, Length(sDiretorio));
  sDiretorio := StringReplace(sDiretorio, sParteAposSRC, 'src', [rfReplaceAll]);
  ShowMessage(Format(sCOMANDO_RANSACK, [sTextoSelecionado, sDiretorio]));
  //Exit;
  FoToolsAPIUtils.AbrirArquivo(Format(sCOMANDO_RANSACK, [sTextoSelecionado, sDiretorio]),
    EmptyStr);
end;

end.

