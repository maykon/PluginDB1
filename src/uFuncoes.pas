unit uFuncoes;

interface

uses
  ToolsAPI, uAguarde, Classes, Menus, uToolsAPIUtils, uConstantes, uExpansorArquivoMVP, SysUtils;

type
  TFuncoes = class
  private
    FoToolsAPIUtils: TToolsAPIUTils;
    FoExpansorArquivoMVP: TExpansorArquivoMVP;
    FenTipoSistema: TTipoSistema;

    function LerDoArquivoINI(const psSecao, psChave: string): string;
    function PegarNomeSistemaSelecionado: string;
    function PegarDiretorioBin: string;
    function PegarCriterioConsulta(var psTextoPadrao: string): string;
    function PegarNomeParaExportacao(const psNomeDataSet: string): string;
    function SalvarArquivoDataSet(const psNomeDataSet, psNomeArquivo: string): boolean;
    function SalvarFiltroDataSet(const psNomeDataSet: string): string;
    function SalvarIndicesDataSet(const psNomeDataSet: string): string;
    function SalvarClasseDataSet(const psNomeDataSet: string): string;
    function ValidarTextoSelecionado(const psTexto: string): boolean;
    function VerificarArquivoExisteNoDiretorioBin(const psNomeArquivo: string): boolean;
    function VerificarExisteThreadProcesso: boolean;
    procedure AlterarConexaoNoArquivoCfg(const psTipoBanco, psAlias: string;
      const psServer: string = '');
    procedure CarregarArquivoDataSet;
    procedure ExcluirArquivo(const psNomeArquivo: string);
    procedure GravarNoArquivoINI(const psSecao, psChave, psValor: string);
    procedure PegarSistemaPadrao;
    procedure VerificarDataSetEstaAtivo(const psNomeDataSet: string);
    procedure VerificarDataSetEstaAssigned(const psNomeDataSet: string);
    procedure VerificarDataSetEstaEmNavegacao(const psNomeDataSet: string);
    function RetornarSecaoBaseConfigurada: string;
    procedure ConfigurarBaseOracle(const pslBase: TStringList);
    procedure ConfigurarBaseSqlServer(const pslBase: TStringList);
    procedure ConfigurarBaseDB2(const pslBase: TStringList);
    procedure ExecutarNieuport(const psTipoBanco, psAlias, psIp: string);
    function PegarDescricaoSistema: string;
  public
    constructor Create;
    destructor Destroy; override;

    // setters
    procedure SetTipoSistema(Value: TTipoSistema);

    // ferramentas internas
    procedure AbrirServidor(Sender: TObject);
    procedure AbrirAplicacao(Sender: TObject);
    procedure AbrirADM(Sender: TObject);
    procedure AbrirDiretorioBin(Sender: TObject);
    procedure AbrirSPCfg(Sender: TObject);
    procedure ExcluirCache(Sender: TObject);
    procedure ConsultarWorkItem(Sender: TObject);
    procedure ConsultarDocDelphi(Sender: TObject);
    procedure ConsultarDocSP4(Sender: TObject);
    procedure ConsultarColabore(Sender: TObject);
    procedure SelecionarBase(Sender: TObject);
    procedure FinalizarProcessos(Sender: TObject);

    // compilação
    procedure CompilarProjetosClientes(Sender: TObject);
    procedure CompilarProjetosServidores(Sender: TObject);
    procedure CompilarProjetosComponentes(Sender: TObject);
    procedure CompilarTodosProjetos(Sender: TObject);
    procedure CompilacaoPersonalizada(Sender: TObject);
    procedure CompilarProjetosComponentesPG;
    procedure CompilarProjetosComponentesMP;
    procedure CompilarProjetosComponentesSG;
    procedure CompilarProjetosClientesPG;
    procedure CompilarProjetosClientesMP;
    procedure CompilarProjetosClientesSG;
    procedure CompilarProjetosServidoresPG;
    procedure CompilarProjetosServidoresMP;
    procedure CompilarProjetosServidoresSG;
    procedure CompilarTodosProjetosPG;
    procedure CompilarTodosProjetosMP;
    procedure CompilarTodosProjetosSG;
    function PegarProjetosCarregados: string;
    function PegarGrupoProjetos: IOTAProjectGroup;

    // ferramentas externas
    procedure AbrirVisualizaDTS(Sender: TObject);
    procedure AbrirSPMonitor(Sender: TObject);
    procedure AbrirSPMonitor3(Sender: TObject);
    procedure AbrirSelectSQL(Sender: TObject);
    procedure AbrirWinSpy(Sender: TObject);
    procedure ConsultarRansack(Sender: TObject);

    // operações com DataSet, StringList e código-fonte
    procedure ProcessarDataSet(const psNomeDataSet: string);
    procedure VisualizarDataSet(Sender: TObject);
    procedure VisualizarDataSetManual(Sender: TObject);
    procedure LerStringList(Sender: TObject);
    procedure TestarSpSelect(Sender: TObject);
    procedure NaoFormatarCodigo(Sender: TObject);
    procedure ExportarDadosDataSet(Sender: TObject);
    procedure AbrirVisualizadorDataSets(Sender: TObject);
    procedure CheckOut(Sender: TObject);
    procedure RemoverReadOnly(const psNomeArquivo: string);

    // configurações
    procedure AbrirConfiguracoes;

    // MVP
    procedure CriarExpansorArquivoMVP;
    procedure AbrirArquivoMVP(const pnIndiceMenu: integer);
    function TestarPossuiBasesConfiguradas(const pslBases: TStringList): boolean;

    property TipoSistema: TTipoSistema read FenTipoSistema write SetTipoSistema;
  end;

implementation

uses
  Forms, IniFiles, TypInfo, ShellAPI, Windows, Dialogs, uStringList, uConfiguracoes,
  uCompilacao, JcfIdeRegister, ActnList;

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
begin
  ProcessarDataSet(FoToolsAPIUtils.PegarTextoSelecionado);
end;

procedure TFuncoes.CarregarArquivoDataSet;
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_VISUALIZADOR_AUTO, EmptyStr);
end;

function TFuncoes.SalvarArquivoDataSet(const psNomeDataSet, psNomeArquivo: string): boolean;
var
  sExpressao: string;
  sResultado: string;
  oThread: IOTAThread;
  oRetorno: TOTAEvaluateResult;
begin
  oThread := FoToolsAPIUtils.PegarThreadAtual;
  try
    sExpressao := Format('%s.SaveToFile(''%s'')', [psNomeDataSet, psNomeArquivo]);
    oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);
    result := oRetorno in [erOK, erDeferred];
  finally
    FreeAndNil(oThread); //PC_OK
  end;
end;

function TFuncoes.SalvarFiltroDataSet(const psNomeDataSet: string): string;
var
  oThread: IOTAThread;
  oRetorno: TOTAEvaluateResult;
  sExpressao: string;
  sResultado: string;
begin
  oThread := FoToolsAPIUtils.PegarThreadAtual;
  sResultado := EmptyStr;
  try
    sExpressao := Format('%s.Filter', [psNomeDataSet]);
    oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);
  finally
    FreeAndNil(oThread); //PC_OK
  end;

  if oRetorno = erError then
    Exit;

  if Trim(StringReplace(sResultado, '''', '', [rfReplaceAll])) = EmptyStr then
    Exit;

  result := sResultado;
end;

procedure TFuncoes.VisualizarDataSetManual(Sender: TObject);
var
  sNomeDataSet: string;
begin
  sNomeDataSet := LerDoArquivoINI(sSECAO_PARAMETROS, 'UltimaVisualizacaoManual');
  if sNomeDataSet = EmptyStr then
    sNomeDataSet := 'Ex: fpgProcessoParte.esajParte';

  if not InputQuery('Informar o DataSet', 'Informe o nome do DataSet:', sNomeDataSet) then
    Exit;

  GravarNoArquivoINI(sSECAO_PARAMETROS, 'UltimaVisualizacaoManual', sNomeDataSet);
  ProcessarDataSet(sNomeDataSet);
end;

procedure TFuncoes.PegarSistemaPadrao;
var
  oArquivoINI: TIniFile;
  sNomeSistema: string;
begin
  oArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI);
  try
    sNomeSistema := oArquivoINI.ReadString(sSECAO_PARAMETROS, 'Sistema', EmptyStr);

    if Trim(sNomeSistema) = EmptyStr then
    begin
      FenTipoSistema := tsPG;
      GravarNoArquivoINI(sSECAO_PARAMETROS, 'Sistema', PegarNomeSistemaSelecionado);
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
    FoToolsAPIUtils.Aviso(Format(sMENSAGEM_ARQUIVO_NAO_ENCONTRADO, [psNomeArquivo]));
end;

procedure TFuncoes.ProcessarDataSet(const psNomeDataSet: string);
var
  oFormAguarde: TfAguarde;
  slPropriedades: TStringList;
begin
  if Trim(psNomeDataSet) = EmptyStr then
    Exit;

  if not VerificarExisteThreadProcesso then
    Exit;

  oFormAguarde := TfAguarde.Create(nil);
  slPropriedades := TStringList.Create;
  try
    ExcluirArquivo(sPATH_ARQUIVO_DADOS);
    ExcluirArquivo(sPATH_PROP_DATASET);

    VerificarDataSetEstaAssigned(psNomeDataSet);
    VerificarDataSetEstaAtivo(psNomeDataSet);
    VerificarDataSetEstaEmNavegacao(psNomeDataSet);

    oFormAguarde.Show;
    Application.ProcessMessages;

    slPropriedades.Add(SalvarFiltroDataSet(psNomeDataSet));
    slPropriedades.Add(SalvarIndicesDataSet(psNomeDataSet));
    slPropriedades.Add(SalvarClasseDataSet(psNomeDataSet));
    slPropriedades.SaveToFile(sPATH_PROP_DATASET);

    if SalvarArquivoDataSet(psNomeDataSet, sPATH_ARQUIVO_DADOS) then
      CarregarArquivoDataSet;
  finally
    oFormAguarde.Close;
    FreeAndNil(slPropriedades);
    FreeAndNil(oFormAguarde);
  end;
end;

procedure TFuncoes.SetTipoSistema(Value: TTipoSistema);
begin
  FenTipoSistema := Value;
  GravarNoArquivoINI(sSECAO_PARAMETROS, 'Sistema', PegarNomeSistemaSelecionado);
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
  sTextoSelecionado := FoToolsAPIUtils.PegarTextoSelecionado;
  if Trim(sTextoSelecionado) = EmptyStr then
    Exit;

  if not VerificarExisteThreadProcesso then
    Exit;

  oThread := FoToolsAPIUtils.PegarThreadAtual;
  try
    sExpressao := Format('%s.Text', [sTextoSelecionado]);
    oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);
  finally
    FreeAndNil(oThread); //PC_OK
  end;

  if not (oRetorno in [erOK, erDeferred]) then
    Exit;

  fStringList := TfStringList.Create(nil);
  try
    fStringList.TextoLista := sResultado;
    fStringList.ShowModal;
  finally
    FreeAndNil(fStringList);
  end;
end;

procedure TFuncoes.AbrirConfiguracoes;
var
  fConfiguracoes: TfConfiguracoes;
begin
  fConfiguracoes := TfConfiguracoes.Create(nil);
  try
    fConfiguracoes.ShowModal;
  finally
    FreeAndNil(fConfiguracoes);
  end;
end;

procedure TFuncoes.ExcluirArquivo(const psNomeArquivo: string);
begin
  if not FileExists(psNomeArquivo) then
    Exit;

  if not DeleteFile(PChar(psNomeArquivo)) then
  begin
    FoToolsAPIUtils.Aviso('Ocorreu um erro no depurador do Delphi.');
    Abort;
  end;
end;

procedure TFuncoes.AbrirServidor(Sender: TObject);
var
  sNomeServidor: string;
begin
  case FenTipoSistema of
    tsPG: sNomeServidor := sNOME_SERVIDOR_PG;
    tsSG: sNomeServidor := sNOME_SERVIDOR_SG;
    tsMP: sNomeServidor := sNOME_SERVIDOR_MP;
  end;

  if not VerificarArquivoExisteNoDiretorioBin(sNomeServidor) then
    Exit;

  FoToolsAPIUtils.AbrirArquivo(PegarDiretorioBin, sNomeServidor);
end;

procedure TFuncoes.AbrirAplicacao(Sender: TObject);
var
  sNomeAplicacao: string;
begin
  case FenTipoSistema of
    tsPG: sNomeAplicacao := sNOME_APLICACAO_PG;
    tsSG: sNomeAplicacao := sNOME_APLICACAO_SG;
    tsMP: sNomeAplicacao := sNOME_APLICACAO_MP;
  end;

  if not VerificarArquivoExisteNoDiretorioBin(sNomeAplicacao) then
    Exit;

  FoToolsAPIUtils.AbrirArquivo(PegarDiretorioBin, sNomeAplicacao);
end;

procedure TFuncoes.AbrirDiretorioBin(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'explore', PChar(PegarDiretorioBin), nil, nil, SW_SHOW);
end;

procedure TFuncoes.AbrirSPCfg(Sender: TObject);
var
  sArquivo: string;
begin
  sArquivo := Format('%s%s', [PegarDiretorioBin, sNOME_ARQUIVO_CONFIG]);
  ShellExecute(0, 'open', PChar(sArquivo), '', '', SW_SHOWNORMAL);
end;

procedure TFuncoes.ConsultarWorkItem(Sender: TObject);
var
  sTexto: string;
  sURL: string;
begin
  sTexto := FoToolsAPIUtils.PegarTextoSelecionado;

  if not ValidarTextoSelecionado(sTexto) then
    sTexto := EmptyStr;

  if not InputQuery('Digite o item do RTC', 'Item do RTC:', sTexto) then
    Exit;

  if Trim(sTexto) = EmptyStr then
    Exit;

  if Pos('/', sTexto) > 0 then
    sURL := Format(sURL_SALT_RTC, [sTexto])
  else
    sURL := Format(sURL_ITEM_RTC, [sTexto]);

  FoToolsAPIUtils.AbrirURL(sURL);
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

procedure TFuncoes.AbrirWinSpy(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_WINSPY, EmptyStr);
end;

function TFuncoes.ValidarTextoSelecionado(const psTexto: string): boolean;
begin
  result := Length(Trim(psTexto)) <= nTAMANHO_MAXIMO_ITEM_RTC;
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
    Exit;

  sDiretorio := FoToolsAPIUtils.PegarDiretorioProjetoAtivo;
  nPosicaoPastaSRC := Pos('src', sDiretorio);

  if nPosicaoPastaSRC <= 0 then
    Exit;

  sParteAposSRC := Copy(sDiretorio, nPosicaoPastaSRC, Length(sDiretorio));
  sDiretorio := StringReplace(sDiretorio, sParteAposSRC, 'src', [rfReplaceAll]);
  FoToolsAPIUtils.AbrirArquivo(Format(sCOMANDO_RANSACK, [sTextoSelecionado, sDiretorio]),
    EmptyStr);
end;

procedure TFuncoes.VerificarDataSetEstaAssigned(const psNomeDataSet: string);
var
  sExpressao: string;
  sResultado: string;
  oThread: IOTAThread;
  oRetorno: TOTAEvaluateResult;
begin
  oThread := FoToolsAPIUtils.PegarThreadAtual;
  try
    sExpressao := Format('Assigned(%s)', [psNomeDataSet]);
    oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);

    if not (oRetorno in [erOK, erDeferred]) then
      Abort;

    if sResultado <> 'True' then
    begin
      FoToolsAPIUtils.Aviso('O DataSet está nil.');
      Abort;
    end;
  finally
    FreeAndNil(oThread); //PC_OK
  end;
end;

procedure TFuncoes.VerificarDataSetEstaAtivo(const psNomeDataSet: string);
var
  sExpressao: string;
  sResultado: string;
  oThread: IOTAThread;
  oRetorno: TOTAEvaluateResult;
begin
  oThread := FoToolsAPIUtils.PegarThreadAtual;
  try
    sExpressao := Format('%s.State', [psNomeDataSet]);
    oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);

    if (oRetorno <> erOK) or (sResultado = 'dsInactive') then
    begin
      FoToolsAPIUtils.Aviso('O DataSet não está ativo.');
      Abort;
    end;
  finally
    FreeAndNil(oThread); //PC_OK
  end;
end;

function TFuncoes.VerificarExisteThreadProcesso: boolean;
var
  oThread: IOTAThread;
begin
  oThread := FoToolsAPIUtils.PegarThreadAtual;
  result := Assigned(oThread);
end;

procedure TFuncoes.ExcluirCache(Sender: TObject);
var
  sDiretorioBin: string;

  procedure ExcluirDiretorio(const psNomeDiretorio: string);
  var
    sDiretorioCache: string;
    sComando: string;
  begin
    sDiretorioCache := Format('%s%s', [sDiretorioBin, psNomeDiretorio]);
    sComando := Format('%s%s', [sCOMANDO_RMDIR, sDiretorioCache]);
    WinExec(PAnsiChar(ansistring(sComando)), 0);
  end;

begin
  sDiretorioBin := PegarDiretorioBin;

  ExcluirDiretorio('Cache');
  ExcluirDiretorio('cfgs');
  ExcluirDiretorio('cfgs_srv');
  ExcluirDiretorio('cfgs_usr');
end;

procedure TFuncoes.ConsultarDocDelphi(Sender: TObject);
var
  sTexto: string;
  sURL: string;
begin
  sTexto := FoToolsAPIUtils.PegarTextoSelecionado;
  PegarCriterioConsulta(sTexto);

  if Trim(sTexto) = EmptyStr then
    Exit;

  sURL := Format(sURL_DOCUMENTACAO_DELPHI, [sTexto]);
  FoToolsAPIUtils.AbrirURL(sURL);
end;

procedure TFuncoes.ConsultarDocSP4(Sender: TObject);
var
  sTexto: string;
  sURL: string;
begin
  sTexto := FoToolsAPIUtils.PegarTextoSelecionado;
  PegarCriterioConsulta(sTexto);

  if Trim(sTexto) = EmptyStr then
    Exit;

  sURL := Format(sURL_DOCUMENTACAO_SP4, [sTexto]);
  FoToolsAPIUtils.AbrirURL(sURL);
end;

procedure TFuncoes.VerificarDataSetEstaEmNavegacao(const psNomeDataSet: string);
var
  sExpressao: string;
  sResultado: string;
  oThread: IOTAThread;
  oRetorno: TOTAEvaluateResult;
  bEstaEmModoInsercao: boolean;
  bEstaEmModoAlteracao: boolean;
begin
  oThread := FoToolsAPIUtils.PegarThreadAtual;
  try
    sExpressao := Format('%s.State', [psNomeDataSet]);
    oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);

    bEstaEmModoInsercao := sResultado = 'dsInsert';
    bEstaEmModoAlteracao := sResultado = 'dsEdit';

    if (oRetorno <> erOK) or (bEstaEmModoInsercao) or (bEstaEmModoAlteracao) then
    begin
      FoToolsAPIUtils.Aviso('O DataSet está em modo de inserção/alteração.');
      Abort;
    end;
  finally
    FreeAndNil(oThread); //PC_OK
  end;
end;

procedure TFuncoes.ConsultarColabore(Sender: TObject);
var
  sTexto: string;
  sURL: string;
begin
  sTexto := FoToolsAPIUtils.PegarTextoSelecionado;
  PegarCriterioConsulta(sTexto);

  if Trim(sTexto) = EmptyStr then
    Exit;

  sURL := Format(sURL_COLABORE, [sTexto]);
  FoToolsAPIUtils.AbrirURL(sURL);
end;

function TFuncoes.PegarCriterioConsulta(var psTextoPadrao: string): string;
begin
  if not InputQuery('Digite o critério de consulta', 'Critério de consulta:', psTextoPadrao) then
    psTextoPadrao := EmptyStr;
end;

function TFuncoes.SalvarIndicesDataSet(const psNomeDataSet: string): string;
var
  oThread: IOTAThread;
  oRetorno: TOTAEvaluateResult;
  sExpressao: string;
  sResultado: string;
begin
  oThread := FoToolsAPIUtils.PegarThreadAtual;
  sResultado := EmptyStr;
  try
    sExpressao := Format('%s.IndexFieldNames', [psNomeDataSet]);
    oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);
  finally
    FreeAndNil(oThread); //PC_OK
  end;

  if oRetorno = erError then
    Exit;

  if Trim(StringReplace(sResultado, '''', '', [rfReplaceAll])) = EmptyStr then
    Exit;

  result := sResultado;
end;

procedure TFuncoes.AbrirArquivoMVP(const pnIndiceMenu: integer);
var
  sArquivo: string;
begin
  sArquivo := TArquivoMVP(FoExpansorArquivoMVP.ListaDeArquivosMVP[pnIndiceMenu]).Arquivo;
  (BorlandIDEServices as IOTAActionServices).OpenFile(sArquivo);
end;

procedure TFuncoes.CriarExpansorArquivoMVP;
begin
  FreeAndNil(FoExpansorArquivoMVP); //PC_OK

  FoExpansorArquivoMVP := TExpansorArquivoMVP.Create(FoToolsAPIUtils.PegarNomeArquivoAtual); //PC_OK
end;

procedure TFuncoes.NaoFormatarCodigo(Sender: TObject);
begin
  TMenuEditFormatar.MeuCliqueMarcarBloco(nil);
end;

procedure TFuncoes.CompilarProjetosClientes(Sender: TObject);
begin
  case FenTipoSistema of
    tsPG: CompilarProjetosClientesPG;
    tsMP: CompilarProjetosClientesMP;
    tsSG: CompilarProjetosClientesSG;
  end;
end;

procedure TFuncoes.CompilarProjetosServidores(Sender: TObject);
begin
  case FenTipoSistema of
    tsPG: CompilarProjetosServidoresPG;
    tsMP: CompilarProjetosServidoresMP;
    tsSG: CompilarProjetosServidoresSG;
  end;
end;

procedure TFuncoes.CompilarProjetosComponentes(Sender: TObject);
begin
  case FenTipoSistema of
    tsPG: CompilarProjetosComponentesPG;
    tsMP: CompilarProjetosComponentesMP;
    tsSG: CompilarProjetosComponentesSG;
  end;
end;

procedure TFuncoes.CompilarTodosProjetos(Sender: TObject);
begin
  case FenTipoSistema of
    tsPG: CompilarTodosProjetosPG;
    tsMP: CompilarTodosProjetosMP;
    tsSG: CompilarTodosProjetosSG;
  end;
end;

procedure TFuncoes.AlterarConexaoNoArquivoCfg(const psTipoBanco, psAlias, psServer: string);
var
  oArquivoINI: TIniFile;
begin
  oArquivoINI := TIniFile.Create(Format('%s%s', [PegarDiretorioBin, sNOME_ARQUIVO_CONFIG]));
  try
    oArquivoINI.WriteString('Database', 'TipoBanco', psTipoBanco);
    oArquivoINI.WriteString('Database', 'Alias', psAlias);
    if psServer <> EmptyStr then
      oArquivoINI.WriteString('Database', 'Server', psServer)
    else
      oArquivoINI.DeleteKey('Database', 'Server');
  finally
    FreeAndNil(oArquivoINI);
  end;

  if MessageDlg('Executar o servidor?', mtConfirmation, [mbYes, mbNo], 0) = idYes then
    AbrirServidor(nil);
end;

procedure TFuncoes.FinalizarProcessos(Sender: TObject);
var
  sNomeServidor: string;
  sNomeAplicacao: string;
begin
  case FenTipoSistema of
    tsPG:
    begin
      sNomeServidor := sNOME_SERVIDOR_PG;
      sNomeAplicacao := sNOME_APLICACAO_PG;
    end;

    tsSG:
    begin
      sNomeServidor := sNOME_SERVIDOR_SG;
      sNomeAplicacao := sNOME_APLICACAO_SG;
    end;

    tsMP:
    begin
      sNomeServidor := sNOME_SERVIDOR_MP;
      sNomeAplicacao := sNOME_APLICACAO_MP;
    end;
  end;

  FoToolsAPIUtils.FinalizarProcesso(sNomeServidor);
  FoToolsAPIUtils.FinalizarProcesso(sNomeAplicacao);
end;

function TFuncoes.SalvarClasseDataSet(const psNomeDataSet: string): string;
var
  oThread: IOTAThread;
  oRetorno: TOTAEvaluateResult;
  sExpressao: string;
  sResultado: string;
begin
  oThread := FoToolsAPIUtils.PegarThreadAtual;
  sResultado := EmptyStr;
  try
    sExpressao := Format('%s.ClassName', [psNomeDataSet]);
    oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);
  finally
    FreeAndNil(oThread); //PC_OK
  end;

  if oRetorno = erError then
    Exit;

  if Trim(StringReplace(sResultado, '''', '', [rfReplaceAll])) = EmptyStr then
    Exit;

  result := sResultado;
end;

procedure TFuncoes.TestarSpSelect(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_TESTE_SPSELECT, EmptyStr);
end;

procedure TFuncoes.GravarNoArquivoINI(const psSecao, psChave, psValor: string);
var
  oArquivoINI: TIniFile;
begin
  oArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI);
  try
    oArquivoINI.WriteString(psSecao, psChave, psValor);
  finally
    FreeAndNil(oArquivoINI);
  end;
end;

function TFuncoes.LerDoArquivoINI(const psSecao, psChave: string): string;
var
  oArquivoINI: TIniFile;
begin
  oArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI);
  try
    result := oArquivoINI.ReadString(psSecao, psChave, EmptyStr);
  finally
    FreeAndNil(oArquivoINI);
  end;
end;

procedure TFuncoes.ExportarDadosDataSet(Sender: TObject);
var
  oFormAguarde: TfAguarde;
  sNomeDataSet: string;
  sNomeArquivo: string;
begin
  sNomeDataSet := FoToolsAPIUtils.PegarTextoSelecionado;

  if Trim(sNomeDataSet) = EmptyStr then
    Exit;

  sNomeArquivo := PegarNomeParaExportacao(sNomeDataSet);

  if Trim(sNomeArquivo) = EmptyStr then
    Exit;

  oFormAguarde := TfAguarde.Create(nil);
  try
    if not VerificarExisteThreadProcesso then
      Exit;

    VerificarDataSetEstaAssigned(sNomeDataSet);
    VerificarDataSetEstaAtivo(sNomeDataSet);
    VerificarDataSetEstaEmNavegacao(sNomeDataSet);

    oFormAguarde.Show;
    Application.ProcessMessages;

    if SalvarArquivoDataSet(sNomeDataSet, sNomeArquivo) then
    begin
      oFormAguarde.Close;
      MessageDlg('Arquivo gerado com sucesso.', mtInformation, [mbOK], 0);

      //jcf:format=off
      GravarNoArquivoINI(sSECAO_PARAMETROS, 'UltimoNomeExportacao', ExtractFileName(sNomeArquivo));
      GravarNoArquivoINI(sSECAO_PARAMETROS, 'UltimoCaminhoExportacao', ExtractFilePath(sNomeArquivo));
      //jcf:format=on
    end;
  finally
    FreeAndNil(oFormAguarde);
  end;
end;

procedure TFuncoes.AbrirVisualizadorDataSets(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_VISUALIZADOR, EmptyStr);
end;

function TFuncoes.PegarProjetosCarregados: string;
begin
  result := FoToolsAPIUtils.PegarProjetosCarregados;
end;

function TFuncoes.PegarGrupoProjetos: IOTAProjectGroup;
begin
  result := FoToolsAPIUtils.PegarGrupoProjetos;
end;

procedure TFuncoes.CompilacaoPersonalizada(Sender: TObject);
var
  fCompilacao: TfCompilacao;
  slProjetos: TStringList;
  oGrupoProjetos: IOTAProjectGroup;
  nContador: byte;
  sUltimoProjetoSelecionado: string;
  bEsperarPorOK: boolean;
begin
  fCompilacao := TfCompilacao.Create(nil);
  slProjetos := TStringList.Create;
  try
    fCompilacao.Funcoes := Self;
    fCompilacao.ShowModal;

    if fCompilacao.ModalResult = idCancel then
      Exit;

    slProjetos.CommaText := fCompilacao.PegarProjetosSelecionados;
    sUltimoProjetoSelecionado := fCompilacao.PegarUltimoProjetoMarcado;

    if slProjetos.Count = 0 then
      Exit;

    oGrupoProjetos := PegarGrupoProjetos;
    for nContador := 0 to Pred(slProjetos.Count) do
    begin
      bEsperarPorOK := slProjetos[nContador] = sUltimoProjetoSelecionado;
      FoToolsAPIUtils.CompilarProjeto(slProjetos[nContador], oGrupoProjetos, bEsperarPorOK);
    end;
  finally
    FreeAndNil(slProjetos);
    FreeAndNil(fCompilacao);
  end;
end;

procedure TFuncoes.AbrirADM(Sender: TObject);
var
  sNomeADM: string;
begin
  case FenTipoSistema of
    tsPG: sNomeADM := sNOME_ADM_PG;
    tsSG: sNomeADM := sNOME_ADM_SG;
    tsMP: sNomeADM := sNOME_ADM_MP;
  end;

  if not VerificarArquivoExisteNoDiretorioBin(sNomeADM) then
    Exit;

  FoToolsAPIUtils.AbrirArquivo(PegarDiretorioBin, sNomeADM);
end;

procedure TFuncoes.CheckOut(Sender: TObject);
var
  sNomeArquivoAtual: string;
  slLinhaComando: TStringList;
begin
  sNomeArquivoAtual := FoToolsAPIUtils.PegarNomeArquivoAtual;

  if MessageDlg(Format('Confirma o checkout do arquivo "%s"?', [sNomeArquivoAtual]),
    mtConfirmation, [mbYes, mbNo], 0) = idNo then
    Exit;

  slLinhaComando := TStringList.Create;
  try
    slLinhaComando.Add(Format(sCOMANDO_TFS_CHECKOUT, [sNomeArquivoAtual]));
    slLinhaComando.SaveToFile('C:\PluginDB1\Checkout.txt');
    WinExec(PAnsiChar(ansistring(slLinhaComando[0])), 0);
    RemoverReadOnly(sNomeArquivoAtual);
  finally
    FreeAndNil(slLinhaComando);
  end;
end;

procedure TFuncoes.CompilarProjetosComponentesPG;
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('pg5D5Completo', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarProjetosComponentesMP;
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('fmpCompleto', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('fmpCompletoDT', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarProjetosComponentesSG;
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('SajSG5cComponentes', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('SajSG5cComponentesDT', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('SajSG5Componentes', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('SajSG5ComponentesDT', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarProjetosClientesMP;
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('fmpCompleto', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('fmpCompletoDT', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('sigapp', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarProjetosClientesSG;
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('SajSG5APP', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarProjetosClientesPG;
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('prcImpl', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('prcCliente', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('pg5D5Completo', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('SAJPG5app', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarProjetosServidoresMP;
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('fmpCompleto', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('fmpCompletoDT', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('sigServidor', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarProjetosServidoresSG;
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('sg5cServidor', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('sg5Servidor', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarProjetosServidoresPG;
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('prcServidor', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('pg5Servidor', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarTodosProjetosMP;
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('fmpCompleto', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('fmpCompletoDT', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('sigServidor', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('sigapp', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarTodosProjetosSG;
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('SajSG5cComponentes', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('SajSG5cComponentesDT', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('SajSG5Componentes', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('SajSG5ComponentesDT', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('sg5cServidor', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('sg5Servidor', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('SajSG5APP', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarTodosProjetosPG;
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('prcAPI', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('prcImpl', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('prcDT', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('prcCliente', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('prcServidor', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('pg5D5Completo', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('pg5Servidor', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('SAJPG5app', oGrupoProjetos, True);
end;

procedure TFuncoes.RemoverReadOnly(const psNomeArquivo: string);
var
  oModule: IOTAModule;
  oBuffer: IOTAEditBuffer;
  nContador: integer;
begin
  oModule := (BorlandIDEServices as IOTAModuleServices).FindModule(psNomeArquivo);
  if not Assigned(oModule) then
    Exit;

  for nContador := 0 to Pred(oModule.GetModuleFileCount) do
    if oModule.GetModuleFileEditor(nContador).QueryInterface(IOTAEditBuffer, oBuffer) = S_OK then
      Break;

  if Assigned(oBuffer) then
    oBuffer.SetIsReadOnly(False);
end;

function TFuncoes.PegarNomeSistemaSelecionado: string;
begin
  result := GetEnumName(TypeInfo(TTipoSistema), integer(FenTipoSistema));
end;

function TFuncoes.PegarNomeParaExportacao(const psNomeDataSet: string): string;
var
  oSaveDialog: TSaveDialog;
begin
  result := LerDoArquivoINI(sSECAO_PARAMETROS, 'UltimoNomeExportacao');
  if result = EmptyStr then
    result := psNomeDataSet;

  oSaveDialog := TSaveDialog.Create(nil);
  try
    oSaveDialog.FileName := psNomeDataSet + '.xml';
    oSaveDialog.InitialDir := LerDoArquivoINI(sSECAO_PARAMETROS, 'UltimoCaminhoExportacao');
    oSaveDialog.DefaultExt := 'xml';
    oSaveDialog.Filter := 'Arquivo XML|*.xml';

    if not oSaveDialog.Execute then
    begin
      result := EmptyStr;
      Exit;
    end;

    result := oSaveDialog.FileName;
  finally
    FreeAndNil(oSaveDialog);
  end;
end;

function TFuncoes.TestarPossuiBasesConfiguradas(const pslBases: TStringList): boolean;
var
  oArquivoINI: TIniFile;
  sSessao: string;
begin
  result := False;
  oArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI);
  try
    pslBases.Clear;
    sSessao := RetornarSecaoBaseConfigurada;
    if not oArquivoINI.SectionExists(sSessao) then
      Exit;

    oArquivoINI.ReadSection(sSessao, pslBases);
    result := pslBases.Count > 0;
  finally
    FreeAndNil(oArquivoINI);
  end;
end;

procedure TFuncoes.SelecionarBase(Sender: TObject);
var
  oAction: TAction;
  oArquivoINI: TIniFile;
  sSessao: string;
  slConfig: TStringList;
  sChaveBase: string;
begin
  if not (Sender is TAction) then
    Exit;

  oAction := Sender as TAction;
  oArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI);
  slConfig := TStringList.Create;
  try
    sSessao := RetornarSecaoBaseConfigurada;
    if not oArquivoINI.SectionExists(sSessao) then
      Exit;

    sChaveBase := StringReplace(oAction.Name, sNOME_BASE_MENU, EmptyStr, []);
    slConfig.CommaText := oArquivoINI.ReadString(sSessao, sChaveBase, EmptyStr);
    if slConfig.Count <> 3 then
      Exit;

    ConfigurarBaseOracle(slConfig);
    ConfigurarBaseSqlServer(slConfig);
    ConfigurarBaseDB2(slConfig);
  finally
    FreeAndNil(slConfig);
    FreeAndNil(oArquivoINI);
  end;
end;

function TFuncoes.RetornarSecaoBaseConfigurada: string;
begin
  result := Format('Bases%s', [PegarDescricaoSistema]);
end;

procedure TFuncoes.ConfigurarBaseDB2(const pslBase: TStringList);
begin
  if pslBase.Strings[0] <> sTIPO_BANCO_DB2 then
    Exit;

  ExecutarNieuport(pslBase.Strings[0], pslBase.Strings[1], pslBase.Strings[2]);
  AlterarConexaoNoArquivoCfg(pslBase.Strings[0], pslBase.Strings[1]);
end;

procedure TFuncoes.ConfigurarBaseOracle(const pslBase: TStringList);
begin
  if pslBase.Strings[0] <> sTIPO_BANCO_ORACLE then
    Exit;

  ExecutarNieuport(pslBase.Strings[0], pslBase.Strings[1], pslBase.Strings[2]);
  AlterarConexaoNoArquivoCfg(pslBase.Strings[0], pslBase.Strings[1]);
end;

procedure TFuncoes.ConfigurarBaseSqlServer(const pslBase: TStringList);
begin
  if pslBase.Strings[0] <> sTIPO_BANCO_SQLSERVER then
    Exit;

  AlterarConexaoNoArquivoCfg(pslBase.Strings[0], pslBase.Strings[1], pslBase.Strings[2]);
end;

procedure TFuncoes.ExecutarNieuport(const psTipoBanco, psAlias, psIp: string);
var
  sParams: string;
  sAlias: string;
begin
  sAlias := StringReplace(psAlias, PegarDescricaoSistema, EmptyStr, [rfReplaceAll]);
  sParams := Format('%s %s %s %s NOREPL', [PegarDescricaoSistema, psIp, sAlias, psTipoBanco]);
  ShellExecute(Application.Handle, 'open', PChar('nieuport.bat'), PChar(sParams), nil, SW_SHOW);
end;

function TFuncoes.PegarDescricaoSistema: string;
begin
  result := GetEnumName(TypeInfo(TTipoSistemaDesc), Ord(FenTipoSistema));
end;

end.

