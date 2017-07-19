unit uFuncoes;

interface

uses
  ToolsAPI, uAguarde, Classes, Menus, uToolsAPIUtils, uExpansorArquivoMVP;

type
  TTipoSistema = (tsPG, tsSG, tsMP);

  TFuncoes = class
  private
    FoToolsAPIUtils: TToolsAPIUTils;
    FoExpansorArquivoMVP: TExpansorArquivoMVP;
    FenTipoSistema: TTipoSistema;

    function PegarDiretorioBin: string;
    function PegarCriterioConsulta(var psTextoPadrao: string): string;
    function SalvarArquivoDataSet(const psNomeDataSet, psNomeArquivo: string): boolean;
    function VerificarArquivoExisteNoDiretorioBin(const psNomeArquivo: string): boolean;
    function ValidarTextoSelecionado(const psTexto: string): boolean;
    function VerificarExisteThreadProcesso: boolean;
    procedure CarregarArquivoDataSet;
    procedure ExcluirArquivo(const psNomeArquivo: string);
    procedure PegarSistemaPadrao;
    function SalvarFiltroDataSet(const psNomeDataSet: string): string;
    function SalvarIndicesDataSet(const psNomeDataSet: string): string;
    function SalvarClasseDataSet(const psNomeDataSet: string): string;
    procedure AlterarConexaoNoArquivoCfg(const psServer: string);
    function LerDoArquivoINI(const psSecao, psChave: string): string;
    procedure GravarNoArquivoINI(const psSecao, psChave, psValor: string);

    procedure VerificarDataSetEstaAtivo(const psNomeDataSet: string);
    procedure VerificarDataSetEstaAssigned(const psNomeDataSet: string);
    procedure VerificarDataSetEstaEmNavegacao(const psNomeDataSet: string);
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
    procedure AbrirItemRTC(Sender: TObject);
    procedure ExcluirCache(Sender: TObject);
    procedure ConsultarDocDelphi(Sender: TObject);
    procedure ConsultarDocSP4(Sender: TObject);
    procedure ConsultarColabore(Sender: TObject);
    procedure SelecionarBase175(Sender: TObject);
    procedure SelecionarBase152(Sender: TObject);
    procedure SelecionarBase202(Sender: TObject);
    procedure FinalizarProcessos(Sender: TObject);

    // compilação
    procedure CompilarProjetosClientes(Sender: TObject);
    procedure CompilarProjetosServidores(Sender: TObject);
    procedure CompilarTodosProjetos(Sender: TObject);
    procedure CompilacaoPersonalizada(Sender: TObject);
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
    procedure GravarSistemaArquivoINI;
    procedure NaoFormatarCodigo(Sender: TObject);
    procedure ExportarDadosDataSet(Sender: TObject);
    procedure AbrirVisualizadorDataSets(Sender: TObject);

    // configurações
    procedure AbrirConfiguracoes;

    // MVP
    procedure CriarExpansorArquivoMVP;
    procedure AbrirArquivoMVP(const pnIndiceMenu: integer);

    property TipoSistema: TTipoSistema read FenTipoSistema write SetTipoSistema;
  end;

implementation

uses
  Forms, IniFiles, TypInfo, SysUtils, ShellAPI, Windows, Dialogs, uConstantes,
  uStringList, uConfiguracoes, uCompilacao, JcfIdeRegister;

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

procedure TFuncoes.GravarSistemaArquivoINI;
var
  oArquivoINI: TIniFile;
  sValor: string;
begin
  oArquivoINI := TIniFile.Create(sPATH_ARQUIVO_INI);
  try
    sValor := GetEnumName(TypeInfo(TTipoSistema), integer(FenTipoSistema));
    oArquivoINI.WriteString(sSECAO_PARAMETROS, 'Sistema', sValor);
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
    sNomeSistema := oArquivoINI.ReadString(sSECAO_PARAMETROS, 'Sistema', EmptyStr);

    if Trim(sNomeSistema) = EmptyStr then
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
  sTextoSelecionado := FoToolsAPIUtils.PegarTextoSelecionado;
  if Trim(sTextoSelecionado) = EmptyStr then
    Exit;

  if not VerificarExisteThreadProcesso then
    Exit;

  oThread := FoToolsAPIUtils.PegarThreadAtual;
  try
    sExpressao := Format('%s.Text', [sTextoSelecionado]);
    oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);

    if not (oRetorno in [erOK, erDeferred]) then
      Exit;

    fStringList := TfStringList.Create(nil);
    try
      fStringList.TextoLista := sResultado;
      fStringList.ShowModal;
    finally
      FreeAndNil(fStringList);
    end;
  finally
    FreeAndNil(oThread); //PC_OK
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

procedure TFuncoes.AbrirItemRTC(Sender: TObject);
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

    if (oRetorno <> erOK) or (sResultado <> 'True') then
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
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('prcImpl', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('prcCliente', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('pg5D5Completo', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('SAJPG5app', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarProjetosServidores(Sender: TObject);
var
  oGrupoProjetos: IOTAProjectGroup;
begin
  oGrupoProjetos := PegarGrupoProjetos;
  FoToolsAPIUtils.CompilarProjeto('prcServidor', oGrupoProjetos);
  FoToolsAPIUtils.CompilarProjeto('pg5Servidor', oGrupoProjetos, True);
end;

procedure TFuncoes.CompilarTodosProjetos(Sender: TObject);
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

procedure TFuncoes.SelecionarBase152(Sender: TObject);
begin
  AlterarConexaoNoArquivoCfg('192.168.226.152\iSAJ01');
end;

procedure TFuncoes.SelecionarBase175(Sender: TObject);
begin
  AlterarConexaoNoArquivoCfg('192.168.225.175\iSAJ01');
end;

procedure TFuncoes.SelecionarBase202(Sender: TObject);
begin
  AlterarConexaoNoArquivoCfg('192.168.225.202\iSAJ01');
end;

procedure TFuncoes.AlterarConexaoNoArquivoCfg(const psServer: string);
var
  oArquivoINI: TIniFile;
begin
  oArquivoINI := TIniFile.Create(Format('%s%s', [PegarDiretorioBin, sNOME_ARQUIVO_CONFIG]));
  try
    oArquivoINI.WriteString('Database', 'Server', psServer);
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
  sNomeAdm: string;
begin
  case FenTipoSistema of
    tsPG:
    begin
      sNomeServidor := sNOME_SERVIDOR_PG;
      sNomeAplicacao := sNOME_APLICACAO_PG;
      sNomeAdm := sNOME_ADM_PG;
    end;

    tsSG:
    begin
      sNomeServidor := sNOME_SERVIDOR_SG;
      sNomeAplicacao := sNOME_APLICACAO_SG;
      sNomeAdm := sNOME_ADM_SG;
    end;

    tsMP:
    begin
      sNomeServidor := sNOME_SERVIDOR_MP;
      sNomeAplicacao := sNOME_APLICACAO_MP;
      sNomeAdm := sNOME_ADM_MP;
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
  oSaveDialog: TSaveDialog;
  oFormAguarde: TfAguarde;
  sNomeDataSet: string;
  sNomeArquivo: string;
begin
  sNomeDataSet := FoToolsAPIUtils.PegarTextoSelecionado;

  if Trim(sNomeDataSet) = EmptyStr then
    Exit;

  sNomeArquivo := LerDoArquivoINI(sSECAO_PARAMETROS, 'UltimoNomeExportacao');
  if sNomeArquivo = EmptyStr then
    sNomeArquivo := sNomeDataSet;

  oSaveDialog := TSaveDialog.Create(nil);
  oFormAguarde := TfAguarde.Create(nil);
  try
    oSaveDialog.FileName := sNomeDataSet + '.xml';
    oSaveDialog.InitialDir := LerDoArquivoINI(sSECAO_PARAMETROS, 'UltimoCaminhoExportacao');
    oSaveDialog.DefaultExt := 'xml';
    oSaveDialog.Filter := 'Arquivo XML|*.xml';

    if not oSaveDialog.Execute then
      Exit;

    if not VerificarExisteThreadProcesso then
      Exit;

    VerificarDataSetEstaAssigned(sNomeDataSet);
    VerificarDataSetEstaAtivo(sNomeDataSet);
    VerificarDataSetEstaEmNavegacao(sNomeDataSet);

    oFormAguarde.Show;
    Application.ProcessMessages;

    if SalvarArquivoDataSet(sNomeDataSet, oSaveDialog.FileName) then
    begin
      oFormAguarde.Close;
      MessageDlg('Arquivo gerado com sucesso.', mtInformation, [mbOK], 0);

      //jcf:format=off
      GravarNoArquivoINI(sSECAO_PARAMETROS, 'UltimoNomeExportacao', ExtractFileName(oSaveDialog.FileName));
      GravarNoArquivoINI(sSECAO_PARAMETROS, 'UltimoCaminhoExportacao', ExtractFilePath(oSaveDialog.FileName));
      //jcf:format=on
    end;
  finally
    FreeAndNil(oFormAguarde);
    FreeAndNil(oSaveDialog);
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

end.

