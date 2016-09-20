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
    function PegarCriterioConsulta(var psTextoPadrao: string): string;
    function SalvarArquivoDataSet(const psNomeDataSet: string): boolean;
    function VerificarArquivoExisteNoDiretorioBin(const psNomeArquivo: string): boolean;
    function ValidarTextoSelecionado(const psTexto: string): boolean;
    function VerificarExisteThreadProcesso: boolean;
    procedure CarregarArquivoDataSet;
    procedure ExcluirArquivo(const psNomeArquivo: string);
    procedure PegarSistemaPadrao;
    procedure SalvarFiltroDataSet(const psNomeDataSet: string);
    procedure SalvarIndicesDataSet(const psNomeDataSet: string);

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
    procedure AbrirDiretorioBin(Sender: TObject);
    procedure AbrirSPCfg(Sender: TObject);
    procedure AbrirItemRTC(Sender: TObject);
    procedure ExcluirCache(Sender: TObject);
    procedure ConsultarDocDelphi(Sender: TObject);
    procedure ConsultarDocSP4(Sender: TObject);
    procedure ConsultarColabore(Sender: TObject);
    procedure ConsultarPadraoCodigo(Sender: TObject);

    // ferramentas externas
    procedure AbrirVisualizaDTS(Sender: TObject);
    procedure AbrirSPMonitor(Sender: TObject);
    procedure AbrirSPMonitor3(Sender: TObject);
    procedure AbrirSelectSQL(Sender: TObject);
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
begin
  ProcessarDataSet(FoToolsAPIUtils.PegarTextoSelecionado);
end;

procedure TFuncoes.CarregarArquivoDataSet;
begin
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
    sExpressao := Format('%s.Filter', [psNomeDataSet]);
    oRetorno := FoToolsAPIUtils.ExecutarEvaluate(oThread, sExpressao, sResultado);
  finally
    FreeAndNil(oThread); //PC_OK
  end;

  if oRetorno = erError then
    Exit;

  if Trim(StringReplace(sResultado, '''', '', [rfReplaceAll])) = EmptyStr then
    Exit;

  slFiltro := TStringList.Create;
  try
    slFiltro.Add(sResultado);
    slFiltro.SaveToFile(sPATH_ARQUIVO_FILTRO);
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
    Exit;

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
begin
  if Trim(psNomeDataSet) = EmptyStr then
    Exit;

  if not VerificarExisteThreadProcesso then
    Exit;

  oFormAguarde := TfAguarde.Create(nil);
  try
    ExcluirArquivo(sPATH_ARQUIVO_DADOS);
    ExcluirArquivo(sPATH_ARQUIVO_FILTRO);
    ExcluirArquivo(sPATH_ARQUIVO_INDICES);

    VerificarDataSetEstaAssigned(psNomeDataSet);
    VerificarDataSetEstaAtivo(psNomeDataSet);
    VerificarDataSetEstaEmNavegacao(psNomeDataSet);

    oFormAguarde.Show;
    Application.ProcessMessages;

    SalvarFiltroDataSet(psNomeDataSet);
    SalvarIndicesDataSet(psNomeDataSet);

    if SalvarArquivoDataSet(psNomeDataSet) then
      CarregarArquivoDataSet;
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
    Exit;

  if not DeleteFile(PChar(psNomeArquivo)) then
  begin
    FoToolsAPIUtils.Aviso('Ocorreu um erro ao manipular os arquivos :(');
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
    tsPJ: sNomeServidor := sNOME_SERVIDOR_PJ;
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
    tsPJ: sNomeAplicacao := sNOME_APLICACAO_PJ;
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

  //sTexto := StringReplace(sTexto, '/', '%2F', [rfReplaceAll]);
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

procedure TFuncoes.ConsultarPadraoCodigo(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirURL(sURL_PADRAO_CODIGO);
end;

procedure TFuncoes.SalvarIndicesDataSet(const psNomeDataSet: string);
var
  oThread: IOTAThread;
  oRetorno: TOTAEvaluateResult;
  sExpressao: string;
  sResultado: string;
  slIndices: TStringList;
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

  slIndices := TStringList.Create;
  try
    slIndices.Add(sResultado);
    slIndices.SaveToFile(sPATH_ARQUIVO_INDICES);
  finally
    FreeAndNil(slIndices);
  end;
end;

end.

