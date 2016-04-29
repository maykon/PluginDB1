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
    function SalvarArquivoDataSet(const psNomeDataSet: string; poThread: IOTAThread): boolean;
    function VerificarArquivoExisteNoDiretorioBin(const psNomeArquivo: string): boolean;
    procedure CarregarArquivoDataSet;
    procedure ExcluirArquivosTemporarios;
    procedure PegarSistemaPadrao;
    procedure SalvarFiltroDataSet(const psNomeDataSet: string; poThread: IOTAThread);
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

    // operações com DataSet e StringList
    procedure AvaliarDataSet(Sender: TObject);
    procedure ProcessarDataSet(const psNomeDataSet: string);
    procedure VisualizarDataSet(Sender: TObject);
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

procedure TFuncoes.AbrirServidor(Sender: TObject);
var
  sNomeServidor: string;
begin
  case FenTipoSistema of
    tsPG: sNomeServidor := sNOME_SERVIDOR_PG5;
    tsSG: sNomeServidor := sNOME_SERVIDOR_SG5;
    tsPJ: sNomeServidor := sNOME_SERVIDOR_PJ;
  end;

  if not VerificarArquivoExisteNoDiretorioBin(sNomeServidor) then
  begin
    MessageDlg(Format(sMENSAGEM_ARQUIVO_NAO_ENCONTRADO, [sNomeServidor]), mtWarning, [mbOK], 0);
    Exit;
  end;

  FoToolsAPIUtils.AbrirArquivo(PegarDiretorioBin, sNomeServidor);
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

procedure TFuncoes.AbrirSPCfg(Sender: TObject);
var
  sArquivo: string;
begin
  sArquivo := Format('%s%s', [PegarDiretorioBin, sNOME_ARQUIVO_CONFIG]);
  ShellExecute(0, 'open', PChar(sArquivo), '', '', SW_SHOWNORMAL);
end;

procedure TFuncoes.AbrirAplicacao(Sender: TObject);
var
  sNomeAplicacao: string;
begin
  case FenTipoSistema of
    tsPG: sNomeAplicacao := sNOME_APLICACAO_PG5;
    tsSG: sNomeAplicacao := sNOME_APLICACAO_SG5;
    tsPJ: sNomeAplicacao := sNOME_APLICACAO_PJ;
  end;

  if not (VerificarArquivoExisteNoDiretorioBin(sNomeAplicacao)) then
  begin
    MessageDlg(Format(sMENSAGEM_ARQUIVO_NAO_ENCONTRADO, [sNomeAplicacao]), mtWarning, [mbOK], 0);
    Exit;
  end;

  FoToolsAPIUtils.AbrirArquivo(PegarDiretorioBin, sNomeAplicacao);
end;

procedure TFuncoes.VisualizarDataSet(Sender: TObject);
var
  sNomeDataSet: string;
begin
  sNomeDataSet := FoToolsAPIUtils.PegarTextoSelecionado;
  ProcessarDataSet(sNomeDataSet);
end;

procedure TFuncoes.ExcluirArquivosTemporarios;
begin
  DeleteFile(sPATH_ARQUIVO_DADOS);
  repeat
    Sleep(150);
  until not FileExists(sPATH_ARQUIVO_DADOS);

  DeleteFile(sPATH_ARQUIVO_FILTRO);
  repeat
    Sleep(150);
  until not FileExists(sPATH_ARQUIVO_FILTRO);
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

function TFuncoes.SalvarArquivoDataSet(const psNomeDataSet: string; poThread: IOTAThread): boolean;
var
  sExpressao: string;
  sResultado: string;
  oRetorno: TOTAEvaluateResult;
begin
  sExpressao := Format('%s.SaveToFile(''%s'')', [psNomeDataSet, sPATH_ARQUIVO_DADOS]);
  oRetorno := FoToolsAPIUtils.ExecutarEvaluate(poThread, sExpressao, sResultado);
  result := oRetorno in [erOK, erDeferred];
end;

procedure TFuncoes.SalvarFiltroDataSet(const psNomeDataSet: string; poThread: IOTAThread);
var
  sExpressao: string;
  oRetorno: TOTAEvaluateResult;
  sResultado: string;
  slFiltro: TStringList;
begin
  sExpressao := Format('%s.Filter', [psNomeDataSet]);
  oRetorno := FoToolsAPIUtils.ExecutarEvaluate(poThread, sExpressao, sResultado);

  if oRetorno = erError then
  begin
    Exit;
  end;

  if Trim(sResultado) = '''' then
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

procedure TFuncoes.AbrirSPMonitor3(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_SP_MONITOR3, EmptyStr);
end;

procedure TFuncoes.AbrirVisualizaDTS(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_VISUALIZA_DTS, EmptyStr);
end;

procedure TFuncoes.AbrirSqlDbx(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_SQLDBX, EmptyStr);
end;

procedure TFuncoes.AbrirWinSpy(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_WINSPY, EmptyStr);
end;

procedure TFuncoes.AbrirSPMonitor(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_SP_MONITOR, EmptyStr);
end;

procedure TFuncoes.AbrirDiretorioBin(Sender: TObject);
begin
  ShellExecute(0, nil, PChar(PegarDiretorioBin), '', '', SW_SHOW);
end;

procedure TFuncoes.AvaliarDataSet(Sender: TObject);
var
  sNomeDataSet: string;
begin
  sNomeDataSet := InputBox('Informar o DataSet', 'Informe o nome do DataSet:',
    'Ex: fpgProcessoParte.esajParte');
  ProcessarDataSet(sNomeDataSet);
end;

constructor TFuncoes.Create;
begin
  inherited;

  FoToolsAPIUtils := TToolsAPIUtils.Create; //PC_OK
  PegarSistemaPadrao;
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
end;

destructor TFuncoes.Destroy;
begin
  FreeAndNil(FoToolsAPIUtils); //PC_OK

  inherited;
end;

procedure TFuncoes.AbrirSelectSQL(Sender: TObject);
begin
  FoToolsAPIUtils.AbrirArquivo(sPATH_SELECT_SQL, EmptyStr);
end;

procedure TFuncoes.ProcessarDataSet(const psNomeDataSet: string);
var
  oThread: IOTAThread;
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
    oThread := FoToolsAPIUtils.PegarThreadAtual;
    if not Assigned(oThread) then
    begin
      Exit;
    end;

    ExcluirArquivosTemporarios;
    SalvarFiltroDataSet(psNomeDataSet, oThread);
    if SalvarArquivoDataSet(psNomeDataSet, oThread) then
    begin
      CarregarArquivoDataSet;
    end;
  finally
    oFormAguarde.Close;
    FreeAndNil(oThread); //PC_OK
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

procedure TFuncoes.AbrirItemRTC(Sender: TObject);
var
  sItem: string;
  sURL: string;
begin
  sItem := FoToolsAPIUtils.PegarTextoSelecionado;
  sItem := InputBox('Digite o item do RTC', 'Item do RTC:', sItem);

  if Trim(sItem) = EmptyStr then
  begin
    Exit;
  end;

  sURL := Format(sURL_RTC, [sItem]);
  ShellExecute(0, 'open', PChar(sURL), '', '', SW_SHOWNORMAL);
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

end.

