unit uToolsAPIUtils;

interface

uses
  ToolsAPI;

type
  TNotificador = class(TInterfacedObject, IOTAThreadNotifier)
  private
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure EvaluteComplete(const ExprStr, ResultStr: string; CanModify: boolean;
      ResultAddress, ResultSize: longword; ReturnCode: integer);
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: integer);
  end;

  TToolsAPIUtils = class
  private
    procedure AguardarProcessamentoThread;
  public
    function PegarThreadAtual: IOTAThread;
    function PegarDiretorioProjetoAtivo: string;
    function PegarTextoSelecionado: string;
    function ExecutarEvaluate(poThread: IOTAThread; const psExpressao: string;
      var psResultado: string): TOTAEvaluateResult;
    procedure AbrirArquivo(const psDiretorio, psArquivo: string);
    procedure AbrirURL(const psURL: string);
    procedure Aviso(const psMensagem: string);
  end;

implementation

uses
  SysUtils, Windows, Forms, Dialogs, ShellAPI, uConstantes;

var
  FbProcessado: boolean;
  FnErroProcessamento: integer;
  FsResultadoDeferred: string;

{ TToolsAPIUtils }

procedure TToolsAPIUtils.AbrirArquivo(const psDiretorio, psArquivo: string);
var
  oInfoProcesso: TProcessInformation;
  oParamsExecucao: TStartupInfo;
  sArquivo: string;
begin
  sArquivo := Format('%s%s', [psDiretorio, psArquivo]);
  FillMemory(@oParamsExecucao, SizeOf(oParamsExecucao), 0);
  oParamsExecucao.cb := SizeOf(oParamsExecucao);

  CreateProcess(nil, PChar(sArquivo), nil, nil, False, NORMAL_PRIORITY_CLASS,
    nil, nil, oParamsExecucao, oInfoProcesso);

  CloseHandle(oInfoProcesso.hProcess);
  CloseHandle(oInfoProcesso.hThread);
end;

procedure TToolsAPIUtils.AbrirURL(const psURL: string);
begin
  ShellExecute(0, 'open', PChar(psURL), '', '', SW_SHOWNORMAL);
end;

procedure TToolsAPIUtils.AguardarProcessamentoThread;
var
  nTentativas: smallint;
begin
  nTentativas := 0;
  repeat
    begin
      Inc(nTentativas);
      Sleep(500);
      Application.ProcessMessages;
    end;
  until FbProcessado or (FnErroProcessamento <> 0) or (nTentativas = nTENTATIVAS_PROCESSAMENTO);

  if (FnErroProcessamento <> 0) or (nTentativas = nTENTATIVAS_PROCESSAMENTO) then
  begin
    Aviso('Ocorreu um erro no processamento da Thread.');
    Abort;
  end;
end;

procedure TToolsAPIUtils.Aviso(const psMensagem: string);
begin
  MessageDlg(psMensagem, mtWarning, [mbOK], 0);
end;

function TToolsAPIUtils.ExecutarEvaluate(poThread: IOTAThread; const psExpressao: string;
  var psResultado: string): TOTAEvaluateResult;
var
  bCanModify: boolean;
  sResultado: array[0..4095] of char;
  nEndereco, nTamanho, nValor: longword;
  oNotificador: TNotificador;
  nIndiceNotificador: integer;
  bVariavelInacessivel: boolean;
begin
  FbProcessado := False;
  result := erOK;

  oNotificador := TNotificador.Create; //PC_OK
  nIndiceNotificador := poThread.AddNotifier(oNotificador);

  try
    while not FbProcessado do
    begin
      result := poThread.Evaluate(psExpressao, @sResultado, Length(sResultado),
        bCanModify, True, '', nEndereco, nTamanho, nValor);
      psResultado := sResultado;

      if result = erOK then
      begin
        FbProcessado := True;
        Break;
      end;

      bVariavelInacessivel := Pos('inacessible', sResultado) > 0;
      if (result = erError) or bVariavelInacessivel then
      begin
        Aviso('O objeto selecionado está inacessível no breakpoint atual.');
        Abort;
      end;

      FnErroProcessamento := 0;
      FsResultadoDeferred := EmptyStr;
      if result = erDeferred then
      begin
        AguardarProcessamentoThread;
        FbProcessado := True;

        if FnErroProcessamento <> 0 then
        begin
          Aviso('Houve um erro ao executar o Evaluate do Delphi.');
          Abort;
        end;

        if Trim(FsResultadoDeferred) <> EmptyStr then
          psResultado := FsResultadoDeferred
        else
          psResultado := sResultado;
      end;
    end;
  finally
    poThread.RemoveNotifier(nIndiceNotificador);
  end;
end;

function TToolsAPIUtils.PegarDiretorioProjetoAtivo: string;
var
  oProjeto: IOTAProject;
  oGrupo: IOTAProjectGroup;
  oModuleServices: IOTAModuleServices;
  oModulo: IOTAModule;
  nCont: integer;
begin
  oModuleServices := BorlandIDEServices as IOTAModuleServices;
  for nCont := 0 to oModuleServices.ModuleCount - 1 do
  begin
    oModulo := oModuleServices.Modules[nCont];
    if oModulo.QueryInterface(IOTAProjectGroup, oGrupo) = S_OK then
    begin
      Break;
    end;
  end;

  result := EmptyStr;
  if Assigned(oGrupo) then
  begin
    oProjeto := oGrupo.ActiveProject;
    if Assigned(oProjeto) then
    begin
      result := oProjeto.FileName;
    end;
  end;
end;

function TToolsAPIUtils.PegarTextoSelecionado: string;
var
  oViewer: IOTAEditView;
  oBloco: IOTAEditBlock;
begin
  result := EmptyStr;
  oViewer := (BorlandIDEServices as IOTAEditorServices).TopView;
  oBloco := oViewer.GetBlock;

  if (oBloco.StartingColumn <> oBloco.EndingColumn) or
    (oBloco.StartingRow <> oBloco.EndingRow) then
  begin
    result := oBloco.Text;
  end;
end;

function TToolsAPIUtils.PegarThreadAtual: IOTAThread;
var
  oProcesso: IOTAProcess;
  oServicoDebug: IOTADebuggerServices;
begin
  result := nil;
  try
    if Supports(BorlandIDEServices, IOTADebuggerServices, oServicoDebug) then
    begin
      oProcesso := oServicoDebug.CurrentProcess;
    end;

    if not Assigned(oProcesso) then
    begin
      Exit;
    end;

    result := oProcesso.CurrentThread;
  finally
    FreeAndNil(oProcesso); //PC_OK
    FreeAndNil(oServicoDebug); //PC_OK
  end;
end;

{ TNotifier }

procedure TNotificador.EvaluteComplete(const ExprStr, ResultStr: string;
  CanModify: boolean; ResultAddress, ResultSize: longword; ReturnCode: integer);
begin
  FbProcessado := True;
  FnErroProcessamento := ReturnCode;
  FsResultadoDeferred := ResultStr;
end;

procedure TNotificador.AfterSave;
begin
end;

procedure TNotificador.BeforeSave;
begin
end;

procedure TNotificador.Destroyed;
begin
end;

procedure TNotificador.Modified;
begin
end;

procedure TNotificador.ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: integer);
begin
end;

procedure TNotificador.ThreadNotify(Reason: TOTANotifyReason);
begin
end;

end.

