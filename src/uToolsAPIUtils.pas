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
    function AguardarProcessamentoThread: boolean;
  public
    function SourceEditor(Module: IOTAMOdule): IOTASourceEditor;
    function PegarNomeArquivoAtual: string;
    function PegarThreadAtual: IOTAThread;
    function PegarDiretorioProjetoAtivo: string;
    function PegarTextoSelecionado: string;
    function PegarProjetosCarregados: string;
    function PegarGrupoProjetos: IOTAProjectGroup;
    function ExecutarEvaluate(poThread: IOTAThread; const psExpressao: string;
      var psResultado: string): TOTAEvaluateResult;
    procedure AbrirArquivo(const psDiretorio, psArquivo: string);
    procedure AbrirURL(const psURL: string);
    procedure Aviso(const psMensagem: string);
    procedure CompilarProjeto(const psNomeProjeto: string; const poGrupoProjetos: IOTAProjectGroup;
      const pbEsperarPorOK: boolean = False);
    procedure FinalizarProcesso(const psNomeProcesso: string);
  end;

implementation

uses
  SysUtils, Classes, tlhelp32, Windows, Forms, Dialogs, ShellAPI, uConstantes;

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

{var
  Execucao: TShellExecuteInfo;
begin
  ShowMessage(psDiretorio + psArquivo);
  FillChar(Execucao, SizeOf(Execucao), 0);
  Execucao.cbSize := SizeOf(Execucao);
  Execucao.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  Execucao.lpVerb := PChar('runas');
  Execucao.lpFile := PChar(psArquivo);
  Execucao.lpDirectory := PChar(psDiretorio);
  Execucao.lpParameters := PChar(EmptyStr);
  Execucao.nShow := SW_SHOWNORMAL;
  ShellExecuteEx(@Execucao);
end;}

procedure TToolsAPIUtils.AbrirURL(const psURL: string);
begin
  ShellExecute(0, 'open', PChar(psURL), '', '', SW_SHOWNORMAL);
end;

function TToolsAPIUtils.AguardarProcessamentoThread: boolean;
var
  nTentativas: smallint;
begin
  result := True;
  nTentativas := 0;
  repeat
    begin
      Inc(nTentativas);
      Sleep(500);
      Application.ProcessMessages;
    end;
  until FbProcessado or (FnErroProcessamento <> 0) or (nTentativas = nTENTATIVAS_PROCESSAMENTO);

  if (FnErroProcessamento <> 0) or (nTentativas = nTENTATIVAS_PROCESSAMENTO) then
    result := False;
end;

procedure TToolsAPIUtils.Aviso(const psMensagem: string);
begin
  MessageDlg(psMensagem, mtWarning, [mbOK], 0);
end;

procedure TToolsAPIUtils.CompilarProjeto(const psNomeProjeto: string;
  const poGrupoProjetos: IOTAProjectGroup; const pbEsperarPorOK: boolean = False);
var
  oProjeto: IOTAProject;
  nCont: integer;
  sNomeProjeto: string;
begin
  for nCont := 0 to Pred(poGrupoProjetos.ProjectCount) do
  begin
    oProjeto := poGrupoProjetos.GetProject(nCont);
    sNomeProjeto := ExtractFileName(oProjeto.FileName);
    if Pos(psNomeProjeto, sNomeProjeto) > 0 then
    begin
      if not oProjeto.ProjectBuilder.BuildProject(cmOTAMake, pbEsperarPorOK) then
      begin
        MessageDlg(Format('Erro ao compilar o projeto: %s.', [sNomeProjeto]),
          mtWarning, [mbOK], 0);
        Abort;
      end;

      Break;
    end;
  end;
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
        Aviso('O objeto selecionado está inacessível.');
        result := erError;
        Exit;
      end;

      FnErroProcessamento := 0;
      FsResultadoDeferred := EmptyStr;
      if result = erDeferred then
      begin
        AguardarProcessamentoThread;

        if FnErroProcessamento <> 0 then
        begin
          Aviso('Ocorreu um erro no processamento da Thread.');
          result := erError;
          Exit;
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

procedure TToolsAPIUtils.FinalizarProcesso(const psNomeProcesso: string);
var
  bLoop: BOOL;
  oHandle: THandle;
  oProcesso: TProcessEntry32;
begin
  oHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  oProcesso.dwSize := SizeOf(oProcesso);
  bLoop := Process32First(oHandle, oProcesso);

  while integer(bLoop) <> 0 do
  begin
    if UpperCase(oProcesso.szExeFile) = UpperCase(psNomeProcesso) then
    begin
      TerminateProcess(OpenProcess($0001, BOOL(0), oProcesso.th32ProcessID), 0);
      Break;
    end;

    bLoop := Process32Next(oHandle, oProcesso);
  end;
  CloseHandle(oHandle);
end;

function TToolsAPIUtils.PegarDiretorioProjetoAtivo: string;
var
  oProjeto: IOTAProject;
  oGrupo: IOTAProjectGroup;
  oModuleServices: IOTAModuleServices;
  oModulo: IOTAModule;
  nCont: integer;
begin
  oModuleServices := (BorlandIDEServices as IOTAModuleServices);
  for nCont := 0 to Pred(oModuleServices.ModuleCount) do
  begin
    oModulo := oModuleServices.Modules[nCont];
    if oModulo.QueryInterface(IOTAProjectGroup, oGrupo) = S_OK then
      Break;
  end;

  result := EmptyStr;
  if Assigned(oGrupo) then
  begin
    oProjeto := oGrupo.ActiveProject;
    if Assigned(oProjeto) then
      result := oProjeto.FileName;
  end;
end;

function TToolsAPIUtils.PegarGrupoProjetos: IOTAProjectGroup;
var
  oModuleServices: IOTAModuleServices;
  oModulo: IOTAModule;
  nCont: smallint;
begin
  oModuleServices := BorlandIDEServices as IOTAModuleServices;
  for nCont := 0 to Pred(oModuleServices.ModuleCount) do
  begin
    oModulo := oModuleServices.Modules[nCont];
    if oModulo.QueryInterface(IOTAProjectGroup, result) = S_OK then
      Break;
  end;
end;

function TToolsAPIUtils.PegarNomeArquivoAtual: string;
var
  oEditor: IOTASourceEditor;
  oModulo: IOTAModule;
begin
  result := EmptyStr;

  oModulo := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  oEditor := SourceEditor(oModulo);

  if not Assigned(oEditor) then
    Exit;

  result := oEditor.GetFileName;
end;

function TToolsAPIUtils.PegarProjetosCarregados: string;
var
  oGrupo: IOTAProjectGroup;
  oProjeto: IOTAProject;
  nCont: integer;
  StringListProjetos: TStringList;
  sNomeProjeto: string;
begin
  oGrupo := PegarGrupoProjetos;

  StringListProjetos := TStringList.Create;
  try
    for nCont := 0 to Pred(oGrupo.ProjectCount) do
    begin
      oProjeto := oGrupo.GetProject(nCont);
      sNomeProjeto := ExtractFileName(oProjeto.FileName);

        //jcf:format=off
      if (Pos('PRC', UpperCase(sNomeProjeto)) > 0) or
         (Pos('PG5', UpperCase(sNomeProjeto)) > 0) or
         (Pos('FMP', UpperCase(sNomeProjeto)) > 0) or
         (Pos('SIG', UpperCase(sNomeProjeto)) > 0) or
         (Pos('SAJ', UpperCase(sNomeProjeto)) > 0) then
        //jcf:format=on                                     
        StringListProjetos.Add(sNomeProjeto);
    end;
    result := StringListProjetos.CommaText;
  finally
    FreeAndNil(StringListProjetos);
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
  result := oBloco.Text;
end;

function TToolsAPIUtils.PegarThreadAtual: IOTAThread;
var
  oProcesso: IOTAProcess;
  oServicoDebug: IOTADebuggerServices;
begin
  result := nil;
  try
    oServicoDebug := (BorlandIDEServices as IOTADebuggerServices);
    oProcesso := oServicoDebug.CurrentProcess;

    if not Assigned(oProcesso) then
      Exit;

    result := oProcesso.CurrentThread;
  finally
    FreeAndNil(oProcesso); //PC_OK
    FreeAndNil(oServicoDebug); //PC_OK
  end;
end;

function TToolsAPIUtils.SourceEditor(Module: IOTAMOdule): IOTASourceEditor;
var
  nQtdeArquivos: integer;
  nContador: integer;
begin
  result := nil;

  if not Assigned(Module) then
    Exit;

  nQtdeArquivos := Module.GetModuleFileCount;
  for nContador := 0 to Pred(nQtdeArquivos) do
    if Module.GetModuleFileEditor(nContador).QueryInterface(IOTASourceEditor, result) = S_OK then
      Break;
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

