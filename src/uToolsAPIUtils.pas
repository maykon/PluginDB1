unit uToolsAPIUtils;

interface

uses
  ToolsAPI;

type
  TToolsAPIUtils = class
  public
    function PegarThreadAtual: IOTAThread;
    function PegarDiretorioProjetoAtivo: string;
    function PegarTextoSelecionado: string;
    function ExecutarEvaluate(poThread: IOTAThread; const psExpressao: string;
      var psResultado: string): TOTAEvaluateResult;
    procedure AbrirArquivo(const psDiretorio, psArquivo: string);
  end;

implementation

uses
  SysUtils, Windows, Forms;

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

function TToolsAPIUtils.ExecutarEvaluate(poThread: IOTAThread; const psExpressao: string;
  var psResultado: string): TOTAEvaluateResult;
var
  bCanModify: boolean;
  sResultado: array[0..4095] of char;
  nEndereco, nTamanho, nValor: longword;
begin
  result := poThread.Evaluate(psExpressao, @sResultado, Length(sResultado),
    bCanModify, True, '', nEndereco, nTamanho, nValor);
  Application.ProcessMessages;
  psResultado := sResultado;
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

end.

