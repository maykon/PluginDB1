unit uMenu;

//jcf:format=off

interface

uses
  ToolsAPI, Classes, Menus, ActnList, Dialogs, Contnrs, ExtCtrls;

type
  TWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier)
  private
    FoTimerAtalhos: TTimer;
    FoMenuDB1: TMenuItem;

    function CriarMenu(const psCaption: string; poEvento: TNotifyEvent;
      const pnVirtualKey: integer = 0): TMenuItem;
    procedure CriarMenuDB1;
    procedure CriarTemporizadorAtalhos;
    procedure AdicionarAcoes;
    procedure AtribuirAtalhos(Sender: TObject);
    procedure MarcarMenu;
    procedure SelecionarSistemaPG(Sender: TObject);
    procedure SelecionarSistemaSG(Sender: TObject);
    procedure SelecionarSistemaPJ(Sender: TObject);
  public
    constructor Create;

    function GetState: TWizardState;
    function GetIDString: string;
    function GetName: string;
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Execute;
    procedure Modified;
  end;

function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc) : boolean; stdcall;

exports
  InitWizard Name WizardEntryPoint;

procedure Register;

implementation

uses
  SysUtils, Forms, Windows, uFuncoes, uConstantes;

var
  FoFuncoes: TFuncoes;
  FActions: TObjectList;
  nIDWizard: integer = 0;

function InitialiseWizard(BIDES: IBorlandIDEServices): TWizard;
begin
  result := TWizard.Create;
  Application.Handle := (BIDES as IOTAServices).GetParentHandle;
end;

function InitWizard(Const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): boolean; stdcall;
begin
  result := BorlandIDEServices <> nil;
  RegisterProc(InitialiseWizard(BorlandIDEServices));
End;

procedure Register;
begin
  nIDWizard := (BorlandIDEServices as IOTAWizardServices).AddWizard(
    InitialiseWizard(BorlandIDEServices));
    
  RegisterPackageWizard(TWizard.Create); //PC_OK
end;

{ TWizard }

function TWizard.CriarMenu(const psCaption: string; poEvento: TNotifyEvent;
  const pnVirtualKey: integer = 0): TMenuItem;
var
  oNTAS: INTAServices;
  oAction: TAction;
  sNome: string;
begin
  oAction := nil;
  oNTAS := (BorlandIDEServices as INTAServices);
  result := TMenuItem.Create(oNTAS.MainMenu); //PC_OK
  sNome := Format('DB1_%d', [FoMenuDB1.Count + 1]);
  
  if Assigned(poEvento) then
  begin
    oAction := TAction.Create(oNTAS.ActionList); //PC_OK
    oAction.ActionList := oNTAS.ActionList;
    oAction.Name := 'act' + sNome;
    oAction.Caption := psCaption;
    oAction.OnExecute := poEvento;
    oAction.Category := 'PluginDB1';

    if pnVirtualKey > 0 then
    begin
      oAction.ShortCut := ShortCut(pnVirtualKey, [ssCtrl]);
      oAction.Tag := ShortCut(pnVirtualKey, [ssCtrl]);
    end;

    FActions.Add(oAction);
  end;

  if psCaption = sSEPARADOR then
    result.Caption := sSEPARADOR
  else
    result.Caption := psCaption;

  result.Action := oAction;
  result.Name := 'im' + sNome;
  FoMenuDB1.Add(result);
end;

constructor TWizard.Create;
begin
  CriarMenuDB1;
  AdicionarAcoes;
  MarcarMenu;
  CriarTemporizadorAtalhos;
end;

function TWizard.GetIDString: string;
begin
  result := 'DB1 Global Software';
end;

function TWizard.GetName: string;
begin
  result := 'Menu de Apoio para Desenvolvedores da DB1';
end;

procedure TWizard.AtribuirAtalhos(Sender: TObject);
var
  nCont: integer;
  oAction: TAction;
begin
  for nCont := 0 to FActions.Count - 1 do
  begin
    oAction := FActions[nCont] as TAction;
    oAction.ShortCut := oAction.Tag;
  end;

  if Assigned(FoTimerAtalhos) then
  begin
    FoTimerAtalhos.Enabled := False;
    FreeAndNil(FoTimerAtalhos); //PC_OK
  end;
end;

procedure TWizard.CriarMenuDB1;
var
  oMainMenu: TMainMenu;
begin
  oMainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  FoMenuDB1 := TMenuItem.Create(oMainMenu); //PC_OK
  FoMenuDB1.Name := 'miDB1';
  FoMenuDB1.Caption := sMENU_DB1;
  FoMenuDB1.OnClick := AtribuirAtalhos;
  oMainMenu.Items.Add(FoMenuDB1);
end;

procedure TWizard.SelecionarSistemaPG(Sender: TObject);
begin
  FoFuncoes.TipoSistema := tsPG;
  MarcarMenu;
end;

procedure TWizard.SelecionarSistemaSG(Sender: TObject);
begin
  FoFuncoes.TipoSistema := tsSG;
  MarcarMenu;
end;

procedure TWizard.SelecionarSistemaPJ(Sender: TObject);
begin
  FoFuncoes.TipoSistema := tsPJ;
  MarcarMenu;
end;

procedure TWizard.MarcarMenu;
var
  nCont: integer;
  oAction: TAction;
  sNomeMenu: string;
begin
  case FoFuncoes.TipoSistema of
    tsPG: sNomeMenu := sNOME_PG5;
    tsSG: sNomeMenu := sNOME_SG5;
    tsPJ: sNomeMenu := sNOME_PJ;
  end;

  for nCont := 0 to FActions.Count - 1 do
  begin
    oAction := FActions[nCont] as TAction;
    oAction.Checked := oAction.Caption = sNomeMenu;
  end;
end;

procedure TWizard.CriarTemporizadorAtalhos;
begin
  FoTimerAtalhos := TTimer.Create(nil); //PC_OK
  FoTimerAtalhos.Interval := 20000;
  FoTimerAtalhos.OnTimer := AtribuirAtalhos;
end;

procedure TWizard.AdicionarAcoes;
begin
  CriarMenu('Abrir Servidor', FoFuncoes.AbrirServidor, VK_NUMPAD1);
  CriarMenu('Abrir Aplicação', FoFuncoes.AbrirAplicacao, VK_NUMPAD2);
  CriarMenu('Abrir Diretório Bin', FoFuncoes.AbrirDiretorioBin);
  CriarMenu('Abrir spCfg.ini', FoFuncoes.AbrirSPCfg, VK_NUMPAD3);
  CriarMenu('Abrir Item no RTC', FoFuncoes.AbrirItemRTC, VK_NUMPAD4);
  CriarMenu(sSEPARADOR, nil);
  CriarMenu('Abrir VisualizaDTS', FoFuncoes.AbrirVisualizaDTS);
  CriarMenu('Abrir spMonitor', FoFuncoes.AbrirSPMonitor);
  CriarMenu('Abrir spMonitor3', FoFuncoes.AbrirSPMonitor3);
  CriarMenu('Abrir SelectSQL', FoFuncoes.AbrirSelectSQL);
  CriarMenu('Abrir SqlDbx', FoFuncoes.AbrirSqlDbx);
  CriarMenu('Abrir WinSpy', FoFuncoes.AbrirWinSpy);
  CriarMenu(sSEPARADOR, nil);
  CriarMenu('Visualizar DataSet', FoFuncoes.VisualizarDataSet, VK_NUMPAD5);
  CriarMenu('Avaliar DataSet', FoFuncoes.AvaliarDataSet);
  CriarMenu('Ler StringList', FoFuncoes.LerStringList);
  CriarMenu(sSEPARADOR, nil);
  CriarMenu(sNOME_PG5, SelecionarSistemaPG);
  CriarMenu(sNOME_SG5, SelecionarSistemaSG);
  CriarMenu(sNOME_PJ, SelecionarSistemaPJ);
end;

function TWizard.GetState: TWizardState;
begin
  result := [];
end;

procedure TWizard.AfterSave;
begin

end;

procedure TWizard.BeforeSave;
begin

end;

procedure TWizard.Destroyed;
begin

end;

procedure TWizard.Execute;
begin

end;

procedure TWizard.Modified;
begin

end;

initialization
  FoFuncoes := TFuncoes.Create; //PC_OK
  FActions := TObjectList.Create(True); //PC_OK

finalization
  FreeAndNil(FoFuncoes);
  FreeAndNil(FActions);

  if nIDWizard > 0 then
  begin
    (BorlandIDEServices as IOTAWizardServices).RemoveWizard(nIDWizard);
  end;

  //jcf:format=on
end.

