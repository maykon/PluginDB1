library PluginDB1;

uses
  ShareMem,
  ToolsAPI,
  SysUtils,
  Classes,
  uMenu in '..\src\uMenu.pas',
  uAguarde in '..\src\uAguarde.pas' {fAguarde},
  uStringList in '..\src\uStringList.pas' {fStringList},
  uConfigurarAtalhos in '..\src\uConfigurarAtalhos.pas' {fConfigurarAtalhos},
  uFuncoes in '..\src\uFuncoes.pas',
  uConstantes in '..\src\uConstantes.pas',
  uToolsAPIUtils in '..\src\uToolsAPIUtils.pas';

{$R *.RES}

exports
  InitWizard name WizardEntryPoint;

begin
end.
