program TesteSpSelect;

uses
  Forms,
  uTesteSpSelect in 'uTesteSpSelect.pas' {fTesteSpSelect},
  uConstantes in '..\src\uConstantes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Teste de spSelect';
  Application.CreateForm(TfTesteSpSelect, fTesteSpSelect);
  Application.Run;
end.
