unit uExpansorArquivoMVP;

interface

uses
  Windows, Messages, SysUtils, Classes, Dialogs, uConstantes;

type
  TArquivoMVP = class
  private
    FsArquivo: string;
    FenTipoArquivo: TenTipoArquivoMVP;

    function PegarDescricaoDoTipoDeArquivo(const penTipoArquivo: TenTipoArquivoMVP): string;
    function PegarSubDiretorioDoTipoDeArquivo(const penTipoArquivo: TenTipoArquivoMVP): string;
  public
    constructor Create(const psRaizArquivo: string; const penTipoArquivo: TenTipoArquivoMVP);
      overload;
    constructor Create(const psDiretorio, psRaizArquivo: string;
      const penTipoArquivo: TenTipoArquivoMVP); overload;
    property Arquivo: string read FsArquivo;
    property TipoArquivo: TenTipoArquivoMVP read FenTipoArquivo;
  end;

  TExpansorArquivoMVP = class
  private
    FsArquivo: string;
    FsArquivoSemExtensao: string;
    FsRaizArquivoSemExtensao: string;
    FbClasseEhAPI: boolean;
    FenTipoArquivo: TenTipoArquivoMVP;
    FoListaDeArquivosMVP: TList;
    FsDiretorioBase: string;

    function GetClasseEhAPI: boolean;
    function GetClasseEhImpl: boolean;
    function GetListaDeArquivosMVP: TList;
    function GetRaizArquivo: string;
    function GetDiretorioBase: string;
    function RemoverUmSubDiretorio(const psDiretorio: string): string;
    function CriarArquivoMVP(const penTipoArquivo: TenTipoArquivoMVP): TArquivoMVP;
    function PegarInversaoAPIImplDoTipoArquivo(penTipoArquivo: TenTipoArquivoMVP): TenTipoArquivoMVP;
    procedure SetArquivo(const Value: string);
    procedure SetTipoArquivo(const Value: TenTipoArquivoMVP);
    procedure SetDiretorioBase(const Value: string);
  public
    constructor Create(const psArquivo: string);

    function TemEsseTipoNoLista(const penTipoArquivo: TenTipoArquivoMVP): boolean;

    property Arquivo: string read FsArquivo write SetArquivo;
    property RaizArquivo: string read GetRaizArquivo;
    property TipoArquivo: TenTipoArquivoMVP read FenTipoArquivo write SetTipoArquivo;
    property DiretorioBase: string read GetDiretorioBase write SetDiretorioBase;
    property ClasseEhAPI: boolean read GetClasseEhAPI;
    property ClasseEhImpl: boolean read GetClasseEhImpl;
    property ListaDeArquivosMVP: TList read GetListaDeArquivosMVP;
  end;

implementation

function TArquivoMVP.PegarSubDiretorioDoTipoDeArquivo(
  const penTipoArquivo: TenTipoArquivoMVP): string;
begin
  case penTipoArquivo of
    taModel: result := 'impl\model';
    taModelAPI: result := 'api\model';
    taPresenter: result := 'impl\presenter';
    taPresenterAPI: result := 'api\presenter';
    taViewFrame: result := 'impl\view';
    taViewPanel: result := 'impl\view';
    taViewForm: result := 'impl\view';
    taView: result := 'impl\view';
    taViewAPI: result := 'api\view';
    taBuilder: result := 'impl\builder';
    taParamsBuild: result := 'impl\builder';
    taParamsBuildAPI: result := 'api\builder';
  else
    result := EmptyStr;
  end;
end;

function TArquivoMVP.PegarDescricaoDoTipoDeArquivo(
  const penTipoArquivo: TenTipoArquivoMVP): string;
begin
  case penTipoArquivo of
    taModel: result := 'Model';
    taModelAPI: result := 'ModelAPI';
    taPresenter: result := 'Presenter';
    taPresenterAPI: result := 'PresenterAPI';
    taViewFrame: result := 'ViewFrame';
    taViewPanel: result := 'ViewPanel';
    taViewForm: result := 'ViewForm';
    taView: result := 'View';
    taViewAPI: result := 'ViewAPI';
    taBuilder: result := 'Builder';
    taParamsBuild: result := 'ParamsBuild';
    taParamsBuildAPI: result := 'ParamsBuildAPI';
  else
    result := EmptyStr;
  end;
end;

function TExpansorArquivoMVP.PegarInversaoAPIImplDoTipoArquivo(
  penTipoArquivo: TenTipoArquivoMVP): TenTipoArquivoMVP;
begin
  case penTipoArquivo of
    taModel: result := taModelAPI;
    taModelAPI: result := taModel;
    taPresenter: result := taPresenterAPI;
    taPresenterAPI: result := taPresenter;
    taViewFrame: result := taViewAPI;
    taViewPanel: result := taViewAPI;
    taViewForm: result := taViewAPI;
    taView: result := taViewAPI;
    taViewAPI: result := taViewAPI;
    taBuilder: result := taBuilder;
    taParamsBuild: result := taParamsBuildAPI;
    taParamsBuildAPI: result := taParamsBuild;
  else
    result := taNaoMVP;
  end;
end;

{ TArquivoMVP }

constructor TArquivoMVP.Create(const psRaizArquivo: string;
  const penTipoArquivo: TenTipoArquivoMVP);
begin
  FenTipoArquivo := penTipoArquivo;
  FsArquivo := Format('%s%s.pas', [psRaizArquivo, PegarDescricaoDoTipoDeArquivo(FenTipoArquivo)]);
end;

constructor TArquivoMVP.Create(const psDiretorio, psRaizArquivo: string;
  const penTipoArquivo: TenTipoArquivoMVP);
var
  sSubDiretorio: string;
  sArquivo: string;
begin
  FenTipoArquivo := penTipoArquivo;
  sSubDiretorio := PegarSubDiretorioDoTipoDeArquivo(FenTipoArquivo);
  sArquivo := psRaizArquivo + PegarDescricaoDoTipoDeArquivo(FenTipoArquivo);
  FsArquivo := Format('%s\%s\%s.pas', [psDiretorio, sSubDiretorio, sArquivo]);
end;

{ TExpansorArquivoMVP }

constructor TExpansorArquivoMVP.Create(const psArquivo: string);
begin
  FenTipoArquivo := taNaoMVP;
  Arquivo := psArquivo;
end;

function TExpansorArquivoMVP.GetDiretorioBase: string;
begin
  result := FsDiretorioBase;
end;

function TExpansorArquivoMVP.GetClasseEhAPI: boolean;
begin
  result := FbClasseEhAPI;
end;

function TExpansorArquivoMVP.GetClasseEhImpl: boolean;
begin
  result := (not FbClasseEhAPI) and (TipoArquivo <> taNaoMVP);
end;

function TExpansorArquivoMVP.GetListaDeArquivosMVP: TList;
var
  enTipoArquivoInversao: TenTipoArquivoMVP;
begin
  if Assigned(FoListaDeArquivosMVP) then
  begin
    result := FoListaDeArquivosMVP;
    Exit;
  end;

  FoListaDeArquivosMVP := TList.Create; //PC_OK
  result := FoListaDeArquivosMVP;

  if TipoArquivo = taNaoMVP then
  begin
    FoListaDeArquivosMVP.Add(TArquivoMVP.Create(FsRaizArquivoSemExtensao, TipoArquivo)); //PC_OK
    Exit;
  end;

  enTipoArquivoInversao := PegarInversaoAPIImplDoTipoArquivo(TipoArquivo);
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(enTipoArquivoInversao));
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(taModelAPI));
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(taPresenterAPI));
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(taViewAPI));
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(taModel));
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(taPresenter));
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(taViewFrame));
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(taViewPanel));
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(taViewForm));
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(taView));
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(taBuilder));
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(taParamsBuild));
  FoListaDeArquivosMVP.Add(CriarArquivoMVP(taParamsBuildAPI));
end;

function TExpansorArquivoMVP.GetRaizArquivo: string;
begin
  result := FsRaizArquivoSemExtensao;
end;

function TExpansorArquivoMVP.RemoverUmSubDiretorio(const psDiretorio: string): string;
var
  sDiretorio: string;
  nPos: integer;
begin
  result := EmptyStr;

  if psDiretorio = EmptyStr then
    Exit;

  sDiretorio := psDiretorio;
  if sDiretorio[Length(sDiretorio)] = '\' then
    sDiretorio := Copy(sDiretorio, 1, Length(sDiretorio) - 1);

  nPos := Length(sDiretorio);
  while nPos > 0 do
  begin
    if sDiretorio[nPos] = '\' then
      Break;

    Dec(nPos);
  end;

  if nPos > 0 then
    result := Copy(sDiretorio, 1, nPos - 1);
end;

procedure TExpansorArquivoMVP.SetArquivo(const Value: string);
var
  nLengthArquivoSemExtensao: smallint;
begin
  FsArquivo := Value;
  DiretorioBase := ExtractFileDir(FsArquivo);
  FsArquivo := ExtractFileName(FsArquivo);

  FsArquivoSemExtensao := AnsiLowerCase(ChangeFileExt(FsArquivo, EmptyStr));
  FbClasseEhAPI := False;

  nLengthArquivoSemExtensao := Length(FsArquivoSemExtensao);
  FsRaizArquivoSemExtensao := FsArquivoSemExtensao;

  if Pos('api', FsArquivoSemExtensao) > 0 then
  begin
    FsRaizArquivoSemExtensao := Copy(FsArquivoSemExtensao, 1, nLengthArquivoSemExtensao - 3);
    FbClasseEhAPI := True;
  end;

  nLengthArquivoSemExtensao := Length(FsRaizArquivoSemExtensao);

  if Pos('model', FsArquivoSemExtensao) > 0 then
  begin
    FsRaizArquivoSemExtensao := Copy(FsArquivoSemExtensao, 1, nLengthArquivoSemExtensao - 5);
    TipoArquivo := taModel;
  end;

  if Pos('presenter', FsArquivoSemExtensao) > 0 then
  begin
    FsRaizArquivoSemExtensao := Copy(FsArquivoSemExtensao, 1, nLengthArquivoSemExtensao - 9);
    TipoArquivo := taPresenter;
  end;

  if Pos('view', FsArquivoSemExtensao) > 0 then
  begin
    FsRaizArquivoSemExtensao := Copy(FsArquivoSemExtensao, 1, nLengthArquivoSemExtensao - 4);
    TipoArquivo := taView;
  end;

  if Pos('viewframe', FsArquivoSemExtensao) > 0 then
  begin
    FsRaizArquivoSemExtensao := Copy(FsArquivoSemExtensao, 1, nLengthArquivoSemExtensao - 9);
    TipoArquivo := taViewFrame;
  end;

  if Pos('viewpanel', FsArquivoSemExtensao) > 0 then
  begin
    FsRaizArquivoSemExtensao := Copy(FsArquivoSemExtensao, 1, nLengthArquivoSemExtensao - 5);
    TipoArquivo := taViewPanel;
  end;

  if Pos('viewform', FsArquivoSemExtensao) > 0 then
  begin
    FsRaizArquivoSemExtensao := Copy(FsArquivoSemExtensao, 1, nLengthArquivoSemExtensao - 8);
    TipoArquivo := taViewForm;
  end;

  if Pos('builder', FsArquivoSemExtensao) > 0 then
  begin
    FsRaizArquivoSemExtensao := Copy(FsArquivoSemExtensao, 1, nLengthArquivoSemExtensao - 7);
    TipoArquivo := taBuilder;
  end;

  if Pos('paramsbuild', FsArquivoSemExtensao) > 0 then
  begin
    FsRaizArquivoSemExtensao := Copy(FsArquivoSemExtensao, 1, nLengthArquivoSemExtensao - 11);
    TipoArquivo := taParamsBuild;
  end;

  if TipoArquivo = taNaoMVP then
    FsRaizArquivoSemExtensao := FsArquivo;
end;


procedure TExpansorArquivoMVP.SetDiretorioBase(const Value: string);
begin
  FsDiretorioBase := Value;
  FsDiretorioBase := RemoverUmSubDiretorio(FsDiretorioBase);
end;

procedure TExpansorArquivoMVP.SetTipoArquivo(const Value: TenTipoArquivoMVP);
begin
  if FenTipoArquivo = Value then
    Exit;

  FenTipoArquivo := Value;
  if ClasseEhAPI then
    FenTipoArquivo := PegarInversaoAPIImplDoTipoArquivo(FenTipoArquivo);
end;

function TExpansorArquivoMVP.TemEsseTipoNoLista(const penTipoArquivo: TenTipoArquivoMVP): boolean;
var
  nContador: integer;
begin
  result := False;

  for nContador := 0 to Pred(ListaDeArquivosMVP.Count) do
  begin
    if TArquivoMVP(ListaDeArquivosMVP.Items[nContador]).TipoArquivo = penTipoArquivo then
    begin
      result := True;
      Break;
    end;
  end;
end;

function TExpansorArquivoMVP.CriarArquivoMVP(const penTipoArquivo: TenTipoArquivoMVP): TArquivoMVP;
begin
  result := TArquivoMVP.Create(FsDiretorioBase, FsRaizArquivoSemExtensao, penTipoArquivo); //PC_OK
end;

end.

