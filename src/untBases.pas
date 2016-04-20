unit untBases;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, DBGrids, DB, Buttons, DBClient;

type
  TForm1 = class(TForm)
    grdDados: TDBGrid;
    Label1: TLabel;
    ClientDataSet: TClientDataSet;
    btnSelecionarBase: TBitBtn;
    DataSource: TDataSource;
    ClientDataSetDescricao: TStringField;
    ClientDataSetIP: TStringField;
    ClientDataSetBaseCliente: TStringField;
    ClientDataSetBasePRO: TStringField;
    ClientDataSetBaseSGC: TStringField;
    procedure ClientDataSetAfterPost(DataSet: TDataSet);
  private
    FsCaminhoPlugin: string;
    FsIP: string;
    FsBaseCliente: string;
    FsBasePRO: string;
    FsBaseSGC: string;

    procedure SalvarBases;
  public
    property IP: string read FsIP;
    property BaseCliente: string read FsBaseCliente;
    property BasePRO: string read FsBasePRO;
    property BaseSGC: string read FsBaseSGC;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.ClientDataSetAfterPost(DataSet: TDataSet);
begin
  SalvarBases;
end;

procedure TForm1.SalvarBases;
begin
  ClientDataSet.MergeChangeLog;
  ClientDataSet.SaveToFile(Format('%s%s', [FsCaminhoPlugin, 'Bases.xml']));
end;

end.

