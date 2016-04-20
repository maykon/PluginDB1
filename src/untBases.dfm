object Form1: TForm1
  Left = 195
  Top = 116
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Lista de Bases'
  ClientHeight = 449
  ClientWidth = 920
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 6
    Width = 72
    Height = 13
    Caption = 'Lista de Bases:'
  end
  object grdDados: TDBGrid
    Left = 11
    Top = 21
    Width = 592
    Height = 120
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Descricao'
        Title.Alignment = taCenter
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'IP'
        Title.Alignment = taCenter
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BaseCliente'
        Title.Alignment = taCenter
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BasePRO'
        Title.Alignment = taCenter
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'BaseSGC'
        Title.Alignment = taCenter
        Width = 100
        Visible = True
      end>
  end
  object btnSelecionarBase: TBitBtn
    Left = 208
    Top = 148
    Width = 165
    Height = 25
    Caption = 'Selecionar Base'
    TabOrder = 1
  end
  object ClientDataSet: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    AfterPost = ClientDataSetAfterPost
    Left = 92
    Top = 312
    Data = {
      A70000009619E0BD010000001800000005000000000003000000A70009446573
      63726963616F0100490000000100055749445448020002001E00024950010049
      0000000100055749445448020002000F000B42617365436C69656E7465010049
      0000000100055749445448020002001400074261736550524F01004900000001
      0005574944544802000200140007426173655347430100490000000100055749
      4454480200020014000000}
    object ClientDataSetDescricao: TStringField
      DisplayLabel = 'Descrição'
      FieldName = 'Descricao'
      Size = 30
    end
    object ClientDataSetIP: TStringField
      FieldName = 'IP'
      Size = 15
    end
    object ClientDataSetBaseCliente: TStringField
      DisplayLabel = 'Base Cliente'
      FieldName = 'BaseCliente'
    end
    object ClientDataSetBasePRO: TStringField
      DisplayLabel = 'Base PRO'
      FieldName = 'BasePRO'
    end
    object ClientDataSetBaseSGC: TStringField
      DisplayLabel = 'Base SGC'
      FieldName = 'BaseSGC'
    end
  end
  object DataSource: TDataSource
    DataSet = ClientDataSet
    Left = 124
    Top = 312
  end
end
