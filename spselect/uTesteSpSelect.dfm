object fTesteSpSelect: TfTesteSpSelect
  Left = 484
  Top = 114
  Width = 415
  Height = 404
  Caption = 'Teste de projeções de consulta (IspSelect)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 407
    Height = 373
    ActivePage = TabSheetParametros
    Align = alClient
    TabOrder = 0
    TabStop = False
    TabWidth = 120
    object TabSheetParametros: TTabSheet
      Caption = '&Parâmetros'
      object LabelClasseConjuntoDados: TLabel
        Left = 41
        Top = 10
        Width = 145
        Height = 13
        Caption = 'Classe do Conjunto de Dados:'
      end
      object LabelNomeSpSelect: TLabel
        Left = 41
        Top = 50
        Width = 89
        Height = 13
        Caption = 'Nome do spSelect:'
      end
      object SpeedButtonAdicionar: TSpeedButton
        Left = 344
        Top = 89
        Width = 30
        Height = 30
        Caption = '+'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = SpeedButtonAdicionarClick
      end
      object SpeedButtonRemover: TSpeedButton
        Left = 344
        Top = 122
        Width = 30
        Height = 30
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        OnClick = SpeedButtonRemoverClick
      end
      object LabelStatus: TLabel
        Left = 2
        Top = 328
        Width = 393
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object EditClasseConjuntoDados: TEdit
        Left = 41
        Top = 25
        Width = 300
        Height = 21
        MaxLength = 100
        TabOrder = 0
      end
      object EditNomeSpSelect: TEdit
        Left = 41
        Top = 64
        Width = 300
        Height = 21
        MaxLength = 100
        TabOrder = 1
      end
      object BitBtnExecutarTeste: TBitBtn
        Left = 107
        Top = 288
        Width = 179
        Height = 35
        Caption = 'Executar Teste'
        TabOrder = 2
        OnClick = BitBtnExecutarTesteClick
        Glyph.Data = {
          F6060000424DF606000000000000360000002800000018000000180000000100
          180000000000C0060000120B0000120B00000000000000000000FF00FFA87D78
          B78183B78183B78183B78183B78183B78183B78183B78183B78183B78183B781
          83B78183B78183B78183B78183B78183B78183B78183FF00FFFF00FFFF00FFFF
          00FFFF00FFA97F79F3DDC4F8E3C6C17E53E5BF98F4D9B2F3D7ACF3D4A7F2D2A0
          F0CF9AF0CE98F0CE98F0CE98F0CE98F0CE98F0CE98F1CF98EFCD97B3897BFF00
          FFFF00FFFF00FFFF00FFFF00FFA97F7AF2DEC8F7E3CACB916AA146107D5F4438
          ACD137ADD337ADD337ADD337ADD3EECC97EECC97EECC97EECC97EECC97EFCD97
          EDCB96B3897BFF00FFFF00FFFF00FFFF00FFFF00FFA97F7AF2E0CEF8E7CFABCE
          CD528C999E470D9E501672868543BDE73CBBE638B8E437ADD3A6BFADEECC97EE
          CC97EECC97EFCD97EDCB96B3897BFF00FFFF00FFFF00FFFF00FFFF00FFA97F7A
          F3E3D2F9E9D438B5DC8FE5FA97C7D59F4F1CAC64229758295C96A53DBBE742BF
          E92EADD8EECC97EECC97EECC97EFCD97EDCB96B3897BFF00FFFF00FFFF00FFFF
          00FFFF00FFA97F7BF4E6D8FAECDA42B8DD9CEAFDAEE5F99DD1E39F5931BC8248
          B6753F8863474FABCA36AFD5EECD9AEECC97EECC97EFCD97EDCB96B3897BFF00
          FFFF00FFFF00FFFF00FFFF00FFAA807BF5E9DDFBEFE044B9DEA8F0FFC4EEFCB8
          E8FA9AE0F895654AC8976CD4B28ABB7F56796452EFCE9FEECC99EECC97EFCD97
          EDCB96B3897BFF00FFFF00FFFF00FFFF00FFFF00FFAD837DF5ECE3FBF2E644B9
          DEABF1FFD2F3FFCDF0FEB0E9FC81DEF88E6E59D1A788EEE2CFE6D0BDBE7B58AF
          623DEECC99EFCD97EDCB96B3897BFF00FFFF00FFFF00FFFF00FFFF00FFB2887E
          F7EFE8FCF5EC44BADEABF1FFD4F4FFD7F4FFC0F0FE90E6FC74DEF9877C6FD2A5
          8DFFFFFFFFFBF4DEB597B36238EFCD9AEDCA96B3897BFF00FFFF00FFFF00FFFF
          00FFFF00FFB68B80F8F2EEFDF8F144BADFABF1FFD3F4FFD7F4FFC5F2FF99ECFE
          7DE5FC78E1FA848F89CC9A80F9E8D5F1CFABE7B887C37741A9613DB3897BFF00
          FFFF00FFFF00FFFF00FFFF00FFBA8E82FAF6F4FEFCF845BADFABF1FFD3F4FFD7
          F4FFC5F2FF9AECFE83E9FE82E7FD80E6FD5297AAC08161E9C39FE1A568DF994F
          D58433A9613DA9613DFF00FFFF00FFFF00FFFF00FFBE9283FBF8F7FFFFFE45BA
          E0ABF1FFD3F4FFD7F4FFC5F2FF9AECFE83E9FE85E8FC8CBCC07F6552B66332DB
          A16CDE9E5CDB9D61D9A071CC8F6AA9613DA9613DFF00FFFF00FFFF00FFC29685
          FBF8F7FFFFFF45BBE0ABF1FFD3F4FFD7F4FFC5F2FF9AECFE84E5F8955C3CCC96
          79E7BA8CE2A565DB934AD98F47AE4703AE4703B48B7EFF00FFFF00FFFF00FFFF
          00FFFF00FFC69986FBF8F7FFFFFF45BBE1ABF1FFD3F4FFD7F4FFC5F2FF9AECFE
          83E9FE87D0DB9F765DD4A78AE7B581D68431D1771CCD6E0DAE4703AE4703FF00
          FFFF00FFFF00FFFF00FFFF00FFCA9C88FBF8F7FFFFFF45BCE1B1F4FFDCF8FFDB
          F5FFC5F1FF99ECFE85EAFF89ECFF8FEFFF62868CBE7D5EE2B184D57F25CB6400
          CC6500C85F00AF4404AE4703FF00FFFF00FFFF00FFCEA089FCF9F7FFFFFF41B9
          E080DCF47ECFEB76CBE96EC9E95EC8E854C7E955C9EB6CD9F441B6DDF2DAC4AE
          4703CF9570D99351D5873FD78B40D98D40CD8140AE4703FF00FFFF00FFD2A38A
          FCF9F7FFFFFF34B0DA8CD7EFA7E4F4A8E8F8A2E8FA95E6FA85E3F773DDF45FD1
          EE2DAED9FBEEDBE7DBC9AE4703AE4703AE4703AE4703AE4703AE4703AE4703AE
          4703FF00FFD7A78CFCF9F7FFFFFFB3E1F137ADD381D9F189E3F880E4F97AE3FA
          77E1F866D5F037ADD3A6CBD3B28176AD8076AA7F76AB7F76AB7F76AD8274FF00
          FFFF00FFFF00FFFF00FFFF00FFDAAB8DFCF9F8FFFFFFFFFFFFDBF1F837ADD337
          ADD337ADD337ADD337ADD337ADD3DCEEEEE3CEC6B38176E3B585E5AD6AE9A654
          EFA039B88285FF00FFFF00FFFF00FFFF00FFFF00FFDEAD8EFDFAF8FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFCFFFEF9E5D1CBB38176EF
          C48DF3BB6DF8B450B88285FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFDEAD8E
          FDFAF8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFE5D4D0B38176EFC38CF3BA6CB88285FF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFDEAD8EFFFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFE9DBD9B38176F1C58BB88285FF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFDAA482DAA482DAA482DAA482DAA482DAA482DA
          A482DAA482DAA482DAA482DAA482DAA482DAA482B38176B88285FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      end
      object DBGridParametros: TDBGrid
        Left = 41
        Top = 90
        Width = 299
        Height = 187
        DataSource = dsParametros
        TabOrder = 3
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        OnKeyDown = DBGridParametrosKeyDown
        Columns = <
          item
            Expanded = False
            FieldName = 'Nome'
            Width = 135
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Valor'
            Width = 130
            Visible = True
          end>
      end
    end
    object TabSheetDados: TTabSheet
      Caption = 'Resultado (&Dados)'
      object DBGridDados: TDBGrid
        Left = 0
        Top = 0
        Width = 399
        Height = 334
        Align = alClient
        DataSource = dsDados
        PopupMenu = PopupMenu
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object TabSheetSQL: TTabSheet
      Caption = 'Resultado (&SQL)'
      object MemoSQL: TMemo
        Left = 0
        Top = 0
        Width = 399
        Height = 349
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object cdsParametros: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    Left = 9
    Top = 300
    Data = {
      4D0000009619E0BD0100000018000000020000000000030000004D00044E6F6D
      6501004900000001000557494454480200020064000556616C6F720100490000
      0001000557494454480200020064000000}
    object cdsParametrosNome: TStringField
      FieldName = 'Nome'
      Size = 100
    end
    object cdsParametrosValor: TStringField
      FieldName = 'Valor'
      Size = 100
    end
  end
  object dsParametros: TDataSource
    DataSet = cdsParametros
    Left = 37
    Top = 300
  end
  object cdsDados: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 9
    Top = 328
  end
  object dsDados: TDataSource
    DataSet = cdsDados
    Left = 37
    Top = 328
  end
  object PopupMenu: TPopupMenu
    OnPopup = PopupMenuPopup
    Left = 374
    Top = 328
    object MenuItemSalvarDados: TMenuItem
      Caption = 'Salvar Dados'
      OnClick = MenuItemSalvarDadosClick
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'xml'
    FileName = 'Dados.xml'
    Filter = 'Arquivo XML|*.xml'
    Left = 374
    Top = 300
  end
end
