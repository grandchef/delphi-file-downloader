object Form1: TForm1
  Left = 439
  Top = 310
  Caption = 'Download'
  ClientHeight = 237
  ClientWidth = 403
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 8
    Top = 72
    Width = 20
    Height = 14
    Caption = 'URL'
  end
  object Label2: TLabel
    Left = 8
    Top = 24
    Width = 80
    Height = 14
    Caption = 'nome do arquivo'
  end
  object lvel: TLabel
    Left = 8
    Top = 128
    Width = 56
    Height = 14
    Caption = 'Velocidade:'
  end
  object Label3: TLabel
    Left = 8
    Top = 112
    Width = 46
    Height = 14
    Caption = 'Tamanho:'
  end
  object Button1: TButton
    Left = 8
    Top = 192
    Width = 75
    Height = 25
    Caption = 'iniciar'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 320
    Top = 192
    Width = 75
    Height = 25
    Caption = 'parar'
    Enabled = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 40
    Width = 385
    Height = 22
    TabOrder = 2
  end
  object Edit2: TEdit
    Left = 8
    Top = 88
    Width = 385
    Height = 22
    TabOrder = 3
    OnChange = Edit2Change
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 152
    Width = 385
    Height = 17
    TabOrder = 4
  end
  object FileDownload1: TFileDownload
    OnStart = FileDownload1Start
    OnProgress = FileDownload1Progress
    OnFinish = FileDownload1Finish
    Left = 120
    Top = 176
  end
end
