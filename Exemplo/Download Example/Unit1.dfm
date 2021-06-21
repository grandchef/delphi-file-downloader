object Form1: TForm1
  Left = 439
  Top = 310
  Width = 419
  Height = 276
  Caption = 'Download'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 72
    Width = 22
    Height = 13
    Caption = 'URL'
  end
  object Label2: TLabel
    Left = 8
    Top = 24
    Width = 79
    Height = 13
    Caption = 'nome do arquivo'
  end
  object lvel: TLabel
    Left = 8
    Top = 128
    Width = 56
    Height = 13
    Caption = 'Velocidade:'
  end
  object Label3: TLabel
    Left = 8
    Top = 112
    Width = 48
    Height = 13
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
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 40
    Width = 385
    Height = 21
    TabOrder = 2
    Text = 'D:\Falcon C++-3.2.0.10-Setup.exe'
  end
  object Edit2: TEdit
    Left = 7
    Top = 88
    Width = 385
    Height = 21
    TabOrder = 3
    Text = 
      'http://sourceforge.net/projects/falconcpp/files/Binaries/3.2.0.1' +
      '0/Falcon C++-3.2.0.10-Setup.exe'
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
    Left = 88
    Top = 192
  end
  object XPManifest1: TXPManifest
    Left = 128
    Top = 192
  end
end
