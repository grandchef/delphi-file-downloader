object Form1: TForm1
  Left = 389
  Top = 314
  Width = 617
  Height = 418
  Caption = 'Download'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 22
    Height = 13
    Caption = 'URL'
  end
  object lvel: TLabel
    Left = 8
    Top = 64
    Width = 56
    Height = 13
    Caption = 'Velocidade:'
  end
  object Label2: TLabel
    Left = 408
    Top = 8
    Width = 45
    Height = 13
    Caption = 'Post data'
  end
  object Label3: TLabel
    Left = 8
    Top = 168
    Width = 30
    Height = 13
    Caption = 'Result'
  end
  object Button1: TButton
    Left = 8
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Iniciar'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 320
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Parar'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit2: TEdit
    Left = 7
    Top = 24
    Width = 385
    Height = 21
    TabOrder = 2
    Text = 'http://localhost/send.php'
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 88
    Width = 385
    Height = 17
    TabOrder = 3
  end
  object Memo1: TMemo
    Left = 408
    Top = 24
    Width = 185
    Height = 129
    Lines.Strings = (
      'Key1=Data1'
      'Key2=Data2')
    TabOrder = 4
  end
  object Memo2: TMemo
    Left = 8
    Top = 184
    Width = 585
    Height = 185
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object FileDownload1: TFileDownload
    Method = httpPOST
    OnStart = FileDownload1Start
    OnProgress = FileDownload1Progress
    OnFinish = FileDownload1Finish
    Left = 88
    Top = 128
  end
  object XPManifest1: TXPManifest
    Left = 128
    Top = 128
  end
end
