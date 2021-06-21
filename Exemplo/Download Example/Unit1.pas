unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FileDownload, FileDownUtils, StdCtrls, XPMan, ComCtrls,
  ThreadFileDownload;

const
  KB_SIZE = 1024;
  MB_SIZE = 1048576;
type
  TForm1 = class(TForm)
    FileDownload1: TFileDownload;
    Button1: TButton;
    Button2: TButton;
    XPManifest1: TXPManifest;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lvel: TLabel;
    ProgressBar1: TProgressBar;
    Label3: TLabel;
    procedure FileDownload1Finish(Sender: TObject; State: TDownloadState;
      Canceled: Boolean);
    procedure FileDownload1Progress(Sender: TObject; ReceivedBytes,
      CalculatedFileSize: Cardinal);
    procedure FileDownload1Start(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    InitTime:TTime;
    initBytes: Cardinal;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FileDownload1Finish(Sender: TObject; State: TDownloadState;
  Canceled: Boolean);
begin
  Button1.Enabled := True;
  Button2.Enabled := False;
  if (State <> dsError) and not Canceled then
  begin
    Caption := 'Download - Concluido';
  end
  else if not Canceled then
    Caption := 'Download - Error'
  else
    Caption := 'Download - Canceled';
end;

procedure TForm1.FileDownload1Progress(Sender: TObject; ReceivedBytes,
  CalculatedFileSize: Cardinal);
var
  i:Double;
begin
  i := GetInterval(InitTime);
  if (i > 0) then
  begin
    i := (ReceivedBytes - initBytes)/GetInterval(InitTime);
    if (i < KB_SIZE) then
      lvel.Caption := FormatFloat('Velocidade: 0.00 B/s',i)
    else if (i < MB_SIZE) then
      lvel.Caption := FormatFloat('Velocidade: 0.00 Kb/s',i/KB_SIZE)
    else if (i >= MB_SIZE) then
      lvel.Caption := FormatFloat('Velocidade: 0.00 Mb/s',i/MB_SIZE);
  end;
  if (CalculatedFileSize < KB_SIZE) then
    Label3.Caption := FormatFloat('Tamanho: 0.00 B', CalculatedFileSize)
  else if (CalculatedFileSize < MB_SIZE) then
    Label3.Caption := FormatFloat('Tamanho: 0.00 KB', CalculatedFileSize / 1024)
  else if (CalculatedFileSize >= MB_SIZE) then
    Label3.Caption := FormatFloat('Tamanho: 0.00 MB', CalculatedFileSize / (1024 * 1024));
  InitTime := Now;
  initBytes := ReceivedBytes;
  ProgressBar1.Max := CalculatedFileSize;
  ProgressBar1.Position := ReceivedBytes;
  if CalculatedFileSize > 0 then
    Caption := FormatFloat('0.00 % Concluido',ReceivedBytes*100/CalculatedFileSize);
end;

procedure TForm1.FileDownload1Start(Sender: TObject);
begin
  Caption := 'Download - Iniciou';
  InitTime := Now;
  initBytes := 0;
  Button1.Enabled := False;
  Button2.Enabled := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FileDownload1.FileName := Edit1.Text;
  FileDownload1.URL := Edit2.Text;
  FileDownload1.Start;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FileDownload1.Stop;
end;

end.
