unit ThreadFileDownload;

interface

uses
  Windows, Classes, SysUtils, FileDownUtils, WinInet;

type

  THTTPMethod = (httpGET, httpPOST);
  TDownloadState = (dsDownloaded, dsAlreadyExist, dsError);
  TProgressEvent = procedure(Sender: TObject; ReceivedBytes: Cardinal; CalculatedFileSize: Cardinal) of Object;
  TFinishEvent = procedure(Sender: TObject; State: TDownloadState; Canceled: Boolean) of Object;

  TThreadDownload = class(TThread)
  private
    FURL: String;
    FMethod: THTTPMethod;
    FPostData: TStrings;
    FFileName: String;
    FPartExt: string;
    FStream: TStream;
    FProxy: Boolean;
    FServer: string;
    FPort: Integer;
    FReceivedBytes: Cardinal;
    FCalculatedFileSize: Cardinal;
    FHeadHasContentLength: Boolean;
    FCanceled: Boolean;
    FDownloadState: TDownloadState;
    FOnStart: TNotifyEvent;
    FOnProgress: TProgressEvent;
    FOnFinish: TFinishEvent;
    procedure DoStart;
    procedure DoProgress;
    procedure DoFinish;
    procedure SetPostData(Value: TStrings);
  protected
    procedure Execute; override;
  public
    constructor Create(const URL, FileName, PartExt:string);
    destructor Destroy; override;
    procedure Stop;
    property Proxy: Boolean read FProxy write FProxy;
    property Server: string read FServer write FServer;
    property Port: Integer read FPort write FPort;
    property Method: THTTPMethod read FMethod write FMethod;
    property PostData: TStrings read FPostData write SetPostData;
    property Stream: TStream read FStream write FStream;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnFinish: TFinishEvent read FOnFinish write FOnFinish;
  end;

implementation

procedure TThreadDownload.Execute;
var
  AStream: TStream;
  hOpen, hConnect, hRequest: HINTERNET;
  Verb, Host, Resource, Header, HeaderType: string;
  PostStr: AnsiString;
  HttpPort: Word;
  Buffer: array[0..DATA_SIZE_8KB - 1] of Byte;
  BytesRead, tmp, buflen: Cardinal;
  dwFlags: Cardinal;
  lpPostData: Pointer;
  lpHeaderType: PChar;
  I, TryCount: Integer;
  NoPartFileSize, DataLength, HeaderTypeLength: Cardinal;
  file_nopart, ProxyName: string;
begin
  Synchronize(DoStart);
  FHeadHasContentLength := False;
  if FStream <> nil then
    FReceivedBytes := FStream.Position
  else
    FReceivedBytes := GetFilePos(FFileName);
  ParseURL(FURL, Host, Resource, HttpPort);
  if FMethod = httpGET then
  begin
    lpPostData := nil;
    DataLength := 0;
    lpHeaderType := nil;
    HeaderTypeLength := 0;
    Verb := GET_VERB;
  end
  else
  begin
    PostStr := '';
    if FPostData.Count > 0 then
      PostStr := AnsiString(FPostData.Strings[0]);
    for I := 1 to FPostData.Count - 1 do
      PostStr := PostStr + '&' + AnsiString(FPostData.Strings[I]);
    lpPostData := PAnsiChar(PostStr);
    DataLength := Length(PostStr);
    Verb := POST_VERB;
    HeaderType := 'Content-Type: application/x-www-form-urlencoded';
    lpHeaderType := PChar(HeaderType);
    HeaderTypeLength := Length(HeaderType);
  end;
  TryCount := 0;
  repeat
    if Proxy then
    begin
      ProxyName := Server + ':' + IntToStr(Port);
      hOpen := InternetOpen(nil, INTERNET_OPEN_TYPE_PROXY, PChar(ProxyName), nil, 0);
    end
    else
      hOpen := InternetOpen(nil, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    dwFlags := 0;
    if INTERNET_DEFAULT_HTTPS_PORT = HttpPort then
      dwFlags := INTERNET_FLAG_SECURE;
    hConnect := InternetConnect(hOpen, PChar(host), HttpPort, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
    hRequest := HttpOpenRequest(hConnect, PChar(Verb), PChar(Resource), nil, nil, nil, dwFlags, 0);
    if (FReceivedBytes > 0) then
    begin
      Header := Format(HEADER_STRUCTURE, [FReceivedBytes]);
      HttpAddRequestHeaders(hRequest, PChar(Header), Length(Header), HTTP_ADDREQ_FLAG_ADD_IF_NEW);
    end;
    HttpSendRequest(hRequest, lpHeaderType, HeaderTypeLength, lpPostData, DataLength);
    buflen := SizeOf(Cardinal);
    tmp := 0;
    if not FHeadHasContentLength and (TryCount = 0) then
    begin
      Inc(TryCount);
      FHeadHasContentLength := HttpQueryInfo(hRequest, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @FCalculatedFileSize, buflen, tmp);
      if FHeadHasContentLength then
      begin
        FCalculatedFileSize := FReceivedBytes + FCalculatedFileSize;
        if (FPartExt <> '') and (Pos(FPartExt, FFileName) = Length(FFileName) - Length(FPartExt) + 1) then
        begin
          file_nopart := Copy(FFileName, 1, Length(FFileName) - Length(FPartExt));
          NoPartFileSize := GetFilePos(file_nopart);
          if (NoPartFileSize = FCalculatedFileSize) then
          begin
            FReceivedBytes := FCalculatedFileSize;
            FDownloadState := dsAlreadyExist;
          end;
        end
        else if (FReceivedBytes = FCalculatedFileSize) and (FCalculatedFileSize > 0) then
          FDownloadState := dsDownloaded;
        if (FCalculatedFileSize = FReceivedBytes) and (FCalculatedFileSize > 0) then
        begin
          InternetCloseHandle(hConnect);
          InternetCloseHandle(hOpen);
          Synchronize(DoFinish);
          Exit;
        end;
        if (FCalculatedFileSize = 0)then
        begin
          InternetCloseHandle(hConnect);
          InternetCloseHandle(hOpen);
          FDownloadState := dsError;
          Synchronize(DoFinish);
          Exit;
        end;
      end;
    end;
    if FStream <> nil then
      AStream := FStream
    else
      AStream := TFileStream.Create(FFileName, FileOpenModes[FileExists(FFileName)]);
    try
      AStream.Position := FReceivedBytes;
      repeat
        InternetReadFile(hRequest, @Buffer, SizeOf(Buffer), BytesRead);
        if BytesRead <= 0 then
          Break;
        AStream.Write(Buffer, BytesRead);
        Inc(FReceivedBytes, Integer(BytesRead));
        Synchronize(DoProgress);
      until ((FCanceled) or (FCalculatedFileSize = FReceivedBytes));
    finally
      if FStream = nil then
        AStream.Free;
      InternetCloseHandle(hConnect);
      InternetCloseHandle(hOpen);
    end;
  until ((FCanceled) or (FReceivedBytes >= FCalculatedFileSize) or (FCalculatedFileSize = 0));
  if (FCalculatedFileSize <> 0) or (FReceivedBytes <> 0) then
    FDownloadState := dsDownloaded
  else
    FDownloadState := dsError;
  Synchronize(DoFinish);
end;

constructor TThreadDownload.Create(const URL, FileName, PartExt:string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FURL := URL;
  FMethod := httpGET;
  FPostData := TStringList.Create;
  FFileName := FileName;
  FPartExt := PartExt;
end;

procedure TThreadDownload.Stop;
begin
  FCanceled := True;
end;

procedure TThreadDownload.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

procedure TThreadDownload.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, FReceivedBytes, FCalculatedFileSize);
end;

procedure TThreadDownload.DoFinish;
begin
  if Assigned(FOnFinish) then
    FOnFinish(Self, FDownloadState, FCanceled);
end;

procedure TThreadDownload.SetPostData(Value: TStrings);
begin
  FPostData.Assign(Value);
end;

destructor TThreadDownload.Destroy;
begin
  FPostData.Free;
  inherited;
end;

end.
