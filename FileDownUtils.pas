unit FileDownUtils;

interface

uses
  Windows, Classes, SysUtils, WinInet, Controls;

const
  DATA_SIZE_8KB = 8192;
  HEADER_STRUCTURE = 'Range: bytes=%d-';
  GET_VERB = 'GET';
  POST_VERB = 'POST';
  HEAD_VERB = 'HEAD';
  FileOpenModes: array[Boolean] of DWORD = (fmCreate, fmOpenWrite);

function GetInterval(InitTime:TTime):Double;
function GetFilePos(const FileName: String): Cardinal;
function GetFileSize(const URL: String; var Size: Cardinal): Boolean;
procedure ParseURL(const AURL: String; var AHost, AResource: String; var APort: Word);

implementation

function GetInterval(InitTime:TTime):Double;
var
  H, M, S, MS: Word;
begin
  DecodeTime(Now - InitTime, H, M, S, MS);
  S := S+M*60+H*3600;
  Result := S+MS/1000;
end;

function GetFilePos(const FileName: String): Cardinal;
begin
  if FileExists(FileName) then
  with TFileStream.Create(FileName, fmOpenRead) do
  begin
    Result := Size;
    Free;
  end
  else
    Result := 0
end;

function GetFileSize(const URL: String; var Size: Cardinal): Boolean;
var
  hOpen, hConnect, hRequest: HINTERNET;
  host, resource: string;
  buflen, tmp: DWORD;
  HttpPort: Word;
  dwFlags: Cardinal;
begin
  Result := False;
  try
    ParseURL(URL, host, resource, HttpPort);
    hOpen := InternetOpen(nil, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    dwFlags := 0;
    if INTERNET_DEFAULT_HTTPS_PORT = HttpPort then
      dwFlags := INTERNET_FLAG_SECURE;
    hConnect := InternetConnect(hOpen, PChar(host), HttpPort, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
    hRequest := HttpOpenRequest(hConnect, HEAD_VERB, PChar(resource), nil, nil, nil, dwFlags, 0);
    HttpSendRequest(hRequest, nil, 0, nil, 0);
    buflen := SizeOf(Size);
    tmp := 0;
    Size := 0;
    Result := HttpQueryInfo(hRequest, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @Size, buflen, tmp);
    InternetCloseHandle(hConnect);
    InternetCloseHandle(hOpen);
  except
    Size := 0;
  end;
end;

procedure ParseURL(const AURL: String; var AHost, AResource: String; var APort: Word);
var
  UrlComponents: TURLComponents;
  host: array[0..INTERNET_MAX_HOST_NAME_LENGTH - 1] of Char;
  urlpath: array[0..INTERNET_MAX_PATH_LENGTH - 1] of Char;
begin
  ZeroMemory(@UrlComponents, SizeOf(TURLComponents));
  UrlComponents.dwStructSize := SizeOf(TURLComponents);
  UrlComponents.lpszHostName := host;
  UrlComponents.dwHostNameLength := High(host) + 1;
  UrlComponents.lpszUrlPath := urlpath;
  UrlComponents.dwUrlPathLength := High(urlpath) + 1;
  InternetCrackUrl(PChar(AURL), Length(AURL), ICU_DECODE or ICU_ESCAPE, UrlComponents);
  AHost := host;
  AResource := urlpath;
  APort := UrlComponents.nPort;
end;

end.

