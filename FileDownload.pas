unit FileDownload;

interface

uses
  SysUtils, Classes, ThreadFileDownload;

type
  TFileDownload = class(TComponent)
  private
    FURL: String;
    FMethod: THTTPMethod;
    FPostData: TStrings;
    FFileName: String;
    FBusy: Boolean;
    FOnStart: TNotifyEvent;
    FOnProgress: TProgressEvent;
    FOnFinish: TFinishEvent;
    FPartExt: string;
    FProxy: Boolean;
    FServer: string;
    FPort: Integer;
    FThreadDownload: TThreadDownload;
    procedure ThreadOnFinish(Sender: TObject; State: TDownloadState;
      Canceled: Boolean);
    procedure SetPostData(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; overload;
    procedure Start(Stream: TStream); overload;
    procedure Stop;
    procedure StartDownload(URL, FileName: String); overload;
    procedure StartDownload(Stream: TStream; URL, FileName: String); overload;
    function IsBusy:Boolean;
    property Proxy: Boolean read FProxy write FProxy;
    property Server: string read FServer write FServer;
    property Port: Integer read FPort write FPort;
  published
    property URL: String read FURL write FURL;
    property Method: THTTPMethod read FMethod write FMethod default httpGET;
    property PostData: TStrings read FPostData write SetPostData;
    property FileName: String read FFileName write FFileName;
    property PartExt: String read FPartExt write FPartExt;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnFinish: TFinishEvent read FOnFinish write FOnFinish;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('File Download', [TFileDownload]);
end;

destructor TFileDownload.Destroy;
begin
  Stop;
  FPostData.Free;
  inherited Destroy;
end;

procedure TFileDownload.Stop;
begin
  if FBusy then
    FThreadDownload.Stop;
end;

procedure TFileDownload.Start;
begin
  StartDownload(FURL, FFileName);
end;

procedure TFileDownload.Start(Stream: TStream);
begin
  StartDownload(Stream, FURL, FFileName);
end;

procedure TFileDownload.StartDownload(URL, FileName: String);
begin
  if not (FBusy) then
  begin
    FBusy := True;
    FThreadDownload := TThreadDownload.Create(URL, FileName, FPartExt);
    with FThreadDownload do
    begin
      PostData := Self.FPostData;
      Method := Self.FMethod;
      Proxy := Self.Proxy;
      Server := Self.Server;
      Port := Self.Port;
      OnStart := FOnStart;
      OnProgress := FOnProgress;
      OnFinish := ThreadOnFinish;
      Start;
    end;
  end;
end;

procedure TFileDownload.StartDownload(Stream: TStream; URL,
  FileName: String);
begin
  if not (FBusy) then
  begin
    FBusy := True;
    FThreadDownload := TThreadDownload.Create(URL, FileName, FPartExt);
    FThreadDownload.Stream := Stream;
    with FThreadDownload do
    begin
      PostData := Self.FPostData;
      Method := Self.FMethod;   
      Proxy := Self.Proxy;
      Server := Self.Server;
      Port := Self.Port;
      OnStart := FOnStart;
      OnProgress := FOnProgress;
      OnFinish := ThreadOnFinish;
      Start;
    end;
  end;
end;

procedure TFileDownload.ThreadOnFinish(Sender: TObject; State: TDownloadState;
  Canceled: Boolean);
begin
  FBusy := False;
  FThreadDownload := nil;
  if Assigned(FOnFinish) then
    FOnFinish(Sender, State, Canceled);
end;

function TFileDownload.IsBusy:Boolean;
begin
  Result := FBusy;
end;

procedure TFileDownload.SetPostData(Value: TStrings);
begin
  FPostData.Assign(Value);
end;

constructor TFileDownload.Create(AOwner: TComponent);
begin
  inherited;
  FMethod := httpGET;
  FPostData := TStringList.Create;
end;

end.
 