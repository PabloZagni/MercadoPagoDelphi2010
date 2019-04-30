unit RestClient_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, OleCtrls, SHDocVw,
  DBXJSON, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, ExtCtrls ;

type

  TJsonObjectHelper = class helper for TJsonObject
  public
    //Helper to find a JSON pair based on the pair string part
    function Get(const PairName: UnicodeString): TJSONPair; overload;
  end;

  TRequest = class
    uri : String ;
    body : TStringList ;
    headers : TStringList ;
    params : TStringList ;
  public
    procedure Clear ;
    constructor Create ;
    destructor Destroy ;
  end;

  TRestClient = Class ( TComponent )
  private
    Handler : TIdSSLIOHandlerSocketOpenSSL;
    DirectorioDeTrabajo : String ;
    procedure SaveToFile( sFileName: String ; sTexto : String );
    function uriSaveName( s : String ) : String ;
    function armarParametros( sl : TStringList ) : String ;
  protected // Pueden acceder mis hijos
    constructor Create ; virtual ;
    destructor Destroy ;
  public
    sURLDominio : String ;
    IDHttp : TIDHttp;
    class function Get( req : TRequest ) : String ;
    class function Post( req : TRequest ) : String ;
  End;

implementation

uses StrUtils ;

{ TRestClient }

function TRestClient.armarParametros(sl: TStringList): String;
var i : integer ;
begin
  result := '?';
  for I := 0 to sl.Count - 1 do begin
    result := result+sl[i];
    if i<sl.Count-1 then
      result := result + '&';
    end;
end;

constructor TRestClient.Create;
begin
  IDHttp := TIDHttp.Create(Self);
  with IdHTTP do begin
    IOHandler := Handler ;
    ConnectTimeout := 4 * 1000 ;
    AllowCookies := True;
    ProxyParams.BasicAuthentication := False;
    ProxyParams.ProxyPort := 0;
    Request.ContentLength := -1;
    Request.ContentRangeEnd := 0;
    Request.ContentRangeStart := 0;
    Request.Accept := 'application/json';
    Request.BasicAuthentication := False;
    HTTPOptions := [hoForceEncodeParams];
    end;
  Handler := TIdSSLIOHandlerSocketOpenSSL.create(IDHttp);
  with handler do begin
    ConnectTimeout := 4 * 1000 ;
    SSLOptions.Method := sslvSSLv23;
    SSLOptions.Mode := sslmUnassigned;
    SSLOptions.VerifyMode := [];
    SSLOptions.VerifyDepth := 0;
    host := '';
    end;
  IdHTTP.IOHandler := Handler ;
  DirectorioDeTrabajo := LeftStr( ExtractFilePath(Application.Exename), StrLen(PChar(ExtractFilePath(Application.Exename)))-1);
end;

destructor TRestClient.Destroy;
begin
  Handler.Free ;
  IDHttp.Free ;
end;

class function TRestClient.Get(req: TRequest): String;
var s : String ;
begin
  With Self.Create do try
    try
      s := sURLDominio+req.uri+armarParametros( req.params ) ;
      result := IDHttp.Get(s);
      SaveToFile( DirectorioDeTrabajo+uriSaveName(req.uri),result );
    except
      on E: Exception do
        Raise Exception.Create( 'Error: '+E.ClassName + ': ' +E.Message );
      end;
  finally
    Free;
    end;
end;

class function TRestClient.Post(req: TRequest): String;
var
  sUrl : String ;
  JsonToSend: TStringStream;
begin
  With Self.Create do try
    try
      sUrl := sURLDominio+req.uri+armarParametros( req.params ) ;
      JsonToSend := TStringStream.Create(Utf8Encode(req.body.Text));
      result := IDHttp.Post(sUrl, JsonToSend );
      SaveToFile( DirectorioDeTrabajo+uriSaveName(req.uri),result );
    except
      on E: Exception do
        Raise Exception.Create( 'Error: '+E.ClassName + ': ' +E.Message );
      end;
  finally
    Free;
    end;
end;

procedure TRestClient.SaveToFile(sFileName, sTexto: String);
var
  f : textfile;
begin
  try
    AssignFile( f, sFileName );
    Rewrite( f );
    Writeln(f, sTexto );
    Flush(f);
    CloseFile(f);
  except
  end;
end;

function TRestClient.uriSaveName(s: String): String;
begin
  s := RightStr( s, Length(s)-1 );
  result := '/'+StringReplace( s, '/','_',[rfReplaceAll])+'.json';
end;

{ TRequest }

procedure TRequest.Clear;
begin
  body.Clear ;
  headers.Clear ;
  params.Clear ;
end;

constructor TRequest.Create;
begin
  body := TStringList.Create ;
  headers := TStringList.Create ;
  params := TStringList.Create ;
end;

destructor TRequest.Destroy;
begin
  body.Free ;
  headers.Free ;
  params.Free ;
end;

{ TJsonObjectHelper }

function TJsonObjectHelper.Get(const PairName: UnicodeString): TJSONPair;
var
  Candidate: TJSONPair;
  I: Integer;
begin
  for i := 0 to Size - 1 do begin
    Candidate := Get(i);
    if (Candidate.JsonString.Value = PairName) then
      Exit(Candidate);
    end;
  Result := nil;
end;

end.
