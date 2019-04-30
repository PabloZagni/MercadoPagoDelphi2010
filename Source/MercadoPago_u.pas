unit MercadoPago_u;

interface

uses

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, OleCtrls, SHDocVw,
  DBXJSON, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, ExtCtrls, RestClient_u ;

type

  TMPRestClient = Class ( TRestClient )
  private
  protected
    constructor Create ; override ;
  End;

  TMPUser = record
    ID,
    nickname,
    first_name,
    last_name,
    email : String ;
  end;

  TMPPayer = record
    id,
    nickname,
    fist_name,
    last_name,
    phone,
    email,
    identification : String ;
  end;

  TMPCardholder = record
    name,
    identification : String ;
  end;

  TMPCobro = record
    ID : String ;
    date_created : String ;
    cuando : TDateTime ;
    status : String ;
    payer : TMPPayer ;
    external_reference,
    merchant_order_id,
    reason,
    currency_id : String ;
    transaction_amount : Real ;
    total_paid_amount : Real ;
    payment_type : string ;
    shipping_cost : Real ;
    cardholder :TMPCardholder ;
    json : String ;
  end;

  TMPQR = record
    PosID : Integer ;
    QR, Document, Image : String ;
  end;

  TProcedureMPCobro = procedure ( x : TMPCobro ) of object ;

//  TOnNewAccessToken = procedure( s : String ) of object ;

  TMercadoPago = Class( TComponent )
  private
    sRespuesta : String ;
    FMemo: TMemo;
    FClient_ID: String;
    FClient_Secret: String;
    FMemoError: TMemo;
    FSandBox: Boolean;
    FUser: TMPUser;
    req : TRequest ;
    Json : TJSONObject;
    FStatus: TLabel;
    TimerApagaStatus : TTimer ;
//    FOnNewAccessToken: TOnNewAccessToken;
    FPublicKey: String;
    FAccessToken: String;
    FQR: TMPQR;
    FCollector_ID: String;
    procedure TimerApagaStatusOnTimer(Sender: TObject);
    procedure Status( s : String );
    procedure m( s : String );
    procedure xError( s : String );
    procedure SetClient_ID(const Value: String);
    procedure SetClient_Secret(const Value: String);
    procedure SetMemo(const Value: TMemo);
    function sandboxurl( s : String ) : String ;
    function JSONDateTimeToDelphiDateTime( s : String ) : TDateTime ;
    procedure SetStatus(const Value: TLabel);
//    function GetAccess_Token: String;
    procedure SetPublicKey(const Value: String);
    procedure SetAccessToken(const Value: String);
  public
    constructor Create( sPublicKey, sAccessToken, sClientID, sClientSecret : String );
    destructor Destroy ;
    procedure get_user_me ;
    procedure search_payment( sUltimoCobro : String; CallbackOnCobro : TProcedureMPCobro ) ;
//    function get_access_token : String ;
    function GenerarQR( nSurt : Integer; sCallback : String ) : String ;
  published
    property lbStatus : TLabel read FStatus write SetStatus ;
    property Memo : TMemo read FMemo write SetMemo ;
    property MemoError : TMemo read FMemoError write FMemoError ;
    property PublicKey : String read FPublicKey write SetPublicKey ;
    property AccessToken : String read FAccessToken write SetAccessToken ;
    property Client_ID : String read FClient_ID write SetClient_ID ;
    property Collector_ID : String read FCollector_ID write FCollector_ID ;
    property Client_Secret : String read FClient_Secret write SetClient_Secret ;
    property User : TMPUser read FUser write FUser ;
    property SandBox : Boolean read FSandBox write FSandBox ;
    property QR : TMPQR read FQR write FQR ;
//    property OnNewAccessToken : TOnNewAccessToken read FOnNewAccessToken write FOnNewAccessToken ;
  End;

implementation

uses DateUtils, Character ;

Type

  TSSL = ( tSSLOff, tSSLOn );
  TPost = ( tText, tXWWWForm, tJson );

function HTTPPost( sUrl: String; SSL : TSSL ; tipo : TPost; sMsg : TStringList; nTimeOut : Integer = 5 ) : String ;
var
  IdHTTP: TIdHTTP;
  Handler: TIdIOHandlerStack;
  HandlerSSL: TIdSSLIOHandlerSocketOpenSSL;

  Json: string;
  JsonToSend: TStringStream;
begin
  try
    Result := '' ;
    try
      IdHTTP := TIdHTTP.create(Application);
      IdHTTP.ReadTimeout := nTimeOut*1000; // si en 5 segundos no responde asumir que no hay respuesta
      if SSL=tSSLOn then begin
        HandlerSSL := TIdSSLIOHandlerSocketOpenSSL.Create( IdHTTP ) ;
//        HandlerSSL.SSLOptions.Method := sslvSSLv23;
        IdHTTP.IOHandler := HandlerSSL ;
        end
      else begin
        Handler := TIdIOHandlerStack.create(IdHTTP);
        IdHTTP.IOHandler := Handler ;
        end;
      IdHTTP.Request.Charset := 'utf-8';
      if tipo=tText then
        IdHTTP.Request.ContentType := 'text/xml';
      if tipo=tXWWWForm then
        IdHTTP.Request.ContentType := 'application/x-www-form-urlencoded';
      if tipo=tJson then begin
        IdHTTP.Request.ContentType := 'application/json' ;
        JSon := sMsg.Text ;
        JsonToSend := TStringStream.Create(Utf8Encode(Json));
        Result := IdHTTP.Post(sUrl, JsonToSend);
        JsonToSend.Free ;
        end
      else
        Result := IdHTTP.Post(sUrl, sMsg);
    except
      on E: Exception do
        raise Exception.Create(E.ClassName + ': ' +E.Message);
      end;
  finally
    if SSL=tSSLOn then
      HandlerSSL.Free
    else
      Handler.Free ;
    IdHTTP.Free ;
    end;
end;

function StripNonJson(s: string): string;
var
  ch: char;
  inString: boolean;
begin
  Result := '';
  inString := false;
  for ch in s do
  begin
    if ch = '"' then
      inString := not inString;
    if TCharacter.IsWhiteSpace(ch) and not inString then
      continue;
    Result := Result + ch;
  end;
end;

function JsonSinComillas(s: String): String;
begin
  result := StringReplace( s, '"','', [rfReplaceAll] );
end;

procedure SaveToFile( sFileName: String ; sTexto : String );
var
  f : textfile;
begin
  try
    AssignFile( f, sFileName );
    Rewrite( f );
    Write(f, sTexto );
    Flush(f);
    CloseFile(f);
  except
  end;
end;

{ TMercadoPago }

constructor TMercadoPago.Create(
        sPublicKey,
        sAccessToken,
        sClientID,
        sClientSecret : String );
begin
  req := TRequest.Create ;
  TimerApagaStatus := TTimer.Create( Self );
  TimerApagaStatus.Interval := 2*1000 ;
  TimerApagaStatus.Enabled := False ;
  TimerApagaStatus.OnTimer := TimerApagaStatusOnTimer ;
  PublicKey := sPublicKey ;
  AccessToken := sAccessToken ;
  Client_ID := sClientID ;
  Client_Secret := sClientSecret ;
end;

destructor TMercadoPago.Destroy;
begin
  TimerApagaStatus.Free ;
  req.Free ;
end;

procedure TMercadoPago.xError(s: String);
begin
  if Assigned( FMemoError ) then
    FMemoError.Lines.Add( s );
end;

function TMercadoPago.GenerarQR(nSurt: Integer; sCallback: String): String;
var
  sUrl : String ;
  sl : TStringList ;
  Template : TJSONObject;
begin
  if FCollector_ID='' then begin
    ShowMessage('Debe definir Collector_ID');
    end
  else begin
    sUrl := 'https://api.mercadopago.com';
    sUrl := sUrl + '/mpmobile/instore/merchant/qr'+
      '/'+FCollector_ID+
      '/'+IntToStr(nSurt)+
      '?access_token='+FAccessToken ;
    sl := TStringList.Create ;
    sl.Add('{ "custom_url":"'+sCallback+'" }');
    Result := HTTPPost( sUrl, tSSLOn, tJson, sl ) ;
    sl.Free ;
    Json := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(sRespuesta)),0));
    FQR.PosID      := StrToInt(JsonSinComillas( TJSONObject(Json.Get('pos_id').JsonValue).ToString ));
    FQR.QR         := JsonSinComillas( TJSONObject(Json.Get('qr').JsonValue).ToString );
    Template       := TJSONObject(Json.Get('qr_template').JsonValue );
    FQR.Document   := JsonSinComillas( TJSONObject(Template.Get('document').JsonValue).ToString );
    FQR.Image := JsonSinComillas( TJSONObject(Template.Get('image').JsonValue).ToString );
    end;
end;

//function TMercadoPago.GetAccess_Token: String;
//begin
//  Result := get_access_token ;
//end;

procedure TMercadoPago.search_payment( sUltimoCobro : String; CallbackOnCobro : TProcedureMPCobro ) ;
var
  results         : TJSONArray;
  collection      : TJSONObject;
  item            : TJSONObject;
  payer           : TJSONObject;
  phone           : TJSONObject;
  identification  : TJSONObject;
  cardholder      : TJSONObject;
  i : integer ;
  s : String ;
  cobro : TMPCobro ;
begin
  // https://www.mercadopago.com.ar/developers/es/reference/payments/_payments_search/get/
  // req.uri := pagosurl('/v1/payments/search');
  Status('Buscando cobros...');
  req.Clear ;
  req.uri := sandboxurl('/collections/search') ;
  req.params.Add( 'access_token='+FAccessToken );
  if sUltimoCobro<>'' then begin
    req.params.Add( 'range=date_created');
    sUltimoCobro[23] := '1'; // le sumo 1 milisegundo al último pago
    req.params.Add( 'begin_date='+sUltimoCobro);
    req.params.Add( 'end_date=2099-02-01T00:00:00Z');
    end;
  try
    sRespuesta := TMPRestClient.Get( req );
  except
    on E: Exception do
      xError( 'Error: '+E.ClassName + ': ' +E.Message );
    end;
  m( 'Respuesta: '+sRespuesta );
  Json := TJSONObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(sRespuesta)),0));
  m( json.ToString );
  SaveToFile( 'cobro.json', json.ToString );
  results := TJSONArray(Json.Get('results').JsonValue);
  for i := 0 to TJSONArray(results).Size-1 do begin
    item := TJSONObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(results.Get(i).ToString)),0));
    collection := TJsonObject(item.Get('collection').JsonValue);
    cobro.id := TJSONObject(collection.Get('id').JsonValue).ToString ; m( 'ID: '+cobro.id );
    cobro.date_created :=  JsonSinComillas( TJSONObject(collection.Get('date_created').JsonValue).ToString );
    cobro.cuando := JSONDateTimeToDelphiDateTime( TJSONObject(collection.Get('date_created').JsonValue).ToString ); m( 'date_created: '+FormatDateTime( 'dd/mm/yyyy hh:mm', cobro.cuando) );
    cobro.status := JsonSinComillas( TJSONObject(collection.Get('status').JsonValue).ToString ) ; m( 'status: '+cobro.status );
    payer := TJSONObject(collection.Get('payer').JsonValue) ;
    cobro.payer.id := TJSONObject(payer.Get('id').JsonValue).ToString ; m( 'payer id: '+cobro.payer.id );
    cobro.payer.nickname := TJSONObject(payer.Get('nickname').JsonValue).ToString ; m( 'payer nickname: '+cobro.payer.nickname );
    cobro.payer.fist_name := TJSONObject(payer.Get('first_name').JsonValue).ToString ; m( 'payer first_name: '+cobro.payer.fist_name );
    cobro.payer.last_name := TJSONObject(payer.Get('last_name').JsonValue).ToString ; m( 'payer last_name: '+cobro.payer.last_name );
    cobro.payer.email := TJSONObject(payer.Get('email').JsonValue).ToString ; m( 'payer email: '+cobro.payer.email );
    phone := TJSONObject(payer.Get('phone').JsonValue) ;
    cobro.payer.phone :=
      TJSONObject(phone.Get('area_code').JsonValue).ToString + ' ' +
      TJSONObject(phone.Get('number').JsonValue).ToString ; m( 'payer phone: '+cobro.payer.phone );
    identification := TJSONObject(payer.Get('identification').JsonValue) ;
    cobro.payer.identification :=
      TJSONObject(identification.Get('type').JsonValue).ToString + ' ' +
      TJSONObject(identification.Get('number').JsonValue).ToString ; m( 'payer identification: '+cobro.payer.identification );
    cobro.external_reference := TJSONObject(collection.Get('external_reference').JsonValue).ToString ; m( 'external_reference: '+cobro.external_reference );
    cobro.merchant_order_id := TJSONObject(collection.Get('merchant_order_id').JsonValue).ToString ; m( 'merchant_order_id: '+cobro.merchant_order_id );
    cobro.reason := JsonSinComillas( TJSONObject(collection.Get('reason').JsonValue).ToString ) ; m( 'reason: '+cobro.reason );
    cobro.currency_id := JsonSinComillas( TJSONObject(collection.Get('currency_id').JsonValue).ToString ) ; m( 'currency_id: '+cobro.currency_id );
    cobro.transaction_amount := StrToFloat( TJSONObject(collection.Get('transaction_amount').JsonValue).ToString) ; m( 'transaction_amount: '+FloatToStr(cobro.transaction_amount) );
    cobro.total_paid_amount := StrToFloat( TJSONObject(collection.Get('total_paid_amount').JsonValue).ToString) ; m( 'total_paid_amount: '+FloatToStr(cobro.total_paid_amount) );
    cobro.shipping_cost := StrToFloat( TJSONObject(collection.Get('shipping_cost').JsonValue).ToString) ; m( 'shipping_cost: '+FloatToStr(cobro.shipping_cost) );
    cobro.payment_type := JsonSinComillas( TJSONObject(collection.Get('payment_type').JsonValue).ToString ); m( 'payment_type: '+cobro.payment_type );

    cardholder :=  TJSONObject(collection.Get('cardholder').JsonValue) ;
    cobro.cardholder.name := JsonSinComillas( TJSONObject(cardholder.Get('name').JsonValue).ToString ); m( 'cardholder name: '+cobro.cardholder.name );
    identification := TJSONObject(cardholder.Get('identification').JsonValue) ;
    cobro.cardholder.identification :=
      JsonSinComillas( TJSONObject(identification.Get('type').JsonValue).ToString ) + ' ' +
      JsonSinComillas( TJSONObject(identification.Get('number').JsonValue).ToString ); m( 'cardholder identification: '+cobro.cardholder.identification );
    cobro.json := json.ToString ;
    CallbackOnCobro( cobro );
    end;
  if i=0 then
    Status('No se encontraron cobros nuevos')
  else
    Status('Nuevos cobros');
end;

//function TMercadoPago.get_access_token : String;
//var request : TRequest ;
//begin
//  if FAccessToken<>'' then
//    // Si ya tengo el access token lo retorno
//    Result := FAccessToken
//  else begin
//    // Si no tengo el access token lo pido
//    Status( 'Ingresando en MercadoPago' );
//    // Le pongo un request distinto porque puede ser llamada desde otro método y lo va a ensuciar si lo comparten
//    request := TRequest.Create ;
//    request.uri := '/oauth/token';
//    request.params.add('client_id='+FClient_ID);
//    request.params.add('client_secret='+FClient_Secret);
//    request.params.add('grant_type=client_credentials');
////    TMPRestClient.Request.ContentType := 'application/x-www-form-urlencoded';
//    try
//      sRespuesta := TMPRestClient.Post(request);
//    except
//      on E: Exception do
//        xError( 'Error: '+E.ClassName + ': ' +E.Message );
//      end;
//    request.Free ;
//    m( 'Respuesta: '+sRespuesta );
//    Json := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(sRespuesta)),0));
//    FAccessToken := JsonSinComillas( TJSONObject(Json.Get('access_token').JsonValue).ToString );
//    result := FAccessToken ;
//    Status( 'Ingreso exitoso' );
//    end;
//end;

procedure TMercadoPago.get_user_me;
begin
  Status( 'Tomando datos de la cuenta' );
  req.Clear ;
  req.uri := '/users/me';
  req.params.Add( 'access_token='+FAccessToken );
  try
    sRespuesta := TMPRestClient.Get(req);
  except
    on E: Exception do
      xError( 'Error: '+E.ClassName + ': ' +E.Message );
    end;
  m( 'Respuesta: '+sRespuesta );
  Json := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(sRespuesta)),0));
  FUser.ID         := JsonSinComillas( TJSONObject(Json.Get('id').JsonValue).ToString );
  FUser.nickname   := JsonSinComillas( TJSONObject(Json.Get('nickname').JsonValue).ToString );
  FUser.first_name := JsonSinComillas( TJSONObject(Json.Get('first_name').JsonValue).ToString );
  FUser.last_name  := JsonSinComillas( TJSONObject(Json.Get('last_name').JsonValue).ToString );
  FUser.email      := JsonSinComillas( TJSONObject(Json.Get('email').JsonValue).ToString );
  Status( 'Datos de cuenta tomados correctamente' );
end;

function TMercadoPago.JSONDateTimeToDelphiDateTime(s: String): TDateTime;
var ano, mes, dia, hora, min, seg : Word;
begin
  // 12345678901234567890
  // 2018-08-02T14:02:33.000-04:00
  s := JsonSinComillas( s );
  ano  := StrToInt( Copy( s, 1, 4 ));
  mes  := StrToInt( Copy( s, 6, 2 ));
  dia  := StrToInt( Copy( s, 9, 2 ));
  hora := StrToInt( Copy( s,12, 2 ));
  min  := StrToInt( Copy( s,15, 2 ));
  seg  := StrToInt( Copy( s,18, 2 ));
  Result := EncodeDateTime( ano, mes, dia, hora, min, seg, 0);
end;

procedure TMercadoPago.m(s: String);
begin
  if Assigned( FMemo ) then
    FMemo.Lines.Add( s );
end;

function TMercadoPago.sandboxurl(s: String): String;
begin
  if FSandBox then
    result := result+'/sandbox';
  result := result + s;
end;

procedure TMercadoPago.SetAccessToken(const Value: String);
begin
  FAccessToken := Value;
  m( 'Access Token: '+Value );
end;

procedure TMercadoPago.SetClient_ID(const Value: String);
begin
  FClient_ID := Value;
  m( 'ClientID: '+Value );
end;

procedure TMercadoPago.SetMemo(const Value: TMemo);
begin
  FMemo := Value;
  PublicKey := FPublicKey ;
  AccessToken := FAccessToken ;
  Client_ID := FClient_ID ; // Para que se vean en el memo
  Client_Secret := FClient_Secret ;
end;

procedure TMercadoPago.SetPublicKey(const Value: String);
begin
  FPublicKey := Value;
  m( 'PublicKey: '+Value);
end;

procedure TMercadoPago.SetStatus(const Value: TLabel);
begin
  FStatus := Value;
  FStatus.Visible := False ;
end;

procedure TMercadoPago.Status(s: String);
begin
  if Assigned( FStatus ) then begin
    FStatus.Visible := True ;
    FStatus.Caption := s ;
    FStatus.Refresh ;
    TimerApagaStatus.Enabled := False ;
    TimerApagaStatus.Enabled := True ;
    end;
end;

procedure TMercadoPago.TimerApagaStatusOnTimer(Sender: TObject);
begin
  if Assigned( FStatus ) then
    FStatus.Visible := False ;
end;

procedure TMercadoPago.SetClient_Secret(const Value: String);
begin
  FClient_Secret := Value;
  m( 'ClientSecret: '+Value );
end;

{ TMPRestClient }

constructor TMPRestClient.Create;
begin
  inherited;
  sURLDominio := 'https://api.mercadopago.com';
  IdHTTP.Request.UserAgent := 'MercadoPago PHP SDK v0.5.2';
end;

end.

