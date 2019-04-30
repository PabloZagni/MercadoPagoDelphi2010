unit Main_u;

interface

uses

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, OleCtrls, SHDocVw,
  DBXJSON, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, MercadoPago_u, ExtCtrls, JvAppStorage, JvAppRegistryStorage, JvComponentBase,
  JvFormPlacement, Config_u, RzButton ;

type

  TForm1 = class(TForm)
    Panel1: TPanel;
    Timer1: TTimer;
    JvFormStorage1: TJvFormStorage;
    JvAppRegistryStorage1: TJvAppRegistryStorage;
    pnConfig: TPanel;
    chSandbox: TCheckBox;
    Label1: TLabel;
    edClientID: TEdit;
    Label2: TLabel;
    edClientSecret: TEdit;
    edAccessToken: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    edPublicKey: TEdit;
    pnMP: TPanel;
    btnMiCuenta: TButton;
    btnPagos: TButton;
    MemoApp: TMemo;
    MemoCore: TMemo;
    lbStatus: TLabel;
    MemoError: TMemo;
    btnConfigLoad: TRzBitBtn;
    btnConfigSave: TRzBitBtn;
    btnStartStop: TButton;
    Label3: TLabel;
    edUltimoCobro: TEdit;
    procedure btnPagosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnMiCuentaClick(Sender: TObject);
    procedure btnConfigLoadClick(Sender: TObject);
    procedure btnConfigSaveClick(Sender: TObject);
    procedure btnStartStopClick(Sender: TObject);
  private
    lActivo : Boolean ;
    Config : TConfig ;
    MP : TMercadoPago ;
    procedure m( s: String );
    procedure OnCobro( x : TMPCobro );
    procedure DoAfterLoad ;
    procedure DoBeforeSave ;
//    procedure DoOnNewAccessToken( s : String );
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnPagosClick(Sender: TObject);
begin
  MP.search_payment( edUltimoCobro.Text, OnCobro ) ;
end;

procedure TForm1.btnStartStopClick(Sender: TObject);
begin
  if not lActivo then begin
    lActivo := True ;
    pnConfig.Enabled := False ;
    m('--- START ---');
    MP := TMercadoPago.Create( edPublicKey.Text, edAccessToken.Text, edClientID.Text, edClientSecret.Text );
    MP.lbStatus := lbStatus ;
    MP.Memo := MemoCore ;
    MP.MemoError := MemoError ;
    MP.SandBox := chSandbox.Checked  ;
//    MP.OnNewAccessToken := DoOnNewAccessToken ;
    pnMP.Enabled := True ;
    btnStartStop.Caption := 'Stop';
    end
  else begin
    m('--- STOP ---');
    pnConfig.Enabled := True ;
    pnMP.Enabled := False ;
    MP.Free ;
    btnStartStop.Caption := 'Start';
    lActivo := False ;
    end;
end;

procedure TForm1.btnMiCuentaClick(Sender: TObject);
begin
  MP.get_user_me ;
  m( 'UserID: '   + MP.User.ID );
  m( 'Nickname: ' + MP.User.nickname );
  m( 'Name: '     + MP.User.first_name+' '+MP.User.last_name );
  m( 'Email: '    + MP.User.email );
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Config.Free ;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  lActivo := False ;
  Config := TConfig.Create ;
  Config.AfterLoad := DoAfterLoad ;
  Config.BeforeSave := DoBeforeSave ;
  Config.Load ;
end;

procedure TForm1.m(s: String);
begin
  MemoApp.Lines.Add( s );
end;

procedure TForm1.OnCobro(x: TMPCobro);
begin
  edUltimoCobro.Text := x.date_created ;
  Config.Save ;
  m( '--- COBRO ---');
  m( 'ID: '+x.ID );
  m( 'date_created: '+x.date_created );
  m( 'Reason: '+x.reason );
  m( 'Importe: '+x.currency_id+' '+FloatToStr(x.total_paid_amount));
  m( 'CardHolder: '+x.cardholder.name );
end;

procedure TForm1.DoAfterLoad;
begin
  edPublicKey.Text    := Config.PublicKey ;
  edAccessToken.Text  := Config.AccessToken ;
  edClientID.Text     := Config.Client_ID ;
  edClientSecret.Text := Config.Client_Secret ;
  chSandbox.Checked   := Config.SandBox ;
  edUltimoCobro.Text  := Config.UltimoCobro ;
  m('Cargando configuración...');
end;

procedure TForm1.DoBeforeSave;
begin
  m('Guardando configuración...');
  Config.PublicKey     := edPublicKey.Text ;
  Config.AccessToken   := edAccessToken.Text ;
  Config.Client_ID     := edClientID.Text ;
  Config.Client_Secret := edClientSecret.Text ;
  Config.SandBox       := chSandbox.Checked ;
  Config.UltimoCobro   := edUltimoCobro.Text ;
end;

//procedure TForm1.DoOnNewAccessToken(s: String);
//begin
//  m('Nuevo access token! '+s);
//  Config.AccessToken := s ;
//  Config.Save ;
//end;

procedure TForm1.btnConfigLoadClick(Sender: TObject);
begin
  Config.Load ;
end;

procedure TForm1.btnConfigSaveClick(Sender: TObject);
begin
  Config.Save ;
end;

end.

