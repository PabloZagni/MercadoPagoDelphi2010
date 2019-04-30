unit Config_u;

interface

type

  TOnLoadSave = procedure of object ;

  TConfig = Class
    PublicKey : String ;
    AccessToken : String ;
    Client_ID : String ;
    Client_Secret : String ;
    SandBox : Boolean ;
    UltimoCobro : String ;
  private
    FAfterLoad: TOnLoadSave;
    FBeforeSave: TOnLoadSave;
  public
    procedure Load ;
    procedure Save ;
  published
    property AfterLoad : TOnLoadSave read FAfterLoad write FAfterLoad ;
    property BeforeSave : TOnLoadSave read FBeforeSave write FBeforeSave ;
  End;

implementation

{ TConfig }

Uses IniFiles, StrUtils, SysUtils, Forms ;

function  DirectorioDelExe : String ;
begin
  Result := LeftStr( ExtractFilePath(Application.Exename), StrLen(PChar(ExtractFilePath(Application.Exename)))-1);
end;

procedure TConfig.Load;
begin
  with TIniFile.Create( DirectorioDelExe+'/config.ini' ) do try
    PublicKey     := ReadString( 'CONFIG', 'PublicKey',     '' );
    AccessToken   := ReadString( 'CONFIG', 'AccessToken',   '' );
    Client_ID     := ReadString( 'CONFIG', 'Client_ID',     '' );
    Client_Secret := ReadString( 'CONFIG', 'Client_Secret', '' );
    SandBox       :=   ReadBool( 'CONFIG', 'SandBox',      True );
    UltimoCobro   := ReadString( 'CONFIG', 'UltimoCobro',   '' );
  finally
    Free ;
    end;
  if Assigned( FAfterLoad ) then
    FAfterLoad ;
end;

procedure TConfig.Save;
begin
  if Assigned( FBeforeSave ) then
    FBeforeSave ;
  with TIniFile.Create( DirectorioDelExe+'/config.ini' ) do try
    WriteString( 'CONFIG', 'PublicKey',     PublicKey     );
    WriteString( 'CONFIG', 'AccessToken',   AccessToken   );
    WriteString( 'CONFIG', 'Client_ID',     Client_ID     );
    WriteString( 'CONFIG', 'Client_Secret', Client_Secret );
      WriteBool( 'CONFIG', 'SandBox',       SandBox       );
    WriteString( 'CONFIG', 'UltimoCobro',   UltimoCobro   );
  finally
    Free ;
    end;
end;

end.
