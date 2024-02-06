﻿unit UnitNotification;

interface

uses
  System.SysUtils,
  WinAPI.WinRT,
  WinAPI.DataRT,
  WinAPI.UI.Notifications,
  WinAPI.ActiveX,
  WinAPI.CommonTypes,
  WinAPI.PropKey,
  WinAPI.PropSys,
  WinAPI.ShlObj,
  System.Win.ComObj,
  Windows;

type
  TNotification = class
  public
    class procedure Init(const AppLinkName: string); static;
    class procedure Show(Const AppID: String; Const XML: String); static;
  protected
    class function CreateDesktopShellLink(const AppLinkName: string)
      : Boolean; static;
  private
    class function HStr(Value: String): HString; static;
    class function ToastTemplateToString(Const Template: Xml_Dom_IXmlDocument)
      : String; static;
    class function GetFactory(Const Name: String; Const GUID: String)
      : IInspectable; static;
    class procedure OverwriteToastTemplateXML(Const Template
      : Xml_Dom_IXmlDocument; Const XML: String); static;
  end;

implementation

class procedure TNotification.Init(const AppLinkName: string);
begin
  if TOSVersion.Major < 10 then
    raise Exception.Create('Windows 10 Required');

  RoInitialize(RO_INIT_MULTITHREADED);

  if not CreateDesktopShellLink(AppLinkName) then
    raise Exception.Create('CreateDesktopShellLink failed');
end;

class function TNotification.CreateDesktopShellLink(const AppLinkName
  : string): Boolean;

  function GetStartMenuFolder(const AppLinkName: string): string;
  var
    Buffer: array [0 .. MAX_PATH - 1] of Char;
  begin
    Result := '';
    GetEnvironmentVariable(PChar('APPDATA'), Buffer, MAX_PATH - 1);
    Result := Buffer + '\Microsoft\Windows\Start Menu\Programs\' +
      AppLinkName + '.lnk';
  end;

var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  LinkName: string;

  LStore: WinAPI.PropSys.IPropertyStore;
  LValue: TPropVariant;
begin
  Result := False;

  IObject := CreateComObject(CLSID_ShellLink);
  ISLink := IObject as IShellLink;
  IPFile := IObject as IPersistFile;
  LStore := IObject as WinAPI.PropSys.IPropertyStore;

  ISLink.SetPath(PChar(ParamStr(0)));
  ISLink.SetArguments(PChar(''));

  if Succeeded(InitPropVariantFromStringAsVector
    (PWideChar('Delphi.DesktopNotification.Sample'), LValue)) and
    Succeeded(LStore.SetValue(PKEY_AppUserModel_ID, LValue)) then
    LStore.Commit;

  LinkName := GetStartMenuFolder(AppLinkName);

  if FileExists(LinkName) or (IPFile.Save(PWideChar(LinkName), True) = S_OK)
  then
    Result := True;
end;

class function TNotification.HStr(Value: String): HString;
begin
  if NOT Succeeded(WindowsCreateString(PWideChar(Value), Length(Value), Result))
  then
    raise Exception.CreateFmt('Unable to create HString for %s', [Value]);
end;

class function TNotification.ToastTemplateToString(Const Template
  : Xml_Dom_IXmlDocument): String;

  function HStringToString(Src: HString): String;
  var
    c: Cardinal;
  begin
    c := WindowsGetStringLen(Src);
    Result := WindowsGetStringRawBuffer(Src, @c);
  end;

begin
  Result := HStringToString
    ((Template.DocumentElement as Xml_Dom_IXmlNodeSerializer).GetXml);
end;

class function TNotification.GetFactory(Const Name: String; Const GUID: String)
  : IInspectable;
var
  FactoryHString: HString;
  FactoryGUID: TGUID;
begin
  FactoryHString := HStr(Name);
  try
    FactoryGUID := TGUID.Create(GUID);

    if NOT Succeeded(RoGetActivationFactory(FactoryHString, FactoryGUID, Result))
    then
      raise Exception.CreateFmt('Error creating factory: %s %s', [Name, GUID]);
  finally
    WindowsDeleteString(FactoryHString);
  end;
end;

class procedure TNotification.OverwriteToastTemplateXML(Const Template
  : Xml_Dom_IXmlDocument; Const XML: String);
var
  hXML: HString;
begin
  hXML := HStr(XML);
  try
    (Template as Xml_Dom_IXmlDocumentIO).LoadXml(hXML);
  finally
    WindowsDeleteString(hXML);
  end;
end;

class procedure TNotification.Show(Const AppID: String; Const XML: String);
const
  SToastNotificationManager =
    'Windows.UI.Notifications.ToastNotificationManager';
  SToastNotification = 'Windows.UI.Notifications.ToastNotification';
var
  ToastNotificationManagerStatics: IToastNotificationManagerStatics;
  ToastTemplate: Xml_Dom_IXmlDocument;
  LToastNotification: IToastNotification;
  ToastNotificationManagerFactory: IInspectable;
  ToastNotificationFactory: IInspectable;
  hAppID: HString;
begin
  ToastNotificationManagerFactory := GetFactory(SToastNotificationManager,
    '{50AC103F-D235-4598-BBEF-98FE4D1A3AD4}');
  ToastNotificationManagerStatics := IToastNotificationManagerStatics
    (ToastNotificationManagerFactory);
  ToastTemplate := ToastNotificationManagerStatics.GetTemplateContent
    (ToastTemplateType.ToastText01);

  OverwriteToastTemplateXML(ToastTemplate, XML);

{$IFDEF CONSOLE}
  WriteLn('XML: ', ToastTemplateToString(ToastTemplate));
{$ENDIF}
  ToastNotificationFactory := GetFactory(SToastNotification,
    '{04124B20-82C6-4229-B109-FD9ED4662B53}');

  LToastNotification := IToastNotificationFactory(ToastNotificationFactory)
    .CreateToastNotification(ToastTemplate);

  hAppID := HStr(AppID);
  try
    ToastNotificationManagerStatics.CreateToastNotifier(hAppID)
      .Show(LToastNotification);
  finally
    WindowsDeleteString(hAppID);
  end;
end;

end.
