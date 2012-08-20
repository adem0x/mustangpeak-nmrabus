unit nodeexplorer_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, FileUtil, Forms, Dialogs;

const
  FILENAME_STANDARD_TEST_FILE_UNIX = '/tests/testMatrixSample.xml';
  FILENAME_STANDARD_TEST_PATH_UNIX = '/tests';
  FILENAME_STANDARD_TEST_FILE_WIN = '\tests\testMatrixSample.xml';
  FILENAME_STANDARD_TEST_PATH_WIN = '\tests';
  FILENAME_OSX_RESOURCES_SUB_PATH = '/Contents/Resources';
  FILENAME_SETTINGS = 'Settings.xml';

type
{ TSettings }

  TSettings = class
  private
    FApplicationPath: String;
    FBaudRate: DWord;
    FComPort: String;
    FProxyNodeAlias: Word;
    FProxyNodeID: Int64;
    FTargetNodeAlias: Word;
    FTargetNodeID: Int64;
    FXMLSettings: TXMLDocument;
    function GetTestMatrixPath: string;
    function GetTextMatrixFile: string;
  public
    property ProxyNodeAlias: Word read FProxyNodeAlias write FProxyNodeAlias;
    property ProxyNodeID: Int64 read FProxyNodeID write FProxyNodeID;
    property TargetNodeAlias: Word read FTargetNodeAlias write FTargetNodeAlias;
    property TargetNodeID: Int64 read FTargetNodeID write FTargetNodeID;
    property ComPort: String read FComPort write FComPort;
    property BaudRate: DWord read FBaudRate write FBaudRate;
    property XMLSettings: TXMLDocument read FXMLSettings write FXMLSettings;
    property ApplicationPath: String read FApplicationPath write FApplicationPath;
    property TestMatrixPath: string read GetTestMatrixPath;
    property TestMatrixFile: string read GetTextMatrixFile;
    constructor Create;
    procedure ReadSettings;
    procedure WriteSettings;
  end;

var
  Settings: TSettings;

implementation

{$ifdef Windows}
uses Windows;
{$endif}
{$IFDEF Darwin}
uses
  MacOSAll;
{$ENDIF}

{ TSettings }

function TSettings.GetTestMatrixPath: string;
begin
  {$IFDEF Windows}
    Result := ApplicationPath + FILENAME_STANDARD_TEST_PATH_WIN;
  {$ENDIF}
  {$IFDEF darwin}
    Result := ApplicationPath + FILENAME_OSX_RESOURCES_SUB_PATH + FILENAME_STANDARD_TEST_PATH_UNIX;
  {$ENDIF}
end;

function TSettings.GetTextMatrixFile: string;
begin
  {$IFDEF Windows}
    Result := ApplicationPath + FILENAME_STANDARD_TEST_FILE_WIN;
  {$ENDIF}
  {$IFDEF darwin}
    Result := ApplicationPath + FILENAME_OSX_RESOURCES_SUB_PATH + FILENAME_STANDARD_TEST_FILE_UNIX;
  {$ENDIF}
end;

constructor TSettings.Create;
{$IFDEF DARWIN}
var
  pathRef: CFURLRef;
  pathCFStr: CFStringRef;
  pathStr: shortstring;
{$ENDIF}
begin
  inherited Create;
  FProxyNodeAlias := $0AAA;
  FProxyNodeID := $010203040506;
  FBaudRate := 333333;
  FComPort := 'COM4';
  FXMLSettings := nil;

  // Under OSX we get the path of the executable
{$IFDEF DARWIN}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);
  FApplicationPath := pathStr;
{$ENDIF}
    // Under Windows we get the path of the executable
{$IFDEF Windows}
  FApplicationPath := ExtractFilePath(Application.ExeName);
{$ENDIF}
end;

procedure TSettings.ReadSettings;
var
  NodeSettings, NodeIDs, NodeCOM, NodeID, NodeAlias, NodeCOMPort, NodeBaudRate: TDOMNode;
begin
  if FileExistsUTF8(GetAppConfigDir(True)+FILENAME_SETTINGS) then
  begin
    ReadXMLFile(FXMLSettings, UTF8ToSys(GetAppConfigDir(True)+FILENAME_SETTINGS));
    NodeSettings := XMLSettings.FindNode('Settings');
    if Assigned(NodeSettings) then
    begin
      NodeIDs := NodeSettings.FindNode('Node');
      if Assigned(NodeIDs) then
      begin
        NodeID := NodeIDs.FindNode('ID');
        if Assigned(NodeID) then
          FProxyNodeID := StrToInt64(NodeID.FirstChild.NodeValue);
        NodeAlias := NodeIDs.FindNode('Alias');
        if Assigned(NodeAlias) then
          FProxyNodeAlias := StrToInt(NodeAlias.FirstChild.NodeValue);
      end;
      NodeCOM := NodeSettings.FindNode('COM');
      if Assigned(NodeCOM) then
      begin
        NodeCOMPort := NodeCOM.FindNode('Port');
        if Assigned(NodeCOMPort) then
          FComPort := NodeCOMPort.FirstChild.NodeValue;
        NodeBaudRate := NodeCOM.FindNode('Baud');
        if Assigned(NodeBaudRate) then
          FBaudRate := StrToInt(NodeBaudRate.FirstChild.NodeValue);
      end;
    end;
  end;
end;

procedure TSettings.WriteSettings;
begin
  WriteXMLFile(FXMLSettings, GetAppConfigDir(True)+FILENAME_SETTINGS);
end;

initialization
  Settings := TSettings.Create;

finalization
  FreeAndNil(Settings);

end.
