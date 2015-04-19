{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tfpdxfwritebridge_pack;

interface

uses
  FPDxfWriteBridge, GeometryUtilsBridge, regdxfbridge, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regdxfbridge', @regdxfbridge.Register);
end;

initialization
  RegisterPackage('tfpdxfwritebridge_pack', @Register);
end.
