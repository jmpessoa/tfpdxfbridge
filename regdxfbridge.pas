unit regdxfbridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FPDxfWriteBridge, LResources;

Procedure Register;

implementation

Procedure Register;

begin
  {$I fpdxfwritebridge1_icon.lrs}
  RegisterComponents('Graphics Bridges',[TFPDXFWriteBridge]);
end;

initialization

end.

end;
