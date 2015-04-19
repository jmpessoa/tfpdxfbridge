unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, FPDxfWriteBridge, Forms, Controls, Graphics,
  Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    FPDxfWriteBridge1: TFPDxfWriteBridge;
    procedure Button1Click(Sender: TObject);
    procedure FPDxfWriteBridge1ProduceBlock(out blockDXF: string);
    procedure FPDxfWriteBridge1ProduceEntity(out entityDXF: string);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  FPDxfWriteBridge1.Produce('');
  FPDxfWriteBridge1.SaveToFile('Test1.dxf');
  ShowMessage('ok');
  Close;
end;

procedure TForm1.FPDxfWriteBridge1ProduceBlock(out blockDXF: string);
var
  x1, y1, x2, y2: real;
  strList: TStringList;
begin
  x1:= 6.0;
  y1:= 4.0;
  x2:= 10.0;
  y2:= 6.0;

  strList:= TStringList.Create;

  strList.Add(FPDxfWriteBridge1.BeginBlock('0'{default layer},'Blk1'{Name}, 0{X} ,0{Y}));
     strList.Add(FPDxfWriteBridge1.Text('0',0, 0.25{h}, (x1+x2)/2, y1,'Hello World!'));
  strList.Add(FPDxfWriteBridge1.EndBlock);

  strList.Add(FPDxfWriteBridge1.BeginBlock('0'{layer},'*D1'{Name}, 0{X} ,0{Y}));
     //strList.Add(FPDxfWriteBridge1.RadialDimension('0','*D1','GENERIC',x1,y1, 1.2{r},0));
     strList.Add(FPDxfWriteBridge1.DiameterDimension('0','*D1','GENERIC',x1,y1, 1.2{r},80{angle}));
  strList.Add(FPDxfWriteBridge1.EndBlock);

  strList.Add(FPDxfWriteBridge1.BeginBlock('0'{layer},'*D2'{Name}, 0{X} ,0{Y}));
     //strList.Add(FPDxfWriteBridge1.Angular3PDimension('0','*D2','GENERIC', 0.6, x2, y2 ,radius,10, 80));
     strList.Add(FPDxfWriteBridge1.Arc3PDimension('0','*D2','CUSTOM', 0.6, x2, y2 ,1.2{radius},10{ang1}, 80{ang2}));
  strList.Add(FPDxfWriteBridge1.EndBlock);

  strList.Add(FPDxfWriteBridge1.BeginBlock('0'{layer},'*D3'{Name}, 0{X} ,0{Y}));
     strList.Add(FPDxfWriteBridge1.LinearAlignedDimension('0','*D3','CUSTOM', 0.6, x1, y1 , x2, y2));
  strList.Add(FPDxfWriteBridge1.EndBlock);

  strList.Add(FPDxfWriteBridge1.BeginBlock('0'{layer},'*D4'{Name}, 0{X} ,0{Y}));
     strList.Add(FPDxfWriteBridge1.RadialDimension('0','*D4','GENERIC',x2,y2,0.8{r}, 60{angle}));
  strList.Add(FPDxfWriteBridge1.EndBlock);

  (*strList.Add(FPDxfWriteBridge1.BeginBlock('0'{layer},'Blk2'{Name}, 0{X} ,0{Y}));
  //strList.Add(FPDxfWriteBridge1.Arc3PDimension('0','','GENERIC', 0.6,(x2+x1)/2,(y1+y2)/2,radius,10, 80));
  strList.Add(FPDxfWriteBridge1.Angular3PDimension('0','','GENERIC', 0.6,(x2+x1)/2,(y1+y2)/2,radius,0, 30));
  strList.Add(FPDxfWriteBridge1.EndBlock);*)

  blockDXF:= Trim(strList.Text);
  strList.Free;
end;

procedure TForm1.FPDxfWriteBridge1ProduceEntity(out entityDXF: string);
var
  x1, y1, x2, y2: real;
  strList: TStringList;
begin
  x1:= 6.0;
  y1:= 4.0;
  x2:= 10.0;
  y2:= 6.0;

  strList:= TStringList.Create;
  strList.Add(FPDxfWriteBridge1.Circle('0', x1, y1, 1.2{radius} ));
  strList.Add(FPDxfWriteBridge1.Arc('0', x2, y2, 1.2{radius}, 10{angle1}, 80{angle1}));
  strList.Add(FPDxfWriteBridge1.Line('0', x1, y1, x2, y2));
  strList.Add(FPDxfWriteBridge1.Circle('0', x2, y2, 0.8{radius} ));

  strList.Add(FPDxfWriteBridge1.Dimension('*D1'));
  strList.Add(FPDxfWriteBridge1.Dimension('*D2'));
  strList.Add(FPDxfWriteBridge1.Dimension('*D3'));
  strList.Add(FPDxfWriteBridge1.Dimension('*D4'));

  strList.Add(FPDxfWriteBridge1.InsertBlock('Blk1',0,0));
  //strList.Add(FPDxfWriteBridge1.InsertBlock('Blk2',0,0));

  entityDXF:= Trim(strList.Text);
  strList.Free;
end;

end.
