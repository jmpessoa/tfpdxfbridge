unit FPDxfWriteBridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GeometryUtilsBridge{, Dialogs};

type

  TDXFColor = (
  DXFByBlock = 0,
  DXFRed = 1,
  DXFYellow = 2,
  DXFGreen = 3,
  DXFCyan = 4,
  DXFBlue = 5,
  DXFMagenta = 6,
  DXFWhite = 7,
  DXFGray = 8,
  DXFBrown = 15,
  DXFltRed = 23,
  DXFltGreen = 121,
  DXFltCyan = 131,
  DXFltBlue = 163,
  DXFltMagenta = 221,
  DXFBlack = 250,
  DXFltGray = 252,
  DXFLyLayer = 256);

  TDxfTextStyleCode = (
      PRIMARY_FILE_NAME = 3,
      FIXED_HEIGHT      = 40,
      WIDTH_FACTOR      = 41,
      LAST_HEIGHT_USED  = 42,
      TEXT_ANGLE        = 50,
      STANDARD_FLAG     = 70,
      GENERATION_FLAGS = 71);

  {TDxfTextStyle}
  TDxfTextStyle = class
     public
        TextStyleName: string;
        FixedHeight: real;
        WidthFactor: real;
        GenerationFlags: integer;
        LastHeightUsed: real;
        PrimaryFileName: string;
        TextAngle: real;
        DxfCode: TDxfTextStyleCode;
        function ToDXF: string;
        constructor Create(textStyle: string; fontFileName: string; fontHeight: real);
        destructor Destroy; Override;
  end;

  TDxfLineTypeCode = (
      ASCII_LINE_PATTERN      =  3,
      SUM_DASH_ELEMENT_LENGTH = 40,
      DASH_ELEMENT_LENGTH     = 49,
      ALIGNMENT               = 72,
      DASH_ELEMENT_COUNT      = 73);

  {TDxfLineType}
  TDxfLineType = class
      LineTypeName: String;
      AsciiLinePatern: string; // '__ __ __ __') ; '__' {Plus}; ' ' {minus}
      Alignment: integer;  //"A"
      DashElementCount: integer;  // Number of linetype elements, ex up: 2
      SUMDashElementLength: real; //Sum Abs DASH_ELEMENT_LENGTH: Abs(DashElementLength:=v[0])+ Abs(DashElementLength:=v[1]);
      DashElementLength: real;   //repeat patern: DashElementLength:=v[0]; DashElementLength:=v[1]; etc..
      VectorDashElementLength: array of real; //alternate numbers: "+" or "-" where:  '__' :plus  ' ':minus
      DxfCode: TDxfLineTypeCode;
      function ToDXF: string;
      constructor Create(typeName: string; VectDashLen: array of Real; linePattern: string);
      destructor Destroy; override;
  end;

  TDxfDimStyleCode = (
      ARROW_SIZE                        = 41,
      OFFSET_EXTENSION_LINE_FROM_ORIGIN = 42,
      EXTENSION_LINE_SIZE_PASSING       = 44,    // Extension line size after Dimension line
      LINE_SIZE_PASSING                 = 46,    // Dimension line size after Extension line
      DIMSTANDARD_FLAGS                 = 70,    //default: 0
      POSITION_TEXT_INSIDE_EXTENSION_LINES = 73,    //position of dimension text inside the extension lines
      VERTICAL_TEXT_LOCATION            = 77,    // Vertical Text Placement
      TEXT_HEIGHT                       = 140,   //Text height
      OFFSET_FROM_DIMENSION_LINE_TO_TEXT  = 147,  // Offset from dimension line to text
      LINE_AND_ARROW_HEAD_COLOR           = 176,  // Dimension line & Arrow heads color
      EXTENSION_LINE_COLOR                = 177,  // Extension line color
      TEXT_COLOR                          = 178);

  {TDxfDimStyle}
  TDxfDimStyle = class
      public
	DimStyleName: string;                           // DimStyle Name
	DimStandardFlags: integer;                      //0
        {DIMCLRD} LineAndArrowHeadColor: integer;
        {DIMDLE}  LineSizePassing: real;                // Dimension line size after Extensionline
        {DIMCLRE} ExtensionLineColor: integer;          // Extension line color
        {DIMEXE}  ExtensionLinePassing: real;           // Extension line size after Dimension  line
        {DIMEXO}  OffsetExtensionLineFromOrigin: real;  // Offset from origin
        {DIMASZ}  ArrowSize: real;
                  ArrowWidth: real;
        {DIMCLRT} TextColor: integer;
        {DIMTXT}  TextHeight: real;
        {DIMTAD}  VerticalTextLocation: integer;
        {DIMTIH}  PositionTextInsideExtensionLines: integer;
        {DIMGAP}  OffsetFromDimensionLineToText:real;   // Gap/offset from dimension line to text
        DxfCode:  TDxfDimStyleCode;
        function ToDXF: string;
        constructor Create(styleName: string; arrwSize, ArrwWidth: real; color: integer;
                                txtHeight: real);
        Destructor Destroy; override;
  end;

  TDxfLayerCode = (
       LINE_TYPE = 6,
       COLOR     = 62);

  TDxfLayer = record
     LayerName: string;
     LineType: string;  //CONTINUOUS.. or Dashed.. or Hidden ... etc
     Color: integer;
     DxfCode: TDxfLayerCode;
  end;

  PDxfLayer = ^TDxfLayer;


  TDxfCommonCode = (
             TEXT1      = 1,
             TEXT2      = 3, {only for MTEXT}
             STYLE_NAME = 7, {Text style}
             X1 = 10,
             X2 = 11,
             X3 = 12,
             X4 = 13,

             Y1 = 20,
             Y2 = 21,
             Y3 = 22,
             Y4 = 23,

             Z1 = 30,
             Z2 = 31,
             Z3 = 32,
             Z4 = 33,
             THICKNESS     = 39,
             SIZE          = 40,       {Radius, Height, ...}
             ANGLE1         = 50,       {start angle, rotation, inclination, ...}
             ANGLE2         = 51,       {end angle}
             FOLLOW_FLAG    = 66,      {only PolyLine}
             OPENED_CLOSED_FLAG = 70,  {PolyLine and LWPolyLine}
             COUNT_VERTICE    = 90);   {only LWPolyLine}

  TDxfCommonData = class
     public
        X1, Y1, Z1: real;
        Thickness: real;
        DxfLayer:  TDxfLayer;
        DxfCommonCode: TDxfCommonCode;
        constructor Create;
        destructor Destroy; override;
  end;

  {TDxfPoint}
  TDxfPoint = class(TDxfCommonData)
    public
      function ToDXF: string;
      constructor Create(layerName: string; x, y: real);
      destructor Destroy; override;
  end;

  {TDxfCircle}
  TDxfCircle = class(TDxfCommonData)
     public
      Radius: real;
      function ToDXF: string;
      constructor Create(layerName: string; x, y, r: real);
      destructor Destroy; override;
  end;

  {TDxfArc}
  TDxfArc = class(TDxfCommonData)
    public
     Radius: real;
     StartAngle: real;
     EndAngle: real;
     function ToDXF: string;
     constructor Create(layerName: string; x, y, r, startAng, endAng: real);
     destructor Destroy; override;
  end;

  TDxfDataText = class(TDxfCommonData)
    public
      Text1: string;
      TextStyleName: string;
      Height: real;
      RotationAngle: real;
      constructor Create;
      Destructor Destroy; override;
  end;

  {TDxfText}
  TDxfText = class(TDxfDataText)
    public
      function ToDXF: string;
      constructor Create(layerName: string; angle, H, x, y: real; txt: string);
      destructor Destroy; override;
  end;

  {TDxfMText}
  TDxfMText = class(TDxfDataText)
    public
      Text2: string;
      function ToDXF: string;
      constructor Create(layerName: string; angle, H, x, y: real; txt: string);
      destructor Destroy; override;
  end;

  {TDxfLine}
  TDxfLine = class(TDxfCommonData)
    public
      X2: real;
      Y2: real;
      Z2: real;
      function ToDXF: string;
      constructor Create(layerName: string; px1, py1, px2, py2: real);
      destructor Destroy; override;
  end;

  TDxfDataPolyLine = class(TDxfCommonData)
    private
      CountVertice: integer;
    public
      OpenedClosedFlag: integer;
      Vertice: array of TRealPoint;
      constructor Create;
      destructor Destroy; override;
  end;

  {TDxfPolyLine}
  TDxfPolyLine = class(TDxfDataPolyLine)
    public
      FollowFlag: integer; //set to 1!
      function ToDXF: string;
      constructor Create(layerName: string; V: array of TRealPoint; closed: boolean);
      destructor Destroy; override;
  end;

  {TDxfLWPolyLine}
  TDxfLWPolyLine = class(TDxfDataPolyLine)
        function ToDXF: string;
        constructor Create(layerName: string; V: array of TRealPoint; closed: boolean);
        destructor Destroy; override;
  end;

  {TDxfSolidArrowHead}
  TDxfSolidArrowHead = class(TDxfCommonData)
     public
      X2, Y2, Z2: real;
      X3, Y3, Z3: real;
      X4, Y4, Z4: real;
      function ToDXF: string;
      constructor Create(layerName: string; V: array of TRealPoint);
      destructor Destroy; override;
  end;

  TDxfGenericDimensionCode = (
        BLOCK_NAME     = 2,
        DIM_STYLE_NAME = 3,    //dimension style name,
        ORIGIN_X       = 10,   //X definition to specify the point of dimension line.
        MIDDLE_TEXT_X  = 11,   //X middle point to specify the point of dimension text
        DEF_POINT1_X   = 13, //X of The point used to specify the first extension line.
        DEF_POINT2_X   = 14, //X of The point used to specify the second extension line.
        DEF_POINT3_X   = 15, //X of The point used to specify the second extension line.
        ORIGIN_Y       = 20,
        MIDDLE_TEXT_Y  = 21,
        DEF_POINT1_Y   = 23,
        DEF_POINT2_Y   = 24,
        DEF_POINT3_Y   = 25,
        ORIGIN_Z       = 30,
        MIDDLE_TEXT_Z  = 31,
        DEF_POINT1_Z   = 33,
        DEF_POINT2_Z   = 34,
        DEF_POINT3_Z   = 35,
        LEADER_LENGTH  = 40,
        MEASUREMENT    = 42,   //actual dimension measurement
        ANGLE          = 50,
        DIMTYPE        = 70);  //0 = rotated, horizontal, or vertical;1 = aligned ...

  TDxfCommonDimensionData = class
    protected
      DimStyleName: string;  //dimension style name,
      OriginX: real;         //X of definition point used to specify the dimension line.
      MiddleTextX: real;     //X of middle point used to specify the dimension text location
      OriginY: real;
      MiddleTextY: real;
      OriginZ: real;
      MiddleTextZ: real;
      Measurement: real;
      DimType: integer;
     public
      constructor Create;
      Destructor Destroy; Override;
  end;

   {TDxfRadialDimension}
  TDxfRadialDimension = class(TDxfCommonDimensionData)
     private
       FlagControl: integer;
       Arrow1, Arrow2: TDxfSolidArrowHead;
       DimText: TDxfText;
       DimLine: TDxfLine;
     public
       BlockName: string;
       DefPoint3X: real;
       DefPoint3Y: real;
       DefPoint3Z: real;
       LeaderLength: real;
       RAngle: real;
       DxfCode: TDxfGenericDimensionCode;
       DxfLayer:  TDxfLayer;
       function ToDXF: string;
       procedure CreateBlock(layerName, anounymousBlockName, typeName: string; objDimStyle: TDxfDimStyle;
                          cx, cy, r, angle: real);
       procedure CreateEntity(layerName, anounymousBlockName, typeName, styleName: string;
                          cx, cy, r, angle: real);
       constructor Create(layerName, anounymousBlockName, typeName, styleName: string;
                          cx, cy, r, angle: real);
       constructor Create(layerName, anounymousBlockName, typeName: string; objDimStyle: TDxfDimStyle;
                          cx, cy, r, angle: real);
       destructor Destroy; override;
   end;

   {TDxfAngular3PDimension}
   TDxfAngular3PDimension = class(TDxfCommonDimensionData)
      private
       FlagControl: integer;
       DimText: TDxfText;
       Arrow1, Arrow2: TDxfSolidArrowHead;
       ExtLine1: TDxfLine;
       ExtLine2: TDxfLine;
       DimArc: TDxfArc;
      public
       BlockName: string;
       Angle: real;
       DefPoint1X: real;
       DefPoint1Y: real;
       DefPoint1Z: real;
       DefPoint2X: real;
       DefPoint2Y: real;
       DefPoint2Z: real;
       DefPoint3X: real;
       DefPoint3Y: real;
       DefPoint3Z: real;
       DxfCode: TDxfGenericDimensionCode;
       DxfLayer:  TDxfLayer;
       function ToDXF: string;
       procedure CreateBlock(layerName, anounymousBlockName, typeName: string;
                    objDimStyle: TDxfDimStyle; offset: real; cx, cy, r, startAngle, endAngle: real);
       procedure CreateEntity(layerName, anounymousBlockName, styleName: string; offset: real;
                         cx, cy, r, startAngle, endAngle: real);

       constructor Create(layerName, anounymousBlockName, styleName: string; offset: real;
                          cx, cy, r, startAngle, endAngle: real);
       constructor Create(layerName, anounymousBlockName, typeName: string;
                      objDimStyle: TDxfDimStyle; offset: real; cx, cy, r, startAngle, endAngle: real);
       destructor Destroy; override;
   end;

  {TDxfLinearDimension}
  TDxfLinearDimension = class(TDxfCommonDimensionData)
    private
      FlagControl: integer;
      DimText: TDxfText;
      Arrow1, Arrow2: TDxfSolidArrowHead;
      DimLine: TDxfLine;
      ExtLine1: TDxfLine;
      ExtLine2: TDxfLine;
    public
      BlockName: string;
      DefPoint1X: real;  //13
      DefPoint2X: real;  //14
      DefPoint1Y: real;  //23
      DefPoint2Y: real;  //24
      DefPoint1Z: real;  //33
      DefPoint2Z: real;  //34
      Angle: real;       //50
      DxfCode: TDxfGenericDimensionCode;
      DxfLayer:  TDxfLayer;
      function ToDXF: string;
      procedure CreateBlock(layerName, anounymousBlockName, typeName: string; objDimStyle: TDxfDimStyle;
                            offset, x1, y1, x2, y2: real);
      procedure CreateEntity(layerName, anounymousBlockName, typeName, styleName: string;
                         offset, x1, y1, x2, y2: real);
      constructor Create(layerName, anounymousBlockName, typeName, styleName: string;
                           offset, x1, y1, x2, y2: real);
      constructor Create(layerName, anounymousBlockName, typeName: string; objDimStyle: TDxfDimStyle;
                           offset, x1, y1, x2, y2: real);
      destructor Destroy; override;
  end;

  TProduceEntity = procedure(out entityDXF: string) of object;
  TProduceBlock = procedure(out blockDXF: string) of object;

  {TFPDxfWriteBridge}
  TFPDxfWriteBridge = class(TComponent)
     private
        DimBlockList: TList;
        FOnProduceEntity: TProduceEntity;
        FOnProduceBlock: TProduceBlock;
        function RadialOrDiameterDimension(layerName, anounymousBlockName, typeName, styleName: string;
                                    cx, cy, r, angle: real): string;
        function LinearOrAlignedDimension(layerName, anounymousBlockName, typeName, styleName: string;
                                    offset, x1, y1, x2, y2: real): string;
        function AngularOrArc3PDimension(layerName, anounymousBlockName, typeName, styleName: string;
                                    offset: real; cx, cy, r, startAngle, endAngle: real): string;
     public

        RestrictedLayer: string;
        ToDxf: TStringList;

        LayerList: TList;
        LineTypeList: TList;
        TextStyleList: TList;
        DimStyleList: TList;

        procedure Produce(selectLayer: string);
        procedure DoProduceDXFEntity(out entityDXF: string);

        procedure DoProduceDXFBlock(out blockDXF: string);

        procedure AddLayer(LayerName, lineType: string; color: integer);
        procedure DeleteLayerByIndex(index: integer);
                                                          //'CENTER',[1.25,-0.25, 0.25, -0.25],'____ _ ____ _ '
        procedure AddLineType(lineTypeName: string; V: array of real; linePattern: string);
        procedure AddTextStyle(fontName: string; fontFileName: string; fontHeight: real);
        procedure AddDimStyle(styleName: string; arrwSize, arrwWidth: real; color: integer;
                                txtHeight: real);
        procedure DxfBegin;

        procedure BeginTables;
        procedure BeginTable(tabName: string; count: integer);

        procedure AddTableLType(tabName: string; V: array of real; graphicsPattern: string);
        procedure AddTableTextStyle(fontName, fontFileName:  string; fontHeight: real);

        procedure AddTableLayer(tabName, lineType: string; color: integer);
        procedure AddTableLayer(tabName, lineType: string; color: TDXFColor);
        procedure AddTableLayer(tabName, lineType: string; color: string);

        procedure AddTableDimStyle(dimStyleName: string; arrwSize, arrwWidth: real;
                                    color: integer; txtHeight:real);
        //procedure AddTableUCS(tabName: string);

        procedure EndTable;
        procedure EndTables;

        procedure BeginBlocks;
        function BeginBlock(layerName, blockName: string; X,Y: real): string;

        procedure AddBlock(dxfBlock: string);

        function EndBlock: string;
        procedure EndBlocks;

        procedure BeginEntities;

        procedure AddEntity(dxfEntity: string);
        procedure AddEntity(objEntity: TObject);

        procedure EndEntities;

        function InsertBlock(blockName: string; x,y: real): string;

        procedure DxfEnd;

  	function Circle(layerName: string; x, y, radius: real): string;
        function Arc(layerName: string; x, y, radius, startAngle, endAngle: real): string;
  	function Point(layerName: string; x, y: real): string;
        function Text(layerName: string; angle, height, x, y: real; txt: string): string;
        function MText(layerName: string; angle, height, x, y: real; txt: string): string;
        function SolidArrowHead(layerName: string; V: array of TRealPoint): string;
  	function Line(layerName: string; x1, y1, x2, y2: real): string;
        function LWPolyLine(layerName: string; V: array of TRealPoint; closed: boolean): string;
        function PolyLine(layerName: string; V: array of TRealPoint; closed: boolean): string;

        function LinearAlignedDimension(layerName, anounymousBlockName, styleName: string;
                                    offset, x1, y1, x2, y2: real): string;
        function LinearHorizontalDimension(layerName, anounymousBlockName, styleName: string;
                                    offset, x1, y1, x2, y2: real): string;
        function LinearVerticalDimension(layerName, anounymousBlockName, styleName: string;
                                    offset, x1, y1, x2, y2: real): string;
        function RadialDimension(layerName, anounymousBlockName, styleName: string;
                                    cx, cy, r, angle: real): string;
        function DiameterDimension(layerName, anounymousBlockName, styleName: string;
                                    cx, cy, r, angle: real): string;
        function Angular3PDimension(layerName, anounymousBlockName, styleName: string; offset: real;
                                    cx, cy, r, startAngle, endAngle: real): string;
        function Arc3PDimension(layerName, anounymousBlockName, styleName: string; offset: real;
                                    cx, cy, r, startAngle, endAngle: real): string;

        function Dimension(AnounymousDimensionanounymousBlockName: string): string;

        function LinearAlignedDimension(layerName, styleName: string;
                                    offset, x1, y1, x2, y2: real): string;
        function LinearHorizontalDimension(layerName, styleName: string;
                                    offset, x1, y1, x2, y2: real): string;
        function LinearVerticalDimension(layerName, styleName: string;
                                    offset, x1, y1, x2, y2: real): string;
        function RadialDimension(layerName, styleName: string;
                                    cx, cy, r, angle: real): string;
        function DiameterDimension(layerName, styleName: string;
                                    cx, cy, r, angle: real): string;
        function Angular3PDimension(layerName, styleName: string; offset: real;
                                    cx, cy, r, startAngle, endAngle: real): string;
        function Arc3PDimension(layerName, styleName: string; offset: real;
                                    cx, cy, r, startAngle, endAngle: real): string;

        procedure SaveToFile(path: string);
        constructor Create(AOwner: TComponent); override;
  	destructor Destroy; override;
     published
        property OnProduceEntity: TProduceEntity read FOnProduceEntity write FOnProduceEntity;
        property OnProduceBlock: TProduceBlock read FOnProduceBlock write FOnProduceBlock;
  end;

(*TODO:
  {TDxfBridge}
  TDxfBridge = class
    public
        Write: TFPDxfWriteBridge;
        Read: TDxfReadBridge; //TODO:
  	constructor Create;
  	destructor Destroy; override;
  end;
*)

function ReplaceChar(query: string; oldchar, newchar: char):string;

implementation

function ReplaceChar(query: string; oldchar, newchar: char):string;
begin
  if query <> '' then
  begin
     while Pos(oldchar,query) > 0 do query[pos(oldchar,query)]:= newchar;
     Result:= query;
  end;
end;

constructor TDxfCommonData.Create;
begin
    //
end;

destructor TDxfCommonData.Destroy;
begin
  //
  inherited Destroy;
end;

constructor TDxfDataText.Create;
begin
    //
end;

destructor TDxfDataText.Destroy;
begin
  //
  inherited Destroy;
end;

constructor TDxfDataPolyLine.Create;
begin
    //
end;

destructor TDxfDataPolyLine.Destroy;
begin
  //
  inherited Destroy;
end;

constructor TDxfCommonDimensionData.Create;
begin
    //
end;

destructor TDxfCommonDimensionData.Destroy;
begin
  //
  inherited Destroy;
end;

{TDxfDimStyle}
function TDxfDimStyle.ToDXF: string;
var
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;

   dxfList.Add('0');
   dxfList.Add('DIMSTYLE');
   dxfList.Add('100');
   dxfList.Add('AcDbSymbolTableRecord');
   dxfList.Add('100');
   dxfList.Add('AcDbDimStyleTableRecord');
   dxfList.Add('2');
   dxfList.Add(DimStyleName);

   dxfList.Add(intToStr(Ord(TDxfDimStyleCode.DIMSTANDARD_FLAGS))); //DxfCode
   dxfList.Add(intToStr(DimStandardFlags));

   dxfList.Add(intToStr(Ord(TDxfDimStyleCode.VERTICAL_TEXT_LOCATION))); //DxfCode
   dxfList.Add(intToStr(VerticalTextLocation));

   dxfList.Add(intToStr(Ord(TDxfDimStyleCode.LINE_AND_ARROW_HEAD_COLOR)));
   dxfList.Add(intToStr(LineAndArrowHeadColor));
   dxfList.Add(intToStr(Ord(TDxfDimStyleCode.EXTENSION_LINE_COLOR)));
   dxfList.Add(intToStr(ExtensionLineColor));
   dxfList.Add(intToStr(Ord(TDxfDimStyleCode.TEXT_COLOR)));
   dxfList.Add(intToStr(TextColor));

   dxfList.Add(intToStr(Ord(TDxfDimStyleCode.ARROW_SIZE)));
   dxfList.Add(ReplaceChar(FloatToStrF(ArrowSize, ffFixed,0,2) , ',', '.'));

   dxfList.Add(intToStr(Ord(TDxfDimStyleCode.LINE_SIZE_PASSING)));
   dxfList.Add(ReplaceChar(FloatToStrF(LineSizePassing, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfDimStyleCode.EXTENSION_LINE_SIZE_PASSING)));
   dxfList.Add(ReplaceChar(FloatToStrF(ExtensionLinePassing, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfDimStyleCode.OFFSET_EXTENSION_LINE_FROM_ORIGIN)));
   dxfList.Add(ReplaceChar(FloatToStrF(OffsetExtensionLineFromOrigin, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfDimStyleCode.OFFSET_FROM_DIMENSION_LINE_TO_TEXT)));
   dxfList.Add(ReplaceChar(FloatToStrF(OffsetFromDimensionLineToText, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfDimStyleCode.TEXT_HEIGHT)));
   dxfList.Add(ReplaceChar(FloatToStrF(TextHeight, ffFixed,0,2),',','.'));
    //autocad need!
   dxfList.Add('3');
   dxfList.Add('');
   dxfList.Add('4');
   dxfList.Add('');
   dxfList.Add('5');
   dxfList.Add('');
   dxfList.Add('6');
   dxfList.Add('');
   dxfList.Add('7');
   dxfList.Add('');
   dxfList.Add('40'); //DIMSCALE
  dxfList.Add('1.0');
   dxfList.Add('43'); //DIMDLI
  dxfList.Add('0.38');    {metric} {0.38 = imperial}
   dxfList.Add('45'); //DIMRND
  dxfList.Add('0.0');
   dxfList.Add('47'); //DIMTP
  dxfList.Add('0.0');
   dxfList.Add('48'); //DIMTM
  dxfList.Add('0.0');
  dxfList.Add('141'); //DIMCEN
  dxfList.Add('0.09');
  dxfList.Add('142'); //DIMTSZ
  dxfList.Add('0.0');
  dxfList.Add('143'); //DIMALTF
  dxfList.Add('25.39999');
  dxfList.Add('144'); //DIMLFAC
  dxfList.Add('1.0');
  dxfList.Add('145'); //DIMTVP
  dxfList.Add('0.0');
  dxfList.Add('146'); //DIMTFAC
  dxfList.Add('1.0');
  dxfList.Add('71');
  dxfList.Add('0');
  dxfList.Add('72');
  dxfList.Add('0');
  dxfList.Add('73');
  dxfList.Add('1');
  dxfList.Add('74');
  dxfList.Add('1');
  dxfList.Add('75');
  dxfList.Add('0');
  dxfList.Add('76');
  dxfList.Add('0');
  dxfList.Add('78');
  dxfList.Add('0');
  dxfList.Add('170');
  dxfList.Add('0');
  dxfList.Add('171');
  dxfList.Add('2');
  dxfList.Add('172');
  dxfList.Add('0');
  dxfList.Add('173');
  dxfList.Add('0');
  dxfList.Add('174');
  dxfList.Add('0');
  dxfList.Add('175');
  dxfList.Add('0');

  Result:= Trim(dxfList.Text);

  dxfList.Free;
end;

//AddDimStyle('CUSTOM', 0.1800{arrwSize}, 0.0625{arrwWidth} , 2{color}, 0.1800 {0.25});
constructor TDxfDimStyle.Create(styleName: string; arrwSize, arrwWidth: real; color: integer;
                                txtHeight: real);
begin
         DimStandardFlags:= 0;
         if (styleName = '') or (Pos(styleName,'GENERIC') > 0 )  then
         begin
           DimStyleName:= 'GENERIC';
 {DIMCLRD} LineAndArrowHeadColor:= 256;
 {DIMDLE}  LineSizePassing:= 0.0; //0:for arrow; OR <>0: for tickes/strok...line size after Extensionline
 {DIMCLRE} ExtensionLineColor:=  256;
 {DIMEXE}  ExtensionLinePassing:= 0.1800;   {or 1.2500 metric}  // Extension line size after Dimension line
 {DIMEXO}  OffsetExtensionLineFromOrigin:= 0.0625;    //distance from extension line from baseline/shape
 {DIMASZ}  ArrowSize:=  0.1800;
           ArrowWidth:= 0.0625;
 {DIMCLRT} TextColor:= 256;
 {DIMTXT}  TextHeight:= 0.1800; {imperial 2.5 = metric}
 {DIMTIH}  PositionTextInsideExtensionLines:= 1; {1= force horizontal  0=metric}
 {DIMTAD}  VerticalTextLocation:= 0; {imperial} {1=metric}  //Place text above the dimension line
 {DIMGAP}  OffsetFromDimensionLineToText:= 0.0900;{0.6250 = metric} {imperial=0.0900};  //Gape from dimension line to text
         end
         else
         begin
            DimStyleName:= styleName;
  {DIMCLRD} LineAndArrowHeadColor:= color;
  {DIMDLE}  LineSizePassing:= 0.0;
  {DIMCLRE} ExtensionLineColor:= color;
  {DIMEXE}  ExtensionLinePassing:= 0.1800;
  {DIMEXO}  OffsetExtensionLineFromOrigin:= 0.0625;
  {DIMASZ}  ArrowSize:=  arrwSize;
            ArrowWidth:= arrwWidth;
  {DIMCLRT} TextColor:= color;
  {DIMTXT}  TextHeight:= txtHeight;
  {DIMTIH}  PositionTextInsideExtensionLines:= 1; {1= force horizontal}
  {DIMTAD}  VerticalTextLocation:= 0;
  {DIMGAP}  OffsetFromDimensionLineToText:= 0.0900; {0.6250 or 0.0900};
         end;
end;

Destructor TDxfDimStyle.Destroy;
begin
   //
   inherited Destroy;
end;

{TDxfLinearDimension}
function TDxfLinearDimension.ToDXF: string;
var
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;
   if FlagControl = 1 then   //Create Block
   begin
      dxfList.Add(DimLine.ToDXF);
      dxfList.Add(Arrow1.ToDXF);
      dxfList.Add(Arrow2.ToDXF);
      dxfList.Add(ExtLine1.ToDXF);
      dxfList.Add(ExtLine2.ToDXF);
      dxfList.Add(DimText.ToDXF);
      Result:= TrimRight(dxfList.Text);
   end
   else //Create Entity
   begin
         dxfList.Add('0');
         dxfList.Add('DIMENSION');
         dxfList.Add('100');
         dxfList.Add('AcDbEntity');
         dxfList.Add('8');
         dxfList.Add(DxfLayer.LayerName);
         dxfList.Add('100');
         dxfList.Add('AcDbDimension');
         dxfList.Add('2');
         dxfList.Add(BlockName);

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.ORIGIN_X)));
         dxfList.Add(ReplaceChar(FloatToStrF(OriginX, ffFixed,0,2), ',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.ORIGIN_Y)));
         dxfList.Add(ReplaceChar(FloatToStrF(OriginY, ffFixed,0,2), ',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.ORIGIN_Z)));
         dxfList.Add(ReplaceChar(FloatToStrF(OriginZ, ffFixed,0,2), ',','.'));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.MIDDLE_TEXT_X)));
         dxfList.Add(ReplaceChar(FloatToStrF(MiddleTextX, ffFixed,0,2), ',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.MIDDLE_TEXT_Y)));
         dxfList.Add(ReplaceChar(FloatToStrF(MiddleTextY, ffFixed,0,2), ',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.MIDDLE_TEXT_Z)));
         dxfList.Add(ReplaceChar(FloatToStrF(MiddleTextZ, ffFixed,0,2), ',','.'));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DIMTYPE)));
         dxfList.Add(intToStr(DimType));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DIM_STYLE_NAME)));
         dxfList.Add(DimSTyleName);

         dxfList.Add('100');
         dxfList.Add('AcDbAlignedDimension');

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT1_X)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint1X, ffFixed,0,2), ',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT1_Y)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint1Y, ffFixed,0,2), ',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT1_Z)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint1Z, ffFixed,0,2), ',','.'));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT2_X)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint2X, ffFixed,0,2), ',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT2_Y)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint2Y, ffFixed,0,2), ',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT2_Z)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint2Z, ffFixed,0,2), ',','.'));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.ANGLE)));
         dxfList.Add(ReplaceChar(FloatToStrF(Angle, ffFixed,0,2), ',','.'));

         if  DimType = 0  then
         begin
           dxfList.Add('100');
           dxfList.Add('AcDbRotatedDimension');
         end;
         Result:= Trim(dxfList.Text);
   end;
   dxfList.Free;
end;

procedure TDxfLinearDimension.CreateBlock(layerName, anounymousBlockName, typeName: string; objDimStyle: TDxfDimStyle;
                                          offset, x1, y1, x2, y2: real);
var
   {DIMEXE,} DIMEXO, DIMASZ{, DIMDLE}: real;
   px,py, px1,py1, px2,py2, angleDegree: real;
   orthoX1,orthoY1,orthoX2,orthoY2: real;
   distP1P2, dLineX1, dLineY1, dLineX2, dLineY2 : real;
   x, y: real;
begin
   FlagControl:= 1;
   BlockName:= anounymousBlockName;
   if objDimStyle <> nil then
   begin
       //DIMEXE:= objDimStyle.ExtensionLinePassing;  {0.1}
       DIMEXO:= objDimStyle.OffsetExtensionLineFromOrigin; {0.1}
       DIMASZ:= objDimStyle.ArrowSize;  {0.2}
       //DIMDLE:= 0.0; DimStyle.DimesionLineSizePassing; {0.0}
   end
   else
   begin
      //DIMEXE:= 0.1800;  {1.25}
      DIMEXO:= 0.0625;  {metric = 0.625}
      DIMASZ:= 0.1800;  {2.5}
      //DIMDLE:= 0.0;
   end;
   x:=x1;
   y:=y1;
   if typeName = 'H' then y:=y2;
   if typeName = 'V' then x:=x2;

   distP1P2:= sqrt(sqr(x2-x) + sqr(y2-y));

   GetLineParallel(offset, x, y, x2, y2, dLineX1,dLineY1,dLineX2,dLineY2);
   DimLine:= TDxfLine.Create(layerName,dLineX1,dLineY1,dLineX2,dLineY2);

   GetLineOrthogonal(-DIMASZ{offset}, objDimStyle.ArrowWidth {r}, dLineX1,dLineY1,dLineX2,dLineY2,
                        orthoX1,orthoY1,orthoX2,orthoY2, px, py, 1);
   Arrow1:= TDxfSolidArrowHead.Create(layerName,[ToRealPoint(orthoX1,orthoY1),ToRealPoint(orthoX2,orthoY2),
                                        ToRealPoint(dLineX1,dLineY1),ToRealPoint(dLineX1,dLineY1)]);

   GetLineOrthogonal(-DIMASZ{offset}, objDimStyle.ArrowWidth {r}, dLineX1,dLineY1,dLineX2,dLineY2,
                       orthoX1,orthoY1,orthoX2,orthoY2, px, py, 2);
   Arrow2:= TDxfSolidArrowHead.Create(layerName,[ToRealPoint(orthoX1,orthoY1),ToRealPoint(orthoX2,orthoY2),
                                        ToRealPoint(dLineX2,dLineY2),ToRealPoint(dLineX2,dLineY2)]);

   GetLineTranslated(dimexo,x, y, dLineX1,dLineY1, px1, py1, px2, py2);
   ExtLine1:= TDxfLine.Create(layerName, px1, py1, px2, py2);

   GetLineTranslated(dimexo,x2, y2, dLineX2,dLineY2, px1, py1, px2, py2);
   ExtLine2:= TDxfLine.Create(layerName, px1, py1, px2, py2);

   Angle:= GetAngleOfLine(dLineX1,dLineY1,dLineX2,dLineY2);
   angleDegree:= ToDegrees(angle);
   DimText:= TDxfText.Create(layerName, angleDegree, objDimStyle.TextHeight, (dLineX1+dLineX2)/2,(dLineY1+dLineY2)/2,
                              ReplaceChar(FloatToStrF(distP1P2,ffFixed,0,2),',','.'));
end;

procedure TDxfLinearDimension.CreateEntity(layerName, anounymousBlockName, typeName, styleName: string;
                   offset, x1, y1, x2, y2: real);
var
   ang, angleDegree: real;
   distP1P2, dLineX1, dLineY1, dLineX2, dLineY2 : real;
   x,y: real;
begin
   FlagControl:=2;
   BlockName:= anounymousBlockName;

   if layerName = '' then DxfLayer.LayerName:='0'
   else DxfLayer.LayerName:= layerName;  //'0';default autocad layer!

   DxfLayer.Color:= 256; //0 : BYBLOCK.
   DxfLayer.LineType:= 'BYLAYER';

   if styleName <> '' then
      DimSTyleName:= styleName
   else DimSTyleName:= 'GENERIC';

   x:=x1;
   y:=y1;
   if typeName = 'A' then
   begin
       ang:=GetAngleOfLine(x, y, x2, y2);
       DimType:= 33;  //1=aligned or 33!;
   end;

   if  typeName = 'H' then
   begin
       y:=y2;
       DimType:= 32;  //0= Linear horizontal  or 32!
       ang:=0;
   end;
   if typeName = 'V' then
   begin
       x:=x2;
       DimType:= 32;  //0= Linear vertical;
       ang:= PI/2;
   end;
   distP1P2:= sqrt(sqr(x2-x)+sqr(y2-y));
   Measurement:= distP1P2;

   GetLineParallel(offset, x, y, x2, y2, dLineX1,dLineY1,dLineX2,dLineY2);

   angleDegree:= ToDegrees(ang);
   Angle:= angleDegree; //50 angle of rotated, horizontal, or vertical Aligned dimensions

   MiddleTextX:=(dLineX1+dLineX2)/2;
   MiddleTextY:=(dLineY1+dLineY2)/2;
   MiddleTextZ:=0.0;

   DefPoint1X:= x;  //13,
   DefPoint2X:= x2;  //14,
   DefPoint1Y:= y;  //23,
   DefPoint2Y:= y2;  //24,
   DefPoint1Z:= 0.0; //33,
   DefPoint2Z:= 0.0; //34,

   OriginX:= dLineX1;
   OriginY:= dLineY1;
   OriginZ:= 0.0;
end;

//create Entity
constructor TDxfLinearDimension.Create(layerName, anounymousBlockName, typeName, styleName: string;
                      offset, x1, y1, x2, y2: real);
begin
   CreateEntity(layerName, anounymousBlockName, typeName,  styleName, offset, x1, y1, x2, y2);
end;

//create Block
constructor TDxfLinearDimension.Create(layerName, anounymousBlockName, typeName: string; objDimStyle: TDxfDimStyle;
                    offset, x1, y1, x2, y2: real);
begin
   CreateBlock(layerName, anounymousBlockName, typeName, objDimStyle, offset, x1, y1, x2, y2);
end;

destructor TDxfLinearDimension.Destroy;
begin
   DimText.Free;
   Arrow1.Free;
   Arrow2.Free;
   DimLine.Free;
   ExtLine1.Free;
   ExtLine2.Free;
   inherited Destroy;
end;

{TDxfRadialDimension}
function TDxfRadialDimension.ToDXF: string;
var
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;
   if flagControl = 1 then   //Create Block
   begin
      dxfList.Add(DimLine.ToDXF);
      dxfList.Add(Arrow1.ToDXF);
      dxfList.Add(Arrow2.ToDXF);
      dxfList.Add(DimText.ToDXF);
      Result:= TrimRight(dxfList.Text);
   end
   else //Create Entity
   begin
         dxfList.Add('0');
         dxfList.Add('DIMENSION');

         dxfList.Add('100');
         dxfList.Add('AcDbEntity');
         dxfList.Add('8');
         dxfList.Add(DxfLayer.LayerName);
         dxfList.Add('100');
         dxfList.Add('AcDbDimension');
         dxfList.Add('2');
         dxfList.Add(BlockName);

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.ORIGIN_X)));
         dxfList.Add(ReplaceChar(FloatToStrF(OriginX, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.ORIGIN_Y)));
         dxfList.Add(ReplaceChar(FloatToStrF(OriginY, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.ORIGIN_Z)));
         dxfList.Add(ReplaceChar(FloatToStrF(OriginZ, ffFixed,0,2),',','.'));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.MIDDLE_TEXT_X)));
         dxfList.Add(ReplaceChar(FloatToStrF(MiddleTextX, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.MIDDLE_TEXT_Y)));
         dxfList.Add(ReplaceChar(FloatToStrF(MiddleTextY, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.MIDDLE_TEXT_Z)));
         dxfList.Add(ReplaceChar(FloatToStrF(MiddleTextZ, ffFixed,0,2),',','.'));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DIMTYPE)));
         dxfList.Add(intToStr(DimType));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DIM_STYLE_NAME)));
         dxfList.Add(DimSTyleName);
         //4 = radius;
         if  DimType = 4  then
         begin
            dxfList.Add('100');
            dxfList.Add('AcDbRadialDimension');
         end
         else
         begin // 3 = diameter
            dxfList.Add('100');
            dxfList.Add('AcDbDiameterDimension');
         end;
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT3_X)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint3X, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT3_Y)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint3Y, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT3_Z)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint3Z, ffFixed,0,2),',','.'));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.LEADER_LENGTH)));
         dxfList.Add(ReplaceChar(FloatToStrF(LeaderLength, ffFixed,0,2),',','.'));

         Result:= Trim(dxfList.Text);
   end;
   dxfList.Free;
end;

procedure TDxfRadialDimension.CreateBlock(layerName, anounymousBlockName, typeName: string; objDimStyle: TDxfDimStyle;
                         cx, cy, r, angle: real);
var
   DIMASZ: real;
   px,py, angleRad: real;
   orthoX1,orthoY1,orthoX2,orthoY2: real;
   defOrigx,defOrigy, rx, ry, defPx, defPy: real;
begin
   FlagControl:=1;
   BlockName:= anounymousBlockName;

   angleRad:= ToRadians(angle);
   RAngle:= angleRad;

   rx:= r*cos(angleRad);
   ry:= r*sin(angleRad);
   defPx:= cx + rx;
   defPy:= cy + ry;

   if typeName = 'R' then  //radial  =4
   begin
       defOrigx:= cx;
       defOrigy:= cy;
       LeaderLength:= r;
   end;

   if typeName = 'D' then  //diameter  = 3
   begin
        defOrigx:= cx - rx;
        defOrigy:= cy - ry;
        LeaderLength:= 2*r;
   end;

   if objDimStyle <> nil then
      DIMASZ:= objDimStyle.ArrowSize  {0.2}
   else DIMASZ:= 0.1800;  {2.5}

   DimLine:= TDxfLine.Create(layerName, defOrigx, defOrigy, defPx, defPy);

   GetLineOrthogonal(-DIMASZ{offset }, objDimStyle.ArrowWidth {r}, defOrigx, defOrigy, defPx, defPy,
                        orthoX1,orthoY1,orthoX2,orthoY2, px, py, 1);

   Arrow1:= TDxfSolidArrowHead.Create(layerName,[ToRealPoint(orthoX1,orthoY1),ToRealPoint(orthoX2,orthoY2),
                                        ToRealPoint(defOrigx, defOrigy),ToRealPoint(defOrigx,defOrigy)]);

   GetLineOrthogonal(-DIMASZ{offset }, objDimStyle.ArrowWidth {r}, defOrigx, defOrigy, defPx, defPy,
                        orthoX1,orthoY1,orthoX2,orthoY2, px, py, 2);

   Arrow2:= TDxfSolidArrowHead.Create(layerName,[ToRealPoint(orthoX1,orthoY1),ToRealPoint(orthoX2,orthoY2),
                                        ToRealPoint(defPx, defPy),ToRealPoint(defPx, defPy)]);

   DimText:= TDxfText.Create(layerName, angle, objDimStyle.TextHeight, (defOrigx+defPx)/2,(defOrigy+defPy)/2,
                              ReplaceChar(FloatToStrF(LeaderLength,ffFixed,0,2),',','.'));
end;


procedure TDxfRadialDimension.CreateEntity(layerName, anounymousBlockName, typeName, styleName: string;
                                           cx, cy, r, angle: real);
var
   angleRad: real;
   defPx, defPy, rx, ry: real;
begin
   FlagControl:=2;
   DxfLayer.Color:= 256;
   DxfLayer.LineType:= 'BYLAYER';
   BlockName:= anounymousBlockName;

   if layerName = '' then DxfLayer.LayerName:='0'
   else DxfLayer.LayerName:= layerName;

   if styleName <> '' then
      DimSTyleName:= styleName
   else DimSTyleName:= 'GENERIC';

   angleRad:= ToRadians(angle);
   rx:= r*cos(angleRad);
   ry:= r*sin(angleRad);
   defPx:= cx + rx;
   defPy:= cy + ry;

   DefPoint3X:= defPx;  //15
   DefPoint3Y:= defPy;
   DefPoint3Z:= 0.0;

   if typeName = 'R' then  //Radial = 4
   begin
       DimType:= 4;
       Measurement:= r;
       OriginX:= cx; //10
       OriginY:= cy;
       OriginZ:= 0.0;
       MiddleTextX:=(cx+defPx)/2;  //11
       MiddleTextY:=(cy+defPy)/2;
       MiddleTextZ:=0.0;
   end;

   if typeName = 'D'  then //diameter =3
   begin
       DimType:= 3;
       Measurement:= 2*r; // distP1P2;
       OriginX:= cx - rx; //10
       OriginY:= cy - ry;
       OriginZ:= 0.0;
       MiddleTextX:=cx;  //11
       MiddleTextY:=cy;
       MiddleTextZ:=0.0;
   end;
end;

//create Entity
constructor TDxfRadialDimension.Create(layerName, anounymousBlockName, typeName, styleName: string;
                          cx{10}, cy, r, angle: real);
begin
    CreateEntity(layerName, anounymousBlockName, typeName, styleName, cx, cy, r, angle);
end;

//create Block
constructor TDxfRadialDimension.Create(layerName, anounymousBlockName, typeName: string; objDimStyle: TDxfDimStyle;
                                       cx, cy, r, angle: real);
begin
    CreateBlock(layerName, anounymousBlockName, typeName, objDimStyle, cx, cy, r, angle);
end;

destructor TDxfRadialDimension.Destroy;
begin
   DimText.Free;
   Arrow1.Free;
   Arrow2.Free;
   DimLine.Free;
   inherited Destroy;
end;

{TDxfAngular3PDimension}
function TDxfAngular3PDimension.ToDXF: string;
var
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;
   if FlagControl = 1 then   //Create Block
   begin
      dxfList.Add(DimArc.ToDXF);
      dxfList.Add(Arrow1.ToDXF);
      dxfList.Add(Arrow2.ToDXF);
      dxfList.Add(ExtLine1.ToDXF);
      dxfList.Add(ExtLine2.ToDXF);
      dxfList.Add(DimText.ToDXF);
      Result:= TrimRight(dxfList.Text);
   end
   else //Create Entity
   begin
         dxfList.Add('0');
         dxfList.Add('DIMENSION');
         dxfList.Add('100');
         dxfList.Add('AcDbEntity');
         dxfList.Add('8');
         dxfList.Add(DxfLayer.LayerName);
         dxfList.Add('100');
         dxfList.Add('AcDbDimension');

         dxfList.Add('2');
         dxfList.Add(BlockName);

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.ORIGIN_X)));
         dxfList.Add(ReplaceChar(FloatToStrF(OriginX, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.ORIGIN_Y)));
         dxfList.Add(ReplaceChar(FloatToStrF(OriginY, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.ORIGIN_Z)));
         dxfList.Add(ReplaceChar(FloatToStrF(OriginZ, ffFixed,0,2),',','.'));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.MIDDLE_TEXT_X)));
         dxfList.Add(ReplaceChar(FloatToStrF(MiddleTextX, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.MIDDLE_TEXT_Y)));
         dxfList.Add(ReplaceChar(FloatToStrF(MiddleTextY, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.MIDDLE_TEXT_Z)));
         dxfList.Add(ReplaceChar(FloatToStrF(MiddleTextZ, ffFixed,0,2),',','.'));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DIMTYPE)));
         dxfList.Add(intToStr(DimType));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DIM_STYLE_NAME)));
         dxfList.Add(DimSTyleName);

         dxfList.Add('100');
         dxfList.Add('AcDbAngular3PDimension');

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT1_X)));   //13
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint1X, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT1_Y)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint1Y, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT1_Z)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint1Z, ffFixed,0,2),',','.'));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT2_X)));  //14
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint2X, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT2_Y)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint2Y, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT2_Z)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint2Z, ffFixed,0,2),',','.'));

         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT3_X)));  //15
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint3X, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT3_Y)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint3Y, ffFixed,0,2),',','.'));
         dxfList.Add(intToStr(Ord(TDxfGenericDimensionCode.DEF_POINT3_Z)));
         dxfList.Add(ReplaceChar(FloatToStrF(DefPoint3Z, ffFixed,0,2),',','.'));

         Result:= Trim(dxfList.Text);
   end;
   dxfList.Free;
end;

procedure TDxfAngular3PDimension.CreateBlock(layerName, anounymousBlockName, typeName: string; objDimStyle: TDxfDimStyle;
                         offset: real; cx, cy, r, startAngle {degrees}, endAngle{degrees}: real);
var
   DIMASZ,{DIMEXE,} DIMEXO: real;
   {arrowWidh: real;   }
   ortX1,ortY1,ortX2,ortY2: real;
   auxX1,auxY1,auxX2,auxY2: real;
   arcLen: real;
   startAngRad,endAngRad, midleAngRad: real;

   startOrigAngRad,endOrigAngRad: real;
   endOrigdefPx, endOrigdefPy, endOrigrx, endOrigry: real;
   startOrigdefPx, startOrigdefPy, startOrigrx, startOrigry: real;

   enddefPx, enddefPy, endrx, endry: real;
   startdefPx, startdefPy, startrx, startry: real;
   k, offsetX, offsetY: real;
   midledefPx, midledefPy, midlerx, midlery: real;
begin
   FlagControl:= 1;
   BlockName:= anounymousBlockName;
   {arrowWidh:= 0.1; }

   startOrigAngRad:= ToRadians(startAngle);
   startOrigrx:= r*cos(startOrigAngRad);
   startOrigry:= r*sin(startOrigAngRad);
   startOrigdefPx:= cx + startOrigrx;   //13 {P1}
   startOrigdefPy:= cy + startOrigry;

   endOrigAngRad:= ToRadians(endAngle);
   endOrigrx:= r*cos(endOrigAngRad);
   endOrigry:= r*sin(endOrigAngRad);
   endOrigdefPx:= cx + endOrigrx;  //14  {P2}
   endOrigdefPy:= cy + endOrigry;

   startAngRad:= ToRadians(startAngle);
   startrx:= (r + offset)*cos(startAngRad);
   startry:= (r + offset)*sin(startAngRad);
   startdefPx:= cx + startrx;   //13 {P1}
   startdefPy:= cy + startry;

   endAngRad:= ToRadians(endAngle);
   endrx:= (r + offset)*cos(endAngRad);
   endry:= (r + offset)*sin(endAngRad);
   enddefPx:= cx + endrx;  //14  {P2}
   enddefPy:= cy + endry;

   midleAngRad:= ToRadians(((endAngle + startAngle)/2));
   midlerx:= (r + offset)*cos(midleAngRad);
   midlery:= (r + offset)*sin(midleAngRad);
   midledefPx:= cx + midlerx;   //11
   midledefPy:= cy + midlery;

   if objDimStyle <> nil then
   begin
       //DIMEXE:= objDimStyle.ExtensionLinePassing;  {0.1}
       DIMEXO:= objDimStyle.OffsetExtensionLineFromOrigin; {0.1}
       DIMASZ:= objDimStyle.ArrowSize;  {0.2}
   end
   else
   begin
     DIMASZ:= 0.1800;  {2.5}
    // DIMEXE:= 0.1800;  {1.25}
     DIMEXO:= 0.0625;  {metric = 0.625} {DIMGAP:= 0.625}
   end;

   DimArc:= TDxfArc.Create(layerName, cx, cy, (r + offset), startAngle, endAngle);

   GetLineTranslated(DIMEXO, startOrigdefPx,startOrigdefPy, startdefPx,startdefPy, auxX1,auxY1,auxX2,auxY2);


   ExtLine1:= TDxfLine.Create(layerName, auxX1,auxY1,auxX2,auxY2);

   k:=1;
   if Abs(startAngle - 90) < 3 then k:= 0.55;

   GetLineOrthogonal(0.0 {offset}, 2*DIMASZ*k{dummy/r},startOrigdefPx,startOrigdefPy, startdefPx,startdefPy,
                                         ortX1,ortY1,ortX2,ortY2,offsetX, offsetY, 2);

   GetLineOrthogonal(- DIMASZ*k {offset}, objDimStyle.ArrowWidth{r}, ortX1,ortY1,ortX2,ortY2,
                                         auxX1,auxY1,auxX2,auxY2,offsetX, offsetY, {2} 1);

   Arrow1:= TDxfSolidArrowHead.Create(layerName, [ToRealPoint(auxX1,auxY1), ToRealPoint(auxX2,auxY2),
                                         ToRealPoint(startdefPx, startdefPy), ToRealPoint(startdefPx,startdefPy)]);

   GetLineTranslated(DIMEXO, endOrigdefPx, endOrigdefPy, enddefPx,enddefPy,
                                     auxX1,auxY1,auxX2,auxY2);

   ExtLine2:= TDxfLine.Create(layerName, auxX1,auxY1,auxX2,auxY2);

   k:=1;
   if Abs(endAngle - 90) < 3 then k:= 0.55;

   GetLineOrthogonal(0.0, 2*DIMASZ*k{dummy/r}, endOrigdefPx,endOrigdefPy, enddefPx,enddefPy,
                  ortX1,ortY1,ortX2,ortY2,offsetX, offsetY, 2);

   GetLineOrthogonal(-DIMASZ*k {offset},objDimStyle.ArrowWidth{r}, ortX1,ortY1,ortX2,ortY2,
                                          auxX1,auxY1,auxX2,auxY2,offsetX, offsetY, {1} 2);
   Arrow2:= TDxfSolidArrowHead.Create(layerName, [ToRealPoint(auxX1,auxY1), ToRealPoint(auxX2,auxY2),
                                                  ToRealPoint(enddefPx,enddefPy), ToRealPoint(enddefPx,enddefPy)]);
   arcLen:= r* ToRadians((endAngle-startAngle));

   if Pos(typeName, 'ARC') > 0 then
     DimText:= TDxfText.Create(layerName, ToDegrees(midleAngRad), objDimStyle.TextHeight,
                              midledefPx,midledefPy,ReplaceChar(FloatToStrF(ArcLen,ffFixed,0,2),',','.'))
   else  //'ANG'
     DimText:= TDxfText.Create(layerName,ToDegrees(midleAngRad), objDimStyle.TextHeight,
                              midledefPx,midledefPy,ReplaceChar(FloatToStrF(endAngle-startAngle,ffFixed,0,2),',','.'));
end;

procedure TDxfAngular3PDimension.CreateEntity(layerName, anounymousBlockName, styleName: string;
   offset: real; cx, cy, r, startAngle, endAngle: real);
var
   arcLen, startAngRad,endAngRad, origAngRad, midleAngRad: real;
   enddefPx, enddefPy, endrx, endry: real;
   startdefPx, startdefPy, startrx, startry: real;
   origdefPx, origdefPy, origrx, origry: real;
   midledefPx, midledefPy, midlerx, midlery: real;
begin
   FlagControl:=2;

   DxfLayer.Color:= 256; //BYLAYER
   DxfLayer.LineType:= 'BYLAYER';

   BlockName:= anounymousBlockName;

   if layerName = '' then DxfLayer.LayerName:='0'
   else DxfLayer.LayerName:= layerName;

   if styleName <> '' then
      DimSTyleName:= styleName
   else DimSTyleName:= 'GENERIC';

   startAngRad:= ToRadians(startAngle);
   startrx:= (r)*cos(startAngRad);
   startry:= (r)*sin(startAngRad);
   startdefPx:= cx + startrx;   //13 {P1}
   startdefPy:= cy + startry;

   endAngRad:= ToRadians(endAngle);
   endrx:= (r)*cos(endAngRad);
   endry:= (r)*sin(endAngRad);
   enddefPx:= cx + endrx;  //14  {P2}
   enddefPy:= cy + endry;

   origAngRad:= ToRadians(endAngle);
   origrx:= (r+offset)*cos(origAngRad);
   origry:= (r+offset)*sin(origAngRad);
   origdefPx:= cx + origrx;   //10 {P1} location of the dimension line arc
   origdefPy:= cy + origry;

   midleAngRad:= ToRadians((endAngle-startAngle));
   midlerx:= (r+offset)*cos(midleAngRad);
   midlery:= (r+offset)*sin(midleAngRad);
   midledefPx:= cx + midlerx;
   midledefPy:= cy + midlery;

   arcLen:= r*ToRadians(Abs((endAngle-startAngle)));

   DefPoint3X:= cx;  //15 {P3}
   DefPoint3Y:= cy;
   DefPoint3Z:= 0.0;

   DefPoint2X:= enddefPx;  //14 {P2}
   DefPoint2Y:= enddefPy;
   DefPoint2Z:= 0.0;

   DefPoint1X:= startdefPx;  //13  {P1}
   DefPoint1Y:= startdefPy;
   DefPoint1Z:= 0.0;

   DimType:= 5; //5 = Angular 3 point;
   Measurement:= arcLen;

   OriginX:= origdefPx; //10
   OriginY:= origdefPy;
   OriginZ:= 0.0;

   MiddleTextX:= midledefPx;  //11
   MiddleTextY:= midledefPy;
   MiddleTextZ:=0.0;
end;

//create Entity
constructor TDxfAngular3PDimension.Create(layerName, anounymousBlockName, styleName: string;
  offset: real; cx{10}, cy, r, startAngle, endAngle: real);
begin
  CreateEntity(layerName, anounymousBlockName, styleName, offset, cx, cy, r, startAngle, endAngle);
end;

//create Block
constructor TDxfAngular3PDimension.Create(layerName, anounymousBlockName, typeName: string; objDimStyle: TDxfDimStyle;
                                       offset: real; cx, cy, r, startAngle, endAngle: real);
begin
   CreateBlock(layerName, anounymousBlockName, typeName, objDimStyle, offset, cx, cy, r, startAngle, endAngle);
end;

destructor TDxfAngular3PDimension.Destroy;
begin
   DimText.Free;
   Arrow1.Free;
   Arrow2.Free;
   DimArc.Free;
   ExtLine1.Free;
   ExtLine2.Free;
   inherited Destroy;
end;

{TDxfTextStyle}
function TDxfTextStyle.ToDXF: string;
var
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;
   dxfList.Add('0');
   dxfList.Add('STYLE');
   dxfList.Add('2');
   dxfList.Add(TextStyleName);

   dxfList.Add(intToStr(Ord(TDxfTextStyleCode.GENERATION_FLAGS)));
   dxfList.Add(intToStr(GenerationFlags));

   dxfList.Add(intToStr(Ord(TDxfTextStyleCode.FIXED_HEIGHT)));
   dxfList.Add(ReplaceChar(FloatToStrF(FixedHeight, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfTextStyleCode.LAST_HEIGHT_USED)));
   dxfList.Add(ReplaceChar(FloatToStrF(LastHeightUsed, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfTextStyleCode.TEXT_ANGLE)));
   dxfList.Add(ReplaceChar(FloatToStrF(TextAngle, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfTextStyleCode.WIDTH_FACTOR)));
   dxfList.Add(ReplaceChar(FloatToStrF(WidthFactor, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfTextStyleCode.PRIMARY_FILE_NAME)));
   dxfList.Add(PrimaryFileName);
   dxfList.Add('100');
   dxfList.Add('AcDbSymbolTableRecord');
   dxfList.Add('100');
   dxfList.Add('AcDbTextStyleTableRecord');
   dxfList.Add('70');
   dxfList.Add('0'); //or 64

   Result:= Trim(dxfList.Text);
   dxfList.Free;
end;

constructor TDxfTextStyle.Create(textStyle: string; fontFileName: string; fontHeight: real);
begin
      if textStyle = '' then TextStyleName:= 'GENERIC'
      else TextStyleName:= textStyle;

      FixedHeight:= fontHeight;    //no fixed = 0.
      WidthFactor:= 1;
      GenerationFlags:= 0;
      LastHeightUsed:= 1.0;
      TextAngle:= 0;

      if fontFileName = '' then PrimaryFileName:= 'TXT' //dummy
      else PrimaryFileName:= fontFileName;
end;

destructor TDxfTextStyle.Destroy;
begin
    inherited Destroy;
end;

{TDxfLineType}
function TDxfLineType.ToDXF: string;
var
   dxfList: TStringList;
   i, count: integer;
begin
   dxfList:= TStringList.Create;
   dxfList.Add('0');
   dxfList.Add('LTYPE');
   dxfList.Add('2');
   dxfList.Add(LineTypeName);

   dxfList.Add('100');
   dxfList.Add('AcDbSymbolTableRecord');
   dxfList.Add('100');
   dxfList.Add('AcDbLinetypeTableRecord');

   dxfList.Add(intToStr(Ord(TDxfLineTypeCode.ASCII_LINE_PATTERN)));
   dxfList.Add(AsciiLinePatern);

   dxfList.Add(intToStr(Ord(TDxfLineTypeCode.ALIGNMENT)));
   dxfList.Add(intToStr(Alignment));

   dxfList.Add(intToStr(Ord(TDxfLineTypeCode.DASH_ELEMENT_COUNT)));
   dxfList.Add(intToStr(DashElementCount));

   dxfList.Add(intToStr(Ord(TDxfLineTypeCode.SUM_DASH_ELEMENT_LENGTH)));
   dxfList.Add(ReplaceChar(FloatToStrF(SUMDashElementLength, ffFixed,0,3),',','.'));

   count:= High(VectorDashElementLength) + 1;
   for i:= 0 to count - 1 do
   begin
      dxfList.Add(intToStr(Ord(TDxfLineTypeCode.DASH_ELEMENT_LENGTH)));
      dxfList.Add(ReplaceChar(FloatToStrF(VectorDashElementLength[i], ffFixed,0,3),',','.'));
   end;

   dxfList.Add('70');
   dxfList.Add('0');

   Result:= Trim(dxfList.Text);
   dxfList.Free;
end;

{lt:= TDxfLineType.Create('HIDDEN',[0.25,-0.125], 2 ,'__ __ __ ');
 lt:= TDxfLineType.Create('CENTER',[1.25,-0.25, 0.25, -0.25], 4 ,'____ _ ____ _ ');}
constructor TDxfLineType.Create(typeName: string; VectDashLen: array of Real; linePattern: string);
var
   i: integer;
begin
    LineTypeName:= typeName;
    AsciiLinePatern:= linePattern;
    Alignment:= 65; //'A'
    SUMDashElementLength:= 0;

    //if an empty array is passed, then High(..) returns -1
    DashElementCount:= High(VectDashLen) + 1;

    SetLength(VectorDashElementLength, DashElementCount);
    for i:= 0 to DashElementCount - 1 do
    begin
       VectorDashElementLength[i]:= VectDashLen[i];
       SUMDashElementLength:= SUMDashElementLength + Abs(VectorDashElementLength[i]);
    end;
end;

destructor TDxfLineType.Destroy;
begin
   SetLength(VectorDashElementLength, 0);
   VectorDashElementLength:= nil;
   inherited Destroy;
end;

{TDxfPoint}
function TDxfPoint.ToDXF: string;
var
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;

   dxfList.Add('0');
   dxfList.Add('POINT');
   dxfList.Add('100');
   dxfList.Add('AcDbEntity');
   dxfList.Add('8');
   dxfList.Add(DxfLayer.LayerName);
   dxfList.Add(intToStr(Ord(TDxfLayerCode.COLOR)));  //62    //DxfLayer.DxfCode.COLOR
   dxfList.Add(intToStr(DxfLayer.Color));
   dxfList.Add(intToStr(Ord(TDxfLayerCode.LINE_TYPE)));  //6
   dxfList.Add(DxfLayer.LineType);
   dxfList.Add(intToStr(Ord(TDxfCommonCode.THICKNESS)));  //39 //DxfCommonCode
   dxfList.Add(ReplaceChar(FloatToStrF(Thickness, ffFixed,0,2),',','.'));

   dxfList.Add('100');
   dxfList.Add('AcDbPoint');
   dxfList.Add(intToStr(Ord(TDxfCommonCode.X1)));  //10
   dxfList.Add(ReplaceChar(FloatToStrF(X1, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Y1)));  //20
   dxfList.Add(ReplaceChar(FloatToStrF(Y1, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Z1)));  //30
   dxfList.Add(ReplaceChar(FloatToStrF(Z1, ffFixed,0,2),',','.'));

   Result:= Trim(dxfList.Text);
   dxfList.Free;
end;

constructor TDxfPoint.Create(layerName: string; x, y: real);
begin
   X1:= x;
   Y1:= y;
   Z1:= 0.0;
   Thickness:= 0;
   DxfLayer.Color:= 256; //BYLAYER
   DxfLayer.LineType:= 'BYLAYER';

   if layerName = '' then DxfLayer.LayerName:= '0'
   else DxfLayer.LayerName:= layerName;
end;

destructor TDxfPoint.Destroy;
begin
   //
   inherited Destroy;
end;

{TDxfCircle}
function TDxfCircle.ToDXF: string;
var
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;

   dxfList.Add('0');
   dxfList.Add('CIRCLE');
   dxfList.Add('100');
   dxfList.Add('AcDbEntity');
   dxfList.Add('8');
   dxfList.Add(DxfLayer.LayerName);
   dxfList.Add(intToStr(Ord(TDxfLayerCode.COLOR)));  //62  //DxfLayer.DxfCode
   dxfList.Add(intToStr(DxfLayer.Color));
   dxfList.Add(intToStr(Ord(TDxfLayerCode.LINE_TYPE)));  //6
   dxfList.Add(DxfLayer.LineType);
   dxfList.Add(intToStr(Ord(TDxfCommonCode.THICKNESS)));  //39 //DxfCommonCode
   dxfList.Add(ReplaceChar(FloatToStrF(Thickness, ffFixed,0,2),',','.'));

   dxfList.Add('100');
   dxfList.Add('AcDbCircle');
   dxfList.Add(intToStr(Ord(TDxfCommonCode.X1)));  //10
   dxfList.Add(ReplaceChar(FloatToStrF(X1, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Y1)));  //20
   dxfList.Add(ReplaceChar(FloatToStrF(Y1, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Z1)));  //30
   dxfList.Add(ReplaceChar(FloatToStrF(Z1, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfCommonCode.SIZE)));  //40
   dxfList.Add(ReplaceChar(FloatToStrF(Radius, ffFixed,0,2),',','.'));

   Result:= Trim(dxfList.Text);
   dxfList.Free;
end;

constructor TDxfCircle.Create(layerName: string; x, y, r: real);
begin
    X1:= x;
    Y1:= y;
    Z1:= 0.0;
    Radius:= r;
    Thickness:= 0;
    DxfLayer.Color:= 256; //BYLAYER
    DxfLayer.LineType:= 'BYLAYER';

   if LayerName = '' then DxfLayer.LayerName:='0'
   else DxfLayer.LayerName:= layerName;
end;

destructor TDxfCircle.Destroy;
begin
    //
    inherited Destroy;
end;

{TDxfArc}
function TDxfArc.ToDXF: string;
var
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;

   dxfList.Add('0');
   dxfList.Add('ARC');
   dxfList.Add('100');
   dxfList.Add('AcDbEntity');
   dxfList.Add('8');
   dxfList.Add(DxfLayer.LayerName);
   dxfList.Add(intToStr(Ord(TDxfLayerCode.COLOR)));  //62 //DxfLayer.DxfCode
   dxfList.Add(intToStr(DxfLayer.Color)); //DxfLayer
   dxfList.Add(intToStr(Ord(TDxfLayerCode.LINE_TYPE)));  //6 DxfLayer.DxfCode
   dxfList.Add(DxfLayer.LineType);
   dxfList.Add(intToStr(Ord(TDxfCommonCode.THICKNESS)));  //39
   dxfList.Add(ReplaceChar(FloatToStrF(Thickness, ffFixed,0,2),',','.'));

   dxfList.Add('100');
   dxfList.Add('AcDbCircle');
   dxfList.Add(intToStr(Ord(TDxfCommonCode.X1)));  //10
   dxfList.Add(ReplaceChar(FloatToStrF(X1, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Y1)));  //20
   dxfList.Add(ReplaceChar(FloatToStrF(Y1, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Z1)));  //30
   dxfList.Add(ReplaceChar(FloatToStrF(Z1, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.SIZE)));  //40
   dxfList.Add(ReplaceChar(FloatToStrF(Radius, ffFixed,0,2),',','.'));

   dxfList.Add('100');
   dxfList.Add('AcDbArc');
   dxfList.Add(intToStr(Ord(TDxfCommonCode.ANGLE1)));  //40
   dxfList.Add(ReplaceChar(FloatToStrF(StartAngle, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.ANGLE2)));  //40
   dxfList.Add(ReplaceChar(FloatToStrF(EndAngle, ffFixed,0,2),',','.'));

   Result:= Trim(dxfList.Text);
   dxfList.Free;
end;

constructor TDxfArc.Create(layerName: string; x, y, r, startAng, endAng: real);
begin
     X1:= x;
     Y1:= y;
     Z1:= 0.0;
     Thickness:= 0;
     Radius:= r;
     StartAngle:= startAng;
     EndAngle:= endAng;
     DxfLayer.Color:= 256; //BYLAYER
     DxfLayer.LineType:= 'BYLAYER';

     if layerName = '' then DxfLayer.LayerName:= '0'
     else DxfLayer.LayerName:= layerName;
end;

destructor TDxfArc.Destroy;
begin
   inherited Destroy;
end;

{TDxfText}
function TDxfText.ToDXF: string;
var
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;

   dxfList.Add('0');
   dxfList.Add('TEXT');
   dxfList.Add('100');
   dxfList.Add('AcDbEntity');
   dxfList.Add('8');
   dxfList.Add(DxfLayer.LayerName);
   dxfList.Add(intToStr(Ord(TDxfLayerCode.COLOR)));  //62 DxfLayer.DxfCode
   dxfList.Add(intToStr(DxfLayer.Color));
   dxfList.Add(intToStr(Ord(TDxfLayerCode.LINE_TYPE)));  //6 DxfLayer.DxfCode
   dxfList.Add(DxfLayer.LineType);

   dxfList.Add('100');
   dxfList.Add('AcDbText');
   dxfList.Add(intToStr(Ord(TDxfCommonCode.X1)));  //10
   dxfList.Add(ReplaceChar(FloatToStrF(X1, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfCommonCode.Y1)));  //20
   dxfList.Add(ReplaceChar(FloatToStrF(Y1, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfCommonCode.Z1)));  //30
   dxfList.Add(ReplaceChar(FloatToStrF(Z1, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfCommonCode.TEXT1)));
   dxfList.Add(Text1);

   dxfList.Add(intToStr(Ord(TDxfCommonCode.STYLE_NAME)));
   dxfList.Add(TextStyleName);

   dxfList.Add(intToStr(Ord(TDxfCommonCode.SIZE)));
   dxfList.Add(ReplaceChar(FloatToStrF(Height, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfCommonCode.ANGLE1)));
   dxfList.Add(ReplaceChar(FloatToStrF(RotationAngle, ffFixed,0,2),',','.'));

   Result:= Trim(dxfList.Text);
   dxfList.Free;
end;

constructor TDxfText.Create(layerName: string; angle, H, x, y: real; txt: string);
begin
    Text1:= txt;
    TextStyleName:= 'GENERIC';
    X1:= x;
    Y1:= y;
    Z1:= 0.0;
    Height:= H;
    RotationAngle:= angle;
    DxfLayer.Color:= 256; //BYLAYER
    DxfLayer.LineType:= 'BYLAYER';

    if layerName = '' then DxfLayer.LayerName:= '0'
    else DxfLayer.LayerName:= layerName;
end;

destructor TDxfText.Destroy;
begin
       //
    inherited Destroy;
end;

{TDxfMText}
function TDxfMText.ToDXF: string;
var
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;

   dxfList.Add('0');
   dxfList.Add('MTEXT');
   dxfList.Add('100');
   dxfList.Add('AcDbEntity');

   dxfList.Add('8');
   dxfList.Add(DxfLayer.LayerName);
   dxfList.Add(intToStr(Ord(TDxfLayerCode.COLOR)));  //62 DxfLayer.DxfCode
   dxfList.Add(intToStr(DxfLayer.Color));
   dxfList.Add(intToStr(Ord(TDxfLayerCode.LINE_TYPE)));  //6 DxfLayer.DxfCode
   dxfList.Add(DxfLayer.LineType);

   dxfList.Add('100');
   dxfList.Add('AcDbMText');
   dxfList.Add(intToStr(Ord(TDxfCommonCode.X1)));  //10
   dxfList.Add(ReplaceChar(FloatToStrF(X1, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfCommonCode.Y1)));  //20
   dxfList.Add(ReplaceChar(FloatToStrF(Y1, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfCommonCode.Z1)));  //30
   dxfList.Add(ReplaceChar(FloatToStrF(Z1, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfCommonCode.TEXT1)));
   dxfList.Add(Text1);

   dxfList.Add(intToStr(Ord(TDxfCommonCode.STYLE_NAME)));
   dxfList.Add(TextStyleName);

   dxfList.Add(intToStr(Ord(TDxfCommonCode.SIZE)));
   dxfList.Add(ReplaceChar(FloatToStrF(Height, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfCommonCode.ANGLE1)));
   dxfList.Add(ReplaceChar(FloatToStrF(RotationAngle, ffFixed,0,2),',','.'));

   Result:= Trim(dxfList.Text);
   dxfList.Free;
end;

constructor TDxfMText.Create(layerName: string; angle, H, x, y: real; txt: string);
begin
  TextStyleName:= 'GENERIC';
  DxfLayer.Color:= 256; //BYLAYER
  DxfLayer.LineType:= 'BYLAYER';

  X1:= x;
  Y1:= y;
  Z1:= 0.0;
  Height:= H;
  Text1:= txt;
  RotationAngle:= angle;

  if layerName = '' then DxfLayer.LayerName:= '0'
  else DxfLayer.LayerName:= layerName;
end;

destructor TDxfMText.Destroy;
begin
    //
    inherited Destroy;
end;

{TDxfSolidArrowHead}
function TDxfSolidArrowHead.ToDXF: string;
var
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;
   dxfList.Add('0');
   dxfList.Add('SOLID');
   dxfList.Add('100');
   dxfList.Add('AcDbEntity');
   dxfList.Add('8');
   dxfList.Add(DxfLayer.LayerName);
   dxfList.Add(intToStr(Ord(TDxfLayerCode.COLOR)));  //62 DxfLayer.DxfCode
   dxfList.Add(intToStr(DxfLayer.Color));
   dxfList.Add(intToStr(Ord(TDxfLayerCode.LINE_TYPE)));  //6 DxfLayer.DxfCode
   dxfList.Add(DxfLayer.LineType);
   dxfList.Add(intToStr(Ord(TDxfCommonCode.THICKNESS)));  //39
   dxfList.Add(ReplaceChar(FloatToStrF(Thickness, ffFixed,0,2),',','.'));

   dxfList.Add('100');
   dxfList.Add('AcDbTrace');
   dxfList.Add(intToStr(Ord(TDxfCommonCode.X1)));  //10
   dxfList.Add(ReplaceChar(FloatToStrF(X1, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Y1)));  //20
   dxfList.Add(ReplaceChar(FloatToStrF(Y1, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Z1)));  //30
   dxfList.Add('0.0');

   dxfList.Add(intToStr(Ord(TDxfCommonCode.X2)));  //11
   dxfList.Add(ReplaceChar(FloatToStrF(X2, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Y2)));  //21
   dxfList.Add(ReplaceChar(FloatToStrF(Y2, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Z2)));  //31
   dxfList.Add('0.0');

   dxfList.Add(intToStr(Ord(TDxfCommonCode.X3)));  //12
   dxfList.Add(ReplaceChar(FloatToStrF(X3, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Y3)));  //22
   dxfList.Add(ReplaceChar(FloatToStrF(Y3, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Z3)));  //32
   dxfList.Add('0.0');

   dxfList.Add(intToStr(Ord(TDxfCommonCode.X4)));  //13
   dxfList.Add(ReplaceChar(FloatToStrF(X4, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Y4)));  //23
   dxfList.Add(ReplaceChar(FloatToStrF(Y4, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Z4)));  //33
   dxfList.Add('0.0');

   Result:= Trim(dxfList.Text);
   dxfList.Free;
end;

constructor TDxfSolidArrowHead.Create(layerName: string; V: array of TRealPoint);
begin
   Thickness:= 0;
   X1:=V[0].x;
   Y1:=V[0].y;
   Z1:= 0.0;
   X2:=V[1].x;
   Y2:=V[1].y;
   Z2:= 0.0;
   X3:=V[2].x;
   Y3:=V[2].y;
   Z3:= 0.0;
   X4:=V[3].x;
   Y4:=V[3].y;
   Z4:= 0.0;
   DxfLayer.Color:= 256; //BYLAYER
   DxfLayer.LineType:= 'BYLAYER';

   if layerName = '' then DxfLayer.LayerName:= '0'
   else DxfLayer.LayerName:= layerName;
end;

destructor TDxfSolidArrowHead.Destroy;
begin
    //
    inherited Destroy;
end;

{TDxfLine}
function TDxfLine.ToDXF: string;
var
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;

   dxfList.Add('0');
   dxfList.Add('LINE');
   dxfList.Add('100');
   dxfList.Add('AcDbEntity');
   dxfList.Add('8');
   dxfList.Add(DxfLayer.LayerName);
   dxfList.Add(intToStr(Ord(TDxfLayerCode.COLOR)));  //62 DxfLayer.DxfCode
   dxfList.Add(intToStr(DxfLayer.Color));
   dxfList.Add(intToStr(Ord(TDxfLayerCode.LINE_TYPE)));  //6 DxfLayer.DxfCode
   dxfList.Add(DxfLayer.LineType);
   dxfList.Add(intToStr(Ord(TDxfCommonCode.THICKNESS)));  //39
   dxfList.Add(ReplaceChar(FloatToStrF(Thickness, ffFixed,0,2),',','.'));

   dxfList.Add('100');
   dxfList.Add('AcDbLine');
   dxfList.Add(intToStr(Ord(TDxfCommonCode.X1)));  //10
   dxfList.Add(ReplaceChar(FloatToStrF(X1, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Y1)));  //20
   dxfList.Add(ReplaceChar(FloatToStrF(Y1, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Z1)));  //30
   dxfList.Add(ReplaceChar(FloatToStrF(Z1, ffFixed,0,2),',','.'));

   dxfList.Add(intToStr(Ord(TDxfCommonCode.X2)));  //11
   dxfList.Add(ReplaceChar(FloatToStrF(X2, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Y2)));  //21
   dxfList.Add(ReplaceChar(FloatToStrF(Y2, ffFixed,0,2),',','.'));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.Z2)));  //31
   dxfList.Add(ReplaceChar(FloatToStrF(Z2, ffFixed,0,2),',','.'));

   Result:= Trim(dxfList.Text);
   dxfList.Free;
end;

constructor TDxfLine.Create(layerName: string; px1, py1, px2, py2: real);
begin
  X1:= px1;
  Y1:= py1;
  Z1:= 0.0;
  X2:= px2;
  Y2:= py2;
  Z2:= 0.0;
  Thickness:= 0;
  DxfLayer.Color:= 256; //BYLAYER
  DxfLayer.LineType:= 'BYLAYER';
  if layerName = '' then DxfLayer.LayerName:= '0'
  else DxfLayer.LayerName:= layerName;
end;

destructor TDxfLine.Destroy;
begin
   //
   inherited Destroy;
end;

{TDxfLWPolyLine}
function TDxfLWPolyLine.ToDXF: string;
var
   i: integer;
   dxfList: TStringList;
begin
   dxfList:= TStringList.Create;

   dxfList.Add('0');
   dxfList.Add('LWPOLYLINE');
   dxfList.Add('100');
   dxfList.Add('AcDbEntity');
   dxfList.Add('8');
   dxfList.Add(DxfLayer.LayerName);
   dxfList.Add(intToStr(Ord(TDxfLayerCode.COLOR)));  //62
   dxfList.Add(intToStr(DxfLayer.Color));
   dxfList.Add(intToStr(Ord(TDxfLayerCode.LINE_TYPE)));  //6
   dxfList.Add(DxfLayer.LineType);
   dxfList.Add(intToStr(Ord(TDxfCommonCode.THICKNESS)));  //39
   dxfList.Add(ReplaceChar(FloatToStrF(Thickness, ffFixed,0,2),',','.'));

   dxfList.Add('100');
   dxfList.Add('AcDbPolyline');
   dxfList.Add(intToStr(Ord(TDxfCommonCode.COUNT_VERTICE)));  //90
   dxfList.Add(IntToStr(CountVertice));
   dxfList.Add(intToStr(Ord(TDxfCommonCode.OPENED_CLOSED_FLAG)));  //70
   dxfList.Add(intToStr(OpenedClosedFlag));

   for i:= 0 to CountVertice-1 do
   begin
     dxfList.Add(IntToStr(Ord(TDxfCommonCode.X1)));  //10
     dxfList.Add(ReplaceChar(FloatToStrF(Vertice[i].x, ffFixed,0,2),',','.'));

     dxfList.Add(IntToStr(Ord(TDxfCommonCode.Y1)));  //20
     dxfList.Add(ReplaceChar(FloatToStrF(Vertice[i].y, ffFixed,0,2),',','.'));

     dxfList.Add(IntToStr(Ord(TDxfCommonCode.Z1)));  //30
     dxfList.Add('0.0');
   end;
   Result:= Trim(dxfList.Text);
   dxfList.Free;
end;

constructor TDxfLWPolyLine.Create(layerName: string; V: array of TRealPoint; closed: boolean);
var
   i: integer;
begin
   Thickness:= 0;
   DxfLayer.Color:= 256; //BYLAYER
   DxfLayer.LineType:= 'BYLAYER';

   CountVertice:= High(V)+ 1;
   SetLength(Vertice, CountVertice);

   for i:= 0 to CountVertice - 1 do
   begin
     Vertice[i].x:= V[i].x;
     Vertice[i].y:= V[i].y;
   end;

   if closed then OpenedClosedFlag:= 1
   else OpenedClosedFlag:= 0;

   if layerName = '' then DxfLayer.LayerName:= '0'
   else DxfLayer.LayerName:= layerName;
end;

destructor TDxfLWPolyLine.Destroy;
begin
  SetLength(Vertice, 0);
  Vertice:= nil;
  inherited Destroy;
end;

{TDxfPolyLine}
function TDxfPolyLine.ToDXF: string;
var
  i: integer;
  dxfList: TStringList;
begin
  dxfList:= TStringList.Create;

  dxfList.Add('0');
  dxfList.Add('POLYLINE');
  dxfList.Add('100');
  dxfList.Add('AcDbEntity');
  dxfList.Add('8');
  dxfList.Add(DxfLayer.LayerName);
  dxfList.Add(intToStr(Ord(TDxfLayerCode.COLOR)));  //62
  dxfList.Add(intToStr(DxfLayer.Color));
  dxfList.Add(intToStr(Ord(TDxfLayerCode.LINE_TYPE)));  //6
  dxfList.Add(DxfLayer.LineType);
  dxfList.Add('100');
  dxfList.Add('AcDb2dPolyline');
  dxfList.Add(intToStr(Ord(TDxfCommonCode.OPENED_CLOSED_FLAG)));  //70
  dxfList.Add(intToStr(OpenedClosedFlag));
  dxfList.Add(intToStr(Ord(TDxfCommonCode.FOLLOW_FLAG)));  //66
  dxfList.Add(intToStr(FollowFlag));

  for i:= 0 to CountVertice-1 do
  begin
    dxfList.Add('0');
    dxfList.Add('VERTEX');
    dxfList.Add('100');
    dxfList.Add('AcDbEntity');
    dxfList.Add('8');
    dxfList.Add(DxfLayer.LayerName);
    dxfList.Add(intToStr(Ord(TDxfLayerCode.COLOR)));  //62
    dxfList.Add(intToStr(DxfLayer.Color));
    dxfList.Add(intToStr(Ord(TDxfLayerCode.LINE_TYPE)));  //6
    dxfList.Add(DxfLayer.LineType);
    dxfList.Add(intToStr(Ord(TDxfCommonCode.THICKNESS)));  //39
    dxfList.Add(ReplaceChar(FloatToStrF(Thickness, ffFixed,0,2),',','.'));

    dxfList.Add('100');
    dxfList.Add('AcDbVertex');
    dxfList.Add('100');
    dxfList.Add('AcDb2dVertex');

    dxfList.Add(IntToStr(Ord(TDxfCommonCode.X1)));  //10
    dxfList.Add(ReplaceChar(FloatToStrF(Vertice[i].x, ffFixed,0,2),',','.'));

    dxfList.Add(IntToStr(Ord(TDxfCommonCode.Y1)));  //20
    dxfList.Add(ReplaceChar(FloatToStrF(Vertice[i].y, ffFixed,0,2),',','.'));

    dxfList.Add(IntToStr(Ord(TDxfCommonCode.Z1)));  //30
    dxfList.Add('0.0');
  end;
  dxfList.Add('0');
  dxfList.Add('SEQEND');

  Result:= Trim(dxfList.Text);
  dxfList.Free;
end;

constructor TDxfPolyLine.Create(layerName: string; V: array of TRealPoint; closed: boolean);
var
    i: integer;
begin
    FollowFlag:= 1;
    Thickness:= 0;
    DxfLayer.Color:= 256; //BYLAYER
    DxfLayer.LineType:= 'BYLAYER';

    CountVertice:= High(V)+ 1;
    SetLength(Vertice, CountVertice);
    for i:= 0 to CountVertice - 1 do
    begin
      Vertice[i].x:= V[i].x;
      Vertice[i].y:= V[i].y;
    end;

    if closed then OpenedClosedFlag:= 1
    else OpenedClosedFlag:= 0;

    if layerName = '' then DxfLayer.LayerName:= '0'
    else DxfLayer.LayerName:= layerName;
end;

destructor TDxfPolyLine.Destroy;
begin
   SetLength(Vertice, 0);
   Vertice:= nil;
   inherited Destroy;
end;

 {TFPDxfWriteBridge}

function TFPDxfWriteBridge.LinearOrAlignedDimension(layerName, {*}anounymousBlockName, typeName, styleName: string;
                                          offset, x1, y1, x2, y2: real): string;
var
  ldim: TDxfLinearDimension;
  i, index: integer;
  dimStyleName: string;
  objStyle: TObject;
begin
  index:= -1;
  for i:= 0 to DimStyleList.Count -1 do
  begin
     if CompareText(TDxfDimStyle(DimStyleList[i]).DimStyleName, styleName) = 0 then
     begin
        index:= i;
     end;
  end;
  if index = -1 then
  begin
     dimStyleName:='';
     objStyle:= nil;
  end
  else
  begin
     objStyle:= TDxfDimStyle(DimStyleList[index]);
     dimStyleName:= TDxfDimStyle(DimStyleList[index]).DimStyleName;
  end;
  if  objStyle <> nil then
     ldim:= TDxfLinearDimension.Create(layerName, anounymousBlockName, typeName,
                                        TDxfDimStyle(objStyle), offset, x1, y1, x2, y2)
  else
     ldim:= TDxfLinearDimension.Create(layerName, anounymousBlockName, typeName, nil,
                                        offset, x1, y1, x2, y2);

  Result:= ldim.ToDXF;
  ldim.Free;
  ldim:= TDxfLinearDimension.Create(layerName, anounymousBlockName, typeName, dimStyleName,
                                      offset, x1, y1, x2, y2);
  if DimBlockList <> nil then DimBlockList.Add(ldim); //persistence...
end;

function TFPDxfWriteBridge.LinearAlignedDimension(layerName, anounymousBlockName, styleName: string;
                            offset, x1, y1, x2, y2: real): string;
begin
  Result:=LinearOrAlignedDimension(layerName,anounymousBlockName,'A',styleName,offset,x1,y1,x2,y2);
end;

function TFPDxfWriteBridge.LinearHorizontalDimension(layerName, anounymousBlockName, styleName: string;
                            offset, x1, y1, x2, y2: real): string;
begin
  Result:=LinearOrAlignedDimension(layerName,anounymousBlockName,'H',styleName,offset,x1,y1,x2,y2);
end;

function TFPDxfWriteBridge.LinearVerticalDimension(layerName, anounymousBlockName, styleName: string;
                            offset, x1, y1, x2, y2: real): string;
begin
  Result:=LinearOrAlignedDimension(layerName,anounymousBlockName,'V',styleName,offset,x1,y1,x2,y2);
end;

function TFPDxfWriteBridge.RadialOrDiameterDimension(layerName, {*}anounymousBlockName, typeName, styleName: string;
                                          cx, cy, r, angle: real): string;
var
  rdim: TDxfRadialDimension;
  i, index: integer;
  dimStyleName: string;
  objStyle: TObject;
begin
  index:= -1;
  for i:= 0 to DimStyleList.Count -1 do
  begin
     if CompareText(TDxfDimStyle(DimStyleList[i]).DimStyleName, styleName) = 0 then
     begin
       index:= i;
     end;
  end;
  if index = -1 then
  begin
     dimStyleName:='';
     objStyle:= nil;
  end
  else
  begin
     objStyle:= TDxfDimStyle(DimStyleList[index]);
     dimStyleName:= TDxfDimStyle(DimStyleList[index]).DimStyleName;
  end;
  rdim:= TDxfRadialDimension.Create(layerName, anounymousBlockName, typeName,
                                    TDxfDimStyle(objStyle), cx, cy, r, angle);
  Result:= rdim.ToDXF;
  rdim.Free;
  rdim:= TDxfRadialDimension.Create(layerName, anounymousBlockName, typeName, dimStyleName,
                                     cx, cy, r, angle);
  if DimBlockList <> nil then DimBlockList.Add(rdim); //persistence...
end;

function TFPDxfWriteBridge.RadialDimension(layerName, {*}anounymousBlockName, styleName: string;
                                          cx, cy, r, angle: real): string;
begin
    Result:=RadialOrDiameterDimension(layerName, anounymousBlockName, 'R', styleName, cx, cy, r, angle);
end;

function TFPDxfWriteBridge.DiameterDimension(layerName, anounymousBlockName,  styleName: string;
                                          cx, cy, r, angle: real): string;
begin
   Result:=RadialOrDiameterDimension(layerName, anounymousBlockName, 'D', styleName, cx, cy, r, angle);
end;

function TFPDxfWriteBridge.Angular3PDimension(layerName, anounymousBlockName, styleName: string;
               offset: real; cx, cy, r, startAngle, endAngle : real): string;
begin
  Result:= AngularOrArc3PDimension(layerName, anounymousBlockName, 'ANG', styleName,
             offset, cx, cy, r, startAngle, endAngle);
end;

function TFPDxfWriteBridge.Arc3PDimension(layerName, anounymousBlockName, styleName: string;
               offset: real; cx, cy, r, startAngle, endAngle : real): string;
begin
    Result:= AngularOrArc3PDimension(layerName, anounymousBlockName, 'ARC', styleName,
               offset, cx, cy, r, startAngle, endAngle);
end;

function TFPDxfWriteBridge.AngularOrArc3PDimension(layerName, {*}anounymousBlockName, typeName, styleName: string;
               offset: real; cx, cy, r, startAngle, endAngle : real): string;
var
  angdim: TDxfAngular3PDimension;
  i, index: integer;
  dimStyleName: string;
  objStyle: TObject;
begin
  index:= -1;
  for i:= 0 to DimStyleList.Count -1 do
  begin
     if CompareText(TDxfDimStyle(DimStyleList[i]).DimStyleName, styleName) = 0 then
     begin
         index:= i;
     end;
  end;
  if index = -1 then
  begin
     dimStyleName:='';
     objStyle:= nil;
  end
  else
  begin
     objStyle:= TDxfDimStyle(DimStyleList[index]);
     dimStyleName:= TDxfDimStyle(DimStyleList[index]).DimStyleName;
  end;

  if objStyle <> nil then
    angdim:= TDxfAngular3PDimension.Create(layerName, anounymousBlockName, typeName,
                   TDxfDimStyle(objStyle), offset, cx, cy, r, startAngle, endAngle)
  else
    angdim:= TDxfAngular3PDimension.Create(layerName, anounymousBlockName, typeName, nil,
                                      offset, cx, cy, r, startAngle, endAngle);
  Result:= angdim.ToDXF;
  angdim.Free;

  angdim:= TDxfAngular3PDimension.Create(layerName, anounymousBlockName, dimStyleName,
                                     offset, cx, cy, r, startAngle, endAngle);

  if DimBlockList <> nil then DimBlockList.Add(angdim); //persistence...
end;

//TODO: complete here....
function TFPDxfWriteBridge.Dimension(AnounymousDimensionanounymousBlockName: string): string;
var
  i: integer;
  obj: TObject;
begin
  for i:= 0 to DimBlockList.Count-1 do
  begin
     obj:= TObject(DimBlockList.Items[i]);
     if obj.ClassNameIs('TDxfLinearDimension') then
     begin
     if CompareText(TDxfLinearDimension(DimBlockList.Items[i]).BlockName,
         AnounymousDimensionanounymousBlockName) = 0 then
            Result:= TDxfLinearDimension(DimBlockList.Items[i]).ToDXF;
     end;
     if obj.ClassNameIs('TDxfAngular3PDimension') then
     begin
        if CompareText(TDxfAngular3PDimension(DimBlockList.Items[i]).BlockName,
                       AnounymousDimensionanounymousBlockName) = 0 then
            Result:= TDxfAngular3PDimension(DimBlockList.Items[i]).ToDXF;
     end;
     if obj.ClassNameIs('TDxfRadialDimension') then
     begin
        if CompareText(TDxfRadialDimension(DimBlockList.Items[i]).BlockName,
                       AnounymousDimensionanounymousBlockName) = 0 then
        Result:= TDxfRadialDimension(DimBlockList.Items[i]).ToDXF;
     end;
  end;
end;

function TFPDxfWriteBridge.LinearAlignedDimension(layerName, styleName: string;
                             offset, x1, y1, x2, y2: real): string;
begin
  Result:= LinearAlignedDimension(layerName, '', styleName, offset, x1, y1, x2, y2);
end;

function TFPDxfWriteBridge.LinearHorizontalDimension(layerName, styleName: string;
                             offset, x1, y1, x2, y2: real): string;
begin
  Result:= LinearHorizontalDimension(layerName, '', styleName, offset, x1, y1, x2, y2);
end;
function TFPDxfWriteBridge.LinearVerticalDimension(layerName, styleName: string;
                             offset, x1, y1, x2, y2: real): string;
begin
  Result:= LinearVerticalDimension(layerName, '', styleName, offset, x1, y1, x2, y2);
end;
function TFPDxfWriteBridge.RadialDimension(layerName, styleName: string;
                             cx, cy, r, angle: real): string;
begin
  Result:= RadialDimension(layerName, '', styleName, cx, cy, r, angle);
end;
function TFPDxfWriteBridge.DiameterDimension(layerName, styleName: string;
                             cx, cy, r, angle: real): string;
begin
  Result:= DiameterDimension(layerName, '', styleName, cx, cy, r, angle);
end;
function TFPDxfWriteBridge.Angular3PDimension(layerName, styleName: string; offset: real;
                             cx, cy, r, startAngle, endAngle: real): string;
begin
  Result:= Angular3PDimension(layerName, '', styleName, offset, cx, cy, r, startAngle, endAngle);
end;
function TFPDxfWriteBridge.Arc3PDimension(layerName, styleName: string; offset: real;
                             cx, cy, r, startAngle, endAngle: real): string;
begin
   Result:= Arc3PDimension(layerName, '', styleName, offset, cx, cy, r, startAngle, endAngle);
end;

function TFPDxfWriteBridge.InsertBlock(blockName: string; x,y: real): string;
var
    lstDxf: TStringList;
begin
   lstDxf:= TStringList.Create;
   lstDxf.Add('0');
   lstDxf.Add('INSERT');
   lstDxf.Add('100');
   lstDxf.Add('AcDbEntity');
   lstDxf.Add('8');
   lstDxf.Add('0');
   lstDxf.Add('100');
   lstDxf.Add('AcDbBlockReference');
   lstDxf.Add('2');
   lstDxf.Add(blockName);
   lstDxf.Add('10');
   lstDxf.Add(ReplaceChar(FloatToStrF(x,ffFixed,0,2),',','.'));
   lstDxf.Add(' 20');
   lstDxf.Add(ReplaceChar(FloatToStrF(y,ffFixed,0,2),',','.'));
   lstDxf.Add('30');
   lstDxf.Add('0.0');
   lstDxf.Add('41');
   lstDxf.Add('1');
   lstDxf.Add('42');
   lstDxf.Add('1');
   lstDxf.Add('50');
   lstDxf.Add('0');

   Result:=  Trim(lstDxf.Text);
   lstDxf.Free;
end;

procedure TFPDxfWriteBridge.DoProduceDXFEntity(out entityDXF: string);
begin
  entityDXF:='';
  if Assigned(FOnProduceEntity) then FOnProduceEntity(entityDXF);
end;

procedure TFPDxfWriteBridge.DoProduceDXFBlock(out blockDXF: string);
begin
  blockDXF:='';
  if Assigned(FOnProduceBlock) then FOnProduceBlock(blockDXF);
end;


procedure TFPDxfWriteBridge.SaveToFile(path: string);
begin
   ToDxf.SaveToFile(path);
end;

procedure TFPDxfWriteBridge.Produce(selectLayer: string);
var
  i: integer;
  strDXFEntity, strDXFBlock: string;
begin

     strDXFEntity:='';
     strDXFBlock:='';
     RestrictedLayer:= selectLayer;

     DxfBegin;
          BeginTables;
            BeginTable('LTYPE', LineTypeList.Count);  //follow 'count' table
               for i:= 0 to LineTypeList.Count-1 do
               begin
                 AddTableLType(TDxfLineType(LineTypeList.Items[i]).LineTypeName,
                                             TDxfLineType(LineTypeList.Items[i]).VectorDashElementLength,
                                             TDxfLineType(LineTypeList.Items[i]).AsciiLinePatern)
               end;
            EndTable;

            BeginTable('STYLE', TextStyleList.Count);  //follow 'count' table
               for i:= 0 to TextStyleList.Count-1 do
               begin                          //fontName: string; fontFileName: string; fontHeight: real
                 AddTableTextStyle(TDxfTextStyle(TextStyleList.Items[i]).TextStyleName,
                                                   TDxfTextStyle(TextStyleList.Items[i]).PrimaryFileName,
                                                   TDxfTextStyle(TextStyleList.Items[i]).FixedHeight
                                                   );
               end;   //'DEFAULT','ARIAL.TTF'{'isocpeur.ttf'}, 0.0 {no fixed!}
            EndTable;

            BeginTable('LAYER', LayerList.Count); //follow 'count' table
                for i:=0 to LayerList.Count-1 do
                begin
                   AddTableLayer(  PDxfLayer(LayerList.Items[i])^.LayerName,
                                                   PDxfLayer(LayerList.Items[i])^.LineType,
                                                   PDxfLayer(LayerList.Items[i])^.Color);
                end;
            EndTable;

            BeginTable('DIMSTYLE', DimStyleList.Count); //follow 'count' table
               for i:=0 to DimStyleList.Count-1 do
               begin
                   AddTableDimStyle(TDxfDimStyle(DimStyleList.Items[i]).DimStyleName,
                                                    TDxfDimStyle(DimStyleList.Items[i]).ArrowSize,
                                                    TDxfDimStyle(DimStyleList.Items[i]).ArrowWidth,
                                                    TDxfDimStyle(DimStyleList.Items[i]).TextColor,
                                                    TDxfDimStyle(DimStyleList.Items[i]).TextHeight);
               end;
               (*
                 AddTableDimStyle('DIM1', 0.1800{arrwSize}, 0.0625{arrwWidth} , 2{color}, 0.1800 {0.25});
                 AddTableDimStyle('GENERIC',0,0,0,0);
               *)
            EndTable;

          EndTables;
          BeginBlocks;
              DoProduceDXFBlock(strDXFBlock{out});
              if  strDXFBlock <> '' then AddBlock(Trim(strDXFBlock));
          EndBlocks;
          BeginEntities;
              DoProduceDXFEntity(strDXFEntity{out});
              if  strDXFEntity <> '' then  AddEntity(Trim(strDXFEntity));
          EndEntities;
     DxfEnd; //End DXF File!;
     //SaveToFile(nameFileDXF);
end;

procedure TFPDxfWriteBridge.AddBlock(dxfBlock: string);
begin
  ToDxf.Add(dxfBlock);
end;

procedure  TFPDxfWriteBridge.AddEntity(objEntity: TObject);
begin
   if objEntity.ClassNameIs('TDxfPoint') then
      AddEntity(TDxfPoint(objEntity).ToDXF)
   else if objEntity.ClassNameIs('TDxfLine') then
      AddEntity(TDxfLine(objEntity).ToDXF)
   else if objEntity.ClassNameIs('TDxfCircle') then
      AddEntity(TDxfCircle(objEntity).ToDXF)
   else if objEntity.ClassNameIs('TDxfArc') then
      AddEntity(TDxfArc(objEntity).ToDXF)
   else if objEntity.ClassNameIs('TDxfText') then
      AddEntity(TDxfText(objEntity).ToDXF)
   else if objEntity.ClassNameIs('TDxfMText') then
      AddEntity(TDxfMText(objEntity).ToDXF)
   else if objEntity.ClassNameIs('TDxfSolidArrowHead') then
      AddEntity(TDxfSolidArrowHead(objEntity).ToDXF)
   else if objEntity.ClassNameIs('TDxfPolyline') then
      AddEntity(TDxfPolyline(objEntity).ToDXF)
   else if objEntity.ClassNameIs('TDxfLWPolyline') then
       AddEntity(TDxfLWPolyline(objEntity).ToDXF)
   else if objEntity.ClassNameIs('TDxfLinearDimension') then
       AddEntity(TDxfLinearDimension(objEntity).ToDXF)
   else if objEntity.ClassNameIs('TDxfAngular3PDimension') then
       AddEntity(TDxfAngular3PDimension(objEntity).ToDXF)
   else if objEntity.ClassNameIs('TDxfRadialDimension') then
       AddEntity(TDxfRadialDimension(objEntity).ToDXF);
end;

procedure TFPDxfWriteBridge.AddEntity(dxfEntity: string);
begin
  ToDxf.Add(dxfEntity);
end;

procedure TFPDxfWriteBridge.DxfBegin;
begin
  ToDxf.Clear;

  ToDxf.Add('0');
  ToDxf.Add('SECTION');
  ToDxf.Add('2');
  ToDxf.Add('HEADER');
  ToDxf.Add('999');
  ToDxf.Add('Generator: TFPDxfWriteBridge');
  ToDxf.Add('999');
  ToDxf.Add('By jmpessoa@hotmail.com');
  ToDxf.Add('9');
  ToDxf.Add('$DIMASZ');
  ToDxf.Add('40');
  ToDxf.Add('0.1800'); {0.1800}
  ToDxf.Add('9');
  ToDxf.Add('$DIMTSZ');
  ToDxf.Add('40');
  ToDxf.Add('0');
   {DIMTSZ Specifies the size of oblique strokes drawn instead of arrowheads for linear,
   radius, and diameter dimensioning  0 = No ticks}
  ToDxf.Add('9');
  ToDxf.Add('$DIMGAP');
  ToDxf.Add('40'); //40
  ToDxf.Add('0.0900'); {0.6250 = metric} {imperial= 0.0900}
  ToDxf.Add('9');
  ToDxf.Add('$DIMEXO');
  ToDxf.Add('40');
  ToDxf.Add('0.0625');   {metric 0.625}
  ToDxf.Add('9');
  ToDxf.Add('$DIMDLI');
  ToDxf.Add('40');
  ToDxf.Add('0.38');
  ToDxf.Add('9');
  ToDxf.Add('$DIMDLE');
  ToDxf.Add('40');
  ToDxf.Add('0.0');
  ToDxf.Add('9');
  ToDxf.Add('$DIMEXE');
  ToDxf.Add('40');
  ToDxf.Add('0.1800');
  ToDxf.Add('9');
  ToDxf.Add('$DIMTXT');
  ToDxf.Add('40');
  ToDxf.Add('0.1800');


  ToDxf.Add('9');
  ToDxf.Add('$DIMTXTDIRECTION');
  ToDxf.Add('70');
  ToDxf.Add('0');

  ToDxf.Add('9');
  ToDxf.Add('$DIMTIH');
  ToDxf.Add('70');
  ToDxf.Add('1');  //1= force horizontal


  ToDxf.Add('9');
  ToDxf.Add('$DIMTAD');
  ToDxf.Add('70');
  ToDxf.Add('0');  //1 = metric

  ToDxf.Add('9');
  ToDxf.Add('$DIMCLRD');
  ToDxf.Add('70');
  ToDxf.Add('256');
  ToDxf.Add('9');
  ToDxf.Add('$DIMCLRE');
  ToDxf.Add('70');
  ToDxf.Add('256');
  ToDxf.Add('9');
  ToDxf.Add('$DIMCLRT');
  ToDxf.Add('70');
  ToDxf.Add('256');
  ToDxf.Add('9');
  ToDxf.Add('$DIMASO'); //0 = Draw individual entities
  ToDxf.Add('70');      // 1 = Create associative dimensioning
  ToDxf.Add('1');       //1 - associative
  ToDxf.Add('9');
  ToDxf.Add('$DIMASSOC');
  ToDxf.Add('280');
  ToDxf.Add('2'); //2 - associative
  {0 = Creates exploded dimensions; there is no association
   1 = Creates non-associative dimension objects; the elements
       of the dimension are formed into a single object, and if the
       definition point on the object moves, then the dimension
       value is updated
   2 = Creates associative dimension objects; the elements of
       the dimension are formed into a single object and one or
       more definition points of the dimension are coupled with
       association points on geometric objects}
  ToDxf.Add('9');
  ToDxf.Add('$DIMSHO');
  ToDxf.Add('70');
  ToDxf.Add('0');
 // 1 = Recompute dimensions while dragging
 // 0 = Drag original image
  ToDxf.Add('9');
  ToDxf.Add('$DIMLUNIT');
  ToDxf.Add('70');
  ToDxf.Add('2');
{DIMLUNIT 70 Sets units for all dimension types except Angular:
1 = Scientific; 2 = Decimal; 3 = Engineering;
4 = Architectural; 5 = Fractional; 6 = Windows desktop}

{DIMDEC 70
Number of decimal places for the tolerance values of a primary units dimension
the number of digits edited after the decimal point depends on the precision set by DIMDEC}
  ToDxf.Add('9');
  ToDxf.Add('$DIMDEC');
  ToDxf.Add('70');
  ToDxf.Add('4'); {2=metric } {4=imperial}

  ToDxf.Add('9');
  ToDxf.Add('$DIMADEC');
  ToDxf.Add('70');
  ToDxf.Add('2');

  ToDxf.Add('9');
  ToDxf.Add('$INSBASE');
  ToDxf.Add('10');
  ToDxf.Add('0.0');
  ToDxf.Add('20');
  ToDxf.Add('0.0');
  ToDxf.Add('30');
  ToDxf.Add('0.0');

  ToDxf.Add('9');
  ToDxf.Add('$EXTMIN');
  ToDxf.Add('10');
  ToDxf.Add('0.0');
  ToDxf.Add('20');
  ToDxf.Add('0.0');
  ToDxf.Add('9');
  ToDxf.Add('$EXTMAX');
  ToDxf.Add('10');
  ToDxf.Add('3200.0');
  ToDxf.Add('20');
  ToDxf.Add('3200.0');

  ToDxf.Add('9');
  ToDxf.Add('$LINMIN');
  ToDxf.Add('10');
  ToDxf.Add('0.0'); //lef
  ToDxf.Add('20');
  ToDxf.Add('0.0'); //down
  ToDxf.Add('9');
  ToDxf.Add('$LINMAX');
  ToDxf.Add('10');
  ToDxf.Add('3200.0'); //top
  ToDxf.Add('20');
  ToDxf.Add('3200.0'); //right
  {DIMADEC 70 Number of precision places displayed in angular dimensions}
  ToDxf.Add('0');
  ToDxf.Add('ENDSEC'); //end header
end;

procedure TFPDxfWriteBridge.BeginTables;
begin
  ToDxf.Add('0');
  ToDxf.Add('SECTION');
  ToDxf.Add('2');
  ToDxf.Add('TABLES');
end;

procedure TFPDxfWriteBridge.BeginTable(tabName: string; count: integer);
var
  acdbTable: string;
begin
  if CompareText(tabName, 'LTYPE') = 0 then
  begin
     // CountLTYPE:= count;
      acdbTable:= 'AcDbLTypeTable';
  end;
  if CompareText(tabName, 'STYLE') = 0 then
  begin
     // CountSTYLE:= count;
      acdbTable:= 'AcDbStyleTable';
  end;
  if CompareText(tabName, 'LAYER') = 0 then
  begin
     // CountLAYER:= count;
      acdbTable:= 'AcDbLayerTable';
  end;
  if CompareText(tabName, 'DIMSTYLE') = 0 then
  begin
     // CountDIMSTYLE:= count;
      acdbTable:= 'AcDbDimStyleTable';
  end;
  if CompareText(tabName, 'UCS') = 0 then
  begin
    // CountUCS:= count;
     acdbTable:= 'AcDbUcsTable';
  end;
  ToDxf.Add('0');
  ToDxf.Add('TABLE');
  ToDxf.Add('2');
  ToDxf.Add(tabName);
  ToDxf.Add('70');
  ToDxf.Add(intToStr(count)); //count table
  ToDxf.Add('100');
  ToDxf.Add('AcDbSymbolTable');
  ToDxf.Add('100');
  ToDxf.Add(acdbTable)
end;

procedure TFPDxfWriteBridge.AddTableLType(tabName: string; V: array of real; graphicsPattern: string);
var
   lt: TDxfLineType;
begin
   //LTYPE
   //lt:= TDxfLineType.Create('HIDDEN',[0.25,-0.125],'__ __ __ ');
   //lt:= TDxfLineType.Create('CENTER',[1.25,-0.25, 0.25, -0.25], 4 ,'____ _ ____ _ ');
   lt:= TDxfLineType.Create(tabName,V, graphicsPattern);
   ToDxf.Add(lt.ToDXF);
   lt.Free;
end;

procedure TFPDxfWriteBridge.AddTableDimStyle(dimStyleName: string; arrwSize, arrwWidth: real; color: integer;
                                txtHeight: real);
var
  ds: TDxfDimStyle;
begin
  ds:= TDxfDimStyle.Create(dimStyleName, arrwSize, arrwWidth, color, txtHeight);
  if ds <> nil then ToDxf.Add(ds.ToDXF);
  if DimStyleList <> nil then DimStyleList.Add(ds);
end;

procedure TFPDxfWriteBridge.AddTableTextStyle(fontName, fontFileName:  string; fontHeight: real);
var
  ts: TDxfTextStyle;
begin
  ts:= TDxfTextStyle.Create(fontName, fontFileName, fontHeight);
  ToDxf.Add(ts.ToDXF);
  ts.Free;
end;

procedure TFPDxfWriteBridge.AddTableLayer(tabName, lineType: string; color: integer);
begin
  ToDxf.Add('0');
  ToDxf.Add('LAYER');
  ToDxf.Add('100');
  ToDxf.Add('AcDbSymbolTableRecord');
  ToDxf.Add('100');
  ToDxf.Add('AcDbLayerTableRecord');
  ToDxf.Add('2');
  ToDxf.Add(tabName);
  ToDxf.Add('70');
  ToDxf.Add('0');
  ToDxf.Add('62');
  ToDxf.Add(IntToStr(color));
  ToDxf.Add('6');
  ToDxf.Add(LineType);
end;

procedure TFPDxfWriteBridge.AddTableLayer(tabName, lineType: string; color: string);
var
  layerColor: integer;
begin
  if CompareText('ByBlock', color) = 0  then layerColor:= 0
  else if CompareText('Red', color) = 0  then layerColor:= 1
  else if CompareText('Yellow', color) = 0  then layerColor:= 2
  else if CompareText('Green', color) = 0  then layerColor:= 3
  else if CompareText('Cyan', color) = 0  then layerColor:= 4
  else if CompareText('Blue', color) = 0  then layerColor:= 5
  else if CompareText('Magenta', color) = 0  then layerColor:= 6
  else if CompareText('White', color) = 0  then layerColor:= 7
  else if CompareText('Gray', color) = 0  then layerColor:= 8
  else if CompareText('Brown', color) = 0  then layerColor:= 15
  else if CompareText('LtRed', color) = 0  then layerColor:= 23
  else if CompareText('LtGreen', color) = 0  then layerColor:= 121
  else if CompareText('LtCyan', color) = 0  then layerColor:= 131
  else if CompareText('LtBlue', color) = 0  then layerColor:= 163
  else if CompareText('LtMagenta', color) = 0  then layerColor:= 221
  else if CompareText('LtGray', color) = 0  then layerColor:= 252
  else if CompareText('Black', color) = 0  then layerColor:= 250
  else if CompareText('ByLayer', color) = 0  then layerColor:= 256;

  AddTableLayer(tabName, lineType, layerColor);
end;

procedure TFPDxfWriteBridge.AddTableLayer(tabName, lineType: string; color: TDXFColor);
begin
  AddTableLayer(tabName, lineType, Ord(color));
end;

(*
procedure TFPDxfWriteBridge.AddTableUCS(tabName: string);
begin
   if CountUCS > 0 then
   begin
      ToDxf.Add('0');
      ToDxf.Add('UCS');
      ToDxf.Add('100');
      ToDxf.Add('AcDbSymbolTable');
      ToDxf.Add('100');
      ToDxf.Add('AcDbUcsTableRecord');
      ToDxf.Add('2');
      ToDxf.Add(tabName);
      ToDxf.Add('10');
      ToDxf.Add('0');
      ToDxf.Add('20');
      ToDxf.Add('0');
      ToDxf.Add('30');
      ToDxf.Add('0');
      ToDxf.Add('11'); //axis x
      ToDxf.Add('1');
      ToDxf.Add('21');
      ToDxf.Add('0');
      ToDxf.Add('31');
      ToDxf.Add('0');
      ToDxf.Add('12');   //axis y
      ToDxf.Add('0');
      ToDxf.Add('22');
      ToDxf.Add('1');
      ToDxf.Add('32');
      ToDxf.Add('0');
      ToDxf.Add('70');
      ToDxf.Add('0');
      Dec(CountUCS);
   end;
end;
*)

procedure TFPDxfWriteBridge.EndTable;
begin
   ToDxf.Add('0');
   ToDxf.Add('ENDTAB');
end;

procedure TFPDxfWriteBridge.EndTables;
begin
 ToDxf.Add('0');
 ToDxf.Add('ENDSEC');
end;

procedure TFPDxfWriteBridge.BeginBlocks;
begin
 ToDxf.Add('0');
 ToDxf.Add('SECTION');
 ToDxf.Add('2');
 ToDxf.Add('BLOCKS');
end;

function TFPDxfWriteBridge.BeginBlock(layerName, blockName: string; X,Y: real): string;
var
   flag: integer;
   lstBlock: TStringList;
begin
  if Pos('*', blockName) > 0 then flag:= 1 // anounymous block
  else flag:= 64;

  lstBlock:= TStringList.Create;
  lstBlock.Add('0');
  lstBlock.Add('BLOCK');
  lstBlock.Add('100');
  lstBlock.Add('AcDbEntity');
  lstBlock.Add('8');
  lstBlock.Add(layerName);
  lstBlock.Add('100');
  lstBlock.Add('AcDbBlockBegin');
  lstBlock.Add('2');
  lstBlock.Add(blockName);
  lstBlock.Add('70');
  lstBlock.Add(intToStr(flag)); //64  or (1 = anounymous block!)
  lstBlock.Add('10');
  lstBlock.Add(ReplaceChar(FloatToStrF(X, ffFixed,0,2),',','.'));
  lstBlock.Add('20');
  lstBlock.Add(ReplaceChar(FloatToStrF(Y, ffFixed,0,2),',','.'));
  lstBlock.Add('30');
  lstBlock.Add('0.0');
  Result:= Trim(lstBlock.Text);
  lstBlock.Free;
end;

function TFPDxfWriteBridge.EndBlock: string;
var
  lstBlock: TStringList;
begin
  lstBlock:= TStringList.Create;
  lstBlock.Add('0');
  lstBlock.Add('ENDBLK');
  lstBlock.Add('100');
  lstBlock.Add('AcDbEntity');
  lstBlock.Add('100');
  lstBlock.Add('AcDbBlockEnd');
  Result:= Trim(lstBlock.Text);
  lstBlock.Free;
end;

procedure TFPDxfWriteBridge.EndBlocks;
begin
  ToDxf.Add('0');
  ToDxf.Add('ENDSEC');
end;

procedure TFPDxfWriteBridge.BeginEntities;
begin
  ToDxf.Add('0');
  ToDxf.Add('SECTION');
  ToDxf.Add('2');
  ToDxf.Add('ENTITIES');
end;

procedure TFPDxfWriteBridge.EndEntities;
begin
    ToDxf.Add('0');
    ToDxf.Add('ENDSEC');
end;

procedure TFPDxfWriteBridge.DxfEnd;
begin
    ToDxf.Add('0');
    ToDxf.Add('EOF');
end;

function TFPDxfWriteBridge.Circle(layerName: string; x, y, radius: real): string;
var
   circ: TDxfCircle;
begin
   circ:= TDxfCircle.Create(layerName,x , y, radius);
   Result:= circ.ToDXF;
   circ.Free;
end;

function TFPDxfWriteBridge.Arc(layerName: string; x, y, radius, startAngle, endAngle: real): string;
var
   ac: TDxfArc;
begin
   ac:= TDxfArc.Create(layerName,x , y, radius, startAngle, endAngle);
   Result:= ac.ToDXF;
   ac.Free;
end;

function TFPDxfWriteBridge.Point(layerName: string; x, y: real): string;
var
   pt: TDxfPoint;
begin
   pt:= TDxfPoint.Create(layerName,x , y);
   Result:= pt.ToDXF;
   pt.Free;
end;

function TFPDxfWriteBridge.Text(layerName: string; angle, height, x, y: real; txt: string): string;
var
   tx: TDxfText;
begin
   tx:= TDxfText.Create(layerName, angle, height, x, y, txt);
   Result:= tx.ToDXF;
   tx.Free;
end;

function TFPDxfWriteBridge.MText(layerName: string; angle, height, x, y: real; txt: string): string;
var
   mtx: TDxfMText;
begin
   mtx:= TDxfMText.Create(layerName, angle, height, x, y, txt);
   Result:= mtx.ToDXF;
   mtx.Free;
end;

function TFPDxfWriteBridge.SolidArrowHead(layerName: string; V: array of TRealPoint): string;
var
   sol: TDxfSolidArrowHead;
begin
   sol:= TDxfSolidArrowHead.Create(layerName,V);
   Result:= sol.ToDXF;
   sol.Free;
end;

function TFPDxfWriteBridge.Line(layerName: string; x1, y1, x2, y2: real): string;
var
   lin: TDxfLine;
begin
   lin:= TDxfLine.Create(layerName, x1, y1, x2, y2);
   Result:= lin.ToDXF;
   lin.Free;
end;

function TFPDxfWriteBridge.LWPolyLine(layerName: string; V: array of TRealPoint; closed: boolean): string;
var
   LWPlin: TDxfLWPolyLine;
begin
   LWPlin:= TDxfLWPolyLine.Create(layerName, V, closed);
   Result:= LWPlin.ToDXF;
   LWPlin.Free;
end;

function TFPDxfWriteBridge.PolyLine(layerName: string; V: array of TRealPoint; closed: boolean): string;
var
   Plin: TDxfPolyLine;
begin
   Plin:= TDxfPolyLine.Create(layerName, V, closed);
   Result:= Plin.ToDXF;
   Plin.Free;
end;

procedure TFPDxfWriteBridge.DeleteLayerByIndex(index: integer);
begin
    LayerList.Delete(index);
end;

//http://users.atw.hu/delphicikk/listaz.php?id=2207&oldal=2 - otimo
//http://www.delphibasics.co.uk/RTL.asp?Name=TList
//http://www.asiplease.net/computing/delphi/programs/tlist.htm  - ref
procedure TFPDxfWriteBridge.AddLayer(LayerName, lineType: string; color: integer);
var
  PLayerRecord: PDxfLayer;
begin
  New(PLayerRecord);
  PLayerRecord^.LayerName:= LayerName;
  PLayerRecord^.LineType:= lineType;  //CONTINUOUS.. or Dashed.. or Hidden ... etc
  PLayerRecord^.Color:= color;
  LayerList.Add(PLayerRecord)
end;

procedure TFPDxfWriteBridge.AddLineType(lineTypeName: string; V: array of real; linePattern: string);
var
  lt: TDxfLineType;
begin
  if LineTypeList <> nil then
  begin
     lt:= TDxfLineType.Create(lineTypeName, V,  linePattern);
     LineTypeList.Add(lt);
  end;
end;

procedure TFPDxfWriteBridge.AddTextStyle(fontName: string; fontFileName: string; fontHeight: real);
var
  ts: TDxfTextStyle;
begin
  if TextStyleList <> nil then
  begin
     ts:= TDxfTextStyle.Create(fontName,fontFileName,fontHeight);
     TextStyleList.Add(ts);
  end;
end;

procedure TFPDxfWriteBridge.AddDimStyle(styleName: string; arrwSize, arrwWidth: real; color: integer;
                                txtHeight: real);
var
  ts: TDxfDimStyle;
begin
  if DimStyleList <> nil then
  begin
     ts:= TDxfDimStyle.Create(styleName,arrwSize, arrwWidth, color, txtHeight);
     DimStyleList.Add(ts);
  end;
end;

constructor TFPDxfWriteBridge.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    DimStyleList:= TList.Create;
    DimBlockList:= TList.Create;
    LayerList:= TList.Create;
    LineTypeList:= TList.Create;
    TextStyleList:= TList.Create;
   // DimStyleList:= TList.Create;

    AddDimStyle('GENERIC',0,0,0,0);
    AddDimStyle('CUSTOM', 0.1800{arrwSize}, 0.0625{arrwWidth} , 3{color}, 0.1800 {0.25});


   //fontName: string; fontFileName: string; fontHeight: real
    AddTextStyle('DEFAULT','ARIAL.TTF'{'isocpeur.ttf'}, 0.0 {no fixed!});
    AddTextStyle('ISOCPEUR','isocpeur.ttf', 0.0 {no fixed!});


    AddLineType('CONTINUOUS',[], '____________');
    AddLineType('HIDDEN',[0.25,-0.125],'__ __ __ ');
    AddLineType('CENTER',[1.25,-0.25, 0.25, -0.25],'____ _ ____ _ ');
    AddLineType('DOT',[0.0, -0.25], '. . . . . ');
    AddLineType('DASHDOT',[0.5, -0.25, 0.0, -0.25], '__ . __ . ');
    AddLineType('DIVIDE',[0.5, -0.25, 0.0, -0.25, 0.0, -0.25], '____ . . ____ . . ');
    AddLineType('BORDER',[0.5, -0.25, 0.5, -0.25, 0.0, -0.25], '__ __ . __ __ . ');


    AddLayer('0','CONTINUOUS', 7 {white}); //must have!
    AddLayer('HIDDEN_YELLOW','HIDDEN', 2 {2 = yellow});
    AddLayer('CENTER_RED','CENTER', 1 {red});
    AddLayer('DOT_GREEN','DOT', 3 {green});
    AddLayer('DASHDOT_CYAN','DASHDOT', 4 {cyan});
    AddLayer('DIVIDE_BLUE','DIVIDE', 5 {blue});
    AddLayer('BORDER_MAGENTA','BORDER', 6 {magenta});
    AddLayer('CONTINUOUS_GRAY','CONTINUOUS',  8 {GRAY});

    ToDxf:= TStringList.Create;
end;

destructor TFPDxfWriteBridge.Destroy;
var
   i: integer;
   obj: TObject;
   dimClassNameFlag: integer;
   PLayerRecord: PDxfLayer;
begin
   ToDxf.Free;
   dimClassNameFlag:= -1;

   if DimStyleList <> nil then
   begin
      for i:=0 to DimStyleList.Count-1 do
      begin
         TDxfDimStyle(DimStyleList[i]).Free;
      end;
      DimStyleList.Free;
   end;

   if LineTypeList <> nil then
   begin
      for i:=0 to LineTypeList.Count-1 do
      begin
         TDxfLineType(LineTypeList[i]).Free;
      end;
      LineTypeList.Free;
   end;

   if TextStyleList <> nil then
   begin
      for i:=0 to TextStyleList.Count-1 do
      begin
         TDxfTextStyle(TextStyleList[i]).Free;
      end;
      TextStyleList.Free;
   end;

   {if DimStyleList <> nil then
   begin
      for i:=0 to DimStyleList.Count-1 do
      begin
         TDxfDimStyle(DimStyleList[i]).Free;
      end;
      DimStyleList.Free;
   end;}

   if LayerList <> nil then
   begin
     for i := 0 to (LayerList.Count - 1) do
     begin
        PLayerRecord:= LayerList.Items[i];
        Dispose(PLayerRecord);
     end;
     LayerList.Free;
   end;

   if DimBlockList <> nil then
   begin
      for i:=0 to DimBlockList.Count-1 do
      begin
            obj:= TObject(DimBlockList[i]);
            if CompareText(obj.ClassName, 'TDxfLinearDimension') = 0  then
                dimClassNameFlag:= 1;
            if CompareText(obj.ClassName, '(TDxfAngular3PDimension') = 0  then
                dimClassNameFlag:= 2;
            if CompareText(obj.ClassName ,'TDxfRadialDimension') = 0 then
                dimClassNameFlag:= 3;
            case dimClassNameFlag of
                 1: TDxfLinearDimension(DimBlockList[i]).Free;
                 2: TDxfAngular3PDimension(DimBlockList[i]).Free;
                 3: TDxfRadialDimension(DimBlockList[i]).Free;
            end;
      end;
      DimBlockList.Free;
   end;
   inherited Destroy;
end;

(* TODO:
{TDxfBridge}
constructor TDxfBridge.Create;
begin
    Write:= TFPDxfWriteBridge.Create;
    Read:= TDxfReadBridge.Create; //TODO: I need learn FPVectorial!
end;

destructor TDxfBridge.Destroy;
begin
    Write.Free;
    Read.Free; //TODO:
end;*)

end.

