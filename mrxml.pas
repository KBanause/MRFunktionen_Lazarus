unit mrxml; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite, XMLRead, base64, zstream;

const
  GZMAGIC = #$1F#$8B;

type
  TMRXMLNodeType = (mrxmlNormalNode, mrxmlCDATANode, mrxmlCommentNode);
  TMRXMLClass = class;
  TMRXMLNode = class;
  MRXMLString = ansistring;

  { TMRXMLNodeAttributes }

  TMRXMLNodeAttributes = class
  private
    FAttributes: TStringList;
    FValue: MRXMLString;
    function GetAsBoolean: Boolean;
    function GetAsDouble: Extended;
    function GetAsInteger: Int64;
    procedure SetAsBoolean(const AValue: Boolean);
    procedure SetAsDouble(const AValue: Extended);
    procedure SetAsInteger(const AValue: Int64);

    procedure SetValue(const AValue: MRXMLString);
  public
    constructor Create(AOwner: TMRXMLNode);
    destructor Destroy; override;

    function AsBooleanDef(DefValue: Boolean): Boolean;
    function AsDoubleDef(DefValue: Extended): Extended;
    function AsIntegerDef(DefValue: Int64): Int64;
    function ValueDef(DefValue: string): string;
  published
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDouble: Extended read GetAsDouble write SetAsDouble;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property Value: MRXMLString read FValue write SetValue;
  end;

  { TMRXMLNode }

  TMRXMLNode = class
  private
    FChildren: TList;
    FNodeName: MRXMLString;
    FNodeType: TMRXMLNodeType;
    FParentNode: TMRXMLNode;
    FProperties: TStringList;
    FValueStream: TMemoryStream;

    constructor Create(ANodeName: MRXMLString; AParentNode: TMRXMLNode);
    constructor Create;
    constructor CreateRoot(ANodeName: MRXMLString; AOwner: TMRXMLClass);
    destructor Destroy; override;

    function AddChild(NewChild: TMRXMLNode): TMRXMLNode;
    function GetAsBoolean: Boolean;
    function GetAsDouble: Extended;
    function GetAsInteger: Int64;
    function GetAsString: MRXMLString;
    function GetProperty(PropertyName: MRXMLString): TMRXMLNodeAttributes;

    procedure SetAsBoolean(const AValue: Boolean);
    procedure SetAsDouble(const AValue: Extended);
    procedure SetAsInteger(const AValue: Int64);
    procedure SetAsString(const AValue: MRXMLString);
    procedure SetNodeType(const AValue: TMRXMLNodeType);
  public
    function AddChild(ANodeName: MRXMLString): TMRXMLNode; overload;
    function AddChild(ANodeName: MRXMLString; aValue: Boolean): TMRXMLNode; overload;
    function AddChild(ANodeName: MRXMLString; aValue: Double): TMRXMLNode; overload;
    function AddChild(ANodeName: MRXMLString; aValue: Int64): TMRXMLNode; overload;
    function AddChild(ANodeName: MRXMLString; aValue: MRXMLString): TMRXMLNode; overload;
    function AsBooleanDef(DefValue: Boolean): Boolean;
    function AsDoubleDef(DefValue: Extended): Extended;
    function AsIntegerDef(DefValue: Int64): Int64;
    function Child(Childname: string): TMRXMLNode;
    function Child(Index: Integer): TMRXMLNode;
    function CountChildren: Integer;
    function GetAttributes: TStringList;
    function GetFirstChild: TMRXMLNode;
    function GetNextSibling(ChildNode: TMRXMLNode): TMRXMLNode;
    function GetNextSibling: TMRXMLNode;
    function GetPrevSibling(ChildNode: TMRXMLNode): TMRXMLNode;
    function GetPrevSibling: TMRXMLNode;
    function HasAttributes: Integer;

    procedure AsStream(Stream: TStream);
    procedure Clear;
    procedure DeleteChild(ChildNode: TMRXMLNode);
    procedure GetAttributes(List: TStrings);
    procedure GetValueAsStream(Stream: TStream);
    procedure GetValueAsStream(Stream: TStream; asBase64: Boolean);
    procedure SetValueAsStream(Stream: TStream);
    procedure SetValueAsStream(Stream: TStream; asBase64: Boolean);

    property Attribute[PropertyName: MRXMLString]: TMRXMLNodeAttributes read GetProperty;
  published
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDouble: Extended read GetAsDouble write SetAsDouble;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property NodeName: MRXMLString read FNodeName;
    property NodeType: TMRXMLNodeType read FNodeType write SetNodeType;
    property ParentNode: TMRXMLNode read FParentNode;
    property Value: MRXMLString read GetAsString write SetAsString;
  end;

  { TMRXMLClass }

  TMRXMLClass = class
  private
    FRootNode: TMRXMLNode;
    Fxdoc: TXMLDocument;

    function AddRootChild(ANodeName: MRXMLString): TMRXMLNode;

    procedure DocumentToNodeList;
    procedure NodeListToDocument;
    procedure SetSalt(var salt: string);
  public
    constructor Create(AFileName: string); overload;
    constructor Create; overload;
    destructor Destroy; override;

    function AddChild(ANodeName: MRXMLString): TMRXMLNode;
    function AddChild(AParentNode: TMRXMLNode; ANodeName: MRXMLString): TMRXMLNode;
    function CreateRoot(RootName: MRXMLString): TMRXMLNode;
    function MoveNode(NodeToMove: TMRXMLNode; NewParent: TMRXMLNode): TMRXMLNode;

    procedure LoadFromFile(FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadfromString(SrcString: MRXMLString);
    procedure SaveToFile(FileName: string);
    procedure SaveToFile(FileName: string; compressed: Boolean);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream; compressed: Boolean);
    procedure SaveToString(var Dst: MRXMLString);
    procedure SaveToString(var Dst: MRXMLString; compressed: Boolean);
  published
    property RootNode: TMRXMLNode read FRootNode;
  end;

implementation

const
  SALTSIZE = 32;

{ TMRXMLClass }

procedure TMRXMLClass.DocumentToNodeList;
  procedure InsertChildren(ParentDOMNode: TDOMNode; ParentMRNode: TMRXMLNode);
  var
    attr: TDOMNamedNodeMap;
    cdnode: TDOMElement;
    dnode: TDOMElement;
    cmnode: TMRXMLNode;
    i: Integer;
  begin
    cdnode := TDOMElement(ParentDOMNode.FirstChild);

    while (cdnode <> nil) do
    begin
      i := cdnode.NodeType;

      if ((cdnode.NodeType = TEXT_NODE) or (cdnode.NodeType = CDATA_SECTION_NODE)) then
      begin
        ParentMRNode.Value := UTF8Encode(cdnode.NodeValue);

        if (cdnode.NodeType = CDATA_SECTION_NODE) then
          ParentMRNode.NodeType := mrxmlCDATANode
      end // if (cdnode.NodeType = TEXT_NODE)
      else
      if (cdnode.NodeType = ELEMENT_NODE) then
      begin
        cmnode := TMRXMLNode.Create(UTF8Encode(cdnode.NodeName), ParentMRNode);

        if (cdnode.HasAttributes) then
        begin
          attr := cdnode.Attributes;

          for i := 0 to attr.Length - 1 do
          begin
            dnode := TDOMElement(attr[i]);
            cmnode.Attribute[dnode.NodeName].Value := UTF8Encode(cdnode.GetAttribute(dnode.NodeName));
          end; // for i := 0 to attr.Length - 1
        end; // if (domnode.HasAttributes)

        InsertChildren(cdnode, cmnode);
      end // if (cdnode.NodeType = ELEMENT_NODE)
      else
      if (cdnode.NodeType = COMMENT_NODE) then
      begin
        cmnode := TMRXMLNode.Create(UTF8Encode(cdnode.NodeValue), ParentMRNode);
        cmnode.NodeType := mrxmlCommentNode;
      end; // if ()

      cdnode := TDOMElement(cdnode.NextSibling);
    end; // while (cdnode <> nil)
  end;

var
  attr: TDOMNamedNodeMap;
  dnode: TDOMElement;
  domnode: TDOMElement;
  i: Integer;
begin
  if (FRootNode <> nil) then
    Exit;

  domnode := TDOMElement(Fxdoc.FirstChild);
  CreateRoot(UTF8Encode(domnode.NodeName));

  if (domnode.HasAttributes) then
  begin
    attr := domnode.Attributes;

    for i := 0 to attr.Length - 1 do
    begin
      dnode := TDOMElement(attr[i]);
      FRootNode.Attribute[dnode.NodeName].Value := UTF8Encode(domnode.GetAttribute(dnode.NodeName));
    end; // for i := 0 to attr.Length - 1
  end; // if (domnode.HasAttributes)

  InsertChildren(domnode, FRootNode);
end;

procedure TMRXMLClass.LoadFromFile(FileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  LoadFromStream(fs);
  fs.Free;
end;

procedure TMRXMLClass.LoadFromStream(Stream: TStream);
//const
  //XMLNORMAL: array [0..4] of Byte = ($3C, $3F, $78, $6D, $6C);
  //XMLUTF8: array[0..11]of Byte = ($FF, $FE, $3C, $00, $3F, $00, $78, $00, $6D, $00, $6C, $00);
var
  s: String;
  ms: TMemoryStream;
  sc: TMemoryStream;
  buf: array[0..4095]of byte;
  gz: Tdecompressionstream;
  count: LongInt;
begin
  SetLength(s, 2);
  FillChar(s[1], Length(s), 0);
  ms := TMemoryStream.Create;
  ms.CopyFrom(Stream, Stream.Size - Stream.Position);
  ms.Position := 0;
  SetLength(s, 2);
  FillChar(s[1], Length(s), 0);
  ms.Read(s[1], 2);

  if (s = GZMAGIC) then
  begin
    gz := Tdecompressionstream.create(ms);
    sc := TMemoryStream.Create;
    count := gz.read(buf, SizeOf(buf));

    while (count > 0) do
    begin
      sc.Write(buf, count);
      count := gz.read(buf, SizeOf(buf));
    end; // while (count > 0)

    ms.Free;
    ms := sc;
    gz.Free;
  end; // if (s = GZMAGIC)

  ms.Position := 0;
  ReadXMLFile(Fxdoc, ms);
  DocumentToNodeList;
  Fxdoc.Free;
end;

procedure TMRXMLClass.LoadfromString(SrcString: MRXMLString);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.Write(SrcString[1], Length(SrcString));
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

function TMRXMLClass.MoveNode(NodeToMove: TMRXMLNode;
  NewParent: TMRXMLNode): TMRXMLNode;
var
  oldp: TMRXMLNode;
  i: LongInt;
begin
  if (NewParent = nil) then
    NewParent := FRootNode;

  oldp := NodeToMove.ParentNode;
  i := oldp.FChildren.IndexOf(NodeToMove);

  if (i > -1) then
  begin
    oldp.FChildren.Delete(i);
  end; // if (i > -1)

  NewParent.FChildren.Add(NodeToMove);
  NodeToMove.FParentNode := NewParent;
  Result := NewParent;
end;

procedure TMRXMLClass.NodeListToDocument;
  function AddNodeChildren(mrnode: TMRXMLNode): TDOMNode;
  var
    i: LongInt;
    j: Integer;
    Liste: TStringList;
    mrnode2: TMRXMLNode;
    node: TDOMNode;
    node2: TDOMText;
    node3: TDOMNode;
  begin
    //node := Fxdoc.CreateElement(ConvertToXML(mrnode.NodeName));
    if (mrnode.NodeType = mrxmlCommentNode) then
      node := Fxdoc.CreateComment(UTF8Decode(mrnode.NodeName))
    else
      node := Fxdoc.CreateElement(UTF8Decode(mrnode.NodeName));

    if (mrnode.Value <> #0) then
    begin
      if (mrnode.NodeType = mrxmlCDATANode) then
        node2 := Fxdoc.CreateCDATASection(UTF8Decode(mrnode.Value))
      else
        node2 := Fxdoc.CreateTextNode(UTF8Decode(mrnode.Value));

      node.AppendChild(node2);
    end; // if (mrnode.Value <> '')

    i := mrnode.HasAttributes;

    if (i > 0) then
    begin
      Liste := TStringList.Create;
      mrnode.GetAttributes(Liste);

      for j := 0 to Liste.Count - 1 do
      begin
        TDOMElement(node).SetAttribute(Liste[j], UTF8Decode(mrnode.Attribute[Liste[j]].Value));
      end; // for j := 0 to Liste.Count - 1

      Liste.Free;
    end; // if (i > 0)

    mrnode2 := mrnode.GetFirstChild;

    while (mrnode2 <> nil) do
    begin
      node3 := AddNodeChildren(mrnode2);
      node.AppendChild(node3);
      mrnode2 := mrnode2.GetNextSibling;
    end; // while (mrnode2 <> nil)

    Result := node;
  end;
var
  mrnode: TMRXMLNode;
  node: TDOMNode;
  xrootnode: TDOMElement;
  j: Integer;
  Liste: TStringList;
begin
  Fxdoc := TXMLDocument.Create;
  xrootnode := Fxdoc.CreateElement(UTF8Decode(FRootNode.NodeName));
  Liste := FRootNode.GetAttributes;

  for j := 0 to Liste.Count - 1 do
  begin
    TDOMElement(xrootnode).SetAttribute(Liste[j], UTF8Decode(FRootNode.Attribute[Liste[j]].Value));
  end; // for j := 0 to Liste.Count - 1

  Liste.Free;
  Fxdoc.AppendChild(xrootnode);
  mrnode := FRootNode.GetFirstChild;

  while (mrnode <> nil) do
  begin
    node := AddNodeChildren(mrnode);
    xrootnode.AppendChild(node);
    mrnode := mrnode.GetNextSibling;
  end; // while (mrnode <> nil)
end;

constructor TMRXMLClass.Create(AFileName: string);
begin
  FRootNode := nil;

  if (AFilename <> '') then
  begin
    LoadFromFile(AFileName);
  end // if (FFilename <> '')
  else
  begin
    Fxdoc := TXMLDocument.Create;
    FRootNode := nil;
  end; // else (if (FFilename <> ''))
end;

constructor TMRXMLClass.Create;
begin
  Self.Create('');
end;

destructor TMRXMLClass.Destroy;
begin
  inherited Destroy;
end;

function TMRXMLClass.AddChild(ANodeName: MRXMLString): TMRXMLNode;
begin
  Result := AddRootChild(ANodeName);
end;

function TMRXMLClass.AddChild(AParentNode: TMRXMLNode;
  ANodeName: MRXMLString): TMRXMLNode;
begin
  Result := TMRXMLNode.Create(ANodeName, AParentNode);
end;

function TMRXMLClass.AddRootChild(ANodeName: MRXMLString): TMRXMLNode;
begin
  Result := AddChild(FRootNode, ANodeName);
end;

function TMRXMLClass.CreateRoot(RootName: MRXMLString): TMRXMLNode;
begin
  if (FRootNode = nil) then
  begin
    FRootNode := TMRXMLNode.Create(RootName, nil);
  end; // if (FRootNode = nil)

  Result := FRootNode;
end;

procedure TMRXMLClass.SaveToFile(FileName: string);
begin
  SaveToFile(FileName, False);
end;

procedure TMRXMLClass.SaveToFile(FileName: string; compressed: Boolean);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  SaveToStream(fs, compressed);
  fs.Free;
end;

procedure TMRXMLClass.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, False);
end;

procedure TMRXMLClass.SaveToStream(Stream: TStream; compressed: Boolean);
var
  gz: Tcompressionstream;
  sc: TMemoryStream;
  st: TMemoryStream;
  s: String;
begin
  NodeListToDocument;
  st := TMemoryStream.Create;
  WriteXMLFile(Fxdoc, st);
  Fxdoc.Free;

  if (compressed) then
  begin
    st.Position := 0;
    sc := TMemoryStream.Create;
    sc.Write(GZMAGIC[1], 2);
    gz := Tcompressionstream.create(clmax, sc);
    gz.CopyFrom(st, st.Size);
    gz.Free;
    st.Free;
    st := sc;
  end; // if (compressed)

  st.Position := 0;
  Stream.CopyFrom(st, st.Size);
  st.Free;
end;

procedure TMRXMLClass.SaveToString(var Dst: MRXMLString);
begin
  SaveToString(Dst, False);
end;

procedure TMRXMLClass.SaveToString(var Dst: MRXMLString; compressed: Boolean);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms, compressed);
  ms.Position := 0;
  SetLength(Dst, ms.Size);
  ms.Read(Dst[1], ms.Size);
  ms.Free;
end;

procedure TMRXMLClass.SetSalt(var salt: string);
var
  i: Integer;
begin
  for i := 0 to 999 do
    Random(High(LongInt));

  i := SALTSIZE;
  SetLength(Salt, i);

  for i := 1 to Length(Salt) do
  begin
    Salt[i] := Chr(Random(256));
  end; // for i := 0 to High(Salt)
end;

{ TMRXMLNode }

constructor TMRXMLNode.Create(ANodeName: MRXMLString; AParentNode: TMRXMLNode);
var
  c: Char;
begin
  c := #0;
  FChildren := TList.Create;
  FNodeName := ANodeName;
  FNodeType := mrxmlNormalNode;
  FParentNode := AParentNode;
  FProperties := TStringList.Create;
  FValueStream := TMemoryStream.Create;
  FValueStream.Write(c, 1);

  if (FParentNode <> nil) then
    FParentNode.AddChild(Self);
end;

constructor TMRXMLNode.Create;
begin
  raise Exception.Create('Not allowed');
end;

constructor TMRXMLNode.CreateRoot(ANodeName: MRXMLString; AOwner: TMRXMLClass);
begin
  Self.Create(ANodeName, nil);
end;

procedure TMRXMLNode.DeleteChild(ChildNode: TMRXMLNode);
var
  i: LongInt;
begin
  i := FChildren.IndexOf(ChildNode);

  if (i > -1) then
  begin
    FChildren.Delete(i);
    ChildNode.Free;
  end; // if (i > -1)
end;

destructor TMRXMLNode.Destroy;
var
  i: Integer;
begin
  Clear;

  for i := 0 to FProperties.Count - 1 do
    TObject(FProperties.Objects[i]).Free;

  FProperties.Free;
  FValueStream.Free;

  inherited Destroy;
end;

function TMRXMLNode.GetProperty(PropertyName: MRXMLString): TMRXMLNodeAttributes;
var
  i: LongInt;
  attr: TMRXMLNodeAttributes;
begin
  i := FProperties.IndexOf(PropertyName);
  Result := nil;

  if (i > -1) then
    Result := TMRXMLNodeAttributes(FProperties.Objects[i])
  else
  begin
    attr := TMRXMLNodeAttributes.Create(Self);
    FProperties.AddObject(PropertyName, attr);
    Result := attr;
  end;
end;

procedure TMRXMLNode.GetValueAsStream(Stream: TStream);
begin
  GetValueAsStream(Stream, False);
end;

procedure TMRXMLNode.GetValueAsStream(Stream: TStream; asBase64: Boolean);
  function HexToByte(HexValue: string): byte;
  const
    hexstr = '0123456789ABCDEF';
  var
    i1: Integer;
    i2: Integer;
  begin
    HexValue := UpperCase(HexValue);
    i1 := Pos(HexValue[1], hexstr);
    i2 := Pos(HexValue[2], hexstr);

    if ((i1 * i2) > 0) then
      Result := 16 * (i1 - 1) + (i2 - 1)
    else
      raise EConvertError.CreateFmt('"%s" is not a hex-value' ,[Copy(HexValue, 1, 2)]);
  end;

var
  b64: TBase64DecodingStream;
  buffer: string;
  gelesen: LongInt;
  b: Byte;
  s: String;
begin
  FValueStream.Position := 0;

  if (asBase64) then
  begin
    b64 := TBase64DecodingStream.Create(FValueStream, bdmStrict);
    Stream.CopyFrom(b64, b64.Size);
    b64.Free;
  end // if (asBase64)
  else
  begin
    SetLength(buffer, 2);
    gelesen := FValueStream.Read(buffer[1], 2);
    s := '';

    while (gelesen > 1) do
    begin
      b := HexToByte(buffer);
      s := s + Chr(b);
      gelesen := FValueStream.Read(buffer[1], 2);
    end; // while (gelesen > 0)

    Stream.Write(s[1], Length(s));
  end; // else (if (asBase64))
end;

function TMRXMLNode.HasAttributes: Integer;
begin
  Result := FProperties.Count;
end;

procedure TMRXMLNode.SetValueAsStream(Stream: TStream);
begin
  SetValueAsStream(Stream, False);
end;

procedure TMRXMLNode.SetValueAsStream(Stream: TStream; asBase64: Boolean);
const
  BUFSIZE = 200;
var
  b64: TBase64EncodingStream;
  buffer: array[0..BUFSIZE - 1]of byte;
  gelesen: LongInt;
  i: Integer;
  s: String;
begin
  FValueStream.Position := 0;
  FValueStream.Size := 0;

  if (asBase64) then
  begin
    b64 := TBase64EncodingStream.Create(FValueStream);
    b64.CopyFrom(Stream, Stream.Size - Stream.Position);
    b64.Free;
  end // if (asBase64)
  else
  begin
    gelesen := Stream.Read(buffer, BUFSIZE);

    while (gelesen > 0) do
    begin
      s := '';

      for i := 0 to gelesen - 1 do
      begin
        s := s + IntToHex(buffer[i], 2);
      end; // for i := 0 to gelesen - 1

      s := UpperCase(s);
      FValueStream.Write(s[1], Length(s));
      gelesen := Stream.Read(buffer, BUFSIZE);
    end; // while (gelesen > 0)
  end; // else (if (asBase64))
end;

procedure TMRXMLNode.SetAsBoolean(const AValue: Boolean);
var
  s: MRXMLString;
begin
  s := BoolToStr(AValue, 'true', 'false');
  SetAsString(s);
end;

procedure TMRXMLNode.SetAsDouble(const AValue: Extended);
var
  s: MRXMLString;
begin
  s := FloatToStr(AValue);
  SetAsString(s);
end;

procedure TMRXMLNode.SetAsInteger(const AValue: Int64);
var
  s: MRXMLString;
begin
  s := IntToStr(AValue);
  SetAsString(s);
end;

procedure TMRXMLNode.SetAsString(const AValue: MRXMLString);
begin
  FValueStream.Position := 0;
  FValueStream.Size := 0;
  FValueStream.Write(AValue[1], Length(AValue));
  FNodeType := mrxmlNormalNode;
end;

procedure TMRXMLNode.SetNodeType(const AValue: TMRXMLNodeType);
begin
  if FNodeType = AValue then exit;
  FNodeType := AValue;
end;

function TMRXMLNode.AddChild(ANodeName: MRXMLString): TMRXMLNode;
var
  node: TMRXMLNode;
begin
  node := TMRXMLNode.Create(ANodeName, Self);
  //FChildren.Add(node);
  Result := node;
end;

function TMRXMLNode.AddChild(ANodeName: MRXMLString;
  aValue: Boolean): TMRXMLNode;
var
  node: TMRXMLNode;
begin
  node := AddChild(ANodeName);
  node.AsBoolean := aValue;
  Result := node;
end;

function TMRXMLNode.AddChild(ANodeName: MRXMLString;
  aValue: Double): TMRXMLNode;
var
  node: TMRXMLNode;
begin
  node := AddChild(ANodeName);
  node.AsDouble := aValue;
  Result := node;
end;

function TMRXMLNode.AddChild(ANodeName: MRXMLString;
  aValue: Int64): TMRXMLNode;
var
  node: TMRXMLNode;
begin
  node := AddChild(ANodeName);
  node.AsInteger := aValue;
  Result := node;
end;

function TMRXMLNode.AddChild(ANodeName: MRXMLString;
  aValue: MRXMLString): TMRXMLNode;
var
  node: TMRXMLNode;
begin
  node := AddChild(ANodeName);
  node.Value := aValue;
  Result := node;
end;

function TMRXMLNode.AddChild(NewChild: TMRXMLNode): TMRXMLNode;
begin
  FChildren.Add(NewChild);
  Result := NewChild;
end;

function TMRXMLNode.GetFirstChild: TMRXMLNode;
begin
  Result := nil;

  if (FChildren.Count > 0) then
    Result := TMRXMLNode(FChildren[0]);
end;

function TMRXMLNode.GetNextSibling(ChildNode: TMRXMLNode): TMRXMLNode;
var
  index: Integer;
begin
  Result := nil;
  index := FChildren.IndexOf(ChildNode) + 1;

  if ((index > 0) and (index < FChildren.Count)) then
    Result := TMRXMLNode(FChildren[index]);
end;

function TMRXMLNode.GetNextSibling: TMRXMLNode;
begin
  Result := nil;

  if (FParentNode <> nil) then
    Result := FParentNode.GetNextSibling(Self);
end;

function TMRXMLNode.GetPrevSibling(ChildNode: TMRXMLNode): TMRXMLNode;
var
  index: Integer;
begin
  index := FChildren.IndexOf(ChildNode) - 1;
  Result := nil;

  if ((index > -1) and (index < FChildren.Count)) then
    Result := TMRXMLNode(FChildren[index]);
end;

function TMRXMLNode.GetPrevSibling: TMRXMLNode;
begin
  Result := nil;

  if (FParentNode <> nil) then
    FParentNode.GetPrevSibling(Self);
end;

function TMRXMLNode.AsBooleanDef(DefValue: Boolean): Boolean;
var
  s: String;
begin
  s := GetAsString;
  Result := StrToBoolDef(s, DefValue);
end;

function TMRXMLNode.GetAsBoolean: Boolean;
begin
  Result := StrToBool(GetAsString);
end;

function TMRXMLNode.AsDoubleDef(DefValue: Extended): Extended;
var
  s: MRXMLString;
  ds: Char;
begin
  ds := DecimalSeparator;
  DecimalSeparator := '.';
  s := GetAsString;
  Result := StrToFloatDef(s, DefValue);
  DecimalSeparator := ds;
end;

function TMRXMLNode.GetAsDouble: Extended;
var
  s: MRXMLString;
  ds: Char;
begin
  ds := DecimalSeparator;
  DecimalSeparator := '.';
  s := GetAsString;
  Result := StrToFloat(s);
  DecimalSeparator := ds;
end;

function TMRXMLNode.AsIntegerDef(DefValue: Int64): Int64;
var
  s: MRXMLString;
begin
  s := GetAsString;
  Result := StrToIntDef(s, DefValue);
end;

procedure TMRXMLNode.AsStream(Stream: TStream);
begin
  FValueStream.Position := 0;
  Stream.Write(FValueStream.Memory, FValueStream.Size);
end;

function TMRXMLNode.Child(Index: Integer): TMRXMLNode;
var
  node: TMRXMLNode;
begin
  node := nil;

  if ((Index > -1) and (Index < FChildren.Count)) then
  begin
    node := TMRXMLNode(FChildren[Index]);
  end; // if ((Index > -1) and (Index < FChildren.Count))

  Result := node;
end;

function TMRXMLNode.Child(Childname: string): TMRXMLNode;
var
  node: TMRXMLNode;
begin
  node := GetFirstChild;

  while (node <> nil) do
  begin
    if (node.NodeName = Childname) then
    begin
      Break;
    end; // if (node.NodeName = Childname)

    node := node.GetNextSibling;
  end; // while (node <> nil)

  if (node = nil) then
  begin
    node := AddChild(Childname);
  end; // if (node = nil)

  Result := node;
end;

function TMRXMLNode.GetAsInteger: Int64;
var
  s: MRXMLString;
begin
  s := GetAsString;
  Result := StrToInt(s);
end;

function TMRXMLNode.GetAsString: MRXMLString;
var
  s: MRXMLString;
begin
  s := '';

  if (FValueStream.Size > 0) then
  begin
    SetLength(s, FValueStream.Size);
    FValueStream.Position := 0;
    FValueStream.Read(s[1], FValueStream.Size);
  end;

  Result := s;
end;

procedure TMRXMLNode.Clear;
var
  node: TMRXMLNode;
begin
  node := GetFirstChild;

  while (node <> nil) do
  begin
    node.Clear;
    node.Free;
    FChildren.Delete(0);
    node := GetFirstChild;
  end; // while (node <> nil)
end;

function TMRXMLNode.CountChildren: Integer;
begin
  Result := FChildren.Count;
end;

procedure TMRXMLNode.GetAttributes(List: TStrings);
begin
  List.Assign(FProperties);
end;

function TMRXMLNode.GetAttributes: TStringList;
begin
  Result := TStringList.Create;
  GetAttributes(Result);
end;

{ TMRXMLNodeAttributes }

procedure TMRXMLNodeAttributes.SetValue(const AValue: MRXMLString);
begin
  if FValue=AValue then exit;
  FValue:=AValue;
end;

function TMRXMLNodeAttributes.ValueDef(DefValue: string): string;
var
  s: String;
begin
  s := FValue;

  if (s = '') then
    s := DefValue;

  Result := s;
end;

constructor TMRXMLNodeAttributes.Create(AOwner: TMRXMLNode);
begin
  FAttributes := TStringList.Create;
  FValue := '';
end;

destructor TMRXMLNodeAttributes.Destroy;
begin
  FAttributes.Free;

  inherited Destroy;
end;

function TMRXMLNodeAttributes.AsBooleanDef(DefValue: Boolean): Boolean;
var
  s: String;
begin
  s := FValue;

  if (s = '') then
    s := BoolToStr(DefValue, 'true', 'false');

  Result := StrToBoolDef(s, DefValue);
end;

function TMRXMLNodeAttributes.GetAsBoolean: Boolean;
var
  s: String;
begin
  s := FValue;

  if (s = '') then
    s := 'false';

  Result := StrToBool(s);
end;

procedure TMRXMLNodeAttributes.SetAsBoolean(const AValue: Boolean);
begin
  FValue := BoolToStr(AValue, 'true', 'false');
end;

procedure TMRXMLNodeAttributes.SetAsDouble(const AValue: Extended);
begin
  FValue := FloatToStr(AValue);
end;

procedure TMRXMLNodeAttributes.SetAsInteger(const AValue: Int64);
begin
  FValue := IntToStr(AValue);
end;

function TMRXMLNodeAttributes.AsDoubleDef(DefValue: Extended): Extended;
begin
  Result := StrToFloatDef(FValue, DefValue);
end;

function TMRXMLNodeAttributes.GetAsDouble: Extended;
begin
  Result := StrToFloat(FValue);
end;

function TMRXMLNodeAttributes.AsIntegerDef(DefValue: Int64): Int64;
begin
  Result := StrToIntDef(FValue, DefValue);
end;

function TMRXMLNodeAttributes.GetAsInteger: Int64;
begin
  Result := StrToInt(FValue);
end;

end.

