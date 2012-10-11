unit MRFunktionen_Arrays;

(*
 *
 * $Id: MRFunktionen_Arrays.pas 17 2008-04-15 12:40:22Z marc $
 *
 *)

interface

uses Classes, {$IFDEF WINDOWS}Windows,{$ENDIF} MRFunktionen_Errors;

type
  TMRAForeach = procedure(Index: Integer; Value: Pointer; UserData: Pointer) of object;
  TMRAForeachAs = procedure(Key: string; Value: Pointer; UserData: Pointer) of object;
  TMRIAForeach = procedure(Index: Integer; Value: Int64; UserData: Pointer) of object;
  TMRIAForeachAs = procedure(Key: string; Value: Int64; UserData: Pointer) of object;
  TMRSAForeach = procedure(Index: Integer; Value: string; UserData: Pointer) of object;
  TMRSAForeachAs = procedure(Key: string; Value: string; UserData: Pointer) of object;

  TMRIniErrors = (mrinierr_NoError, mrinierr_NoFilenameGiven,
    mrinierr_FileNotExists, mrinierr_FileNotReadable,
    mrinierr_NoFirstSectionFound);

  TMRArray = class(TObject)
  private
    FCurIndex: Integer;
    FDefValue: Pointer;
    FItems: TStringList;
    FListe: string;
    FUseDefValue: Boolean;

    function GetCurrent: Pointer;
    function GetCurrentKey: string;
    function GetItemsIndex(Index: Integer): Pointer;
    function GetItemsStr(Key: string): Pointer;
    function GetKey(Index: Integer): string;

    procedure AddNew;
    procedure CheckKey(var Key: string);
    procedure Initialize(Laenge: Cardinal);
    procedure SetDefValue(const Value: Pointer);
    procedure SetItemsIndex(Index: Integer; const Value: Pointer);
    procedure SetItemsStr(Key: string; const Value: Pointer);
    procedure SetLength(Laenge: Cardinal);
    procedure SetUseDefValue(const Value: Boolean);
  public
    constructor Create; overload;
    constructor Create(Laenge: Cardinal); overload;
    constructor Create(dValue: Pointer); overload;
    destructor Destroy; override;

    function Count: Integer;
    function Next: Boolean;
    function Prev: Boolean;
    function First: Boolean;
    function Last: Boolean;
    function KeyExists(Key: string): Boolean;
    function IndexOf(Value: Pointer): string;
    function RenameIndex(oldIndex, newIndex: string): Boolean;

    procedure Add(Value: Pointer); overload;
    procedure Add(Value: Pointer; Key: string); overload;
    procedure Clear;
    procedure Foreach(DoForeach: TMRAForeach; UserData: Pointer);
    procedure ForeachAs(DoForeachAs: TMRAForeachAs; UserData: Pointer);
    procedure Sort;
    procedure SortByIndex;
    procedure SortByValue;
    procedure Unset(Key: string);

    property Items[Index: Integer]: Pointer read GetItemsIndex write SetItemsIndex; default;
    property ItemsStr[Key: string]: Pointer read GetItemsStr write SetItemsStr;
    property Key[Index: Integer]: string read GetKey;
    property Liste: string read FListe;
    property Current: Pointer read GetCurrent;
    property CurrentKey: string read GetCurrentKey;
    property DefValue: Pointer read FDefValue write SetDefValue default nil;
    property UseDefValue: Boolean read FUseDefValue write SetUseDefValue default False;
  end;

  TMRIntegerArray = class(TObject)
  private
    FCurIndex: Integer;
    FDefValue: Int64;
    FItems: TStringList;
    FListe: string;
    FUseDefValue: Boolean;

    function GetCurrent: Int64;
    function GetCurrentKey: string;
    function GetItemsIndex(Index: Integer): Int64;
    function GetItemsStr(Key: string): Int64;
    function GetKey(Index: Integer): string;

    procedure AddNew;
    procedure CheckKey(var Key: string);
    procedure Initialize(Laenge: Cardinal);
    procedure SetDefValue(const Value: Int64);
    procedure SetItemsIndex(Index: Integer; const Value: Int64);
    procedure SetItemsStr(Key: string; const Value: Int64);
    procedure SetLength(Laenge: Cardinal);
    procedure SetUseDefValue(const Value: Boolean);
  public
    constructor Create; overload;
    constructor Create(Laenge: Cardinal); overload;
    constructor Create(Laenge: Cardinal; DefValue: Int64); overload;
    constructor Create(dValue: Int64); overload;
    destructor Destroy; override;

    function Count: Integer;
    function Next: Boolean;
    function Prev: Boolean;
    function First: Boolean;
    function Last: Boolean;
    function KeyExists(Key: string): Boolean;
    function IndexOf(Value: Int64): string;
    function Text: string;

    procedure Add(Value: Int64); overload;
    procedure Add(Value: Int64; Key: string); overload;
    procedure Clear;
    procedure Foreach(DoForeach: TMRIAForeach; UserData: Pointer);
    procedure ForeachAs(DoForeachAs: TMRIAForeachAs; UserData: Pointer);
    procedure Sort;
    procedure SortByIndex;
    procedure SortByValue;
    procedure Unset(Key: string); overload;
    procedure Unset(Key: Integer); overload;

    property Items[Index: Integer]: Int64 read GetItemsIndex write SetItemsIndex; default;
    property ItemsStr[Key: string]: Int64 read GetItemsStr write SetItemsStr;
    property Key[Index: Integer]: string read GetKey;
    property Liste: string read FListe;
    property Current: Int64 read GetCurrent;
    property CurrentKey: string read GetCurrentKey;
    property DefValue: Int64 read FDefValue write SetDefValue default 0;
    property UseDefValue: Boolean read FUseDefValue write SetUseDefValue default False;
  end;

  TMRStringArray = class(TObject)
  private
    FCurIndex: Integer;
    FDefValue: string;
    FItems: TStringList;
    FListe: string;
    FUseDefValue: Boolean;

    function GetCurrent: string;
    function GetCurrentKey: string;
    function GetItemsIndex(Index: Integer): string;
    function GetItemsStr(Key: string): string;
    function GetKey(Index: Integer): string;

    procedure AddNew;
    procedure CheckKey(var Key: string);
    procedure Initialize(Laenge: Cardinal);
    procedure SetDefValue(const Value: string);
    procedure SetItemsIndex(Index: Integer; const Value: string);
    procedure SetItemsStr(Key: string; const Value: string);
    procedure SetLength(Laenge: Cardinal);
    procedure SetUseDefValue(const Value: Boolean);
  public
    constructor Create; overload;
    constructor Create(Laenge: Cardinal); overload;
    constructor Create(sDefValue: string); overload;
    constructor Create(Laenge: Cardinal; sDefValue: string); overload;
    destructor Destroy; override;

    function Count: Integer;
    function First: Boolean;
    function IndexOf(Value: string): string;
    function KeyExists(Key: string): Boolean;
    function Last: Boolean;
    function Next: Boolean;
    function Prev: Boolean;
    function Text: string;
    function IsLast: Boolean;
    function IsFirst: Boolean;

    procedure Add(Value: string); overload;
    procedure Add(Value: string; Key: string); overload;
    procedure Clear;
    procedure Foreach(DoForeach: TMRSAForeach; UserData: Pointer);
    procedure ForeachAs(DoForeachAs: TMRSAForeachAs; UserData: Pointer);
    procedure Sort;
    procedure SortByIndex;
    procedure SortByValue;
    procedure Unset(Key: string);
    procedure GetKeyNames(Strings: TStrings);

    property Items[Index: Integer]: string read GetItemsIndex write SetItemsIndex; default;
    property ItemsStr[Key: string]: string read GetItemsStr write SetItemsStr;
    property Key[Index: Integer]: string read GetKey;
    property Liste: string read FListe;
    property Current: string read GetCurrent;
    property CurrentKey: string read GetCurrentKey;
    property DefValue: string read FDefValue write SetDefValue;
    property UseDefValue: Boolean read FUseDefValue write SetUseDefValue default False;
  end;

  TMRArray2D = class
  private
    FLengthX: Cardinal;
    FLengthY: Cardinal;
    FRows: TList;
    FUseDefValue: Boolean;
    FDefValue: Pointer;

    function GetItems(const X, Y: Cardinal): Pointer;

    procedure AddNewRow(Laenge: Cardinal);
    procedure InitDevValue;
    procedure SetDefValue(const Value: Pointer);
    procedure SetItems(const X, Y: Cardinal; Value: Pointer);
    procedure SetLengthX(const Value: Cardinal);
    procedure SetLengthY(const Value: Cardinal);
    procedure SetUseDefValue(const Value: Boolean);
  public
    constructor Create(x, y: Cardinal); overload;
    constructor Create; overload;
    destructor Destroy; override;

    property DefValue: Pointer read FDefValue write SetDefValue default nil;
    property Items[const X, Y: Cardinal]: Pointer read GetItems write SetItems; default;
    property LengthX: Cardinal read FLengthX write SetLengthX default 0;
    property LengthY: Cardinal read FLengthY write SetLengthY default 0;
    property UseDevValue: Boolean read FUseDefValue write SetUseDefValue default False;
  end;

  TMRIntArray2D = class
  private
    FLengthX: Cardinal;
    FLengthY: Cardinal;
    FRows: TList;
    FUseDefValue: Boolean;
    FDefValue: Int64;

    function GetItems(const X, Y: Cardinal): Int64;

    procedure AddNewRow(Laenge: Cardinal);
    procedure InitDevValue;
    procedure SetDefValue(const Value: Int64);
    procedure SetItems(const X, Y: Cardinal; Value: Int64);
    procedure SetLengthX(const Value: Cardinal);
    procedure SetLengthY(const Value: Cardinal);
    procedure SetUseDefValue(const Value: Boolean);
    function GetText(const Zeile: Integer): string;
  public
    constructor Create(x, y: Cardinal); overload;
    constructor Create; overload;
    destructor Destroy; override;

    property DefValue: Int64 read FDefValue write SetDefValue default 0;
    property Items[const X, Y: Cardinal]: Int64 read GetItems write SetItems; default;
    property LengthX: Cardinal read FLengthX write SetLengthX default 0;
    property LengthY: Cardinal read FLengthY write SetLengthY default 0;
    property UseDevValue: Boolean read FUseDefValue write SetUseDefValue default False;
    property Text[const Zeile: Integer]: string read GetText;
  end;

  TMRStrArray2D = class
  private
    FLengthX: Cardinal;
    FLengthY: Cardinal;
    FRows: TList;
    FUseDefValue: Boolean;
    FDefValue: string;

    function GetItems(const X, Y: Cardinal): string;

    procedure AddNewRow(Laenge: Cardinal);
    procedure InitDevValue;
    procedure SetDefValue(const Value: string);
    procedure SetItems(const X, Y: Cardinal; Value: string);
    procedure SetLengthX(const Value: Cardinal);
    procedure SetLengthY(const Value: Cardinal);
    procedure SetUseDefValue(const Value: Boolean);
  public
    constructor Create(x, y: Cardinal); overload;
    constructor Create; overload;
    destructor Destroy; override;

    property DefValue: string read FDefValue write SetDefValue;
    property Items[const X, Y: Cardinal]: string read GetItems write SetItems; default;
    property LengthX: Cardinal read FLengthX write SetLengthX default 0;
    property LengthY: Cardinal read FLengthY write SetLengthY default 0;
    property UseDevValue: Boolean read FUseDefValue write SetUseDefValue default False;
  end;

  TMRIniArray = class(TComponent)
  private
    FInifile: string;
    FLastErrorStr: string;
    FLastError: TMRIniErrors;
    FKommentarZeichen: string;
    FINI: TMRArray;

    function GetLastErrorStr: string;

    procedure SetInifile(const Value: string);
    procedure SetKommentarZeichen(const Value: string);
    function GetSektion(const SektionsName: string): TMRStringArray;
    function GetAnzSektionen: Integer;
  published
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ReadInifile: Boolean;
    function SaveInifile: Boolean;
    function First: Boolean;
    function Next: Boolean;
    function Prev: Boolean;
    function Last: Boolean;
    function Current: TMRStringArray;
    function CurrentSektion: string;

    procedure Clear;
    procedure GetSektionsnamen(Strings: TStrings);

    property Sektion[const SektionsName: string]: TMRStringArray read GetSektion; default;
  published
    property Inifile: string read FInifile write SetInifile;
    property LastErrorStr: string read GetLastErrorStr;
    property LastError: TMRIniErrors read FLastError;
    property KommentarZeichen: string read FKommentarZeichen write SetKommentarZeichen;
    property AnzSektionen: Integer read GetAnzSektionen;
  end;

function IniFileToArray(const DateiName: string): TMRStrArray2D;

implementation

uses SysUtils, DateUtils, MRFunktionen, IniFiles;

function IsZahl(Value: string): Boolean;
begin
  try
    Result := True;
  except
    Result := False;
  end;
end;

function IniFileToArray(const DateiName: string): TMRStrArray2D;
var
  mrstr2d                     : TMRStrArray2D;
  ini                         : TIniFile;
  sl                          : TStringList;
begin
  Result := nil;

  if (not FileExists(DateiName)) then
    exit;

  mrstr2d := TMRStrArray2D.Create;
  ini := TIniFile.Create(DateiName);
  sl := TStringList.Create;
  ini.ReadSections(sl);
  mrstr2d.LengthY := sl.Count;

  Result := mrstr2d;
end;

{ TMRArray }

procedure TMRArray.Add(Value: Pointer);
var
  i                           : Integer;
begin
  i := FItems.Count;

  while KeyExists(IntToStr(i)) do
    inc(i);

  SetItemsStr(IntToStr(i), Value);
end;

procedure TMRArray.Add(Value: Pointer; Key: string);
begin
  CheckKey(Key);
  SetItemsStr(Key, Value);
end;

procedure TMRArray.AddNew;
begin
  Add(nil);
end;

procedure TMRArray.CheckKey(var Key: string);
var
  i                           : Integer;
begin
  i := Pos(FItems.NameValueSeparator, Key);

  while i > 0 do
  begin
    Delete(Key, i, 1);
    i := Pos(FItems.NameValueSeparator, Key);
  end;
end;

procedure TMRArray.Clear;
begin
  FItems.Clear;
end;

function TMRArray.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TMRArray.Initialize(Laenge: Cardinal);
var
  i                           : Cardinal;
  s                           : string;
begin
  FItems := TStringList.Create;
  FCurIndex := -1;
  FItems.NameValueSeparator := #255;
  FDefValue := nil;
  FUseDefValue := False;
  s := '0';

  if (Laenge > 0) then
    for i := 0 to Laenge - 1 do
      FItems.Values[IntToStr(i)] := s
end;

constructor TMRArray.Create(Laenge: Cardinal);
begin
  inherited Create;

  Initialize(Laenge);
end;

constructor TMRArray.Create;
begin
  inherited Create;

  Initialize(0);
end;

destructor TMRArray.Destroy;
begin
  FItems.Free;

  inherited;
end;

function TMRArray.First: Boolean;
begin
  if (FItems.Count > 0) then
    FCurIndex := 0
  else
    FCurIndex := -1;

  Result := (FCurIndex > -1);
end;

procedure TMRArray.Foreach(DoForeach: TMRAForeach; UserData: Pointer);
var
  i                           : Integer;
begin
  if (Assigned(DoForeach)) then
  begin
    for i := 0 to FItems.Count - 1 do
    begin
      DoForeach(i, Pointer(StrToInt(FItems.ValueFromIndex[i])), UserData);
    end; // for i := 0 to FItems.Count - 1
  end;
end;

procedure TMRArray.ForeachAs(DoForeachAs: TMRAForeachAs; UserData: Pointer);
var
  i                           : Integer;
begin
  if (Assigned(DoForeachAs)) then
  begin
    for i := 0 to FItems.Count - 1 do
    begin
      DoForeachAs(FItems.Names[i], Pointer(StrToInt(FItems.ValueFromIndex[i])), UserData);
    end; // for i := 0 to FItems.Count - 1
  end; // if (Assigned(DoForeachAs))
end;

function TMRArray.GetCurrent: Pointer;
var
  s: string;
begin
  if (FCurIndex > -1) then
  begin
    s := FItems[FCurIndex];
    Result := Pointer(StrToInt(FItems.ValueFromIndex[FCurIndex]));
  end // if (FCurIndex > -1)
  else
  begin
    raise EMRArrayIndexOutOfBound.Create('Index ausserhalb der Grenzen.');
  end;
end;

function TMRArray.GetCurrentKey: string;
begin
  if (FCurIndex > -1) then
  begin
    Result := FItems.Names[FCurIndex];
  end // if (FCurIndex > -1)
  else
  begin
    raise EMRArrayIndexOutOfBound.Create('Index ausserhalb der Grenzen.');
  end;
end;

function TMRArray.GetItemsIndex(Index: Integer): Pointer;
begin
  Result := GetItemsStr(IntToStr(Index));
end;

function TMRArray.GetItemsStr(Key: string): Pointer;
var
  i                           : Integer;
begin
  i := FItems.IndexOfName(Key);

  if (i > -1) then
  begin
    Result := Pointer(StrToInt(FItems.Values[Key]));
    FCurIndex := i;
  end // if (i > -1)
  else
  begin
    if (not FUseDefValue) then
      raise EMRArrayIndexNotFound.Create(Format('Index %s wurde nicht gefunden.', [Key]))
    else
    begin
      Result := FDefValue;
    end;
  end;
end;

function TMRArray.GetKey(Index: Integer): string;
begin
  Result := FItems.Names[Index];
end;

function TMRArray.IndexOf(Value: Pointer): string;
var
  i                           : Integer;
  j                           : Integer;
begin
  j := -1;

  for i := 0 to FItems.Count - 1 do
  begin
    if (Pointer(StrtoInt(FItems.ValueFromIndex[i])) = Value) then
    begin
      j := i;
      break;
    end; // if (FItems.ValueFromIndex[i] = Value)
  end; // for i := 0 to FItems.Count - 1

  if (j > -1) then
  begin
    Result := FItems.Names[j];
  end // if (j > -1)
  else
    raise EMRArrayIndexNotFound.Create('Kein Index gefunden');
end;

function TMRArray.KeyExists(Key: string): Boolean;
begin
  CheckKey(Key);
  Result := (FItems.IndexOfName(Key) > -1);
end;

function TMRArray.Last: Boolean;
begin
  FCurIndex := FItems.Count - 1;
  Result := (FCurIndex > -1);
end;

function TMRArray.Next: Boolean;
begin
  if (FCurIndex < FItems.Count - 1) and (FItems.Count > 0) then
  begin
    FCurIndex := FCurIndex + 1;
    Result := True;
  end
  else
    Result := False;
end;

function TMRArray.Prev: Boolean;
begin
  if (FCurIndex > 0) then
  begin
    FCurIndex := FCurIndex - 1;
    Result := True;
  end
  else
    Result := False;
end;

function TMRArray.RenameIndex(oldIndex, newIndex: string): Boolean;
var
  i1: Integer;
  i2: Integer;
  s: string;
  sa: TStrArray;
begin
  i1 := FItems.IndexOfName(oldIndex);
  i2 := FItems.IndexOfName(newIndex);

  if ((i1 > -1) and (i2 = -1)) then
  begin
    s := FItems[i1];
    Explode(s, FItems.NameValueSeparator, sa);
    sa[0] := newIndex;
    FItems[i1] := Implode(FItems.NameValueSeparator, sa);
    Result := True;
  end // if ((i1 > -1) and (i2 = -1))
  else
  begin
    Result := False;
  end;
end;

procedure TMRArray.SetDefValue(const Value: Pointer);
begin
  FDefValue := Value;
end;

procedure TMRArray.SetItemsIndex(Index: Integer; const Value: Pointer);
var
  i                           : Integer;
begin
  i := FItems.IndexOfName(IntToStr(Index));

  if (i > -1) then
  begin
    FItems.Values[IntToStr(Index)] := IntToStr(Integer(Value));
    FCurIndex := i;
  end // if (i > -1)
  else
  begin
    //    FItems.Add(Format('%d%s%d', [Index, FItems.NameValueSeparator, Value]));
    FItems.Values[IntToStr(Index)] := IntToStr(Integer(Value));
    FCurIndex := FItems.Count - 1;
  end;

  FListe := FItems.Text;
end;

procedure TMRArray.SetItemsStr(Key: string; const Value: Pointer);
var
  i                           : Integer;
begin
  i := FItems.IndexOfName(Key);

  if (i = -1) then
  begin
    //    FItems.Add(Format('%s%s%d', [Key, FItems.NameValueSeparator, Value]));
    FItems.Values[Key] := IntToStr(Integer(Value));
    FCurIndex := FItems.Count - 1;
  end
  else
  begin
    FItems.Values[Key] := IntToStr(Integer(Value));
    FCurIndex := i;
  end;

  FListe := FItems.Text;
end;

procedure TMRArray.SetLength(Laenge: Cardinal);
var
  alt                         : Cardinal;
  i                           : Cardinal;
  s                           : string;
  sa                          : TStrArray;
begin
  alt := FItems.Count;

  if (alt <> Laenge) then
  begin
    if (Laenge > alt) then
    begin
      for i := alt to Laenge - 1 do
      begin
        AddNew;
      end; // for i := alt to Laenge - 1
    end // if (Laenge > alt)
    else
    begin
      s := FItems.Text;
      Explode(s, #13#10, sa);
      System.SetLength(sa, Laenge);
      s := Implode(#13#10, sa);

      if (s[Length(s)] <> #13#10) then
        s := s + #13#10;

      FItems.Text := s;
    end;

    FListe := FItems.Text;
  end; // if (alt <> Laenge)
end;

procedure TMRArray.SetUseDefValue(const Value: Boolean);
begin
  FUseDefValue := Value;
end;

procedure TMRArray.Sort;
begin
  SortByValue;
end;

procedure TMRArray.SortByIndex;
var
  i                           : Integer;
  j                           : Integer;
  h                           : string;
begin
  for i := 0 to FItems.Count - 2 do
  begin
    for j := FItems.Count - 1 downto i + 1 do
    begin
      if (FItems[i] > FItems[j]) then
      begin
        h := FItems[i];
        FItems[i] := FItems[j];
        FItems[j] := h;
      end; // if (FItems[i] > FItems[j])
    end; // for j := FItems.Count - 1 downto i + 1
  end; // for i := 0 to FItems.Count - 2
end;

procedure TMRArray.SortByValue;
var
  i                           : Integer;
  j                           : Integer;
  h                           : string;
begin
  for i := 0 to FItems.Count - 2 do
  begin
    for j := FItems.Count - 1 downto i + 1 do
    begin
      if (StrToInt(FItems.ValueFromIndex[i]) > StrToInt(FItems.ValueFromIndex[j])) then
      begin
        h := FItems[i];
        FItems[i] := FItems[j];
        FItems[j] := h;
      end; // if (FItems[i] > FItems[j])
    end; // for j := FItems.Count - 1 downto i + 1
  end; // for i := 0 to FItems.Count - 2

  First;
end;

procedure TMRArray.Unset(Key: string);
var
  i                           : Integer;
begin
  i := FItems.IndexOfName(Key);

  if (i > -1) then
    FItems.Delete(i);

  if (FCurIndex >= FItems.Count) then
    FCurIndex := FItems.Count - 1;
end;

constructor TMRArray.Create(dValue: Pointer);
begin
  Create;
  FUseDefValue := True;
  FDefValue := dValue;
end;

{ TMRArray2D }

constructor TMRArray2D.Create(x, y: Cardinal);
var
  i                           : Cardinal;
begin
  InitDevValue;
  FUseDefValue := False;
  FDefValue := nil;
  FLengthX := x;
  FLengthY := y;
  FRows := TList.Create;

  if (y > 0) then
    for i := 0 to (y - 1) do
      Self.AddNewRow(x);
end;

procedure TMRArray2D.AddNewRow(Laenge: Cardinal);
var
  mra                         : TMRArray;
begin
  mra := TMRArray.Create(Laenge);
  mra.DefValue := FDefValue;
  mra.UseDefValue := FUseDefValue;
  FRows.Add(mra);
end;

constructor TMRArray2D.Create;
begin
  Create(0, 0);
end;

destructor TMRArray2D.Destroy;
var
  i                           : Integer;
begin
  for i := 0 to FRows.Count - 1 do
  begin
    TMRArray(FRows[i]).Free;
  end; // for i := 0 to FRows.Count - 1

  FRows.Clear;
  FRows.Free;

  inherited;
end;

function TMRArray2D.GetItems(const X, Y: Cardinal): Pointer;
var
  mra                         : TMRArray;
begin
  if (X >= FLengthX) then
  begin
    raise ERangeError.Create(Format('X (%d) ausserhalb des Bereichs', [X]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  if (Y >= FLengthY) then
  begin
    raise ERangeError.Create(Format('Y (%d) ausserhalb des Bereichs', [Y]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  mra := TMRArray(FRows[Y]);
  mra.UseDefValue := FUseDefValue;
  mra.DefValue := FDefValue;
  Result := mra[X];
end;

procedure TMRArray2D.InitDevValue;
begin
  FDefValue := nil;
end;

procedure TMRArray2D.SetDefValue(const Value: Pointer);
begin
  FDefValue := Value;
end;

procedure TMRArray2D.SetItems(const X, Y: Cardinal; Value: Pointer);
var
  mra                         : TMRArray;
begin
  if (X >= FLengthX) then
  begin
    raise ERangeError.Create(Format('X (%d) ausserhalb des Bereichs', [X]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  if (Y >= FLengthY) then
  begin
    raise ERangeError.Create(Format('Y (%d) ausserhalb des Bereichs', [Y]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  mra := TMRArray(FRows[Y]);
  mra[X] := Value;
end;

procedure TMRArray2D.SetLengthX(const Value: Cardinal);
var
  i                           : Cardinal;
begin
  if (Value <> FLengthX) then
  begin
    FLengthX := Value;

    for i := 0 to FRows.Count - 1 do
    begin
      TMRArray(FRows[i]).SetLength(Value);
    end; // for i := 0 to FRows.Count - 1
  end; // if (Value <> FLengthX)
end;

procedure TMRArray2D.SetLengthY(const Value: Cardinal);
var
  alt                         : Cardinal;
  i                           : Cardinal;
  mra                         : TMRArray;
begin
  if (Value <> FLengthY) then
  begin
    alt := FLengthY;
    FLengthY := Value;

    if (FLengthY > alt) then
    begin
      FRows.Capacity := FLengthY;

      for i := alt to FLengthY - 1 do
      begin
        mra := TMRArray.Create(FLengthX);
        mra.DefValue := FDefValue;
        mra.UseDefValue := FUseDefValue;
        FRows.Add(mra);
      end; // for i := alt to FLengthY - 1
    end // if (FLengthY > alt)
    else
    begin
      for i := (alt - 1) downto FLengthY do
      begin
        TMRArray(FRows[i]).Free;
        FRows.Delete(i);
      end; // for i := FLengthY to alt - 1
    end;
  end; // if (Value <> FLengthY)
end;

procedure TMRArray2D.SetUseDefValue(const Value: Boolean);
begin
  FUseDefValue := Value;
end;

{ TMRIntegerArray }

procedure TMRIntegerArray.Add(Value: Int64);
var
  i                           : Integer;
begin
  i := FItems.Count;

  while KeyExists(IntToStr(i)) do
    inc(i);

  SetItemsStr(IntToStr(i), Value);
end;

procedure TMRIntegerArray.Add(Value: Int64; Key: string);
begin
  CheckKey(Key);
  SetItemsStr(Key, Value);
end;

procedure TMRIntegerArray.AddNew;
begin
  Add(0);
end;

procedure TMRIntegerArray.CheckKey(var Key: string);
var
  i                           : Integer;
begin
  i := Pos(FItems.NameValueSeparator, Key);

  while i > 0 do
  begin
    Delete(Key, i, 1);
    i := Pos(FItems.NameValueSeparator, Key);
  end;
end;

procedure TMRIntegerArray.Clear;
begin
  FItems.Clear;
end;

function TMRIntegerArray.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TMRIntegerArray.Initialize(Laenge: Cardinal);
var
  i                           : Cardinal;
  s                           : string;
begin
  FItems := TStringList.Create;
  FCurIndex := -1;
  FItems.NameValueSeparator := #255;
  FDefValue := 0;
  FUseDefValue := False;
  s := '0';

  if (Laenge > 0) then
    for i := 0 to Laenge - 1 do
      FItems.Values[IntToStr(i)] := s
end;

constructor TMRIntegerArray.Create(Laenge: Cardinal);
begin
  inherited Create;

  Initialize(Laenge);
end;

constructor TMRIntegerArray.Create;
begin
  inherited Create;

  Initialize(0);
end;

destructor TMRIntegerArray.Destroy;
begin
  FItems.Free;

  inherited;
end;

function TMRIntegerArray.First: Boolean;
begin
  if (FItems.Count > 0) then
    FCurIndex := 0
  else
    FCurIndex := -1;

  Result := (FCurIndex > -1);
end;

procedure TMRIntegerArray.Foreach(DoForeach: TMRIAForeach; UserData: Pointer);
var
  i                           : Integer;
begin
  if (Assigned(DoForeach)) then
  begin
    for i := 0 to FItems.Count - 1 do
    begin
      DoForeach(i, StrToInt(FItems.ValueFromIndex[i]), UserData);
    end; // for i := 0 to FItems.Count - 1
  end;
end;

procedure TMRIntegerArray.ForeachAs(DoForeachAs: TMRIAForeachAs; UserData: Pointer);
var
  i                           : Integer;
begin
  if (Assigned(DoForeachAs)) then
  begin
    for i := 0 to FItems.Count - 1 do
    begin
      DoForeachAs(FItems.Names[i], StrToInt(FItems.ValueFromIndex[i]), UserData);
    end; // for i := 0 to FItems.Count - 1
  end; // if (Assigned(DoForeachAs))
end;

function TMRIntegerArray.GetCurrent: Int64;
begin
  if (FCurIndex > -1) then
  begin
    Result := StrToInt(FItems.ValueFromIndex[FCurIndex]);
  end // if (FCurIndex > -1)
  else
  begin
    raise EMRArrayIndexOutOfBound.Create('Index ausserhalb der Grenzen.');
  end;
end;

function TMRIntegerArray.GetCurrentKey: string;
begin
  if (FCurIndex > -1) then
  begin
    Result := FItems.Names[FCurIndex];
  end // if (FCurIndex > -1)
  else
  begin
    raise EMRArrayIndexOutOfBound.Create('Index ausserhalb der Grenzen.');
  end;
end;

function TMRIntegerArray.GetItemsIndex(Index: Integer): Int64;
begin
  Result := GetItemsStr(IntToStr(Index));
end;

function TMRIntegerArray.GetItemsStr(Key: string): Int64;
var
  i                           : Integer;
begin
  i := FItems.IndexOfName(Key);

  if (i > -1) then
  begin
    Result := StrToInt(FItems.Values[Key]);
    FCurIndex := i;
  end // if (i > -1)
  else
  begin
    if (not FUseDefValue) then
      raise EMRArrayIndexNotFound.Create(Format('Index %s wurde nicht gefunden.', [Key]))
    else
    begin
      Result := FDefValue;
    end;
  end;
end;

function TMRIntegerArray.GetKey(Index: Integer): string;
begin
  Result := FItems.Names[Index];
end;

function TMRIntegerArray.IndexOf(Value: Int64): string;
var
  i                           : Integer;
  j                           : Integer;
begin
  j := -1;

  for i := 0 to FItems.Count - 1 do
  begin
    if (StrtoInt(FItems.ValueFromIndex[i]) = Value) then
    begin
      j := i;
      break;
    end; // if (FItems.ValueFromIndex[i] = Value)
  end; // for i := 0 to FItems.Count - 1

  if (j > -1) then
  begin
    Result := FItems.Names[j];
  end // if (j > -1)
  else
    raise EMRArrayIndexNotFound.Create('Kein Index gefunden');
end;

function TMRIntegerArray.KeyExists(Key: string): Boolean;
begin
  CheckKey(Key);
  Result := (FItems.IndexOfName(Key) > -1);
end;

function TMRIntegerArray.Last: Boolean;
begin
  FCurIndex := FItems.Count - 1;
  Result := (FCurIndex > -1);
end;

function TMRIntegerArray.Next: Boolean;
begin
  if (FCurIndex < FItems.Count - 1) and (FItems.Count > 0) then
  begin
    FCurIndex := FCurIndex + 1;
    Result := True;
  end
  else
    Result := False;
end;

function TMRIntegerArray.Prev: Boolean;
begin
  if (FCurIndex > 0) then
  begin
    FCurIndex := FCurIndex - 1;
    Result := True;
  end
  else
    Result := False;
end;

procedure TMRIntegerArray.SetDefValue(const Value: Int64);
begin
  FDefValue := Value;
end;

procedure TMRIntegerArray.SetItemsIndex(Index: Integer; const Value: Int64);
var
  i                           : Integer;
begin
  i := FItems.IndexOfName(IntToStr(Index));

  if (i > -1) then
  begin
    FItems.Values[IntToStr(Index)] := IntToStr(Value);
    FCurIndex := i;
  end // if (i > -1)
  else
  begin
    //    FItems.Add(Format('%d%s%d', [Index, FItems.NameValueSeparator, Value]));
    FItems.Values[IntToStr(Index)] := IntToStr(Value);
    FCurIndex := FItems.Count - 1;
  end;

  FListe := FItems.Text;
end;

procedure TMRIntegerArray.SetItemsStr(Key: string; const Value: Int64);
var
  i                           : Integer;
begin
  i := FItems.IndexOfName(Key);

  if (i = -1) then
  begin
    //    FItems.Add(Format('%s%s%d', [Key, FItems.NameValueSeparator, Value]));
    FItems.Values[Key] := IntToStr(Value);
    FCurIndex := FItems.Count - 1;
  end
  else
  begin
    FItems.Values[Key] := IntToStr(Value);
    FCurIndex := i;
  end;

  FListe := FItems.Text;
end;

procedure TMRIntegerArray.SetLength(Laenge: Cardinal);
var
  alt                         : Cardinal;
  i                           : Cardinal;
  s                           : string;
  sa                          : TStrArray;
begin
  alt := FItems.Count;

  if (alt <> Laenge) then
  begin
    if (Laenge > alt) then
    begin
      for i := alt to Laenge - 1 do
      begin
        AddNew;
      end; // for i := alt to Laenge - 1
    end // if (Laenge > alt)
    else
    begin
      s := FItems.Text;
      Explode(s, #13#10, sa);
      System.SetLength(sa, Laenge);
      s := Implode(#13#10, sa);

      if (s[Length(s)] <> #13#10) then
        s := s + #13#10;

      FItems.Text := s;
    end;

    FListe := FItems.Text;
  end; // if (alt <> Laenge)
end;

procedure TMRIntegerArray.SetUseDefValue(const Value: Boolean);
begin
  FUseDefValue := Value;
end;

procedure TMRIntegerArray.Sort;
begin
  SortByValue;
end;

procedure TMRIntegerArray.SortByIndex;
var
  i                           : Integer;
  j                           : Integer;
  h                           : string;
begin
  for i := 0 to FItems.Count - 2 do
  begin
    for j := FItems.Count - 1 downto i + 1 do
    begin
      if (FItems[i] > FItems[j]) then
      begin
        h := FItems[i];
        FItems[i] := FItems[j];
        FItems[j] := h;
      end; // if (FItems[i] > FItems[j])
    end; // for j := FItems.Count - 1 downto i + 1
  end; // for i := 0 to FItems.Count - 2
end;

procedure TMRIntegerArray.SortByValue;
var
  i                           : Integer;
  j                           : Integer;
  h                           : string;
begin
  for i := 0 to FItems.Count - 2 do
  begin
    for j := FItems.Count - 1 downto i + 1 do
    begin
      if (StrToInt(FItems.ValueFromIndex[i]) > StrToInt(FItems.ValueFromIndex[j])) then
      begin
        h := FItems[i];
        FItems[i] := FItems[j];
        FItems[j] := h;
      end; // if (FItems[i] > FItems[j])
    end; // for j := FItems.Count - 1 downto i + 1
  end; // for i := 0 to FItems.Count - 2

  First;
end;

function TMRIntegerArray.Text: string;
var
  i                           : Integer;
  s                           : string;
begin
  s := '';

  for i := 0 to FItems.Count - 1 do
  begin
    s := s + FItems.ValueFromIndex[i] + #13#10;
  end; // for i := 0 to FItems.Count - 1

  Result := s;
end;

procedure TMRIntegerArray.Unset(Key: Integer);
begin
  Unset(IntToStr(Key));
end;

procedure TMRIntegerArray.Unset(Key: string);
var
  i                           : Integer;
begin
  i := FItems.IndexOfName(Key);

  if (i > -1) then
    FItems.Delete(i);

  if (FCurIndex >= FItems.Count) then
    FCurIndex := FItems.Count - 1;
end;

constructor TMRIntegerArray.Create(Laenge: Cardinal; DefValue: Int64);
var
  i                           : Cardinal;
begin
  inherited Create;

  Initialize(Laenge);

  if (Laenge > 0) then
  begin
    for i := 0 to Laenge - 1 do
    begin
      FItems.ValueFromIndex[i] := IntToStr(DefValue);
    end; // for i := 0 to Laenge - 1
  end; // if (Laenge > 0)
end;

constructor TMRIntegerArray.Create(dValue: Int64);
begin
  Create;
  FUseDefValue := True;
  FDefValue := dValue;
end;

{ TMRIntArray2D }

constructor TMRIntArray2D.Create(x, y: Cardinal);
var
  i                           : Cardinal;
begin
  InitDevValue;
  FUseDefValue := False;
  FDefValue := 0;
  FLengthX := x;
  FLengthY := y;
  FRows := TList.Create;

  if (y > 0) then
    for i := 0 to (y - 1) do
      Self.AddNewRow(x);
end;

procedure TMRIntArray2D.AddNewRow(Laenge: Cardinal);
var
  mra                         : TMRIntegerArray;
begin
  mra := TMRIntegerArray.Create(Laenge);
  mra.DefValue := FDefValue;
  mra.UseDefValue := FUseDefValue;
  FRows.Add(mra);
end;

constructor TMRIntArray2D.Create;
begin
  Create(0, 0);
end;

destructor TMRIntArray2D.Destroy;
var
  i                           : Integer;
begin
  for i := 0 to FRows.Count - 1 do
  begin
    TMRIntegerArray(FRows[i]).Free;
  end; // for i := 0 to FRows.Count - 1

  FRows.Clear;
  FRows.Free;

  inherited;
end;

function TMRIntArray2D.GetItems(const X, Y: Cardinal): Int64;
var
  mra                         : TMRIntegerArray;
begin
  if (X >= FLengthX) then
  begin
    raise ERangeError.Create(Format('X (%d) ausserhalb des Bereichs', [X]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  if (Y >= FLengthY) then
  begin
    raise ERangeError.Create(Format('Y (%d) ausserhalb des Bereichs', [Y]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  mra := TMRIntegerArray(FRows[Y]);
  mra.UseDefValue := FUseDefValue;
  mra.DefValue := FDefValue;
  Result := mra[X];
end;

function TMRIntArray2D.GetText(const Zeile: Integer): string;
var
  mra                         : TMRIntegerArray;
begin
  if (Cardinal(Zeile) >= FLengthY) then
  begin
    raise ERangeError.Create(Format('Zeile (%d) ausserhalb des Bereichs', [Zeile]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  mra := TMRIntegerArray(FRows[Zeile]);
  Result := mra.Text;
end;

procedure TMRIntArray2D.InitDevValue;
begin
  FDefValue := 0;
end;

procedure TMRIntArray2D.SetDefValue(const Value: Int64);
begin
  FDefValue := Value;
end;

procedure TMRIntArray2D.SetItems(const X, Y: Cardinal; Value: Int64);
var
  mra                         : TMRIntegerArray;
begin
  if (X >= FLengthX) then
  begin
    raise ERangeError.Create(Format('X (%d) ausserhalb des Bereichs', [X]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  if (Y >= FLengthY) then
  begin
    raise ERangeError.Create(Format('Y (%d) ausserhalb des Bereichs', [Y]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  mra := TMRIntegerArray(FRows[Y]);
  mra[X] := Value;
end;

procedure TMRIntArray2D.SetLengthX(const Value: Cardinal);
var
  i                           : Cardinal;
begin
  if (Value <> FLengthX) then
  begin
    FLengthX := Value;

    for i := 0 to FRows.Count - 1 do
    begin
      TMRIntegerArray(FRows[i]).SetLength(Value);
    end; // for i := 0 to FRows.Count - 1
  end; // if (Value <> FLengthX)
end;

procedure TMRIntArray2D.SetLengthY(const Value: Cardinal);
var
  alt                         : Cardinal;
  i                           : Cardinal;
  mra                         : TMRIntegerArray;
begin
  if (Value <> FLengthY) then
  begin
    alt := FLengthY;
    FLengthY := Value;

    if (FLengthY > alt) then
    begin
      FRows.Capacity := FLengthY;

      for i := alt to FLengthY - 1 do
      begin
        mra := TMRIntegerArray.Create(FLengthX);
        mra.DefValue := FDefValue;
        mra.UseDefValue := FUseDefValue;
        FRows.Add(mra);
      end; // for i := alt to FLengthY - 1
    end // if (FLengthY > alt)
    else
    begin
      for i := (alt - 1) downto FLengthY do
      begin
        TMRIntegerArray(FRows[i]).Free;
        FRows.Delete(i);
      end; // for i := FLengthY to alt - 1
    end;
  end; // if (Value <> FLengthY)
end;

procedure TMRIntArray2D.SetUseDefValue(const Value: Boolean);
begin
  FUseDefValue := Value;
end;

{ TMRStringArray }

procedure TMRStringArray.Add(Value: string);
var
  i                           : Integer;
begin
  i := FItems.Count;

  while KeyExists(IntToStr(i)) do
    inc(i);

  SetItemsStr(IntToStr(i), Value);
end;

procedure TMRStringArray.Add(Value: string; Key: string);
begin
  CheckKey(Key);
  SetItemsStr(Key, Value);
end;

procedure TMRStringArray.AddNew;
begin
  Add('');
end;

procedure TMRStringArray.CheckKey(var Key: string);
var
  i                           : Integer;
begin
  i := Pos(FItems.NameValueSeparator, Key);

  while i > 0 do
  begin
    Delete(Key, i, 1);
    i := Pos(FItems.NameValueSeparator, Key);
  end;
end;

procedure TMRStringArray.Clear;
begin
  FItems.Clear;
end;

function TMRStringArray.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TMRStringArray.Initialize(Laenge: Cardinal);
var
  i                           : Cardinal;
  s                           : string;
begin
  FItems := TStringList.Create;
  FCurIndex := -1;
  FItems.NameValueSeparator := #255;
  FDefValue := '';
  FUseDefValue := False;
  s := '';

  if (Laenge > 0) then
    for i := 0 to Laenge - 1 do
      FItems.Values[IntToStr(i)] := s
end;

function TMRStringArray.IsFirst: Boolean;
begin
  Result := (FCurIndex = 0);
end;

function TMRStringArray.IsLast: Boolean;
begin
  Result := ((FCurIndex = FItems.Count - 1) AND (FCurIndex > -1));
end;

constructor TMRStringArray.Create(Laenge: Cardinal);
begin
  inherited Create;

  Initialize(Laenge);
end;

constructor TMRStringArray.Create;
begin
  inherited Create;

  Initialize(0);
end;

destructor TMRStringArray.Destroy;
begin
  FItems.Free;

  inherited;
end;

function TMRStringArray.First: Boolean;
begin
  if (FItems.Count > 0) then
    FCurIndex := 0
  else
    FCurIndex := -1;

  Result := (FCurIndex > -1);
end;

procedure TMRStringArray.Foreach(DoForeach: TMRSAForeach; UserData: Pointer);
var
  i                           : Integer;
begin
  if (Assigned(DoForeach)) then
  begin
    for i := 0 to FItems.Count - 1 do
    begin
      DoForeach(i, FItems.ValueFromIndex[i], UserData);
    end; // for i := 0 to FItems.Count - 1
  end;
end;

procedure TMRStringArray.ForeachAs(DoForeachAs: TMRSAForeachAs; UserData: Pointer);
var
  i                           : Integer;
begin
  if (Assigned(DoForeachAs)) then
  begin
    for i := 0 to FItems.Count - 1 do
    begin
      DoForeachAs(FItems.Names[i], FItems.ValueFromIndex[i], UserData);
    end; // for i := 0 to FItems.Count - 1
  end; // if (Assigned(DoForeachAs))
end;

function TMRStringArray.GetCurrent: string;
begin
  if (FCurIndex > -1) then
  begin
    Result := FItems.ValueFromIndex[FCurIndex];
  end // if (FCurIndex > -1)
  else
  begin
    raise EMRArrayIndexOutOfBound.Create('Index ausserhalb der Grenzen.');
  end;
end;

function TMRStringArray.GetCurrentKey: string;
begin
  if (FCurIndex > -1) then
  begin
    Result := FItems.Names[FCurIndex];
  end // if (FCurIndex > -1)
  else
  begin
    raise EMRArrayIndexOutOfBound.Create('Index ausserhalb der Grenzen.');
  end;
end;

function TMRStringArray.GetItemsIndex(Index: Integer): string;
begin
  Result := GetItemsStr(IntToStr(Index));
end;

function TMRStringArray.GetItemsStr(Key: string): string;
var
  i                           : Integer;
begin
  i := FItems.IndexOfName(Key);

  if (i > -1) then
  begin
    Result := FItems.Values[Key];
    FCurIndex := i;
  end // if (i > -1)
  else
  begin
    if (not FUseDefValue) then
      raise EMRArrayIndexNotFound.Create(Format('Index %s wurde nicht gefunden.', [Key]))
    else
    begin
      Result := FDefValue;
    end;
  end;
end;

function TMRStringArray.GetKey(Index: Integer): string;
begin
  Result := FItems.Names[Index];
end;

procedure TMRStringArray.GetKeyNames(Strings: TStrings);
var
  sl: TStringList;
begin
  if (Strings is TStringList) then
    sl := TStringList(Strings)
  else
    sl := TStringList.Create;

  sl.Clear;

  if Self.First then
  begin
    repeat
      sl.Add(Self.CurrentKey);
    until (not Self.Next);
  end; // if FINI.First

  Strings.Text := sl.Text;

  if (not (Strings is TStringList)) then
    sl.Free;
end;

function TMRStringArray.IndexOf(Value: string): string;
var
  i                           : Integer;
  j                           : Integer;
begin
  j := -1;

  for i := 0 to FItems.Count - 1 do
  begin
    if (FItems.ValueFromIndex[i] = Value) then
    begin
      j := i;
      break;
    end; // if (FItems.ValueFromIndex[i] = Value)
  end; // for i := 0 to FItems.Count - 1

  if (j > -1) then
  begin
    Result := FItems.Names[j];
  end // if (j > -1)
  else
    raise EMRArrayIndexNotFound.Create('Kein Index gefunden');
end;

function TMRStringArray.KeyExists(Key: string): Boolean;
begin
  CheckKey(Key);
  Result := (FItems.IndexOfName(Key) > -1);
end;

function TMRStringArray.Last: Boolean;
begin
  FCurIndex := FItems.Count - 1;
  Result := (FCurIndex > -1);
end;

function TMRStringArray.Next: Boolean;
begin
  if (FCurIndex < FItems.Count - 1) and (FItems.Count > 0) then
  begin
    FCurIndex := FCurIndex + 1;
    Result := True;
  end
  else
    Result := False;
end;

function TMRStringArray.Prev: Boolean;
begin
  if (FCurIndex > 0) then
  begin
    FCurIndex := FCurIndex - 1;
    Result := True;
  end
  else
    Result := False;
end;

procedure TMRStringArray.SetDefValue(const Value: string);
begin
  FDefValue := Value;
end;

procedure TMRStringArray.SetItemsIndex(Index: Integer; const Value: string);
var
  i                           : Integer;
begin
  i := FItems.IndexOfName(IntToStr(Index));

  if (i > -1) then
  begin
    FItems.Values[IntToStr(Index)] := Value;
    FCurIndex := i;
  end // if (i > -1)
  else
  begin
    //    FItems.Add(Format('%d%s%d', [Index, FItems.NameValueSeparator, Value]));
    FItems.Values[IntToStr(Index)] := Value;
    FCurIndex := FItems.Count - 1;
  end;

  FListe := FItems.Text;
end;

procedure TMRStringArray.SetItemsStr(Key: string; const Value: string);
var
  i                           : Integer;
begin
  i := FItems.IndexOfName(Key);

  if (i = -1) then
  begin
    //    FItems.Add(Format('%s%s%d', [Key, FItems.NameValueSeparator, Value]));
    FItems.Values[Key] := Value;
    FCurIndex := FItems.Count - 1;
  end
  else
  begin
    FItems.Values[Key] := Value;
    FCurIndex := i;
  end;

  FListe := FItems.Text;
end;

procedure TMRStringArray.SetLength(Laenge: Cardinal);
var
  alt                         : Cardinal;
  i                           : Cardinal;
  s                           : string;
  sa                          : TStrArray;
begin
  alt := FItems.Count;

  if (alt <> Laenge) then
  begin
    if (Laenge > alt) then
    begin
      for i := alt to Laenge - 1 do
      begin
        AddNew;
      end; // for i := alt to Laenge - 1
    end // if (Laenge > alt)
    else
    begin
      s := FItems.Text;
      Explode(s, #13#10, sa);
      System.SetLength(sa, Laenge);
      s := Implode(#13#10, sa);

      if (s[Length(s)] <> #13#10) then
        s := s + #13#10;

      FItems.Text := s;
    end;

    FListe := FItems.Text;
  end; // if (alt <> Laenge)
end;

procedure TMRStringArray.SetUseDefValue(const Value: Boolean);
begin
  FUseDefValue := Value;
end;

procedure TMRStringArray.Sort;
begin
  SortByValue;
end;

procedure TMRStringArray.SortByIndex;
var
  i                           : Integer;
  j                           : Integer;
  h                           : string;
begin
  for i := 0 to FItems.Count - 2 do
  begin
    for j := FItems.Count - 1 downto i + 1 do
    begin
      if (FItems[i] > FItems[j]) then
      begin
        h := FItems[i];
        FItems[i] := FItems[j];
        FItems[j] := h;
      end; // if (FItems[i] > FItems[j])
    end; // for j := FItems.Count - 1 downto i + 1
  end; // for i := 0 to FItems.Count - 2

  FListe := FItems.Text;
end;

procedure TMRStringArray.SortByValue;
var
  i                           : Integer;
  j                           : Integer;
  h                           : string;
begin
  for i := 0 to FItems.Count - 2 do
  begin
    for j := FItems.Count - 1 downto i + 1 do
    begin
      if (FItems.ValueFromIndex[i] > FItems.ValueFromIndex[j]) then
      begin
        h := FItems[i];
        FItems[i] := FItems[j];
        FItems[j] := h;
      end; // if (FItems[i] > FItems[j])
    end; // for j := FItems.Count - 1 downto i + 1
  end; // for i := 0 to FItems.Count - 2

  First;
end;

function TMRStringArray.Text: string;
var
  i                           : Integer;
  s                           : string;
begin
  s := '';

  for i := 0 to FItems.Count - 1 do
  begin
    s := s + FItems.ValueFromIndex[i] + #13#10;
  end; // for i := 0 to FItems.Count - 1

  Result := s;
end;

procedure TMRStringArray.Unset(Key: string);
var
  i                           : Integer;
begin
  i := FItems.IndexOfName(Key);

  if (i > -1) then
    FItems.Delete(i);

  if (FCurIndex >= FItems.Count) then
    FCurIndex := FItems.Count - 1;
end;

constructor TMRStringArray.Create(sDefValue: string);
begin
  inherited Create;

  Initialize(0);
  FUseDefValue := True;
  FDefValue := sDefValue;
end;

constructor TMRStringArray.Create(Laenge: Cardinal; sDefValue: string);
begin
  inherited Create;

  Initialize(Laenge);
  FUseDefValue := True;
  FDefValue := sDefValue;
end;

{ TMRStrArray2D }

constructor TMRStrArray2D.Create(x, y: Cardinal);
var
  i                           : Cardinal;
begin
  InitDevValue;
  FUseDefValue := False;
  FDefValue := '';
  FLengthX := x;
  FLengthY := y;
  FRows := TList.Create;

  if (y > 0) then
    for i := 0 to (y - 1) do
      Self.AddNewRow(x);
end;

procedure TMRStrArray2D.AddNewRow(Laenge: Cardinal);
var
  mra                         : TMRStringArray;
begin
  mra := TMRStringArray.Create(Laenge);
  mra.DefValue := FDefValue;
  mra.UseDefValue := FUseDefValue;
  FRows.Add(mra);
end;

constructor TMRStrArray2D.Create;
begin
  Create(0, 0);
end;

destructor TMRStrArray2D.Destroy;
var
  i                           : Integer;
begin
  for i := 0 to FRows.Count - 1 do
  begin
    TMRArray(FRows[i]).Free;
  end; // for i := 0 to FRows.Count - 1

  FRows.Clear;
  FRows.Free;

  inherited;
end;

function TMRStrArray2D.GetItems(const X, Y: Cardinal): string;
var
  mra                         : TMRStringArray;
begin
  if (X >= FLengthX) then
  begin
    raise ERangeError.Create(Format('X (%d) ausserhalb des Bereichs', [X]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  if (Y >= FLengthY) then
  begin
    raise ERangeError.Create(Format('Y (%d) ausserhalb des Bereichs', [Y]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  mra := TMRStringArray(FRows[Y]);
  mra.UseDefValue := FUseDefValue;
  mra.DefValue := FDefValue;
  Result := mra[X];
end;

procedure TMRStrArray2D.InitDevValue;
begin
  FDefValue := '';
end;

procedure TMRStrArray2D.SetDefValue(const Value: string);
begin
  FDefValue := Value;
end;

procedure TMRStrArray2D.SetItems(const X, Y: Cardinal; Value: string);
var
  mra                         : TMRStringArray;
begin
  if (X >= FLengthX) then
  begin
    raise ERangeError.Create(Format('X (%d) ausserhalb des Bereichs', [X]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  if (Y >= FLengthY) then
  begin
    raise ERangeError.Create(Format('Y (%d) ausserhalb des Bereichs', [Y]));

    exit;
  end; // if (ACol >= FColCount) or (ARow >= FRows.Count)

  mra := TMRStringArray(FRows[Y]);
  mra[X] := Value;
end;

procedure TMRStrArray2D.SetLengthX(const Value: Cardinal);
var
  i                           : Cardinal;
begin
  if (Value <> FLengthX) then
  begin
    FLengthX := Value;

    if (FRows.Count > 0) then
    begin
      for i := 0 to FRows.Count - 1 do
      begin
        TMRStringArray(FRows[i]).SetLength(Value);
      end; // for i := 0 to FRows.Count - 1
    end;
  end; // if (Value <> FLengthX)
end;

procedure TMRStrArray2D.SetLengthY(const Value: Cardinal);
var
  alt                         : Cardinal;
  i                           : Cardinal;
  mra                         : TMRStringArray;
begin
  if (Value <> FLengthY) then
  begin
    alt := FLengthY;
    FLengthY := Value;

    if (FLengthY > alt) then
    begin
      FRows.Capacity := FLengthY;

      for i := alt to FLengthY - 1 do
      begin
        mra := TMRStringArray.Create(FLengthX);
        mra.DefValue := FDefValue;
        mra.UseDefValue := FUseDefValue;
        FRows.Add(mra);
      end; // for i := alt to FLengthY - 1
    end // if (FLengthY > alt)
    else
    begin
      for i := (alt - 1) downto FLengthY do
      begin
        TMRStringArray(FRows[i]).Free;
        FRows.Delete(i);
      end; // for i := FLengthY to alt - 1
    end;
  end; // if (Value <> FLengthY)
end;

procedure TMRStrArray2D.SetUseDefValue(const Value: Boolean);
begin
  FUseDefValue := Value;
end;

{ TMRIniArray }

procedure TMRIniArray.Clear;
begin
  if FINI.First then
  begin
    repeat
      TObject(FINI.Current).Free;
    until (not FINI.Next);
  end; // if FINI.First

  FINI.Clear;
end;

constructor TMRIniArray.Create(AOwner: TComponent);
begin
  inherited;

  FINI := TMRArray.Create;
  FINI.UseDefValue := True;
  FINI.DefValue := nil;

  FInifile := '';
  FLastErrorStr := '';
  FLastError := mrinierr_NoError;
  FKommentarZeichen := '#';
end;

function TMRIniArray.Current: TMRStringArray;
begin
  Result := TMRStringArray(FINI.Current);
end;

function TMRIniArray.CurrentSektion: string;
begin
  Result := FINI.CurrentKey;
end;

destructor TMRIniArray.Destroy;
begin
  Self.Clear;
  FINI.Free;
  
  inherited;
end;

function TMRIniArray.First: Boolean;
begin
  Result := FINI.First;
end;

function TMRIniArray.GetAnzSektionen: Integer;
begin
  Result := FINI.Count;
end;

function TMRIniArray.GetLastErrorStr: string;
begin
  case FLastError of
    mrinierr_NoError: Result := '';
    mrinierr_NoFilenameGiven: Result := 'Es wurde kein Dateiname angegeben';
    mrinierr_FileNotExists: Result := Format('Die Datei "%s" existiert nicht', [FInifile]);
    mrinierr_FileNotReadable: Result := Format('Die Datei "%s" ist nicht lesbar', [FInifile]);
    mrinierr_NoFirstSectionFound: Result := Format('Die Datei "%s" beginnt nicht mit einem Sektionsnamen', [FInifile]);
  end;
end;

function TMRIniArray.GetSektion(const SektionsName: string): TMRStringArray;
var
  sek: TMRStringArray;
begin
  Result := TMRStringArray(FINI.ItemsStr[SektionsName]);

  if (Result = nil) then
  begin
    sek := TMRStringArray.Create;
    FINI.Add(sek, SektionsName);
    Result := sek;
  end;
end;

procedure TMRIniArray.GetSektionsnamen(Strings: TStrings);
var
  sl: TStringList;
begin
  if (Strings is TStringList) then
    sl := TStringList(Strings)
  else
    sl := TStringList.Create;

  sl.Clear;

  if FINI.First then
  begin
    repeat
      sl.Add(FINI.CurrentKey);
    until (not FINI.Next);
  end; // if FINI.First

  Strings.Text := sl.Text;

  if (not (Strings is TStringList)) then
    sl.Free;
end;

function TMRIniArray.Last: Boolean;
begin
  Result := FINI.Last;
end;

function TMRIniArray.Next: Boolean;
begin
  Result := FINI.Next;
end;

function TMRIniArray.Prev: Boolean;
begin
  Result := FINI.Prev;
end;

function TMRIniArray.ReadInifile: Boolean;
var
  i: Integer;
  s: string;
  sa: TStrArray;
  sek: TMRStringArray;
  sl                          : TStringList;
  t: string;
  u: string;
begin
  if (FInifile <> '') then
  begin
    if (FileExists(FInifile)) then
    begin
      sl := TStringList.Create;

      try
        sl.LoadFromFile(FInifile);

        // Leerzeilen entfernen
        i := sl.IndexOf('');

        while (i > -1) do
        begin
          sl.Delete(i);
          i := sl.IndexOf('');
        end;

        // Leerzeichen am Anfang jeder Zeile entfernen:
        for i := 0 to sl.Count - 1 do
          sl[i] := Trim(sl[i]);

        // Kommentarzeilen entfernen:
        for i := sl.Count - 1 downto 0 do
        begin
          s := sl[i];

          if (Copy(s, 1, Length(FKommentarZeichen)) = FKommentarZeichen) then
            sl.Delete(i);
        end;

        s := sl[0] + '123';
        t := '123' + sl[0];

        if ((s[1] <> '[') or (t[Length(t)] <> ']')) then
        begin
          FLastError := mrinierr_NoFirstSectionFound;
          Result := False;
          sl.Free;
          exit;
        end; // if ((s[1] <> '[') or (t[Length(t)] <> ']'))

        // ab sofort werden alle Zeilen ignoriert, in denen kein = vorhanden ist
        // und die kein Sektionsname sind.
        // Daten auswerten
        Self.Clear;

        for i := 0 to sl.Count - 1 do
        begin
          s := sl[i];

          if (Pos('=', s) = 0) then
          begin
            t := s + '123';
            u := '123' + s;

            if ((t[1] = '[') and (u[Length(u)] = ']')) then
            begin
              sek := TMRStringArray.Create;
              t := Copy(s, 2, Length(s));
              Delete(t, Length(t), 1);
              FINI.Add(sek, t);
            end; // if ((t[1] = '[') and (u[Length(u)] = ']'))
          end // if (Pos('=', s) = 0)
          else
          begin
            Explode(s, '=', sa);
            sek.Add(sa[1], sa[0]);
          end;
        end; // for i := 0 to sl.Count - 1

        FLastError := mrinierr_NoError;
        Result := True;
      except
        Result := False;
        FLastError := mrinierr_FileNotReadable;
      end;

      sl.Free;
    end // if (FileExists(FInifile))
    else
    begin
      FLastError := mrinierr_FileNotExists;
      Result := False;
    end;
  end // if (FInifile <> '')
  else
  begin
    FLastError := mrinierr_NoFilenameGiven;
    Result := False;
  end;
end;

function TMRIniArray.SaveInifile: Boolean;
begin
  Result := True;
end;

procedure TMRIniArray.SetInifile(const Value: string);
begin
  FInifile := Value;
end;

procedure TMRIniArray.SetKommentarZeichen(const Value: string);
begin
  FKommentarZeichen := Value;
end;

end.

