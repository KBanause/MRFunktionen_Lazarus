unit MRFunktionen;

{$mode objfpc}{$H+}

(*
 *
 * $Id: MRFunktionen.pas 22 2008-10-03 19:26:30Z marc $
 *
 *)

interface

uses
  SysUtils,
  StrUtils,
  Classes;

const
  OSBIT = 32{$IFDEF CPU64} + 32{$ENDIF};

type
  TStringArray = array of string;
  TStrArray = TStringArray;
  TIntArray = array of Integer;
  TDblArray = array of Extended;
  TDblSep = (dsKomma, dsPunkt);
  TFileVersionInfo = record
    FileType,
    CompanyName,
    FileDescription,
    FileVersion,
    InternalName,
    LegalCopyRight,
    LegalTradeMarks,
    OriginalFileName,
    ProductName,
    ProductVersion,
    Comments,
    SpecialBuildStr,
    PrivateBuildStr,
    FileFunction: string;
    DebugBuild,
    PreRelease,
    SpecialBuild,
    PrivateBuild,
    Patched,
    InfoInferred: Boolean;
  end;

function Between(Value, Min, Max: Integer): Boolean; overload;
function Between(Value, Min, Max: Integer; excl: Boolean): Boolean; overload;
function BinaryToHexStr(BinWert: string): string;
function CreateConfigFile(AppName: string; ConfigFileExtension: string): string;
function CreateConfigFile(ConfigFileExtension: string): string;
function CreateConfigFile: string;
function FileVersionInfo(const sAppNamePath: TFileName; defVersion: String = ''): TFileVersionInfo;
function GetBaseName(FileName: string): string; overload;
function GetBaseName: string; overload;
function GetConfigFileName(AppName: string; ConfigFileExtension: string): string;
function GetConfigFileName(ConfigFileExtension: string): string;
function GetConfigFileName: string;
function GetOSBit: Byte;
function GetOwnDir(progname: string): string; overload;
function GetOwnDir: string; overload;
function GetRandomString(laenge: Integer): string; overload;
function GetRandomString(laenge: Integer; lesbarezeichen: Boolean): string; overload;
function GetRandomString(laenge: Integer; zeichen: string): string; overload;
function HexStrToBinary(HexWert: string): string;
function HexToInt(HexWert: string): Integer;
function HexToInt64(HexWert: string): Int64;
function HexToString(hexstr: string): string;
function Implode(const Glue: string; const Pieces: array of Integer): string; overload;
function Implode(const Glue: string; const Pieces: array of string): string; overload;
function InArray(const Wert: string; arra: TStrArray): Integer;
function MaxIntArray(const iArray: TIntArray): Int64;
function MRPosEx(const SubStr: string; const S: string; Offset: Integer; Reverse: Boolean = False): Integer;
function OsterSonntag(Jahr: Integer): TDateTime;
function ReplaceStr(SubStr: string; Str: string; NewStr: string; RepAll: Boolean = false): string;
function ReplaceString(SubStr: string; Str: string; NewStr: string; RepAll: Boolean = false): string;
function ReverseString(Str: string): string;
function StringToHex(str: string): string;
function Trenn1000er(Wert: Int64): string;

procedure Chomp(var Str: string);
procedure CopyArray(srcArray: TStrArray; startPosSrc: Integer; var dstArray: TStrArray; startPosDst: Integer; anzahl: Integer);
procedure Explode(const Werte, Trenner: string; var ResArray: TDblArray; DblSep: TDblSep; Limit: Integer = 0; const EndeElement: Boolean = false); overload;
procedure Explode(const Werte, Trenner: string; var ResArray: TIntArray; Limit: Integer = 0; const EndeElement: Boolean = false); overload;
procedure Explode(const Werte, Trenner: string; var ResArray: TStrArray; Limit: Integer = 0; const EndeElement: Boolean = false); overload;
procedure GetCurrentScreen(var Index: Integer; var Extends: TRect);
procedure IntArrayToStrArray(ia: TIntArray; var sa: TStrArray);
procedure QuickSort(var Ar: TIntArray);
procedure QuickSort(var Ar: TStrArray);
procedure ShuffleStrings(sl: TStrings);
procedure StrArrayToIntArray(sa: TStrArray; var ia: TIntArray);
procedure Swap(var p1, p2: Extended); overload;
procedure Swap(var p1, p2: Int64); overload;
procedure Swap(var p1, p2: Pointer); overload;
procedure Swap(var p1, p2: string); overload;

implementation

uses DateUtils,
  {$IFDEF WINDOWS}
  windows,
  ShellApi,
  {$ENDIF}
  {$IFDEF UNIX}
  unix,
  baseunix,
  {$ENDIF}
     Forms;

{Funktionen und Prozeduren}

function ReverseString(Str: string): string;
var
  i                           : Integer;
  s                           : string;
begin
  s := '';

  for i := 1 to Length(Str) do
  begin
    s := Str[i] + s;
  end; // for i := 1 to Length(Str)

  Result := s;
end;

function GetRandomString(laenge: Integer; zeichen: string): string;
var
  s                           : string;
  i                           : Integer;
begin
  s := '';

  for i := 0 to 19 do
  begin
    s := '';

    while Length(s) < laenge do
      s := s + zeichen[Random(Length(zeichen)) + 1];
  end; // for i := 0 to 19

  Result := s;
end;

function GetRandomString(laenge: Integer; lesbarezeichen: Boolean): string;
const
  charslesbar                 = ' ,.-;:_<>|öäüÖÄÜ#+*''^°1234567890ß´`?=)(/&%$§"!{[]}@€';

var
  i                           : Integer;
  zeichen                     : string;
begin

  if (lesbarezeichen) then
  begin
    zeichen := charslesbar;

    for i := 0 to 25 do
    begin
      zeichen := zeichen + Chr(65 + i);
      zeichen := zeichen + Chr(Ord('a') + i);
    end; // for i := 0 to 25
  end // if (lesbarezeichen)
  else
  begin
    zeichen := '';

    for i := 0 to 255 do
    begin
      zeichen := zeichen + Chr(i);
    end; // for i := 0 to 255
  end;

  Result := GetRandomString(laenge, zeichen);
end;

function GetConfigFileName(AppName: string; ConfigFileExtension: string
  ): string;
var
  s: String;
  cf: String;
  cd: String;
begin
  if (ConfigFileExtension <> '') then
  begin
    if (ConfigFileExtension[1] <> '.') then
    begin
      ConfigFileExtension := '.' + ConfigFileExtension;
    end; // if ()
  end; // if ()

  {$IFDEF WINDOWS}
  s := GetAppConfigFile(False);
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  s := GetAppConfigFile(False, True);
  {$ENDIF UNIX}
  cf := ExtractFileName(s);
  cd := IncludeTrailingPathDelimiter(ExtractFilePath(s));
  Delete(cd, Length(cd), 1);
  cd := cd + '.sa';
  cd := IncludeTrailingPathDelimiter(ExtractFilePath(cd));
  cf := cd + 'MRTools' + PathDelim + AppName + PathDelim + AppName + ConfigFileExtension;
  Result := cf;
end;

function GetConfigFileName(ConfigFileExtension: string): string;
begin
  Result := GetConfigFileName(ApplicationName, ConfigFileExtension);
end;

function GetConfigFileName: string;
begin
  Result := GetConfigFileName(ApplicationName, ConfigExtension);
end;

function GetOSBit: Byte;
begin
  {$IFDEF CPU64}
  Result := 64;
  {$ELSE}
  Result := 32;
  {$ENDIF}
end;

function GetOwnDir(progname: string): string; overload;
(*******************************************************************************
 *
 * Prozedur      : GetOwnDir
 * Rueckgabetyp  : string
 * Autor         : Marc Rasmussen
 * Datum         : 23.05.2010
 * Parameter     : progname: string
 * Erklaerung    : Liefert das Verzeichnis eines Programms inkl. abschliessendem \ bzw. /
 *
 ******************************************************************************)
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(progname));
end;

function GetOwnDir: string; overload;
begin
  Result := GetOwnDir(ParamStr(0));
end;

function GetRandomString(laenge: Integer): string;
begin
  Result := GetRandomString(laenge, false);
end;

function HexStrToBinary(HexWert: string): string;
var
  p                           : PChar;
  bufsize                     : Integer;
  s                           : string;
begin
  if Length(HexWert) mod 2 = 1 then
    HexWert := '0' + HexWert;

  bufsize := Length(HexWert) div 2;
  p := nil;
  GetMem(p, bufsize + 1);
  HexToBin(PChar(HexWert), p, bufsize);
  s := p;
  SetLength(s, bufsize);
  FreeMem(p, bufsize + 1);

  Result := s;
end;

function BinaryToHexStr(BinWert: string): string;
var
  p                           : PChar;
  bufsize                     : Integer;
  s                           : string;
begin
  bufsize := Length(BinWert);
  p := nil;
  GetMem(p, bufsize * 2);
  BinToHex(PChar(BinWert), p, bufsize);
  s := p;
  SetLength(s, bufsize * 2);
  FreeMem(p);
  Result := s;
end;

function HexToInt(HexWert: string): Integer;
(*******************************************************************************
 *
 * Prozedur      : HexToInt
 * Rueckgabetyp  : Integer
 * Autor         : Marc Rasmussen
 * Datum         : 08.10.2006
 * Parameter     : HexWert: string
 * Erklaerung    : Konvertiert einen Hexwert in einen Integer
 *
 ******************************************************************************)
var
  s                           : string;
  erg                         : Integer;
  i                           : Integer;
begin
  s := UpperCase(HexWert);
  erg := 0;

  while Length(s) > 0 do
  begin
    case s[1] of
      '0'..'9': i := Ord(s[1]) - Ord('0');
      'A'..'F': i := Ord(s[1]) - Ord('A') + 10;
    else
      i := 0;
    end;

    erg := erg * 16 + i;
    Delete(s, 1, 1);
  end;

  Result := erg;
end;

function HexToInt64(HexWert: string): Int64;
(*******************************************************************************
 *
 * Prozedur      : HexToInt
 * Rueckgabetyp  : Integer
 * Autor         : Marc Rasmussen
 * Datum         : 08.10.2006
 * Parameter     : HexWert: string
 * Erklaerung    : Konvertiert einen Hexwert in einen Integer
 *
 ******************************************************************************)
var
  s                           : string;
  erg                         : Int64;
  i                           : Integer;
begin
  s := UpperCase(HexWert);
  erg := 0;

  while Length(s) > 0 do
  begin
    case s[1] of
      '0'..'9': i := Ord(s[1]) - Ord('0');
      'A'..'F': i := Ord(s[1]) - Ord('A') + 10;
    else
      i := 0;
    end;

    erg := erg * 16 + i;
    Delete(s, 1, 1);
  end;

  Result := erg;
end;

function Implode(const Glue: string; const Pieces: array of Integer): string;
var
  i, Len                      : Integer;
  P                           : PChar;
  GlueLen                     : Integer;
  s                           : string;
begin
  GlueLen := Length(Glue);
  Len := GlueLen * High(Pieces);

  for i := 0 to High(Pieces) do
    Inc(Len, Length(IntToStr(Pieces[i])));

  SetLength(Result, Len);

  if Len > 0 then
  begin
    P := @Result[1];
    for i := 0 to High(Pieces) do
    begin
      s := IntToStr(Pieces[i]);

      if (GlueLen > 0) and (i > 0) then
        P := StrLCopy(P, Pointer(Glue), GlueLen) + GlueLen;

      Len := Length(s);
      if Len > 0 then
        P := StrLCopy(P, Pointer(s), Len) + Len;
    end;
  end;
end;

function Implode(const Glue: string; const Pieces: array of string): string;
var
  i, Len                      : Integer;
  P                           : PChar;
  GlueLen                     : Integer;
begin
  GlueLen := Length(Glue);
  Len := GlueLen * High(Pieces);
  for i := 0 to High(Pieces) do
    Inc(Len, Length(Pieces[i]));
  SetLength(Result, Len);
  if Len > 0 then
  begin
    P := @Result[1];
    for i := 0 to High(Pieces) do
    begin
      if (GlueLen > 0) and (i > 0) then
        P := StrLCopy(P, Pointer(Glue), GlueLen) + GlueLen;
      Len := Length(Pieces[i]);
      if Len > 0 then
        P := StrLCopy(P, Pointer(Pieces[i]), Len) + Len;
    end;
  end;
end;

function InArray(const Wert: string; arra: TStrArray): Integer;
var
  i                           : Integer;
begin
  Result := Low(arra) - 1;

  for i := Low(arra) to High(arra) do
  begin
    if (arra[i] = Wert) then
    begin
      Result := i;
      break;
    end; // if (arra[i]=Wert)

  end; // for i := Low(arra) to High(arra)

end; // function InArray(const Wert: String; arra: TStrArray)

function MaxIntArray(const iArray: TIntArray): Int64;
var
  i                           : Integer;
  ma                          : Integer;
begin
  ma := iArray[Low(iArray)];

  for i := Low(iArray) to High(iArray) do
  begin
    if iArray[i] > ma then
      ma := iArray[i];

  end;

  Result := ma;
end;

function OsterSonntag(Jahr: Integer): TDateTime;
var
  d0: Integer;
  d1: Integer;
  d2: Integer;
  d3: Integer;
  d4: Integer;
  d5: Integer;
  d6: Integer;
  d7: Integer;
  d8: Integer;
  d9: Integer;
  mo: Integer;
begin
//j = 2009
//If j = "" Then j = Year(Now) End if
//If j = 0 Then j = Year(Now) End if
//If j < 1583 OR j > 3000 Then j = Year(Now) End if
//d0 = Int(j / 100)
//d1 = 15 + Int((3 * d0 + 3) / 4) - Int((8 * d0 + 13) / 25)
//d2 = 2 - Int((3 * d0 + 3) / 4)
//d3 = j Mod 19
//d4 = (19 * d3 + d1) Mod 30
//d5 = Int(d4 / 29) + (Int(d4 / 28) - Int(d4 / 29)) * Int(d3 / 11)
//d6 = Int(21 + d4 - d5)
//d7 = 7 - (j + Int(j / 4) + d2) Mod 7
//d8 = 7 - (d6 - d7) Mod 7
//d9 = d6 + d8
//oso = DateSerial(j, 3, d9)
//msgbox(oso)

  if ((Jahr < 1583) or (Jahr > 3000)) then
  begin
    Jahr := YearOf(Now);
  end; // if ((Jahr < 1583) or (Jahr > 3000))

  d0 := Jahr div 100;
  d1 := 15 + ((3 * d0 + 3) div 4) - ((8 * d0 + 13) div 25);
  d2 := 2 - ((3 * d0 + 3) div 4);
  d3 := Jahr mod 19;
  d4 := (19 * d3 + d1) mod 30;
  d5 := (d4 div 29) + ((d4 div 28) - (d4 div 29)) * (d3 div 11);
  d6 := 21 + d4 - d5;
  d7 := 7 - (Jahr + (Jahr div 4) + d2) mod 7;
  d8 := 7 - (d6 - d7) mod 7;
  d9 := d6 + d8;
  mo := 3;

  if (d9 > 31) then
  begin
    mo := mo + 1;
    d9 := d9 - 31;
  end; // if (d9 > 31)

  Result := EncodeDate(Jahr, mo, d9);
end;

function ReplaceStr(SubStr: string; Str: string; NewStr: string; RepAll: Boolean = false): string;
begin
  Result := ReplaceString(SubStr, Str, NewStr, RepAll);
end;

function ReplaceString(SubStr: string; Str: string; NewStr: string; RepAll: Boolean = false): string;
var
  i                           : Integer;
begin
  i := Pos(SubStr, Str);
  if i > 0 then
  begin
    repeat
      Delete(Str, i, Length(SubStr));
      Insert(NewStr, Str, i);
      i := Pos(SubStr, Str);
    until (i = 0) or (not RepAll);
  end;

  Result := Str;
end;

function Trenn1000er(Wert: Int64): string;
begin
  Result := Format('%0.0n', [Wert * 1.0]);
end;

procedure Explode(const Werte, Trenner: string; var ResArray: TDblArray; DblSep: TDblSep; Limit: Integer = 0; const EndeElement: Boolean = false);
var
  SepLen                      : Integer;
  F, P                        : PChar;
  s                           : string;
  ALen, Index                 : Integer;
begin
  Index := 0;
  ALen := 0;
  SetLength(ResArray, 0);
  if Length(Werte) > 0 then
  begin
    if Trenner = '' then
      exit
    else
    begin
      SepLen := Length(Trenner);
      ALen := Limit;
      SetLength(ResArray, ALen);
      Index := 0;
      P := PChar(Werte);
      while P^ <> #0 do
      begin
        F := P;
        P := AnsiStrPos(P, PChar(Trenner));
        if (P = nil) then
          P := StrEnd(F);
        if Index >= ALen then
        begin
          inc(ALen, 5);
          SetLength(ResArray, ALen);
        end;
        //            SetLength(Result, Index+1);
        SetString(s, F, P - F);
        if DblSep = dsKomma then
          s := ReplaceStr('.', s, ',')
        else
          s := ReplaceStr(',', s, '.');
        ResArray[Index] := StrToFloat(s);
        inc(Index);
        if P^ <> #0 then
          inc(P, SepLen);
      end;
    end;
  end;
  if Index < ALen then
    SetLength(ResArray, Index);
end;

procedure Explode(const Werte, Trenner: string; var ResArray: TIntArray; Limit: Integer = 0; const EndeElement: Boolean = false);
var
  SepLen                      : Integer;
  F, P                        : PChar;
  s                           : string;
  ALen, Index                 : Integer;
begin
  SetLength(ResArray, 0);
  Index := 0;
  ALen := Limit;
  
  if Length(Werte) > 0 then
  begin
    if Trenner = '' then
      exit
    else
    begin
      SepLen := Length(Trenner);
      ALen := Limit;
      SetLength(ResArray, ALen);
      Index := 0;
      P := PChar(Werte);

      while P^ <> #0 do
      begin
        F := P;
        P := AnsiStrPos(P, PChar(Trenner));

        if (P = nil) then
          P := StrEnd(F);

        if Index >= ALen then
        begin
          inc(ALen, 5);
          SetLength(ResArray, ALen);
        end;

        SetString(s, F, P - F);
        ResArray[Index] := StrToIntDef(s, 0);
        inc(Index);
        
        if P^ <> #0 then
          inc(P, SepLen);
      end;
    end;
  end;

  if Index < ALen then
    SetLength(ResArray, Index);
end;

procedure Explode(const Werte, Trenner: string; var ResArray: TStrArray; Limit: Integer = 0; const EndeElement: Boolean = false);
var
  SepLen                      : Integer;
  F, P                        : PChar;
  ALen, Index                 : Integer;
begin
  SetLength(ResArray, 0);
  if (Werte = '') or (Limit < 0) then
    Exit;
  if Trenner = '' then
  begin
    SetLength(ResArray, 1);
    ResArray[0] := Werte;
    Exit;
  end;
  SepLen := Length(Trenner);
  ALen := Limit;
  SetLength(ResArray, ALen);

  Index := 0;
  F := nil;
  P := PChar(Werte);

  while P^ <> #0 do
  begin
    F := P;
    P := AnsiStrPos(P, PChar(Trenner));

    if (P = nil) or ((Limit > 0) and (Index = Limit - 1)) then
      P := StrEnd(F);

    if Index >= ALen then
    begin
      Inc(ALen, 5);
      SetLength(ResArray, ALen);
    end;
    
    SetString(ResArray[Index], F, P - F);
    Inc(Index);
    
    if P^ <> #0 then
      Inc(P, SepLen);
  end;

  if (EndeElement) then
  begin
    F := AnsiStrPos(F, PChar(Trenner));

    if (F <> nil) then
    begin
      Inc(Index);

      if (Index >= ALen) then
      begin
        inc(ALen);
        SetLength(ResArray, ALen);
      end;
    end;
  end;

  if Index < ALen then
    SetLength(ResArray, Index);
end;

procedure QuickSort(var Ar: TIntArray);
  procedure Qs(var A : TIntArray; l,r: Integer);
  var
    pivot,b,i,j :  Integer;
  begin
    if l < r then
    begin
      pivot := A[random(r-l) + l+1];
      i := l-1;
      j := r+1;

      repeat
        repeat
          i := i+1;
        until (pivot <= A[i]);

        repeat
          j := j-1;
        until pivot >= A[j];

        b := A[i];
        A[i] := A[j];
        A[j] := b;
      until i >= j;

      A[j] := A[i];
      A[i] := b;
      Qs(A, l, i-1);
      Qs(A, i, r)
    end;
  end; { Quicksort }
begin
  Qs(Ar, 0, High(Ar));
end;

procedure QuickSort(var Ar: TStrArray);
  procedure Qs(var A : TStrArray; l,r: Integer);
  var
    pivot, b: string;
    i: Integer;
    j: Integer;
  begin
    if l < r then
    begin
      pivot := A[random(r-l) + l+1];
      i := l-1;
      j := r+1;

      repeat
        repeat
          i := i+1;
        until (AnsiStrIComp(PChar(pivot), PChar(A[i])) < 1);

        repeat
          j := j-1;
        until (AnsiStrIComp(PChar(pivot), PChar(A[j])) > -1);

        b:=A[i];
        A[i]:=A[j];
        A[j]:=b;
      until i >= j;

      A[j]:=A[i];
      A[i]:=b;
      Qs(A,l,i-1);
      Qs(A,i,r)
    end;
  end; { Quicksort }
begin
  Qs(Ar, 0, High(Ar));
end;

procedure ShuffleStrings(sl: TStrings);
var
  i                           : integer;
begin
  for i := 1 to sl.Count - 1 do
    sl.Exchange(Pred(i), Pred(i + Random(sl.Count - i)));
end;

function MRPosEx(const SubStr: string; const S: string; Offset: Integer; Reverse: Boolean = False): Integer;
var
  i                           : Integer;
  j                           : Integer;
begin
  j := 0;
  
  if (not Reverse) then
    Result := PosEx(SubStr, S, Offset)
  else
  begin
    i := Pos(SubStr, s);

    if (i >= Offset) or (i = 0) then
      Result := 0
    else
    begin
      while (i < Offset) and (i > 0) do
      begin
        j := i;
        i := PosEx(SubStr, S, j + 1);
      end;

      Result := j;
    end;
  end;
end;

procedure Swap(var p1, p2: Pointer);
var
  h                           : Pointer;
begin
  h := p1;
  p1 := p2;
  p2 := h;
end;

procedure Swap(var p1, p2: Int64);
var
  h                           : Int64;
begin
  h := p1;
  p1 := p2;
  p2 := h;
end;

procedure Swap(var p1, p2: string);
var
  s                           : string;
begin
  s := p1;
  p1 := p2;
  p2 := s;
end;

procedure GetCurrentScreen(var Index: Integer; var Extends: TRect);
var
  MonitorMouse                : TMonitor;
  MousePosition               : TPoint;
begin
  GetCursorPos(MousePosition);
  MonitorMouse := Screen.MonitorFromPoint(MousePosition);

  Index := MonitorMouse.MonitorNum;
  Extends := MonitorMouse.WorkareaRect;
end;

procedure Swap(var p1, p2: Extended);
var
  e                           : Extended;
begin
  e := p1;
  p1 := p2;
  p2 := e;
end;

procedure StrArrayToIntArray(sa: TStrArray; var ia: TIntArray);
var
  i                           : Integer;
begin
  SetLength(ia, Length(sa));

  for i := 0 to High(sa) do
  begin
    ia[i] := StrToIntDef(sa[i], 0);
  end; // for i := 0 to High(sa)
end;

procedure IntArrayToStrArray(ia: TIntArray; var sa: TStrArray);
var
  i                           : Integer;
begin
  SetLength(sa, Length(ia));

  for i := 0 to High(ia) do
  begin
    sa[i] := IntToStr(ia[i]);
  end; // for i := 0 to High(sa)
end;

function StringToHex(str: string): string;
var
  i                           : Integer;
  s                           : string;
begin
  s := '';

  for i := 1 to Length(str) do
  begin
    s := s + IntToHex(Ord(str[i]), 2);
  end; // for i := 1 to Length(str)

  Result := s;
end;

function HexToString(hexstr: string): string;
var
  s                           : string;
  t                           : string;
begin
  s := '';

  if (Length(hexstr) mod 2 = 1) then
    hexstr := '0' + hexstr;

  while (hexstr <> '') do
  begin
    t := Copy(hexstr, 1, 2);
    Delete(hexstr, 1, 2);
    s := s + Chr(HexToInt(t));
  end;

  Result := s;
end;

procedure Chomp(var Str: string);
begin
  if (Length(Str) > 0) then
  begin
    while (Str[Length(Str)] in [#13,#10]) do
    begin
      Delete(Str, Length(Str), 1);

      if (Length(Str) = 0) then
        break;
    end;
  end; // if (Length(Str) > 0)
end;

function Between(Value, Min, Max: Integer; excl: Boolean): Boolean;
begin
  Result := False;

  if ((Value >= Min) and (Value <= Max)) then
  begin
    Result := True;

    if (excl = True) then
    begin
      if ((Value = Min) or (Value = Max)) then
      begin
        Result := False;
      end; // if ((Value = Min) or (Value = Max))
    end; // if (excl = True)
  end; // if ((Value >= Min) and (Value <= Max))
end;

function Between(Value, Min, Max: Integer): Boolean;
begin
  Result := Between(Value, Min, Max, False);
end;

procedure CopyArray(srcArray: TStrArray; startPosSrc: Integer; var dstArray: TStrArray; startPosDst: Integer; anzahl: Integer);
var
  diff: Integer;
  i: Integer;
begin
  diff := startPosSrc - startPosDst;

  for i := startPosSrc to (startPosSrc + anzahl - 1) do
  begin
    dstArray[i - diff] := srcArray[i];
  end;
end;

function CreateConfigFile(AppName: string; ConfigFileExtension: string): string;
var
  s: String;
  p: String;
begin
  s := GetConfigFileName(AppName, ConfigFileExtension);
  p := ExtractFilePath(s);
  p := IncludeTrailingPathDelimiter(p);

  if (not DirectoryExists(p)) then
  begin
    if (not ForceDirectories(p)) then
    begin
      Result := '';
      Exit;
    end; // if (not ForceDirectories(p))
  end; // if (not DirectoryExists(p))

  Result := s;
end;

function CreateConfigFile(ConfigFileExtension: string): string;
var
  s: String;
  p: String;
begin
  s := GetConfigFileName(ConfigFileExtension);
  p := ExtractFilePath(s);
  p := IncludeTrailingPathDelimiter(p);

  if (not DirectoryExists(p)) then
  begin
    if (not ForceDirectories(p)) then
    begin
      Result := '';
      Exit;
    end; // if (not ForceDirectories(p))
  end; // if (not DirectoryExists(p))

  Result := s;
end;

function CreateConfigFile: string;
var
  s: String;
  p: String;
begin
  s := GetConfigFileName;
  p := ExtractFilePath(s);
  p := IncludeTrailingPathDelimiter(p);

  if (not DirectoryExists(p)) then
  begin
    if (not ForceDirectories(p)) then
    begin
      Result := '';
      Exit;
    end; // if (not ForceDirectories(p))
  end; // if (not DirectoryExists(p))

  Result := s;
end;

function FileVersionInfo(const sAppNamePath: TFileName; defVersion: String = ''): TFileVersionInfo;
{$IFDEF WINDOWS}
var
  rSHFI: TSHFileInfo;
  iRet: Integer;
  VerSize: Integer;
  VerBuf: PChar;
  VerBufValue: Pointer;
  VerHandle: Cardinal;
  VerBufLen: Cardinal;
  VerKey: string;
  FixedFileInfo: PVSFixedFileInfo;

  // dwFileType, dwFileSubtype
  function GetFileSubType(FixedFileInfo: PVSFixedFileInfo): string;
  begin
    case FixedFileInfo^.dwFileType of

      VFT_UNKNOWN: Result    := 'Unknown';
      VFT_APP: Result        := 'Application';
      VFT_DLL: Result        := 'DLL';
      VFT_STATIC_LIB: Result := 'Static-link Library';

      VFT_DRV:
        case
          FixedFileInfo^.dwFileSubtype of
          VFT2_UNKNOWN: Result         := 'Unknown Driver';
          VFT2_DRV_COMM: Result        := 'Communications Driver';
          VFT2_DRV_PRINTER: Result     := 'Printer Driver';
          VFT2_DRV_KEYBOARD: Result    := 'Keyboard Driver';
          VFT2_DRV_LANGUAGE: Result    := 'Language Driver';
          VFT2_DRV_DISPLAY: Result     := 'Display Driver';
          VFT2_DRV_MOUSE: Result       := 'Mouse Driver';
          VFT2_DRV_NETWORK: Result     := 'Network Driver';
          VFT2_DRV_SYSTEM: Result      := 'System Driver';
          VFT2_DRV_INSTALLABLE: Result := 'InstallableDriver';
          VFT2_DRV_SOUND: Result       := 'Sound Driver';
        end;
      VFT_FONT:
        case FixedFileInfo^.dwFileSubtype of
          VFT2_UNKNOWN: Result       := 'Unknown Font';
          VFT2_FONT_RASTER: Result   := 'Raster Font';
          VFT2_FONT_VECTOR: Result   := 'Vector Font';
          VFT2_FONT_TRUETYPE: Result := 'Truetype Font';
          else;
        end;
      VFT_VXD: Result := 'Virtual Defice Identifier = ' +
          IntToHex(FixedFileInfo^.dwFileSubtype, 8);
    end;
  end;


  function HasdwFileFlags(FixedFileInfo: PVSFixedFileInfo; Flag: Word): Boolean;
  begin
    Result := (FixedFileInfo^.dwFileFlagsMask and
      FixedFileInfo^.dwFileFlags and
      Flag) = Flag;
  end;

  function GetFixedFileInfo: PVSFixedFileInfo;
  begin
    if not VerQueryValue(VerBuf, '', Pointer(Result), VerBufLen) then
      Result := nil
  end;

  function GetInfo(const aKey: string): string;
  begin
    Result := '';
    VerKey := Format('\StringFileInfo\%.4x%.4x\%s',
      [LoWord(Integer(VerBufValue^)),
      HiWord(Integer(VerBufValue^)), aKey]);
    if VerQueryValue(VerBuf, PChar(VerKey), VerBufValue, VerBufLen) then
      Result := StrPas(VerBufValue);
  end;

  function QueryValue(const aValue: string): string;
  begin
    Result := '';
    // obtain version information about the specified file
    if GetFileVersionInfo(PChar(sAppNamePath), VerHandle, VerSize, VerBuf) and
      // return selected version information
      VerQueryValue(VerBuf, '\VarFileInfo\Translation', VerBufValue, VerBufLen) then
      Result := GetInfo(aValue);
  end;
{$ENDIF}
begin
  // Initialize the Result
  with Result do
  begin
    FileType         := '';
    CompanyName      := '';
    FileDescription  := '';
    FileVersion      := '';
    InternalName     := '';
    LegalCopyRight   := '';
    LegalTradeMarks  := '';
    OriginalFileName := '';
    ProductName      := '';
    ProductVersion   := '';
    Comments         := '';
    SpecialBuildStr  := '';
    PrivateBuildStr  := '';
    FileFunction     := '';
    DebugBuild       := False;
    Patched          := False;
    PreRelease       := False;
    SpecialBuild     := False;
    PrivateBuild     := False;
    InfoInferred     := False;
  end;

  {$IFNDEF WINDOWS}
  Result.FileVersion := SPVERSION;
  {$ELSE}
  // Get the file type
  if SHGetFileInfo(PChar(sAppNamePath), 0, rSHFI, SizeOf(rSHFI), SHGFI_TYPENAME) <> 0 then
  begin
    Result.FileType := rSHFI.szTypeName;
  end;

  iRet := SHGetFileInfo(PChar(sAppNamePath), 0, rSHFI, SizeOf(rSHFI), SHGFI_EXETYPE);
  if iRet <> 0 then
  begin
    // determine whether the OS can obtain version information
    VerSize := GetFileVersionInfoSize(PChar(sAppNamePath), VerHandle);
    if VerSize > 0 then
    begin
      VerBuf := AllocMem(VerSize);
      try
        with Result do
        begin
          CompanyName      := QueryValue('CompanyName');
          FileDescription  := QueryValue('FileDescription');
          FileVersion      := QueryValue('FileVersion');
          InternalName     := QueryValue('InternalName');
          LegalCopyRight   := QueryValue('LegalCopyRight');
          LegalTradeMarks  := QueryValue('LegalTradeMarks');
          OriginalFileName := QueryValue('OriginalFileName');
          ProductName      := QueryValue('ProductName');
          ProductVersion   := QueryValue('ProductVersion');
          Comments         := QueryValue('Comments');
          SpecialBuildStr  := QueryValue('SpecialBuild');
          PrivateBuildStr  := QueryValue('PrivateBuild');
          // Fill the VS_FIXEDFILEINFO structure
          FixedFileInfo := GetFixedFileInfo;
          DebugBuild    := HasdwFileFlags(FixedFileInfo, VS_FF_DEBUG);
          PreRelease    := HasdwFileFlags(FixedFileInfo, VS_FF_PRERELEASE);
          PrivateBuild  := HasdwFileFlags(FixedFileInfo, VS_FF_PRIVATEBUILD);
          SpecialBuild  := HasdwFileFlags(FixedFileInfo, VS_FF_SPECIALBUILD);
          Patched       := HasdwFileFlags(FixedFileInfo, VS_FF_PATCHED);
          InfoInferred  := HasdwFileFlags(FixedFileInfo, VS_FF_INFOINFERRED);
          FileFunction  := GetFileSubType(FixedFileInfo);
        end;
      finally
        FreeMem(VerBuf, VerSize);
      end
    end;
  end;
  {$ENDIF}
end;

function GetBaseName(FileName: string): string;
var
  s: string;
  i: Integer;
begin
  Result := '';

  if (FileName <> '') then
  begin
    s := ExtractFileName(FileName);
    i := Pos('.', s);

    if (i > 0) then
    begin
      while (s[Length(s)] <> '.') do
      begin
        Delete(s, Length(s), 1);
      end; // while (s[Length(s)] <> '.')

      Delete(s, Length(s), 1);
    end; // if (i > 0)

    Result := s;
  end; // if (FileName <> '')
end;

function GetBaseName: string;
begin
  Result := GetBaseName(ExpandFileName(ParamStr(0)));
end;

end.
