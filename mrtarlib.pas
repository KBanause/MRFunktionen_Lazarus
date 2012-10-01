unit MRTarLib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, dateutils,
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  {$IFDEF UNIX}
  baseunix, unix,
  {$ENDIF}
  zstream, ComCtrls;

const
  RECORDSIZE = 512;
  NAMSIZ     = 100;
  TUNMLEN    =  32;
  TGNMLEN    =  32;
  CHKBLANKS  = #32#32#32#32#32#32#32#32;

type
  TMRTarLibCompression = (mrcAuto, mrcNoTar, mrcNone, mrcGZIP, mrcBZIP2);
  TMRTarLibArchiveMode = (mramRead, mramCreate, mramAppend);

  TMRTarLibPermission  = (mrtpReadByOwner, mrtpWriteByOwner, mrtpExecuteByOwner,
    mrtpReadByGroup, mrtpWriteByGroup, mrtpExecuteByGroup,
    mrtpReadByOther, mrtpWriteByOther, mrtpExecuteByOther);
  TMRTarLibPermissions = set of TMRTarLibPermission;

  TMRTarLibMode  = (mrtmSetUid, mrtmSetGid, mrtmSaveText);
  TMRTarLibModes = set of TMRTarLibMode;

  TMRTarLibHeader = packed record
    Name: array [0..NAMSIZ - 1] of Char;
    Mode: array [0..7] of Char;
    UID: array [0..7] of Char;
    GID: array [0..7] of Char;
    Size: array [0..11] of Char;
    MTime: array [0..11] of Char;
    ChkSum: array [0..7] of Char;
    LinkFlag: Char;
    LinkName: array [0..NAMSIZ-1] of Char;
    Magic: array [0..7] of Char;
    UName: array [0..TUNMLEN-1] of Char;
    GName: array [0..TGNMLEN-1] of Char;
    DevMajor: array [0..7] of Char;
    DevMinor: array [0..7] of Char;
  end;

  TMRTarLibFileType = (mrftRegular, mrftTarLink, mrftSymbolicLink,
    mrftCharacterDevice, mrftBlockDevice, mrftDirectory, mrftFIFO,
    mrftReserved, mrftDumpDir, mrftMultiVolume, mrftVolumeHeader,
    mrftLongLink);

  TMRTarLibFileInfo = record
    Basename: string;
    Extension: string;
    GID: Longint;
    Group: string;
    IsDir: Boolean;
    IsLink: Boolean;
    LinkTarget: string;
    Modes: TMRTarLibModes;
    Path: string;
    Permissions: TMRTarLibPermissions;
    Size: Int64;
    UID: Longint;
    User: string;
    UTCTimestamp: Longint;
  end;

  TMRTarLibDirRec  = class;

  { TMRTarArchive }

  TMRTarArchive = class
  private
    FArchiv: TStream;
    FArchiveFileName: string;
    FBytesToGo: Int64;
    FChanged: Boolean;
    FCompressionType: TMRTarLibCompression;
    FCurFile: TMRTarLibDirRec;
    FFileMode: TMRTarLibArchiveMode;
    FgzipStream: TGZFileStream;

    constructor Create(const aArchiveFileName: string; const aFileMode: TMRTarLibArchiveMode); overload;
    constructor Create(const aArchiveFileName: string; const aFileMode: TMRTarLibArchiveMode; const aCompressionType: TMRTarLibCompression); overload;
    constructor Create; overload;

    function AddStream(Stream: TStream; DirRec: TMRTarLibDirRec): Boolean; overload;
    function ConvertFileName(DateiName: string): string; overload;
    function ConvertFileName(DateiName: string; const Reading: Boolean): string; overload;
    function FileSize(FileName: string): Int64;
    function FileSize(SearchRec: TSearchRec): Int64;
    function FileTimeGMT(FileName: string): Longint;
    function FileTimeGMT(SearchRec: TSearchRec): Longint;
    function GetReading: Boolean;
    function IsFinalized: Boolean;
    function IsTarFile(var aCompressionType: TMRTarLibCompression): Boolean;

    procedure AddEmptyBlock;
    procedure CreateLongLink(var DateiName: string);
    procedure GetFileInfo(DateiName: string; var FileInfo: TMRTarLibFileInfo);
    procedure WriteHeader(dr: TMRTarLibDirRec);
    procedure TreeViewCompare(Sender: TObject; Node1, Node2: TTreeNode;
      var Compare: Integer);
  public
    destructor Destroy; override;

    class function AppendTarArchive(var TarArchiv: TMRTarArchive;
      const aArchiveFileName: string): string;
    class function CreateTarArchive(var TarArchiv: TMRTarArchive;
      const aArchiveFileName: string): string; overload;
    class function CreateTarArchive(var TarArchiv: TMRTarArchive;
      const aArchiveFileName: string;
      const aCompressionType: TMRTarLibCompression): string; overload;
    class function GetInstance(var TarArchiv: TMRTarArchive;
      const aArchiveFileName: string;
      const aFileMode: TMRTarLibArchiveMode): string; overload;
    class function GetInstance(var TarArchiv: TMRTarArchive;
      const aArchiveFileName: string;
      const aFileMode: TMRTarLibArchiveMode;
      const aCompressionType: TMRTarLibCompression): string; overload;
    class function ReadTarArchiv(var TarArchiv: TMRTarArchive;
      const aArchiveFileName: string): string;

    function AddDir(DirectoryName: string; NameInTarFile: string): Boolean;
    function AddFile(DateiName: string; NameInTarFile: string): Boolean;
    function AddStream(Stream: TStream; NameInTarFile: string): Boolean; overload;
    function AddString(aText: string; NameInTarFile: string): Boolean;
    function FindNext(var DirRec: TMRTarLibDirRec): Boolean;
    function GetCurFile: string;
    function ListFiles(List: TStrings): Int64;
    function ListFiles(List: TStrings; WithDirRecData: Boolean): Int64;

    procedure GetCurFile(DstFileName: string);
    procedure GetCurFile(DstStream: TStream);
    procedure ListFiles(RootNode: TTreeNode);
    procedure ListFiles(TreeView: TTreeView);
    procedure ListFiles(TreeView: TTreeView; ClearTV: Boolean);
    procedure SaveToFile(DirRec: TMRTarLibDirRec; DateiName: string);
    procedure SaveToStream(DirRec: TMRTarLibDirRec; Stream: TStream);
  published
    property ArchiveFileName: string read FArchiveFileName;
    property CompressionType: TMRTarLibCompression read FCompressionType;
    property FileMode: TMRTarLibArchiveMode read FFileMode;
    property Reading: Boolean read GetReading;
  end;

  { TMRTarLibDirRec }

  TMRTarLibDirRec  = class
  private
    FChecksumOK: Boolean;
    FDateTime: Longint;
    FFilePos: Int64;
    FFileType: TMRTarLibFileType;
    FGID: Longint;
    FGroupName: string;
    FLinkName: string;
    FMagic: string;
    FMajorDevNo: Integer;
    FMinorDevNo: Integer;
    FModes: TMRTarLibModes;
    FName: string;
    FPermissions: TMRTarLibPermissions;
    FPosition: Int64;
    FSize: Int64;
    FUID: Longint;
    FUserName: string;
  public
    constructor Create;

    procedure AssignFrom(Src: TMRTarLibDirRec);
    procedure Clear;
  published
    property ChecksumOK: Boolean read FChecksumOK write FChecksumOK;
    property DateTime: Longint read FDateTime write FDateTime;
    property FilePos: Int64 read FFilePos write FFilePos;
    property FileType: TMRTarLibFileType read FFileType write FFileType;
    property GID: Longint read FGID write FGID;
    property GroupName: string read FGroupName write FGroupName;
    property LinkName: string read FLinkName write FLinkName;
    property Magic: string read FMagic write FMagic;
    property MajorDevNo: Integer read FMajorDevNo write FMajorDevNo;
    property MinorDevNo: Integer read FMinorDevNo write FMinorDevNo;
    property Modes: TMRTarLibModes read FModes write FModes;
    property Name: string read FName write FName;
    property Permissions: TMRTarLibPermissions read FPermissions write FPermissions;
    property Position: Int64 read FPosition write FPosition;
    property Size: Int64 read FSize write FSize;
    property UID: Longint read FUID write FUID;
    property UserName: string read FUserName write FUserName;
  end;

implementation

uses MRFunktionen;

function RemoveLeadingPathDelimiter(Path: string): string;
begin
  while ((Pos('\', Path) = 1) or (Pos('/', Path) = 1)) do
    Delete(Path, 1, 1);

  Result := Path;
end;

procedure Octal(Value: Int64; pc: PChar; Length: Integer);
var
  i: Integer;
begin
  for i := Length-2 downto 0 do
  begin
    (pc+i)^ := Chr(Ord('0') + Ord(Value and $07));
    Value := Value shr 3;
  end;

  (pc + Length - 1)^ := '0';
end;

procedure OctalN(Value: Int64; pc: PChar; Length: Integer);
begin
  Octal(Value, pc, Length);
  (pc + Length - 1)^ := #0;
end;

function OctalToInt(oct: PChar): Longint;
var
  a: Longint;
  s: string;
begin
  s := Trim(ansistring(oct));
  oct := PChar(s);
  Result := 0;

  while ((oct^ <> #32) and (oct^ <> #0)) do
  begin
    Result := (Result shl 3) + (Ord(oct^) - Ord('0'));
    inc(oct);
  end; // while ((oct^ <> #32) and (oct^ <> #0))
end;

function OctalToInt64(oct: PChar): Int64;
var
  a: Longint;
  s: string;
begin
  s := Trim(ansistring(oct));
  oct := PChar(s);
  Result := 0;

  while ((oct^ <> #32) and (oct^ <> #0)) do
  begin
    Result := (Result shl 3) + (Ord(oct^) - Ord('0'));
    inc(oct);
  end; // while ((oct^ <> #32) and (oct^ <> #0))
end;

function HeaderChecksum(th: TMRTarLibHeader): Longint;
var
  s: Longint;
  i: Integer;
  rec: array [0 .. RECORDSIZE - 1] of Char absolute th;
begin
  s := 0;

  for i := 0 to SizeOf(TMRTarLibHeader) - 1 do
    inc(s, Integer(Ord(rec[i])));

  Result := s;
end;

function Records(Bytes: Int64): Int64;
begin
  Result := Bytes div RECORDSIZE;

  if (Bytes mod RECORDSIZE > 0) then
    Result := Result + 1;
end;

function IsFileLinkDir(DateiName: string): Byte;
var
  srec: TSearchRec;
begin
  Result := 255;

  if (FindFirst(DateiName, faAnyFile, srec) = 0) then
  begin
    if ((srec.Attr and faDirectory) = faDirectory) then
      Result := 2
    else
    if ((srec.Attr and faSymLink) = faSymLink) then
      Result := 1
    else
      Result := 0;
  end;

  SysUtils.FindClose(srec);
end;

{ TMRTarArchive }

function TMRTarArchive.GetReading: Boolean;
begin
  Result := (FFileMode = mramRead);
end;

function TMRTarArchive.IsFinalized: Boolean;
var
  curpos: Int64;
  rec1: array [0..2*RECORDSIZE - 1] of byte;
  rec2: array [0..2*RECORDSIZE - 1] of byte;
begin
  curpos := FArchiv.Position;
  FArchiv.Seek(2*RECORDSIZE, soFromEnd);
  FillChar(rec1, 2*RECORDSIZE, 0);
  FillChar(rec2, 2*RECORDSIZE, 0);
  FArchiv.Read(rec2, 2*RECORDSIZE);
  FArchiv.Seek(curpos, soFromBeginning);
  Result := CompareMem(@rec1, @rec2, 2*RECORDSIZE);
end;

function TMRTarArchive.IsTarFile(
  var aCompressionType: TMRTarLibCompression): Boolean;
var
  buf: array[0..1] of Char;
  fs: TFileStream;
  gz: TGZFileStream;
  s: string;
  ms: TMemoryStream;
begin
  aCompressionType := mrcNone;
  fs := TFileStream.Create(FArchiveFileName, fmOpenRead or fmShareDenyWrite);
  fs.Read(buf, 2);

  if ((buf[0] = #$1f) and (buf[1] = #$8b)) then
    aCompressionType := mrcGZIP
  else
  if ((buf[0] = #$42) and (buf[1] = #$5a)) then
    aCompressionType := mrcBZIP2;

  fs.Free;
  ms := TMemoryStream.Create;

  if (aCompressionType = mrcGZIP) then
  begin
    gz := TGZFileStream.create(FArchiveFileName, gzopenread);
    ms.CopyFrom(gz, 512);
    ms.Position := 0;
    gz.Free;
  end // if (aCompressionType = mrcGZIP)
  else
  if (aCompressionType = mrcNone) then
  begin
    fs := TFileStream.Create(FArchiveFileName, fmOpenRead or fmShareDenyWrite);
    ms.CopyFrom(fs, 512);
    ms.Position := 0;
    fs.Free;
  end; // if (aCompressionType = mrcNone)

  if (ms.Size = 512) then
  begin
    SetLength(s, 5);
    ms.Seek(257, soFromBeginning);
    ms.Read(s[1], 5);

    if (s <> 'ustar') then
      aCompressionType := mrcNoTar;
  end; // if (fs.Size > (261))

  ms.Free;
  Result := (aCompressionType <> mrcNoTar);
end;

function TMRTarArchive.ConvertFileName(DateiName: string): string;
begin
  Result := ConvertFileName(DateiName, False);
end;

function TMRTarArchive.ConvertFileName(DateiName: string;
  const Reading: Boolean): string;
begin
  Result := DateiName;

  if (Reading) then
    Result := StringReplace(DateiName, '/', PathDelim, [rfReplaceAll])
  else
    Result := StringReplace(DateiName, PathDelim, '/', [rfReplaceAll]);
end;

function TMRTarArchive.FileSize(FileName: string): Int64;
var
  SR: TSearchRec;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
    Result := FileSize(SR);
  SysUtils.FindClose(SR);
end;

function TMRTarArchive.FileSize(SearchRec: TSearchRec): Int64;
begin
  Result := SearchRec.Size;
end;

function TMRTarArchive.FileTimeGMT(FileName: string): Longint;
var
  SR: TSearchRec;
begin
  Result := 0;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
    Result := FileTimeGMT(SR);
  SysUtils.FindClose(SR);
end;

function TMRTarArchive.FileTimeGMT(SearchRec: TSearchRec): Longint;
{$IFDEF WINDOWS}
var
  SystemFileTime: TSystemTime;
{$ENDIF}
{$IFDEF Unix}
var
  TimeVal:  TTimeVal;
  TimeZone: TTimeZone;
{$ENDIF}
begin
  { TODO 5 -oselber : Erstellungszeit für Verzeichnis bestimmen (UNIX) }
  Result := 0;
  {$IFDEF MSWINDOWS}{$WARNINGS OFF}
  if(SearchRec.FindData.dwFileAttributes AND faDirectory) = 0 then
  begin
    if FileTimeToSystemTime(SearchRec.FindData.ftLastWriteTime, SystemFileTime) then
      Result := DateTimeToUnix(EncodeDate(SystemFileTime.wYear, SystemFileTime.wMonth, SystemFileTime.wDay)
        + EncodeTime(SystemFileTime.wHour, SystemFileTime.wMinute, SystemFileTime.wSecond, SystemFileTime.wMilliseconds));
  end
  else
  begin
    if FileTimeToSystemTime(SearchRec.FindData.ftCreationTime, SystemFileTime) then
      Result := DateTimeToUnix(EncodeDate(SystemFileTime.wYear, SystemFileTime.wMonth, SystemFileTime.wDay)
        + EncodeTime(SystemFileTime.wHour, SystemFileTime.wMinute, SystemFileTime.wSecond, SystemFileTime.wMilliseconds));
  end;
  {$ENDIF}{$WARNINGS ON}
  {$IFDEF Unix}
  if SearchRec.Attr AND faDirectory = 0 then
  begin
    Result := FileDateToDateTime(SearchRec.Time);
    fpGetTimeOfDay(@TimeVal, @TimeZone);
    Result := Result + TimeZone.tz_minuteswest /(60 * 24);
  end;
  {$ENDIF}
end;

procedure TMRTarArchive.AddEmptyBlock;
var
  rec: array [0..RECORDSIZE - 1] of Byte;
begin
  FillChar(rec, RECORDSIZE, 0);
  FArchiv.Write(rec, RECORDSIZE);
end;

procedure TMRTarArchive.CreateLongLink(var DateiName: string);
var
  DirRec: TMRTarLibDirRec;
  rec: array[0..RECORDSIZE - 1] of Char;
  ms: TMemoryStream;
  a: Integer;
  i: Integer;
begin
  DateiName := RemoveLeadingPathDelimiter(DateiName);
  DirRec := TMRTarLibDirRec.Create;

  if (Length(DateiName) > 99) then
  begin
    FillChar(rec, RECORDSIZE, 0);
    DirRec.Clear;
    DirRec.Name := '././@LongLink';
    DirRec.FileType := mrftLongLink;
    ms := TMemoryStream.Create;
    a := (Length(DateiName) + 1) div 512;

    for i := 0 to a do
    begin
      ms.Write(rec, RECORDSIZE);
    end; // for i := 0 to a

    ms.Seek(0, soFromBeginning);
    ms.Write(DateiName[1], Length(DateiName));
    ms.Seek(0, soFromBeginning);
    SetLength(DateiName, 99);
    WriteHeader(DirRec);
    FArchiv.CopyFrom(ms, ms.Size);
    ms.Free;
  end; // if (Length(DateiName) > 99)

  DirRec.Free;
end;

procedure TMRTarArchive.GetFileInfo(DateiName: string;
  var FileInfo: TMRTarLibFileInfo);
var
  i: Byte;
{$IFDEF UNIX}
  Info: Stat;
{$ENDIF}
begin
  i := IsFileLinkDir(DateiName);
  FileInfo.Basename := ExtractFileName(ExpandFileName(DateiName));
  FileInfo.Extension := ExtractFileExt(FileInfo.Basename);
  FileInfo.Path := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(DateiName)));
  FileInfo.User := 'nobody';
  FileInfo.Group := 'nogroup';
  FileInfo.UID := 65534;
  FileInfo.GID := 65534;
  FileInfo.Modes := [];
  FileInfo.Permissions := [mrtpReadByOwner, mrtpWriteByOwner, mrtpExecuteByOwner,
    mrtpReadByGroup, mrtpWriteByGroup, mrtpExecuteByGroup,
    mrtpReadByOther, mrtpWriteByOther, mrtpExecuteByOther];
  FileInfo.UTCTimestamp := FileTimeGMT(DateiName);
  FileInfo.Size := FileSize(DateiName);
  FileInfo.IsDir := (i = 2);
  FileInfo.IsLink := (i = 1);

  {$IFDEF UNIX}
  if (fpLstat(DateiName, Info) = 0) then
  begin
    FileInfo.GID := Info.st_gid;
    FileInfo.UID := Info.st_uid;
    FileInfo.UTCTimestamp := Info.st_mtime;
    FileInfo.LinkTarget := fpReadLink(DateiName);
  end; // if (fpLstat(DateiName, Info) = 0)
  {$ENDIF}
end;

procedure TMRTarArchive.WriteHeader(dr: TMRTarLibDirRec);
var
  CheckSum: Integer;
  i: Integer;
  rec: array [0..RECORDSIZE - 1]of Char;
  th: TMRTarLibHeader absolute rec;
  mode: Integer;
begin
  FillChar(rec, RECORDSIZE, 0);
  strlcopy(th.Name, PChar(dr.Name), NAMSIZ - 1);
  th.Name[NAMSIZ - 1] := #0;
  mode := 0;

  if mrtmSaveText IN dr.Modes then
    mode := mode OR $0200;
  if mrtmSetGid   IN dr.Modes then
    mode := mode OR $0400;
  if mrtmSetUid   IN dr.Modes then
    mode := mode OR $0800;
  if mrtpReadByOwner    IN dr.Permissions then
    mode := mode OR $0100;
  if mrtpWriteByOwner   IN dr.Permissions then
    mode := mode OR $0080;
  if mrtpExecuteByOwner IN dr.Permissions then
    mode := mode OR $0040;
  if mrtpReadByGroup    IN dr.Permissions then
    mode := mode OR $0020;
  if mrtpWriteByGroup   IN dr.Permissions then
    mode := mode OR $0010;
  if mrtpExecuteByGroup IN dr.Permissions then
    mode := mode OR $0008;
  if mrtpReadByOther    IN dr.Permissions then
    mode := mode OR $0004;
  if mrtpWriteByOther   IN dr.Permissions then
    mode := mode OR $0002;
  if mrtpExecuteByOther IN dr.Permissions then
    mode := mode OR $0001;

  OctalN(mode, @th.Mode, 8);
  OctalN(dr.UID, @th.UID, 8);
  OctalN(dr.GID, @th.GID, 8);
  OctalN(dr.Size, @th.Size, 12);
  OctalN(dr.DateTime, @th.MTime, 12);
  StrMove(th.ChkSum, CHKBLANKS, 8);

  case dr.FileType of
    mrftRegular       :
      th.LinkFlag := '0';
    mrftTarLink         :
      th.LinkFlag := '1';
    mrftSymbolicLink :
      th.LinkFlag := '2';
    mrftCharacterDevice    :
      th.LinkFlag := '3';
    mrftBlockDevice        :
      th.LinkFlag := '4';
    mrftDirectory    :
      th.LinkFlag := '5';
    mrftFIFO         :
      th.LinkFlag := '6';
    mrftReserved   :
      th.LinkFlag := '7';
    mrftDumpDir      :
      th.LinkFlag := 'D';
    mrftMultiVolume  :
      th.LinkFlag := 'M';
    mrftVolumeHeader :
      th.LinkFlag := 'V';
    mrftLongLink     :
      th.LinkFlag := 'L';
  end;

  strlcopy(th.LinkName, PChar(dr.LinkName), NAMSIZ - 1);
  strlcopy(th.Magic, PChar('ustar  '), Length('ustar  '));
  strlcopy(th.UName, PChar(dr.UserName), Length(dr.UserName));
  strlcopy(th.GName, PChar(dr.GroupName), Length(dr.GroupName));

  CheckSum := HeaderChecksum(th);
  OctalN(CheckSum, @th.ChkSum, 7);
  th.ChkSum[6] := #0;
  th.ChkSum[7] := ' ';
  FArchiv.Write(th, RECORDSIZE);
end;

procedure TMRTarArchive.TreeViewCompare(Sender: TObject; Node1,
  Node2: TTreeNode; var Compare: Integer);
var
  c1: Char;
  c2: Char;
  i: Integer;
  s1: string;
  s2: string;
begin
  i := 0;
  Compare := 0;

  if (node1.Level = node2.Level) then
  begin
    s1 := node1.Text;
    c1 := s1[Length(s1)];
    s2 := node2.Text;
    c2 := s2[Length(s2)];

    if (((c1 = '/') and (c2 = '/')) or ((c1 <> '/') and (c2 <> '/'))) then
    begin
      Compare := AnsiStrIComp(PChar(s1), PChar(s2));
    end // if (((c1 = '/') and (c2 = '/')) or ((c1 <> '/') and (c2 <> '/')))
    else
    begin
      if (c1 = '/') then
        Compare := -1
      else
        Compare := 1;
    end;
  end; // if (node1.Level = node2.Level)
end;

constructor TMRTarArchive.Create(const aArchiveFileName: string;
  const aFileMode: TMRTarLibArchiveMode);
begin
  Self.Create(aArchiveFileName, aFileMode, mrcAuto);
end;

constructor TMRTarArchive.Create(const aArchiveFileName: string;
  const aFileMode: TMRTarLibArchiveMode;
  const aCompressionType: TMRTarLibCompression);
begin
  FArchiveFileName := aArchiveFileName;
  FBytesToGo := 0;
  FChanged := False;
  FCompressionType := aCompressionType;
  FFileMode := aFileMode;
  FCurFile := TMRTarLibDirRec.Create;

  if (FFileMode = mramRead) then
  begin
    if (not IsTarFile(FCompressionType)) then
      raise Exception.Create(Format('Datei "%s" ist kein TAR-Archiv', [aArchiveFileName]));

    if (FCompressionType = mrcNone) then
      FArchiv := TFileStream.Create(FArchiveFileName, fmOpenRead or fmShareDenyWrite)
    else
    if (FCompressionType = mrcGZIP) then
      FArchiv := TGZFileStream.create(FArchiveFileName, gzopenread)
    else
      FArchiv := nil;
  end
  else
  if (FFileMode = mramCreate) then
  begin
    if (FCompressionType = mrcGZIP) then
      FArchiv := TGZFileStream.create(FArchiveFileName, gzopenwrite)
    else
    if (FCompressionType = mrcNone) then
      FArchiv := TFileStream.Create(FArchiveFileName, fmCreate or fmShareDenyWrite)
    else
      FArchiv := nil;

    FChanged := True;
  end
  else
  if (FFileMode = mramAppend) then
  begin
    if (not FileExists(FArchiveFileName)) then
    begin
      FCompressionType := mrcNone;
      FArchiv := TFileStream.Create(FArchiveFileName, fmCreate or fmShareDenyWrite);
      FChanged := True;
    end // if (not FileExists(FArchiveFileName))
    else
    begin
      if (not IsTarFile(FCompressionType)) then
        raise Exception.Create(Format('Datei "%s" ist kein TAR-Archiv', [aArchiveFileName]));

      if (FCompressionType = mrcNone) then
      begin
        FArchiv := TFileStream.Create(FArchiveFileName, fmOpenReadWrite or fmShareDenyWrite);

        if (IsFinalized) then
          FArchiv.Seek(2*RECORDSIZE, soFromEnd)
        else
          FArchiv.Seek(0, soFromEnd);
      end
      else
      begin
        raise Exception.Create('APPEND bei komprimierten Archiven nicht erlaubt.');
        FArchiv := nil;
      end;
    end;
  end // if (FFileMode = mramAppend)
  else
    FArchiv := nil;
end;

constructor TMRTarArchive.Create;
begin
  raise Exception.Create('Not supported');
end;

function TMRTarArchive.AddStream(Stream: TStream; DirRec: TMRTarLibDirRec): Boolean;
var
  BlockSize: Int64;
  BytesToRead: Int64;
  rec: array [0..RECORDSIZE - 1] of Char;
  s: String;
begin
  FChanged := True;
  s := ConvertFileName(DirRec.Name);
  CreateLongLink(s);
  DirRec.Name := s;
  WriteHeader(DirRec);

  BytesToRead := DirRec.Size;

  while (BytesToRead > 0) do
  begin
    BlockSize := BytesToRead;

    if BlockSize > RECORDSIZE then
      BlockSize := RECORDSIZE;

    FillChar(Rec, RECORDSIZE, 0);
    Stream.Read(Rec, BlockSize);
    FArchiv.Write(Rec, RECORDSIZE);
    dec(BytesToRead, BlockSize);
  end; // while (BytesToRead > 0)
end;

destructor TMRTarArchive.Destroy;
begin
  if ((not GetReading) and (FArchiv <> nil) and (FChanged)) then
  begin
    AddEmptyBlock;
    AddEmptyBlock;
  end; // if ((not GetReading) and (FArchiv <> nil) and (FChanged))

  FArchiv.Free;
  FCurFile.Free;

  inherited;
end;

class function TMRTarArchive.AppendTarArchive(var TarArchiv: TMRTarArchive;
  const aArchiveFileName: string): string;
var
  a: TMRTarArchive;
  s: String;
begin
  s := '';

  try
    a := TMRTarArchive.Create(aArchiveFileName, mramAppend);
  except
    on e: Exception do
    begin
      a := nil;
      s := e.Message;
    end;
  end;

  TarArchiv := a;
  Result := s;
end;

class function TMRTarArchive.CreateTarArchive(var TarArchiv: TMRTarArchive;
  const aArchiveFileName: string): string;
begin
  Result := Self.CreateTarArchive(TarArchiv, aArchiveFileName, mrcNone);
end;

class function TMRTarArchive.CreateTarArchive(var TarArchiv: TMRTarArchive;
  const aArchiveFileName: string;
  const aCompressionType: TMRTarLibCompression): string;
var
  a: TMRTarArchive;
  s: String;
begin
  s := '';

  try
    a := TMRTarArchive.Create(aArchiveFileName, mramCreate, aCompressionType);
  except
    on e: Exception do
    begin
      a := nil;
      s := e.Message;
    end;
  end;

  TarArchiv := a;
  Result := s;
end;

class function TMRTarArchive.GetInstance(var TarArchiv: TMRTarArchive;
  const aArchiveFileName: string;
  const aFileMode: TMRTarLibArchiveMode): string;
begin
  Result := GetInstance(TarArchiv, aArchiveFileName, aFileMode, mrcAuto);
end;

class function TMRTarArchive.GetInstance(var TarArchiv: TMRTarArchive;
  const aArchiveFileName: string;
  const aFileMode: TMRTarLibArchiveMode;
  const aCompressionType: TMRTarLibCompression): string;
var
  s: String;
begin
  s := '';

  try
    TarArchiv := TMRTarArchive.Create(aArchiveFileName, aFileMode, aCompressionType);
  except
    on e: Exception do
    begin;
      TarArchiv := nil;
      s := e.Message;
    end;
  end;

  Result := s;
end;

class function TMRTarArchive.ReadTarArchiv(var TarArchiv: TMRTarArchive;
  const aArchiveFileName: string): string;
begin
  Result := GetInstance(TarArchiv, aArchiveFileName, mramRead);
end;

function TMRTarArchive.AddDir(DirectoryName: string;
  NameInTarFile: string): Boolean;
var
  DirRec: TMRTarLibDirRec;
begin
  DirRec := TMRTarLibDirRec.Create;
  NameInTarFile := ConvertFileName(IncludeTrailingPathDelimiter(NameInTarFile));
  CreateLongLink(NameInTarFile);
  DirRec.Name := NameInTarFile;
  DirRec.FileType := mrftDirectory;
  WriteHeader(DirRec);
  DirRec.Free;
end;

function TMRTarArchive.AddFile(DateiName: string;
  NameInTarFile: string): Boolean;
var
  DirRec: TMRTarLibDirRec;
  fi: TMRTarLibFileInfo;
  s: TFileStream;
begin
  s := TFileStream.Create(DateiName, fmOpenRead or fmShareDenyWrite);
  DirRec := TMRTarLibDirRec.Create;
  GetFileInfo(DateiName, fi);
  DirRec.DateTime := fi.UTCTimestamp;
  DirRec.Modes := fi.Modes;
  DirRec.Name := NameInTarFile;
  DirRec.Permissions := fi.Permissions;
  DirRec.Size := fi.Size;

  {$IFDEF UNIX}
  DirRec.GID := fi.GID;
  DirRec.UID := fi.UID;
  DirRec.UserName := fi.User;
  DirRec.GroupName := fi.Group;
  {$ENDIF}

  if (fi.IsLink) then
  begin
    DirRec.FileType := mrftSymbolicLink;
    DirRec.LinkName := fi.LinkTarget;
  end; // if (fi.IsLink)

  Result := AddStream(s, DirRec);
  s.Free;
  DirRec.Free;
end;

function TMRTarArchive.AddStream(Stream: TStream;
  NameInTarFile: string): Boolean;
var
  DirRec: TMRTarLibDirRec;
  fi: TMRTarLibFileInfo;
begin
  DirRec := TMRTarLibDirRec.Create;
  DirRec.Name := NameInTarFile;
  DirRec.Size := Stream.Size - Stream.Position;
  Result := AddStream(Stream, DirRec);
  DirRec.Free;
end;

function TMRTarArchive.AddString(aText: string;
  NameInTarFile: string): Boolean;
var
  s: TStringStream;
begin
  s := TStringStream.Create(aText);
  Result := AddStream(s, NameInTarFile);
  s.Free;
end;

function TMRTarArchive.FindNext(var DirRec: TMRTarLibDirRec): Boolean;
var
  anz: LongInt;
  chksum: LongInt;
  chksum2: LongInt;
  CurFilePos: Int64;
  I: LongInt;
  rec: array [0 .. RECORDSIZE - 1] of Char;
  s: String;
  th: TMRTarLibHeader absolute rec;
  cf: TMRTarLibDirRec;
begin
  Result := False;

  if (FBytesToGo > 0) then
    FArchiv.Seek(Records(FBytesToGo) * RECORDSIZE, soFromCurrent);

  CurFilePos := FArchiv.Position;
  FillChar(rec, RECORDSIZE, 0);
  anz := FArchiv.Read(rec, RECORDSIZE);

  if (anz < RECORDSIZE) then
    Exit;

  if (rec[0] = #0) then
    Exit;

  DirRec.Clear;
  DirRec.Position := CurFilePos;
  DirRec.Name := th.Name;
  I := OctalToInt(@th.Mode);

  if I AND $0100 <> 0 then
    Include(DirRec.FPermissions, mrtpReadByOwner);
  if I AND $0080 <> 0 then
    Include(DirRec.FPermissions, mrtpWriteByOwner);
  if I AND $0040 <> 0 then
    Include(DirRec.FPermissions, mrtpExecuteByOwner);
  if I AND $0020 <> 0 then
    Include(DirRec.FPermissions, mrtpReadByGroup);
  if I AND $0010 <> 0 then
    Include(DirRec.FPermissions, mrtpWriteByGroup);
  if I AND $0008 <> 0 then
    Include(DirRec.FPermissions, mrtpExecuteByGroup);
  if I AND $0004 <> 0 then
    Include(DirRec.FPermissions, mrtpReadByOther);
  if I AND $0002 <> 0 then
    Include(DirRec.FPermissions, mrtpWriteByOther);
  if I AND $0001 <> 0 then
    Include(DirRec.FPermissions, mrtpExecuteByOther);
  if I AND $0200 <> 0 then
    Include(DirRec.FModes, mrtmSaveText);
  if I AND $0400 <> 0 then
    Include(DirRec.FModes, mrtmSetGid);
  if I AND $0800 <> 0 then
    Include(DirRec.FModes, mrtmSetUid);

  case th.LinkFlag of
    #0, '0' :
      DirRec.FileType := mrftRegular;
    '1'     :
      DirRec.FileType := mrftTarLink;
    '2'     :
      DirRec.FileType := mrftSymbolicLink;
    '3'     :
      DirRec.FileType := mrftCharacterDevice;
    '4'     :
      DirRec.FileType := mrftBlockDevice;
    '5'     :
      DirRec.FileType := mrftDirectory;
    '6'     :
      DirRec.FileType := mrftFIFO;
    '7'     :
      DirRec.FileType := mrftReserved;
    'D'     :
      DirRec.FileType := mrftDumpDir;
    'M'     :
      DirRec.FileType := mrftMultiVolume;
    'V'     :
      DirRec.FileType := mrftVolumeHeader;
    'L'     :
      DirRec.FileType := mrftLongLink;
  end;

  DirRec.DateTime := OctalToInt(@th.MTime);
  DirRec.GID := OctalToInt(@th.GID);
  DirRec.GroupName := th.GName;
  DirRec.LinkName := th.LinkName;
  DirRec.Magic := Trim(th.Magic);
  DirRec.MajorDevNo := OctalToInt(@th.DevMajor);
  DirRec.MinorDevNo := OctalToInt(@th.DevMinor);
  DirRec.Size := OctalToInt64(@th.Size);
  DirRec.UID := OctalToInt(@th.UID);
  DirRec.UserName := th.UName;
  chksum := OctalToInt(@th.ChkSum);
  strmove(th.ChkSum, PChar(CHKBLANKS), Length(CHKBLANKS));
  chksum2 := HeaderChecksum(th);
  DirRec.ChecksumOK := (chksum = chksum2);

  if (DirRec.FileType in [mrftTarLink, mrftSymbolicLink, mrftDirectory, mrftFIFO, mrftVolumeHeader]) then
    FBytesToGo := 0
  else
    FBytesToGo := DirRec.Size;

  Result := True;

  if(DirRec.FileType = mrftLongLink) then
  begin
    s           := Trim(GetCurFile);
    Result      := Self.FindNext(DirRec);
    DirRec.Name := s;
  end;

  DirRec.Name := RemoveLeadingPathDelimiter(DirRec.Name);
  FCurFile.AssignFrom(DirRec);
end;

function TMRTarArchive.GetCurFile: string;
var
  RestBytes: Integer;
begin
  if FBytesToGo = 0 then
    Exit;

  RestBytes := Records(FBytesToGo) * RECORDSIZE - FBytesToGo;
  SetLength(Result, FBytesToGo);
  FArchiv.ReadBuffer(PChar(Result)^, FBytesToGo);
  FArchiv.Seek(RestBytes, soFromCurrent);
  FBytesToGo := 0;
end;

function TMRTarArchive.ListFiles(List: TStrings): Int64;
begin
  Result := ListFiles(List, True);
end;

function TMRTarArchive.ListFiles(List: TStrings;
  WithDirRecData: Boolean): Int64;
var
  dr: TMRTarLibDirRec;
  p: TMRTarLibDirRec;
begin
  FArchiv.Seek(0, soFromBeginning);
  Result := 0;
  List.Clear;
  dr := TMRTarLibDirRec.Create;

  while (FindNext(dr)) do
  begin
    p := nil;

    if (WithDirRecData) then
    begin
      p := TMRTarLibDirRec.Create;
      p.AssignFrom(dr);
    end; // if (WithDirRecData)

    List.AddObject(dr.Name, p);
  end; // while (FindNext(dr))

  dr.Free;
  Result := List.Count;
end;

procedure TMRTarArchive.GetCurFile(DstFileName: string);
var
  fs: TFileStream;
begin
{ DONE 1 -oselber : Dateidatum und Berechtigungen nach schreiben setzen (wenn möglich) }
  if (FCurFile.FileType = mrftRegular) then
  begin
    fs := TFileStream.Create(DstFileName, fmCreate);
    GetCurFile(fs);
    fs.Free;
  end; // if (FCurFile.FileType = mrftRegular)
end;

procedure TMRTarArchive.GetCurFile(DstStream: TStream);
var
  RestBytes: Integer;
begin
  if FBytesToGo = 0 then
    Exit;

  RestBytes := Records(FBytesToGo) * RECORDSIZE - FBytesToGo;
  DstStream.CopyFrom(FArchiv, FBytesToGo);
  FArchiv.Seek(RestBytes, soFromCurrent);
  FBytesToGo := 0;
end;

procedure TMRTarArchive.ListFiles(RootNode: TTreeNode);
var
  i: Integer;
  sl: TStringList;
begin
  if (RootNode <> nil) then
  begin
    sl := TStringList.Create;
    ListFiles(sl);

    for i := 0 to sl.Count - 1 do
    begin

    end; // for i := 0 to sl.Count - 1

    sl.Free;
  end; // if (RootNode <> nil)
end;

procedure TMRTarArchive.ListFiles(TreeView: TTreeView);
begin
  ListFiles(TreeView, True);
end;

procedure TMRTarArchive.ListFiles(TreeView: TTreeView; ClearTV: Boolean);
  procedure ClearChildren(rootnode: TTreeNode);
  var
    node: TTreeNode;
  begin
    node := rootnode.GetFirstChild;

    while (node <> nil) do
    begin
      ClearChildren(node);

      try
        TObject(node.Data).Free;
      finally
        node := node.GetNextSibling;
      end;
    end; // while (node <> nil)
  end;

  function FindNode(rootnode: TTreeNode; aCaption: string): TTreeNode;
  var
    node: TTreeNode;
    s: String;
  begin
    if (rootnode = nil) then
      node := TreeView.Items.GetFirstNode
    else
      node := rootnode.GetFirstChild;

    while (node <> nil) do
    begin
      s := node.Text;

      if (s = aCaption) then
        Break;

      node := node.GetNextSibling;
    end; // while (node <> nil)

    Result := node;
  end;

var
  dr: TMRTarLibDirRec;
  dr2: TMRTarLibDirRec;
  i: Integer;
  j: Integer;
  node: TTreeNode;
  node1: TTreeNode;
  node2: TTreeNode;
  oldcomp: TTVCompareEvent;
  s: String;
  sa: TStrArray;
  sl: TStringList;
  k: Integer;
begin
  TreeView.BeginUpdate;

  if (ClearTV) then
  begin
    node := TreeView.Items.GetFirstNode;

    while (node <> nil) do
    begin
      ClearChildren(node);

      try
        TObject(node.Data).Free;
      finally
        node := node.GetNextSibling;
      end;
    end; // while (node <> nil)

    TreeView.Items.Clear;
  end; // if (ClearTV)

  oldcomp := TreeView.OnCompare;
  TreeView.OnCompare := @TreeViewCompare;
  sl := TStringList.Create;
  ListFiles(sl);

  for k := 0 to sl.Count - 1 do
  begin
    dr := TMRTarLibDirRec(sl.Objects[k]);
    s := dr.Name;

    if (s[Length(s)] = '/') then
      s[Length(s)] := '\';

    Explode(s, '/', sa);
    node := nil;
    i := 0;

    while (i < High(sa)) do
    begin
      node2 := FindNode(node, sa[i] + '/');

      if (node2 = nil) then
      begin
        dr2 := TMRTarLibDirRec.Create;
        dr2.AssignFrom(dr);
        dr2.FileType := mrftDirectory;
        s := '';
        node2 := TreeView.Items.AddChild(node, sa[i] + '/');
        node1 := node2;

        for j := 0 to i do
        begin
          s := s + sa[j] + '/';
        end; // for j := 0 to i

        dr2.Name := s;
        node2.Data := dr2;
      end;

      i := i + 1;
      node := node2;
    end; // while (i < High(sa))

    s := sa[i];

    if (s[Length(s)] = '\') then
      s[Length(s)] := '/';

    node := TreeView.Items.AddChild(node, s);
    node.Data := dr;
  end; // for i := 0 to sl.Count - 1

  sl.Free;
  TreeView.AlphaSort;
  TreeView.OnCompare := oldcomp;
  TreeView.EndUpdate;
end;

procedure TMRTarArchive.SaveToFile(DirRec: TMRTarLibDirRec; DateiName: string);
begin
  FArchiv.Seek(DirRec.Position + RECORDSIZE, soFromBeginning);
  FBytesToGo := DirRec.Size;
  GetCurFile(DateiName);
end;

procedure TMRTarArchive.SaveToStream(DirRec: TMRTarLibDirRec; Stream: TStream);
begin
  FArchiv.Seek(DirRec.Position + RECORDSIZE, soFromBeginning);
  FBytesToGo := DirRec.Size;
  GetCurFile(Stream);
end;

{ TMRTarLibDirRec }

constructor TMRTarLibDirRec.Create;
begin
  inherited;

  Self.Clear;
end;

procedure TMRTarLibDirRec.AssignFrom(Src: TMRTarLibDirRec);
begin
  FChecksumOK := Src.ChecksumOK;
  FDateTime := Src.DateTime;
  FFilePos := Src.FilePos;
  FFileType := Src.FileType;
  FGID := Src.GID;
  FGroupName := Src.GroupName;
  FLinkName := Src.LinkName;
  FMagic := Src.Magic;
  FMajorDevNo := Src.MajorDevNo;
  FMinorDevNo := Src.MinorDevNo;
  FModes := Src.Modes;
  FName := Src.Name;
  FPermissions := Src.Permissions;
  FPosition := Src.Position;
  FSize := Src.Size;
  FUID := Src.UID;
  FUserName := Src.UserName;
end;

procedure TMRTarLibDirRec.Clear;
begin
  FChecksumOK := True;
  FDateTime := DateTimeToUnix(EncodeDateTime(1970, 1, 1, 0, 0, 0, 0));
  FFilePos := 0;
  FFileType := mrftRegular;
  FGID := 65534;
  FGroupName := 'nogroup';
  FLinkName := '';
  FMajorDevNo := 0;
  FMinorDevNo := 0;
  FModes := [];
  FName := '';
  FPermissions := [mrtpReadByOwner, mrtpWriteByOwner, mrtpExecuteByOwner,
    mrtpReadByGroup, mrtpWriteByGroup, mrtpExecuteByGroup,
    mrtpReadByOther, mrtpWriteByOther, mrtpExecuteByOther];
  FSize := 0;
  FUID := 65534;
  FUserName := 'nobody';
end;

end.

