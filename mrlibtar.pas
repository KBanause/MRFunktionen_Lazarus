unit mrlibtar;

interface

{$IFDEF FPC}
 {$MODE Delphi}
{$ELSE}
  {$IFDEF LINUX}
     {$DEFINE Kylix}
     {$DEFINE LIBCUNIT}
  {$ENDIF}
{$ENDIF} 

uses
{$IFDEF LIBCUNIT}
  Libc,    // MvdV: Nothing is used from this???
{$ENDIF}
{$ifdef Unix} 
  BaseUnix, Unix,
  UnixType, {$endif}
  {$IFDEF MSWINDOWS}
  windows,
  {$ENDIF}
  Classes, SysUtils;


type
  // --- File Access Permissions
  TTarPermission  = (tpReadByOwner, tpWriteByOwner, tpExecuteByOwner,
    tpReadByGroup, tpWriteByGroup, tpExecuteByGroup,
    tpReadByOther, tpWriteByOther, tpExecuteByOther);
  TTarPermissions = set of TTarPermission;

  // --- Type of File
  TFileType = (ftNormal,          // Regular file
    ftLink,            // Link to another, previously archived, file(LinkName)
    ftSymbolicLink,    // Symbolic link to another file(LinkName)
    ftCharacter,       // Character special files
    ftBlock,           // Block special files
    ftDirectory,       // Directory entry. Size is zero(unlimited) or max. number of bytes
    ftFifo,            // FIFO special file. No data stored in the archive.
    ftContiguous,      // Contiguous file, if supported by OS
    ftDumpDir,         // List of files
    ftMultiVolume,     // Multi-volume file part
    ftVolumeHeader,    // Volume header. Can appear only as first record in the archive
    ftLongLink);

  // --- Mode
  TTarMode  = (tmSetUid, tmSetGid, tmSaveText);
  TTarModes = set of TTarMode;

  // --- Record for a Directory Entry
  //     Adjust the ClearDirRec procedure when this record changes!
  TTarDirRec  = record
    Name: string;            // File path and name
    Size: Int64;             // File size in Bytes
    DateTime: TDateTime;         // Last modification date and time
    Permissions: TTarPermissions;   // Access permissions
    FileType: TFileType;         // Type of file
    LinkName: string;            // Name of linked file(for ftLink, ftSymbolicLink)
    UID: Integer;           // User ID
    GID: Integer;           // Group ID
    UserName: string;            // User name
    GroupName: string;            // Group name
    ChecksumOK: Boolean;           // Checksum was OK
    Mode: TTarModes;         // Mode
    Magic: string;            // Contents of the "Magic" field
    MajorDevNo: Integer;           // Major Device No. for ftCharacter and ftBlock
    MinorDevNo: Integer;           // Minor Device No. for ftCharacter and ftBlock
    FilePos: Int64;             // Position in TAR file
  end;

  // --- The TAR Archive CLASS
  TTarArchive = class
  protected
    FStream: TStream;   // Internal Stream
    FOwnsStream: Boolean;   // True if FStream is owned by the TTarArchive instance
    FBytesToGo: Int64;     // Bytes until the next Header Record
  public
    constructor Create(Stream: TStream); overload;
    constructor Create(Filename: string;
      FileMode: WORD = fmOpenRead OR fmShareDenyWrite); overload;
    destructor Destroy; override;
    procedure Reset;                                         // Reset File Pointer
    function FindNext(var DirRec: TTarDirRec): Boolean;  // Reads next Directory Info Record. False if EOF reached
    procedure ReadFile(Buffer: Pointer); overload;       // Reads file data for last Directory Record
    procedure ReadFile(Stream: TStream); overload;       // -;-
    procedure ReadFile(Filename: string); overload;       // -;-
    function ReadFile: string; overload;         // -;-

    procedure GetFilePos(var Current, Size: Int64);        // Current File Position
    procedure SetFilePos(NewPos: Int64);                   // Set new Current File Position
  end;

  // --- The TAR Archive Writer CLASS

  { TTarWriter }

  TTarWriter = class
  private
    procedure SetLongLink(var TarFileName: string);
  protected
    FStream: TStream;
    FOwnsStream: Boolean;
    FFinalized: Boolean;
    // --- Used at the next "Add" method call: ---
    FPermissions: TTarPermissions;   // Access permissions
    FUID: Integer;           // User ID
    FGID: Integer;           // Group ID
    FUserName: string;            // User name
    FGroupName: string;            // Group name
    FMode: TTarModes;         // Mode
    FMagic: string;            // Contents of the "Magic" field
    constructor CreateEmpty;
  public
    constructor Create(TargetStream: TStream); overload;
    constructor Create(TargetFilename: string; Mode: Integer = fmCreate); overload;
    destructor Destroy; override;                   // Writes End-Of-File Tag
    procedure AddFile(Filename: string;  TarFilename: string = '');
    procedure AddStream(Stream: TStream; TarFilename: string; FileDateGmt: TDateTime);
    procedure AddString(Contents: string;  TarFilename: string; FileDateGmt: TDateTime);
    procedure AddDir(Dirname: string; DateGmt: TDateTime; MaxDirSize: Int64 = 0);
    procedure AddSymbolicLink(Filename, Linkname: string; DateGmt: TDateTime);
    procedure AddLink(Filename, Linkname: string; DateGmt: TDateTime);
    procedure AddVolumeHeader(VolumeId: string; DateGmt: TDateTime);
    procedure Finalize;
    property Permissions: TTarPermissions read FPermissions write FPermissions;   // Access permissions
    property UID: Integer read FUID write FUID;           // User ID
    property GID: Integer read FGID write FGID;           // Group ID
    property UserName: string read FUserName write FUserName;      // User name
    property GroupName: string read FGroupName write FGroupName;     // Group name
    property Mode: TTarModes read FMode write FMode;          // Mode
    property Magic: string read FMagic write FMagic;         // Contents of the "Magic" field
  end;

// --- Some useful constants
const
  FILETYPE_NAME: array [TFileType] of string =
    ('Regular', 'Link', 'Symbolic Link', 'Char File', 'Block File',
    'Directory', 'FIFO File', 'Contiguous', 'Dir Dump', 'Multivol',
    'Volume Header', 'LongLink');

  ALL_PERMISSIONS     = [tpReadByOwner, tpWriteByOwner, tpExecuteByOwner,
    tpReadByGroup, tpWriteByGroup, tpExecuteByGroup,
    tpReadByOther, tpWriteByOther, tpExecuteByOther];
  READ_PERMISSIONS    = [tpReadByOwner, tpReadByGroup,  tpReadByOther];
  WRITE_PERMISSIONS   = [tpWriteByOwner, tpWriteByGroup, tpWriteByOther];
  EXECUTE_PERMISSIONS = [tpExecuteByOwner, tpExecuteByGroup, tpExecuteByOther];


function PermissionString(Permissions: TTarPermissions): string;
function ConvertFilename(Filename: string): string;
function FileTimeGMT(FileName: string): TDateTime; overload;
function FileTimeGMT(SearchRec: TSearchRec): TDateTime; overload;
procedure ClearDirRec(var DirRec: TTarDirRec);


(*
===============================================================================================
IMPLEMENTATION
===============================================================================================
*)

implementation

function PermissionString(Permissions: TTarPermissions): string;
begin
  Result := '';

  if tpReadByOwner    IN Permissions then
    Result                             := Result + 'r'
  else
    Result                             := Result + '-';

  if tpWriteByOwner   IN Permissions then
    Result                             := Result + 'w'
  else
    Result                             := Result + '-';

  if tpExecuteByOwner IN Permissions then
    Result                             := Result + 'x'
  else
    Result                             := Result + '-';

  if tpReadByGroup    IN Permissions then
    Result                             := Result + 'r'
  else
    Result                             := Result + '-';

  if tpWriteByGroup   IN Permissions then
    Result                             := Result + 'w'
  else
    Result                             := Result + '-';

  if tpExecuteByGroup IN Permissions then
    Result                             := Result + 'x'
  else
    Result                             := Result + '-';

  if tpReadByOther    IN Permissions then
    Result                             := Result + 'r'
  else
    Result                             := Result + '-';

  if tpWriteByOther   IN Permissions then
    Result                             := Result + 'w'
  else
    Result                             := Result + '-';

  if tpExecuteByOther IN Permissions then
    Result                             := Result + 'x'
  else
    Result                             := Result + '-';
end;


function ConvertFilename(Filename: string): string;
  // Converts the filename to Unix conventions
  // could be empty and inlined away for FPC. FPC I/O should be 
  // forward/backward slash safe.
begin
  (*$IFDEF Unix *)
  Result := Filename;
  (*$ELSE *)
  Result := StringReplace(Filename, '\', '/', [rfReplaceAll]);
  (*$ENDIF *)
end;

function FileTimeGMT(FileName: string): TDateTime;
  // Returns the Date and Time of the last modification of the given File
  // The Result is zero if the file could not be found
  // The Result is given in UTC(GMT) time zone
var
  SR: TSearchRec;
begin
  Result := 0.0;
  if FindFirst(FileName, faAnyFile, SR) = 0 then
    Result := FileTimeGMT(SR);
  FindClose(SR);
end;


function FileTimeGMT(SearchRec: TSearchRec): TDateTime;
  (*$IFDEF MSWINDOWS *)
var
  SystemFileTime: TSystemTime;
  (*$ENDIF *)
  (*$IFDEF Unix *)
var
  TimeVal:  TTimeVal;
  TimeZone: TTimeZone;
  (*$ENDIF *)
begin
  Result := 0.0;
  (*$IFDEF MSWINDOWS *)(*$WARNINGS OFF *)
  if(SearchRec.FindData.dwFileAttributes AND faDirectory) = 0 then
    if FileTimeToSystemTime(SearchRec.FindData.ftLastWriteTime, SystemFileTime) then
      Result := EncodeDate(SystemFileTime.wYear, SystemFileTime.wMonth, SystemFileTime.wDay)
        + EncodeTime(SystemFileTime.wHour, SystemFileTime.wMinute, SystemFileTime.wSecond, SystemFileTime.wMilliseconds);
  (*$ENDIF *)(*$WARNINGS ON *)
  (*$IFDEF Unix *)
  if SearchRec.Attr AND faDirectory = 0 then
  begin
    Result := FileDateToDateTime(SearchRec.Time);
       {$IFDEF Kylix}
    GetTimeOfDay(TimeVal, TimeZone);
       {$ELSE}
    fpGetTimeOfDay(@TimeVal, @TimeZone);
       {$ENDIF}
    Result := Result + TimeZone.tz_minuteswest /(60 * 24);
  end;
  (*$ENDIF *)
end;


procedure ClearDirRec(var DirRec: TTarDirRec);
// This is included because a FillChar(DirRec, SizeOf(DirRec), 0)
// will destroy the long string pointers, leading to strange bugs
begin
  with DirRec do
  begin
    Name        := '';
    Size        := 0;
    DateTime    := 0.0;
    Permissions := [];
    FileType    := TFileType(0);
    LinkName    := '';
    UID         := 0;
    GID         := 0;
    UserName    := '';
    GroupName   := '';
    ChecksumOK  := False;
    Mode        := [];
    Magic       := '';
    MajorDevNo  := 0;
    MinorDevNo  := 0;
    FilePos     := 0;
  end;
end;

(*
===============================================================================================
TAR format
===============================================================================================
*)

const
  RECORDSIZE = 512;
  NAMSIZ     = 100;
  TUNMLEN    =  32;
  TGNMLEN    =  32;
  CHKBLANKS  = #32#32#32#32#32#32#32#32;

type
  TTarHeader = packed record
    Name: array [0..NAMSIZ-1] of Char;
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

function ExtractText(P: PChar): string;
begin
  Result := string(P);
end;


function ExtractNumber(P: PChar): Integer; overload;
var
  Strg: string;
begin
  Strg   := Trim(StrPas(P));
  P      := PChar(Strg);
  Result := 0;
  while(P^ <> #32) AND(P^ <> #0) do
  begin
    Result :=(ORD(P^) - ORD('0')) OR(Result SHL 3);
    INC(P);
  end;
end;

function ExtractNumber64(P: PChar): Int64; overload;
var
  Strg: string;
begin
  Strg   := Trim(StrPas(P));
  P      := PChar(Strg);
  Result := 0;
  while(P^ <> #32) AND(P^ <> #0) do
  begin
    Result :=(ORD(P^) - ORD('0')) OR(Result SHL 3);
    INC(P);
  end;
end;


function ExtractNumber(P: PChar; MaxLen: Integer): Integer; overload;
var
  S0:   array [0..255] of Char;
  Strg: string;
begin
  StrLCopy(S0, P, MaxLen);
  Strg   := Trim(StrPas(S0));
  P      := PChar(Strg);
  Result := 0;
  while(P^ <> #32) AND(P^ <> #0) do
  begin
    Result :=(ORD(P^) - ORD('0')) OR(Result SHL 3);
    INC(P);
  end;
end;


function ExtractNumber64(P: PChar; MaxLen: Integer): Int64; overload;
var
  S0:   array [0..255] of Char;
  Strg: string;
begin
  StrLCopy(S0, P, MaxLen);
  Strg   := Trim(StrPas(S0));
  P      := PChar(Strg);
  Result := 0;
  while(P^ <> #32) AND(P^ <> #0) do
  begin
    Result :=(ORD(P^) - ORD('0')) OR(Result SHL 3);
    INC(P);
  end;
end;


function Records(Bytes: Int64): Int64;
begin
  Result := Bytes DIV RECORDSIZE;
  if Bytes MOD RECORDSIZE > 0 then
    INC(Result);
end;


procedure Octal(N: Integer; P: PChar; Len: Integer);
// Makes a string of octal digits
// The string will always be "Len" characters long
var
  I: Integer;
begin
  for I := Len-2 downto 0 do
  begin
    (P+I)^ := CHR(ORD('0') + ORD(N AND $07));
    N      := N SHR 3;
  end;
  //FOR I := 0 TO Len-3 DO
  //IF(P+I)^ = '0'
  //THEN(P+I)^ := '0'
  //ELSE BREAK;
  (P+Len-1)^ := '0';
end;


procedure Octal64(N: Int64; P: PChar; Len: Integer);
// Makes a string of octal digits
// The string will always be "Len" characters long
var
  I: Integer;
begin
  for I := Len-2 downto 0 do
  begin
    (P+I)^ := CHR(ORD('0') + ORD(N AND $07));
    N      := N SHR 3;
  end;
  //FOR I := 0 TO Len-3 DO
  //IF(P+I)^ = '0'
  //THEN(P+I)^ := '0'
  //ELSE BREAK;
  (P+Len-1)^ := #0;
end;


procedure OctalN(N: Integer; P: PChar; Len: Integer);
begin
  Octal(N, P, Len-1);
  (P+Len-1)^ := #0;
end;

procedure OctalNN(N: Integer; P: PChar; Len: Integer);
var
  i: Integer;
begin
  Octal(N, P, Len-1);

  for i := 0 to Len - 1 do
    if((P+I)^ = '0') then
      (P+I)^ := #0;

  (P+Len-1)^ := #0;
end;


procedure WriteTarHeader(Dest: TStream; DirRec: TTarDirRec);
var
  Rec:      array [0..RECORDSIZE-1] of Char;
  TH:       TTarHeader ABSOLUTE Rec;
  Mode:     Integer;
  NullDate: TDateTime;
  Checksum: CARDINAL;
  I:        Integer;
  s:        string;
begin
  FillChar(Rec, RECORDSIZE, 0);
  StrLCopy(TH.Name, PChar(DirRec.Name), NAMSIZ);
  Mode := 0;
  if tmSaveText IN DirRec.Mode then
    Mode := Mode OR $0200;
  if tmSetGid   IN DirRec.Mode then
    Mode := Mode OR $0400;
  if tmSetUid   IN DirRec.Mode then
    Mode := Mode OR $0800;
  if tpReadByOwner    IN DirRec.Permissions then
    Mode := Mode OR $0100;
  if tpWriteByOwner   IN DirRec.Permissions then
    Mode := Mode OR $0080;
  if tpExecuteByOwner IN DirRec.Permissions then
    Mode := Mode OR $0040;
  if tpReadByGroup    IN DirRec.Permissions then
    Mode := Mode OR $0020;
  if tpWriteByGroup   IN DirRec.Permissions then
    Mode := Mode OR $0010;
  if tpExecuteByGroup IN DirRec.Permissions then
    Mode := Mode OR $0008;
  if tpReadByOther    IN DirRec.Permissions then
    Mode := Mode OR $0004;
  if tpWriteByOther   IN DirRec.Permissions then
    Mode := Mode OR $0002;
  if tpExecuteByOther IN DirRec.Permissions then
    Mode := Mode OR $0001;
  OctalN(Mode, @TH.Mode, 8);
  OctalN(DirRec.UID, @TH.UID, 8);
  OctalN(DirRec.GID, @TH.GID, 8);
  Octal64(DirRec.Size, @TH.Size, 12);
  NullDate := EncodeDate(1970, 1, 1);
  if DirRec.DateTime >= NullDate
  then
    OctalN(Trunc((DirRec.DateTime - NullDate) * 86400.0), @TH.MTime, 12)
  else OctalN(Trunc(                   NullDate  * 86400.0), @TH.MTime, 12);
  case DirRec.FileType of
    ftNormal       :
      TH.LinkFlag := '0';
    ftLink         :
      TH.LinkFlag := '1';
    ftSymbolicLink :
      TH.LinkFlag := '2';
    ftCharacter    :
      TH.LinkFlag := '3';
    ftBlock        :
      TH.LinkFlag := '4';
    ftDirectory    :
      TH.LinkFlag := '5';
    ftFifo         :
      TH.LinkFlag := '6';
    ftContiguous   :
      TH.LinkFlag := '7';
    ftDumpDir      :
      TH.LinkFlag := 'D';
    ftMultiVolume  :
      TH.LinkFlag := 'M';
    ftVolumeHeader :
      TH.LinkFlag := 'V';
    ftLongLink     :
      TH.LinkFlag := 'L';
  end;
  StrLCopy(TH.LinkName, PChar(DirRec.LinkName), NAMSIZ);
  s := DirRec.Magic + #32#32#32#32#32#32#32;
  SetLength(s, 7);
  s := s + #0;
  StrLCopy(TH.Magic, PChar(s), 8);
  StrLCopy(TH.UName, PChar(DirRec.UserName), TUNMLEN);
  StrLCopy(TH.GName, PChar(DirRec.GroupName), TGNMLEN);
  OctalNN(DirRec.MajorDevNo, @TH.DevMajor, 8);
  OctalNN(DirRec.MinorDevNo, @TH.DevMinor, 8);
  StrMove(TH.ChkSum, CHKBLANKS, 8);

  CheckSum := 0;
  for I := 0 to SizeOf(TTarHeader)-1 do
    INC(CheckSum, Integer(ORD(Rec [I])));
  OctalN(CheckSum, @TH.ChkSum, 8);
  TH.ChkSum[6] := #0;
  TH.ChkSum[7] := ' ';

  Dest.Write(TH, RECORDSIZE);
end;


(*
===============================================================================================
TTarArchive
===============================================================================================
*)

constructor TTarArchive.Create(Stream: TStream);
begin
  inherited Create;
  FStream     := Stream;
  FOwnsStream := False;
  Reset;
end;


constructor TTarArchive.Create(Filename: string; FileMode: WORD);
begin
  inherited Create;
  FStream     := TFileStream.Create(Filename, FileMode);
  FOwnsStream := True;
  Reset;
end;


destructor TTarArchive.Destroy;
begin
  if FOwnsStream then
    FStream.Free;
  inherited Destroy;
end;


procedure TTarArchive.Reset;
// Reset File Pointer
begin
  FStream.Position := 0;
  FBytesToGo       := 0;
end;


function TTarArchive.FindNext(var DirRec: TTarDirRec): Boolean;
  // Reads next Directory Info Record
  // The Stream pointer must point to the first byte of the tar header
var
  Rec:          array [0..RECORDSIZE-1] of Char;
  CurFilePos:   Integer;
  Header:       TTarHeader ABSOLUTE Rec;
  I:            Integer;
  HeaderChkSum: WORD;
  Checksum:     CARDINAL;
  s:            string;
begin
  // --- Scan until next pointer
  if FBytesToGo > 0 then
    FStream.Seek(Records(FBytesToGo) * RECORDSIZE, soFromCurrent);

  // --- EOF reached?
  Result     := False;
  CurFilePos := FStream.Position;
  try
    FStream.ReadBuffer(Rec, RECORDSIZE);
    if Rec [0] = #0 then
      EXIT;   // EOF reached
  except
    EXIT;   // EOF reached, too
  end;
  Result := True;

  ClearDirRec(DirRec);

  DirRec.FilePos  := CurFilePos;
  DirRec.Name     := ExtractText(Header.Name);
  DirRec.Size     := ExtractNumber64(@Header.Size, 12);
  DirRec.DateTime := EncodeDate(1970, 1, 1) +(ExtractNumber(@Header.MTime, 12) / 86400.0);
  I               := ExtractNumber(@Header.Mode);
  if I AND $0100 <> 0 then
    Include(DirRec.Permissions, tpReadByOwner);
  if I AND $0080 <> 0 then
    Include(DirRec.Permissions, tpWriteByOwner);
  if I AND $0040 <> 0 then
    Include(DirRec.Permissions, tpExecuteByOwner);
  if I AND $0020 <> 0 then
    Include(DirRec.Permissions, tpReadByGroup);
  if I AND $0010 <> 0 then
    Include(DirRec.Permissions, tpWriteByGroup);
  if I AND $0008 <> 0 then
    Include(DirRec.Permissions, tpExecuteByGroup);
  if I AND $0004 <> 0 then
    Include(DirRec.Permissions, tpReadByOther);
  if I AND $0002 <> 0 then
    Include(DirRec.Permissions, tpWriteByOther);
  if I AND $0001 <> 0 then
    Include(DirRec.Permissions, tpExecuteByOther);
  if I AND $0200 <> 0 then
    Include(DirRec.Mode, tmSaveText);
  if I AND $0400 <> 0 then
    Include(DirRec.Mode, tmSetGid);
  if I AND $0800 <> 0 then
    Include(DirRec.Mode, tmSetUid);
  case Header.LinkFlag of
    #0, '0' :
      DirRec.FileType := ftNormal;
    '1'     :
      DirRec.FileType := ftLink;
    '2'     :
      DirRec.FileType := ftSymbolicLink;
    '3'     :
      DirRec.FileType := ftCharacter;
    '4'     :
      DirRec.FileType := ftBlock;
    '5'     :
      DirRec.FileType := ftDirectory;
    '6'     :
      DirRec.FileType := ftFifo;
    '7'     :
      DirRec.FileType := ftContiguous;
    'D'     :
      DirRec.FileType := ftDumpDir;
    'M'     :
      DirRec.FileType := ftMultiVolume;
    'V'     :
      DirRec.FileType := ftVolumeHeader;
    'L'     :
      DirRec.FileType := ftLongLink;
  end;
  DirRec.LinkName   := ExtractText(Header.LinkName);
  DirRec.UID        := ExtractNumber(@Header.UID);
  DirRec.GID        := ExtractNumber(@Header.GID);
  DirRec.UserName   := ExtractText(Header.UName);
  DirRec.GroupName  := ExtractText(Header.GName);
  DirRec.Magic      := Trim(ExtractText(Header.Magic));
  DirRec.MajorDevNo := ExtractNumber(@Header.DevMajor);
  DirRec.MinorDevNo := ExtractNumber(@Header.DevMinor);

  HeaderChkSum := ExtractNumber(@Header.ChkSum);   // Calc Checksum
  CheckSum     := 0;
  StrMove(Header.ChkSum, CHKBLANKS, 8);
  for I := 0 to SizeOf(TTarHeader)-1 do
    INC(CheckSum, Integer(ORD(Rec [I])));
  DirRec.CheckSumOK := WORD(CheckSum) = WORD(HeaderChkSum);

  if DirRec.FileType in [ftLink, ftSymbolicLink, ftDirectory, ftFifo, ftVolumeHeader]
  then
    FBytesToGo    := 0
  else FBytesToGo := DirRec.Size;

  if(DirRec.FileType = ftLongLink) then
  begin
    s           := Trim(Self.ReadFile);
    Result      := Self.FindNext(DirRec);
    DirRec.Name := s;
  end;
end;


procedure TTarArchive.ReadFile(Buffer: Pointer);
// Reads file data for the last Directory Record. The entire file is read into the buffer.
// The buffer must be large enough to take up the whole file.
var
  RestBytes: Integer;
begin
  if FBytesToGo = 0 then
    EXIT;
  RestBytes := Records(FBytesToGo) * RECORDSIZE - FBytesToGo;
  FStream.ReadBuffer(Buffer^, FBytesToGo);
  FStream.Seek(RestBytes, soFromCurrent);
  FBytesToGo := 0;
end;


procedure TTarArchive.ReadFile(Stream: TStream);
// Reads file data for the last Directory Record.
// The entire file is written out to the stream.
// The stream is left at its current position prior to writing
var
  RestBytes: Integer;
begin
  if FBytesToGo = 0 then
    EXIT;
  RestBytes := Records(FBytesToGo) * RECORDSIZE - FBytesToGo;
  Stream.CopyFrom(FStream, FBytesToGo);
  FStream.Seek(RestBytes, soFromCurrent);
  FBytesToGo := 0;
end;


procedure TTarArchive.ReadFile(Filename: string);
// Reads file data for the last Directory Record.
// The entire file is saved in the given Filename
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(Filename, fmCreate);
  try
    ReadFile(FS);
  finally
    FS.Free;
  end;
end;


function TTarArchive.ReadFile: string;
  // Reads file data for the last Directory Record. The entire file is returned
  // as a large ANSI string.
var
  RestBytes: Integer;
begin
  if FBytesToGo = 0 then
    EXIT;
  RestBytes := Records(FBytesToGo) * RECORDSIZE - FBytesToGo;
  SetLength(Result, FBytesToGo);
  FStream.ReadBuffer(PChar(Result)^, FBytesToGo);
  FStream.Seek(RestBytes, soFromCurrent);
  FBytesToGo := 0;
end;


procedure TTarArchive.GetFilePos(var Current, Size: Int64);
// Returns the Current Position in the TAR stream
begin
  Current := FStream.Position;
  Size    := FStream.Size;
end;


procedure TTarArchive.SetFilePos(NewPos: Int64);                   // Set new Current File Position
begin
  if NewPos < FStream.Size then
    FStream.Seek(NewPos, soFromBeginning);
end;


(*
===============================================================================================
TTarWriter
===============================================================================================
*)

procedure TTarWriter.SetLongLink(var TarFileName: string);
var
  BlockSize:   Int64;
  BytesToRead: Int64;
  DirRec:      TTarDirRec;
  ms:          TMemoryStream;
  Rec:         array [0..RECORDSIZE-1] of Char;
  sloc:        string;
  strstr:      TStringStream;
begin
  sloc            := '././@LongLink';
  strstr          := TStringStream.Create(TarFilename);
  ms              := TMemoryStream.Create;
  strstr.Position := 0;
  ms.CopyFrom(strstr, 0);
  ms.Position := 0;
  WriteLn(ms.Size);

  DirRec.Name        := sloc;
  DirRec.Size        := ms.Size + 1;
  DirRec.DateTime    := EncodeDate(1970, 1, 1);
  DirRec.Permissions := [];
  DirRec.FileType    := ftLongLink;
  DirRec.LinkName    := '';
  DirRec.UID         := 0;
  DirRec.GID         := 0;
  DirRec.UserName    := 'root';
  DirRec.GroupName   := 'root';
  DirRec.ChecksumOK  := True;
  DirRec.Mode        := [];
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;
  WriteTarHeader(FStream, DirRec);
  BytesToRead := DirRec.Size;

  while BytesToRead > 0 do
  begin
    BlockSize := BytesToRead;

    if BlockSize > RECORDSIZE then
      BlockSize := RECORDSIZE;

    FillChar(Rec, RECORDSIZE, 0);
    ms.Read(Rec, BlockSize);
    FStream.Write(Rec, RECORDSIZE);
    DEC(BytesToRead, BlockSize);
  end;

  ms.Free;
  strstr.Free;
  ClearDirRec(DirRec);
  SetLength(TarFileName, 100);
end;

constructor TTarWriter.CreateEmpty;
var
  TP: TTarPermission;
begin
  inherited Create;
  FOwnsStream  := False;
  FFinalized   := False;
  FPermissions := [];
  for TP := Low(TP) to High(TP) do
    Include(FPermissions, TP);
  FUID       := 0;
  FGID       := 0;
  FUserName  := '';
  FGroupName := '';
  FMode      := [];
  FMagic     := 'ustar';
end;

constructor TTarWriter.Create(TargetStream: TStream);
begin
  CreateEmpty;
  FStream     := TargetStream;
  FOwnsStream := False;
end;


constructor TTarWriter.Create(TargetFilename: string; Mode: Integer = fmCreate);
begin
  CreateEmpty;
  FStream     := TFileStream.Create(TargetFilename, Mode);
  FOwnsStream := True;
end;


destructor TTarWriter.Destroy;
begin
  if NOT FFinalized then
  begin
    Finalize;
    FFinalized := True;
  end;
  if FOwnsStream then
    FStream.Free;
  inherited Destroy;
end;


procedure TTarWriter.AddFile(Filename: string;  TarFilename: string = '');
var
  S:    TFileStream;
  Date: TDateTime;
begin
  Date := FileTimeGMT(Filename);
  if TarFilename = '' then
    TarFilename    := ConvertFilename(Filename)
  else TarFilename := ConvertFilename(TarFilename);
  S := TFileStream.Create(Filename, fmOpenRead OR fmShareDenyWrite);
  try
    AddStream(S, TarFilename, Date);
  finally
    S.Free
  end;
end;


procedure TTarWriter.AddStream(Stream: TStream; TarFilename: string; FileDateGmt: TDateTime);
var
  DirRec:      TTarDirRec;
  Rec:         array [0..RECORDSIZE-1] of Char;
  BytesToRead: Int64;      // Bytes to read from the Source Stream
  BlockSize:   Int64;      // Bytes to write out for the current record
begin
  ClearDirRec(DirRec);

  if(Length(TarFilename) > 100) then
    SetLongLink(TarFilename);

  DirRec.Name        := TarFilename;
  DirRec.Size        := Stream.Size - Stream.Position;
  DirRec.DateTime    := FileDateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftNormal;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := True;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader(FStream, DirRec);
  BytesToRead := DirRec.Size;
  while BytesToRead > 0 do
  begin
    BlockSize := BytesToRead;
    if BlockSize > RECORDSIZE then
      BlockSize := RECORDSIZE;
    FillChar(Rec, RECORDSIZE, 0);
    Stream.Read(Rec, BlockSize);
    FStream.Write(Rec, RECORDSIZE);
    DEC(BytesToRead, BlockSize);
  end;
end;


procedure TTarWriter.AddString(Contents: string; TarFilename: string; FileDateGmt: TDateTime);
var
  S: TStringStream;
begin
  S := TStringStream.Create(Contents);
  try
    AddStream(S, TarFilename, FileDateGmt);
  finally
    S.Free
  end;
end;


procedure TTarWriter.AddDir(Dirname: string; DateGmt: TDateTime; MaxDirSize: Int64 = 0);
var
  DirRec:      TTarDirRec;
  sloc:        string;
  strstr:      TStringStream;
  ms:          TMemoryStream;
  BytesToRead: Int64;
  BlockSize:   Int64;
  Rec:         array [0..RECORDSIZE-1] of Char;
begin
  ClearDirRec(DirRec);

  if(Length(Dirname) > 100) then
    SetLongLink(Dirname);

  DirRec.Name        := Dirname;
  DirRec.Size        := MaxDirSize;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftDirectory;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := True;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader(FStream, DirRec);
end;


procedure TTarWriter.AddSymbolicLink(Filename, Linkname: string; DateGmt: TDateTime);
var
  DirRec: TTarDirRec;
begin
  ClearDirRec(DirRec);

  if(Length(Filename) > 100) then
    SetLongLink(Filename);

  DirRec.Name        := Filename;
  DirRec.Size        := 0;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftSymbolicLink;
  DirRec.LinkName    := Linkname;
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := True;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader(FStream, DirRec);
end;


procedure TTarWriter.AddLink(Filename, Linkname: string; DateGmt: TDateTime);
var
  DirRec: TTarDirRec;
begin
  ClearDirRec(DirRec);

  if(Length(Filename) > 100) then
    SetLongLink(Filename);

  DirRec.Name        := Filename;
  DirRec.Size        := 0;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftLink;
  DirRec.LinkName    := Linkname;
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := True;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader(FStream, DirRec);
end;


procedure TTarWriter.AddVolumeHeader(VolumeId: string; DateGmt: TDateTime);
var
  DirRec: TTarDirRec;
begin
  ClearDirRec(DirRec);

  if(Length(VolumeId) > 100) then
    SetLongLink(VolumeId);

  DirRec.Name        := VolumeId;
  DirRec.Size        := 0;
  DirRec.DateTime    := DateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftVolumeHeader;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := True;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;

  WriteTarHeader(FStream, DirRec);
end;


procedure TTarWriter.Finalize;
// Writes the End-Of-File Tag
// Data after this tag will be ignored
// The destructor calls this automatically if you didn't do it before
var
  Rec: array [0..RECORDSIZE-1] of Char;
begin
  FillChar(Rec, SizeOf(Rec), 0);
  FStream.Write(Rec, RECORDSIZE);
  FStream.Write(Rec, RECORDSIZE);
  FFinalized := True;
end;


end.
