unit MRConsoleUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, getopts, strutils, CustApp;

const
  CPUBIT = {$IFDEF CPU64}'64' + {$ENDIF}{$IFDEF CPU32}'32' + {$ENDIF}'bit';

type
  TMRArgumentType = (mratNone, mratRequired, mratOptional);
  TMRLongoptions = array of TOption;
  TMROnLongOptionFound = procedure(Option: string; aValue: string) of object;
  TMROnShortOptionFound = procedure(Option: Char; aValue: string) of object;
  TMRStrArray = array of string;
  TMRExclusionArray = array of array of string;

  EMROptionAlreadyExists = class(Exception);
  EMROptionHasNoValue = class(Exception);
  EMROptionIsNotThatType = class(Exception);
  EMRShortOptionAlreadyExists = class(Exception);

  TMRLongOpts = class;
  TMRLongOpt = class;
  TMRParam = class;

  { TMRConsoleApplication }

  TMRConsoleApplication = class(TCustomApplication)
  private
    procedure CorrectParams;
    procedure GetArgv(var Strarray: TMRStrArray);
    procedure SetArgv(var Strarray: TMRStrArray);
  protected
    Options: TMRLongOpts;
    RemainingOptions: TStringList;

    procedure OnLongOptFound(Option: string; aValue: string); virtual;
    procedure OnShortOptFound(Option: Char; aValue: string); virtual;
    procedure OnUnallowedLongoptFound(Param1, Param2: string); virtual;
    procedure OnUnallowedShortoptFound(Param1, Param2: Char); virtual;

    function CorrectParam(Parameter: string; const ParamIndex: Integer): string; virtual;
    function GetVersion(var ShowBit: Boolean): string; virtual;
    function StdOutIsRedirected: Boolean;

    procedure SetLongOptExclusions(var ExclusionArray: TMRExclusionArray); virtual;
    procedure SetLongOpts; virtual;
    procedure SetShotOptExclusions(var ExclusionArray: TMRStrArray); virtual;
    procedure ShowUsage; virtual;
    procedure ShowVersion; virtual;
    procedure Write(args: array of const); overload;
    procedure Write(args: Int64); overload;
    procedure Write(args: string); overload;
    procedure Write(var aOutput: Text; args: array of const); overload;
    procedure Write(var aOutput: Text; args: Int64); overload;
    procedure Write(var aOutput: Text; args: string); overload;
    procedure Write; overload;
    procedure WriteLn(args: array of const); overload;
    procedure WriteLn(args: Int64); overload;
    procedure WriteLn(args: string); overload;
    procedure WriteLn(var aOutput: Text; args: array of const); overload;
    procedure WriteLn(var aOutput: Text; args: Int64); overload;
    procedure WriteLn(var aOutput: Text; args: string); overload;
    procedure WriteLn; overload;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToOem(s: string): string;

    procedure DoRun; virtual;
    procedure WaitUntilKeypressed(aMessage: string); overload;
    procedure WaitUntilKeypressed(aMessage: string; TextCol: Byte); overload;
    procedure WaitUntilKeypressed(showMessage: Boolean); overload;
    procedure WaitUntilKeypressed(showMessage: Boolean; aMessage: string); overload;
    procedure WaitUntilKeypressed(showMessage: Boolean; aMessage: string; TextCol: Byte); overload;
    procedure WaitUntilKeypressed(showMessage: Boolean; TextCol: Byte); overload;
    procedure WaitUntilKeypressed(TextCol: Byte); overload;
    procedure WaitUntilKeypressed; overload;
  end;

  { TMRLongOpt }

  TMRLongOpt = class
  private
    FArgumentType: TMRArgumentType;
    FLongOption: string;
    FShortOption: Char;
    procedure SetArgumentType(const AValue: TMRArgumentType);
    procedure SetLongOption(const AValue: string);
    procedure SetShortOption(const AValue: Char);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ArgumentType: TMRArgumentType read FArgumentType write SetArgumentType;
    property LongOption: string read FLongOption write SetLongOption;
    property ShortOption: Char read FShortOption write SetShortOption;
  end;

  { TMRLongOpts }

  TMRLongOpts = class
  private
    FLongOptExcl: TMRExclusionArray;
    FOnOptionFound: TMROnLongOptionFound;
    FOnShortOptionFound: TMROnShortOptionFound;
    FOptions: TStringList;
    FParams: TList;
    FParent: TMRConsoleApplication;
    FShortOptExcl: TMRStrArray;
    FShortOpts: TStringList;

    function LongOptIndex(ShortOpt: Char; LongOpts: TMRLongoptions): Integer;
    function ShortOptAllowed(ShortOptsFound: string; newShortOpt: Char): Char;
    function LongOptAllowed(LongOptsFound: TStringList; newLongOpt: string): string;
    procedure AddShortOption(aShortOpt: Char; aArgumentType: TMRArgumentType; IgnoreExistingShortOpt: Boolean); overload;
    procedure SetParamExclusions;
  public
    constructor Create(aParent: TMRConsoleApplication);
    destructor Destroy; override;

    function GetOptions: TMRLongoptions;
    function GetOptValue(aLongOption: string; aShortOption: string): string; overload;
    function GetOptValue(aOption: string): string; overload;

    procedure AddOption(aLongOpt: string); overload;
    procedure AddOption(aLongOpt: string; aShortOpt: Char); overload;
    procedure AddOption(aLongOpt: string; aArgumentType: TMRArgumentType); overload;
    procedure AddOption(aLongOpt: string; aShortOpt: Char; aArgumentType: TMRArgumentType); overload;
    procedure AddOption(aLongOpt: string; aShortOpt: Char; aArgumentType: TMRArgumentType; IgnoreExistingShortOpt: Boolean); overload;
    procedure AddShortOption(aShortOpt: Char); overload;
    procedure AddShortOption(aShortOpt: Char; aArgumentType: TMRArgumentType); overload;
    procedure ChangeOption(aLongOpt: string; aShortOpt: Char; aArgumentType: TMRArgumentType);
    procedure DeleteOption(aLongOpt: string);
    procedure DeleteShortOption(aShortOpt: Char);
    procedure ParseOptions; overload;
    procedure ParseOptions(var aRemainingOptions: TStringList); overload;
    procedure Reset;
  published
    property OnLongOptionFound: TMROnLongOptionFound read FOnOptionFound write FOnOptionFound;
    property OnShortOptionFound: TMROnShortOptionFound read FOnShortOptionFound write FOnShortOptionFound;
    property Parameter: TList read FParams;
  end;

  { TMRParam }

  TMRParam = class
  private
    FLongOption: string;
    FShortOption: Char;
    FValue: string;
    procedure SetLongOption(const AValue: string);
    procedure SetShortOption(const AValue: Char);
    procedure SetValue(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;

    function OptionHasValue: Boolean;
    function ValueAsBoolean: Boolean;
    function ValueAsExtended: Extended;
    function ValueAsInt64: Int64;
    function ValueAsInteger: Longint;
  published
    property LongOption: string read FLongOption write SetLongOption;
    property ShortOption: Char read FShortOption write SetShortOption;
    property Value: string read FValue write SetValue;
  end;

function StringCase(const Possible: array of string; const Value: string): Integer; overload;
function StringCase(const Possible: array of string; const Value: string; const CaseSensitiv: Boolean): Integer; overload;

implementation

uses
  {$IFDEF WINDOWS}
    windows,
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
    BaseUnix, Unix,
  {$ENDIF UNIX}
  crt;

function StringCase(const Possible: array of string;
  const Value: string): Integer;
begin
  Result := StringCase(Possible, Value, False);
end;

function StringCase(const Possible: array of string; const Value: string;
  const CaseSensitiv: Boolean): Integer;
var
  i: Integer;
  a: LongInt;
begin
  Result := -1;

  for i := 0 to High(Possible) do
  begin
    if (CaseSensitiv) then
      a := AnsiStrComp(PChar(Possible[i]), PChar(Value))
    else
      a := AnsiStrIComp(PChar(Possible[i]), PChar(Value));

    if (a = 0) then
    begin
      Result := i;
      Exit;
    end; // if (a = 0)
  end; // for i := 0 to High(Possible)
end;

{ TMRConsoleApplication }

function TMRConsoleApplication.ToOem(s: string): string;
begin
  {$IFDEF WINDOWS}
  s := Utf8ToAnsi(s);
  SetLength(Result, Length(s));
  CharToOemBuff(PChar(s), PChar(Result), Length(s));
  {$ENDIF WINDOWS}

  Result := s;
end;

procedure TMRConsoleApplication.DoRun;
begin
  Options.SetParamExclusions;
  Options.ParseOptions(RemainingOptions);
end;

procedure TMRConsoleApplication.WaitUntilKeypressed(aMessage: string);
begin
  WaitUntilKeypressed(True, aMessage);
end;

procedure TMRConsoleApplication.WaitUntilKeypressed(aMessage: string;
  TextCol: Byte);
begin
  TextColor(TextCol);
  WaitUntilKeypressed(aMessage);
end;

procedure TMRConsoleApplication.WaitUntilKeypressed(showMessage: Boolean);
begin
  WaitUntilKeypressed(showMessage, 'Press any key...');
end;

function TMRConsoleApplication.GetVersion(var ShowBit: Boolean): string;
begin
  ShowBit := True;
  Result := '';
end;

function TMRConsoleApplication.StdOutIsRedirected: Boolean;
begin
  Result := (StdOutputHandle <> 7);
end;

procedure TMRConsoleApplication.CorrectParams;
var
  sa: TMRStrArray;
  i: Integer;
begin
  GetArgv(sa);

  for i := 0 to High(sa) do
    sa[i] := CorrectParam(sa[i], i + 1);

  SetArgv(sa);
end;

procedure TMRConsoleApplication.GetArgv(var Strarray: TMRStrArray);
var
  i: Integer;
  s: String;
begin
  SetLength(Strarray, argc);

  for i := 0 to argc - 1 do
  begin
    Strarray[i] := ansistring(argv[i]);
  end; // for i := 0 to argc - 1
end;

procedure TMRConsoleApplication.OnLongOptFound(Option: string; aValue: string);
begin
  if (Option = 'version') then
  begin
    ShowVersion;
    Halt;
  end;
end;

procedure TMRConsoleApplication.OnShortOptFound(Option: Char; aValue: string);
begin
  if (Option = 'h') then
  begin
    ShowUsage;
    Halt;
  end;
end;

procedure TMRConsoleApplication.OnUnallowedLongoptFound(Param1,
  Param2: string);
begin

end;

procedure TMRConsoleApplication.OnUnallowedShortoptFound(Param1, Param2: Char);
begin

end;

procedure TMRConsoleApplication.SetArgv(var Strarray: TMRStrArray);
var
  i: Integer;
  s: string;
  t: String;
begin
  s := '';

  for i := 0 to High(Strarray) do
  begin
    t := ansistring(argv[i]);

    if (Length(Strarray[i]) <> Length(t)) then
    begin
      SysReAllocMem(argv[i], Length(Strarray[i]) + 1);
      t := Strarray[i] + #0;
      System.Move(t[1], argv[i]^, Length(t));
      t := ansistring(argv[i]);
    end;
  end; // for i := 0 to High(Strarray)
end;

function TMRConsoleApplication.CorrectParam(Parameter: string;
  const ParamIndex: Integer): string;
begin
  Result := Parameter;
end;

procedure TMRConsoleApplication.SetLongOpts;
begin
  Options.AddOption('help', 'h');
  Options.AddOption('version');
end;

// if some parameters aren't allowed to exists at the same time
// (e.g. a deleting parameter and a creating parameter)
// they should get insert here:
//
// ExclusionArray := [['create', 'delete'], ['list', 'append', 'extract']];
procedure TMRConsoleApplication.SetLongOptExclusions(
  var ExclusionArray: TMRExclusionArray);
var
  i: Integer;
begin
  for i := 0 to High(ExclusionArray) do
  begin
    SetLength(ExclusionArray[i], 0);
  end; // for i := 0 to High(ExclusionArray)

  SetLength(ExclusionArray, 0);
end;

// if some parameters aren't allowed to exists at the same time
// (e.g. a deleting parameter and a creating parameter)
// they should get insert here:
//
// ExclusionArray := ['cd', 'lae'];
procedure TMRConsoleApplication.SetShotOptExclusions(
  var ExclusionArray: TMRStrArray);
begin
  SetLength(ExclusionArray, 0);
end;

procedure TMRConsoleApplication.ShowUsage;
var
  s: String;
begin
  s := ChangeFileExt(ExtractFileName(ExpandFileName(ParamStr(0))), '');

  WriteLn(Format('Usage: %s [OPTION]...', [s]));
  WriteLn;
  WriteLn('  -h, --help            display this help and exit');
  WriteLn('      --version         output version information and exit');
end;

procedure TMRConsoleApplication.ShowVersion;
var
  s: String;
  v: String;
  sb: Boolean;
begin
  s := ChangeFileExt(ExtractFileName(ExpandFileName(ParamStr(0))), '');
  sb := True;
  v := GetVersion(sb);

  if (sb) then
  begin
    if (v <> '') then
      WriteLn(Format('%s %s %s', [s, v, CPUBIT]))
    else
      WriteLn(Format('%s %s', [s, CPUBIT]));
  end
  else
  begin
    if (v <> '') then
      WriteLn(Format('%s %s', [s, v]))
    else
      WriteLn(Format('%s', [s]));
  end;
end;

procedure TMRConsoleApplication.Write(args: array of const);
var
  index: Integer;
  s: string;
  sh: shortstring;
begin
  if (StdOutIsRedirected) then
    Write(StdOut, args)
  else
  begin
    for index := 0 to High(args) do
    begin
      if (args[index].VType = vtInteger) then
        System.Write(args[index].VInteger);

      if (args[index].VType = vtBoolean) then
        System.Write(args[index].VBoolean);

      if (args[index].VType = vtChar) then
      begin
        s := args[index].VChar;
        s := ToOem(s);
        System.Write(s);
      end;

  {$ifndef FPUNONE}
      if (args[index].VType = vtExtended) then
        System.Write(args[index].VExtended^);
  {$endif}

      if (args[index].VType = vtString) then
      begin
        sh := args[index].VString^;
        System.Write(sh);
      end;

      if (args[index].VType = vtPointer) then
        System.Write(PtrUInt(args[index].VPointer));

      if (args[index].VType = vtPChar) then
      begin
        s := args[index].VPChar;
        System.Write(s);
      end;

      if (args[index].VType = vtWideChar) then
      begin
        s := args[index].VWideChar;
        System.Write(s);
      end;

      if (args[index].VType = vtPWideChar) then
      begin
        s := args[index].VPWideChar;
        System.Write(s);
      end;

      if (args[index].VType = vtCurrency) then
        System.Write(args[index].VCurrency^);

      if (args[index].VType = vtWideString) then
      begin
        s := widestring(args[index].VWideString);
        System.Write(s);
      end;

      if (args[index].VType = vtAnsiString) then
      begin
        s := ansistring(args[index].VAnsiString);
        System.Write(s);
      end;

      if (args[index].VType = vtInt64) then
        System.Write(args[index].VInt64^);

      if (args[index].VType = vtQWord) then
        System.Write(args[index].VQWord^);
    end; // for i := 0 to High(args)
  end;
end;

procedure TMRConsoleApplication.Write(var aOutput: Text; args: array of const);
var
  index: Integer;
  s: string;
  sh: shortstring;
begin
  for index := 0 to High(args) do
  begin
    if (args[index].VType = vtInteger) then
      System.Write(aOutput, args[index].VInteger);

    if (args[index].VType = vtBoolean) then
      System.Write(aOutput, args[index].VBoolean);

    if (args[index].VType = vtChar) then
    begin
      s := args[index].VChar;
      s := ToOem(s);
      System.Write(aOutput, s);
    end;

{$ifndef FPUNONE}
    if (args[index].VType = vtExtended) then
      System.Write(aOutput, args[index].VExtended^);
{$endif}

    if (args[index].VType = vtString) then
    begin
      sh := args[index].VString^;
      System.Write(aOutput, sh);
    end;

    if (args[index].VType = vtPointer) then
      System.Write(aOutput, PtrUInt(args[index].VPointer));

    if (args[index].VType = vtPChar) then
    begin
      s := args[index].VPChar;
      System.Write(aOutput, s);
    end;

    if (args[index].VType = vtWideChar) then
    begin
      s := args[index].VWideChar;
      System.Write(aOutput, s);
    end;

    if (args[index].VType = vtPWideChar) then
    begin
      s := args[index].VPWideChar;
      System.Write(aOutput, s);
    end;

    if (args[index].VType = vtCurrency) then
      System.Write(aOutput, args[index].VCurrency^);

    if (args[index].VType = vtWideString) then
    begin
      s := widestring(args[index].VWideString);
      System.Write(aOutput, s);
    end;

    if (args[index].VType = vtAnsiString) then
    begin
      s := ansistring(args[index].VAnsiString);
      System.Write(aOutput, s);
    end;

    if (args[index].VType = vtInt64) then
      System.Write(aOutput, args[index].VInt64^);

    if (args[index].VType = vtQWord) then
      System.Write(aOutput, args[index].VQWord^);
  end; // for i := 0 to High(args)
end;

procedure TMRConsoleApplication.Write(var aOutput: Text; args: Int64);
begin
  Write(aOutput, [args]);
end;

procedure TMRConsoleApplication.Write(args: string);
begin
  Write([args]);
end;

procedure TMRConsoleApplication.Write(args: Int64);
begin
  Write([args]);
end;

procedure TMRConsoleApplication.Write(var aOutput: Text; args: string);
begin
  Write(aOutput, [args]);
end;

procedure TMRConsoleApplication.Write;
begin
  Write(['']);
end;

procedure TMRConsoleApplication.WriteLn(args: array of const);
begin
  Write(args);
  Write([LineEnding]);
end;

procedure TMRConsoleApplication.WriteLn(args: string);
begin
  WriteLn([args]);
end;

procedure TMRConsoleApplication.WriteLn(args: Int64);
begin
  WriteLn([args]);
end;

procedure TMRConsoleApplication.WriteLn(var aOutput: Text; args: array of const);
begin
  Write(aOutput, args);
  System.WriteLn(aOutput);
end;

procedure TMRConsoleApplication.WriteLn(var aOutput: Text; args: Int64);
begin
  WriteLn(aOutput, [args]);
end;

procedure TMRConsoleApplication.WriteLn(var aOutput: Text; args: string);
begin
  WriteLn(aOutput, [args]);
end;

procedure TMRConsoleApplication.WriteLn;
begin
  WriteLn(['']);
end;

constructor TMRConsoleApplication.Create;
begin
  CorrectParams;
  RemainingOptions := TStringList.Create;
  Options := TMRLongOpts.Create(Self);
  Options.OnShortOptionFound := @OnShortOptFound;
  Options.OnLongOptionFound := @OnLongOptFound;
  SetLongOpts;
end;

destructor TMRConsoleApplication.Destroy;
begin
  Options.Free;
  RemainingOptions.Free;

  inherited Destroy;
end;

procedure TMRConsoleApplication.WaitUntilKeypressed(showMessage: Boolean; aMessage: string);
begin
  if (not StdOutIsRedirected) then
  begin
    if (showMessage) then
      WriteLn(ToOem(aMessage));

    repeat
    until (KeyPressed);

    ReadKey;
  end; // if (not StdOutIsRedirected)
end;

procedure TMRConsoleApplication.WaitUntilKeypressed(showMessage: Boolean;
  aMessage: string; TextCol: Byte);
begin
  TextColor(TextCol);
  WaitUntilKeypressed(showMessage, aMessage);
end;

procedure TMRConsoleApplication.WaitUntilKeypressed(showMessage: Boolean;
  TextCol: Byte);
begin
  TextColor(TextCol);
  WaitUntilKeypressed(showMessage);
end;

procedure TMRConsoleApplication.WaitUntilKeypressed(TextCol: Byte);
begin
  TextColor(TextCol);
  WaitUntilKeypressed;
end;

procedure TMRConsoleApplication.WaitUntilKeypressed;
begin
  WaitUntilKeypressed(True, 'Press any key...');
end;

{ TMRLongOpt }

procedure TMRLongOpt.SetArgumentType(const AValue: TMRArgumentType);
begin
  if FArgumentType=AValue then exit;
  FArgumentType:=AValue;
end;

procedure TMRLongOpt.SetLongOption(const AValue: string);
begin
  if FLongOption=AValue then exit;
  FLongOption:=AValue;
end;

procedure TMRLongOpt.SetShortOption(const AValue: Char);
begin
  if FShortOption=AValue then exit;
  FShortOption:=AValue;
end;

constructor TMRLongOpt.Create;
begin
  ArgumentType := mratNone;
  LongOption := '';
  ShortOption := #0;
end;

destructor TMRLongOpt.Destroy;
begin
  inherited Destroy;
end;

{ TMRLongOpts }

function TMRLongOpts.GetOptValue(aLongOption: string;
  aShortOption: string): string;
var
  i: Integer;
  par: TMRParam;
  found: TMRParam;
begin
  found := nil;

  for i := 0 to FParams.Count - 1 do
  begin
    par := TMRParam(FParams[i]);

    if (aShortOption <> #0) then
    begin
      if (par.ShortOption = aShortOption) then
      begin
        found := par;
        Break;
      end; // if (par.ShortOption = aShortOption)
    end // if (aShortOption <> #0)
    else
    begin
      if (par.LongOption = aLongOption) then
      begin
        found := par;
        Break;
      end; // if (par.ShortOption = aShortOption)
    end;
  end; // for i := 0 to FParams.Count - 1

  Result := '';

  if (found <> nil) then
  begin
    Result := found.Value;
  end; // if (found <> nil)
end;

function TMRLongOpts.GetOptValue(aOption: string): string;
begin
  if (Length(aOption) = 1) then
    Result := GetOptValue('', aOption[1])
  else
    Result := GetOptValue(aOption, #0);
end;

function TMRLongOpts.LongOptIndex(ShortOpt: Char;
  LongOpts: TMRLongoptions): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to High(LongOpts) do
  begin
    if (LongOpts[i].Value = ShortOpt) then
    begin
      Result := i;
      Exit;
    end; // if (LongOpts[i].Value = ShortOpt)
  end; // for i := 0 to High(LongOpts)
end;

function TMRLongOpts.ShortOptAllowed(ShortOptsFound: string;
  newShortOpt: Char): Char;
var
  c: Char;
  i: Integer;
  j: Integer;
begin
  Result := #0;

  for i := 0 to High(FShortOptExcl) do
  begin
    c := #0;

    for j := 1 to Length(ShortOptsFound) do
    begin
      if (Pos(ShortOptsFound[j], FShortOptExcl[i]) > 0) then
      begin
        c := ShortOptsFound[j];
        Break;
      end; // if (Pos(ShortOptsFound[j], FShortOptExcl[i]) > 0)
    end; // for j := 1 to Length(ShortOptsFound)

    j := Pos(newShortOpt, FShortOptExcl[i]);

    if ((c <> #0) and (j > 0)) then
    begin
      Result := c;
      Exit;
    end; // if (c and (Pos(newShortOpt, FShortOptExcl[i]) > 0))
  end; // for i := 0 to High(FShortOptExcl)
end;

function TMRLongOpts.LongOptAllowed(LongOptsFound: TStringList;
  newLongOpt: string): string;
var
  c: String;
  i: Integer;
  j: Integer;
  s: String;
begin
  Result := '';

  for i := 0 to High(FLongOptExcl) do
  begin
    c := '';
    s := '';

    for j := 0 to High(FLongOptExcl[i]) do
    begin
      if (LongOptsFound.IndexOf(FLongOptExcl[i][j]) > -1) then
        c := FLongOptExcl[i][j];

      if (FLongOptExcl[i][j] = newLongOpt) then
        s := newLongOpt;

      if ((s <> '') and (c <> '')) then
        Break;
    end; // for j := 0 to High(FLongOptExcl[i])

    if ((s <> '') and (c <> '')) then
    begin
      Result := c;
      Exit;
    end;
  end; // for i := 0 to High(FLongOptExcl)
end;

procedure TMRLongOpts.AddShortOption(aShortOpt: Char;
  aArgumentType: TMRArgumentType; IgnoreExistingShortOpt: Boolean);
var
  s: string;
begin
  if (aShortOpt <> #0) then
  begin
    s := aShortOpt;
    SetLength(s, Integer(aArgumentType) + 1);

    if (aArgumentType > mratNone) then
      System.FillChar(s[2], Integer(aArgumentType), ':');

    if (FShortOpts.IndexOf(s) = -1) then
      FShortOpts.Add(s)
    else
    begin
      if (not IgnoreExistingShortOpt) then
        raise EMRShortOptionAlreadyExists.Create(aShortOpt);
    end;
  end; // if (aShortOpt <> #0)
end;

procedure TMRLongOpts.SetParamExclusions;
begin
  FParent.SetLongOptExclusions(FLongOptExcl);
  FParent.SetShotOptExclusions(FShortOptExcl);
end;

constructor TMRLongOpts.Create(aParent: TMRConsoleApplication);
begin
  FShortOpts := TStringList.Create;
  FOptions := TStringList.Create;

  FLongOptExcl := nil;
  FOnOptionFound := nil;
  FOnShortOptionFound := nil;
  FParams := TList.Create;
  FParent := aParent;
  FShortOptExcl := nil;
  FShortOpts.CaseSensitive := True;
end;

destructor TMRLongOpts.Destroy;
begin
  Self.Reset;
  FOptions.Free;
  FShortOpts.Free;
  FParams.Free;

  inherited Destroy;
end;

function TMRLongOpts.GetOptions: TMRLongoptions;
var
  i: Integer;
  opt: TMRLongOpt;
begin
  SetLength(Result, FOptions.Count + 1);

  for i := 0 to FOptions.Count - 1 do
  begin
    opt := TMRLongOpt(FOptions.Objects[i]);
    Result[i].Name := opt.LongOption;
    Result[i].Has_arg := Integer(opt.ArgumentType);
    Result[i].Flag := nil;
    Result[i].Value := opt.ShortOption;
  end; // for i := 0 to FOptions.Count - 1

  i := High(Result);
  Result[i].Name := '';
  Result[i].Has_arg := 0;
  Result[i].Flag := nil;
end;

procedure TMRLongOpts.AddOption(aLongOpt: string);
begin
  AddOption(aLongOpt, #0, mratNone);
end;

procedure TMRLongOpts.AddOption(aLongOpt: string; aShortOpt: Char);
begin
  AddOption(aLongOpt, aShortOpt, mratNone);
end;

procedure TMRLongOpts.AddOption(aLongOpt: string;
  aArgumentType: TMRArgumentType);
begin
  AddOption(aLongOpt, #0, aArgumentType);
end;

procedure TMRLongOpts.AddOption(aLongOpt: string; aShortOpt: Char;
  aArgumentType: TMRArgumentType);
begin
  AddOption(aLongOpt, aShortOpt, aArgumentType, False);
end;

procedure TMRLongOpts.AddOption(aLongOpt: string; aShortOpt: Char;
  aArgumentType: TMRArgumentType; IgnoreExistingShortOpt: Boolean);
var
  sloOpt: TMRLongOpt;
begin
  if (FOptions.IndexOf(aLongOpt) > -1) then
  begin
    raise EMROptionAlreadyExists.Create(aLongOpt);
    Exit;
  end; // if (FOptions.IndexOf(aLongOpt) > -1)

  sloOpt := TMRLongOpt.Create;
  sloOpt.LongOption := aLongOpt;
  sloOpt.ShortOption := aShortOpt;
  sloOpt.ArgumentType := aArgumentType;
  AddShortOption(aShortOpt, aArgumentType, IgnoreExistingShortOpt);
  FOptions.AddObject(aLongOpt, sloOpt);
end;

procedure TMRLongOpts.AddShortOption(aShortOpt: Char);
begin
  AddShortOption(aShortOpt, mratNone);
end;

procedure TMRLongOpts.AddShortOption(aShortOpt: Char;
  aArgumentType: TMRArgumentType);
begin
  AddShortOption(aShortOpt, aArgumentType, False);
end;

procedure TMRLongOpts.ChangeOption(aLongOpt: string; aShortOpt: Char;
  aArgumentType: TMRArgumentType);
var
  opt: TMRLongOpt;
  i: Integer;
begin
  i := FOptions.IndexOf(aLongOpt);

  if (i > -1) then
  begin
    opt := TMRLongOpt(FOptions.Objects[i]);
    DeleteShortOption(opt.ShortOption);
    opt.ShortOption := aShortOpt;

    if (opt.ShortOption = '') then
    begin
      opt.ShortOption := #0;
    end; // if (opt.ShortOption = '')

    opt.ArgumentType := aArgumentType;

    if (opt.ShortOption <> #0) then
    begin
      AddShortOption(aShortOpt, aArgumentType);
    end;
  end // if (i > -1)
  else
  begin
    opt := TMRLongOpt.Create;
    opt.LongOption := aLongOpt;
    opt.ShortOption := aShortOpt;

    if (opt.ShortOption = '') then
    begin
      opt.ShortOption := #0;
    end; // if (opt.ShortOption = '')

    opt.ArgumentType := aArgumentType;
    FOptions.AddObject(aLongOpt, opt);
    AddShortOption(aShortOpt, aArgumentType);
  end; // else (if (i > -1))
end;

procedure TMRLongOpts.DeleteOption(aLongOpt: string);
var
  opt: TMRLongOpt;
  i: Integer;
begin
  i := FOptions.IndexOf(aLongOpt);

  if (i > -1) then
  begin
    opt := TMRLongOpt(FOptions.Objects[i]);
    DeleteShortOption(opt.ShortOption);
    opt.Free;
    FOptions.Delete(i);
  end;
end;

procedure TMRLongOpts.DeleteShortOption(aShortOpt: Char);
var
  i: Integer;
  opt: string;
begin
  for i := 0 to FShortOpts.Count - 1 do
  begin
    opt := FShortOpts[i];

    if (opt[1] = aShortOpt) then
    begin
      FShortOpts.Delete(i);
      Exit;
    end; // if (opt[1] = aShortOpt)
  end; // for i := 0 to FShortOpts.Count - 1
end;

procedure TMRLongOpts.ParseOptions;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  ParseOptions(sl);
  sl.Free;
end;

procedure TMRLongOpts.ParseOptions(var aRemainingOptions: TStringList);
var
  c: Char;
  i: Integer;
  j: Integer;
  lindex: Integer;
  optindex: Longint;
  opts: TMRLongoptions;
  par: TMRParam;
  s: string;
  sopts: string;
  sof: string;
  lof: TStringList;
  ch: Char;
begin
  c := #0;
  s := '';
  sopts := '';
  opts := GetOptions;
  aRemainingOptions.Clear;
  lof := TStringList.Create;
  lof.Sorted := True;
  lof.Duplicates := dupIgnore;
  sof := '';

  for i := 0 to FShortOpts.Count - 1 do
  begin
    sopts := sopts + FShortOpts[i];
  end; // for i := 0 to FShortOpts.Count - 1

  repeat
    optindex := -1;
    c := GetLongOpts(sopts, @opts[0], optindex);

    if (c <> #255) then
    begin
      if (optindex > -1) then
      begin
        if (opts[optindex - 1].Value <> #0) then
        begin
          ch := ShortOptAllowed(sof, opts[optindex - 1].Value);

          if (ch <> #0) then
            FParent.OnUnallowedShortoptFound(ch, opts[optindex - 1].Value);

          if (Pos(opts[optindex - 1].Value, sof) = 0) then
            sof := sof + opts[optindex - 1].Value;

          lof.Add(opts[optindex - 1].Name);

          if (FOnShortOptionFound <> nil) then
          begin
            FOnShortOptionFound(c, OptArg);
          end; // if (FOnOptionFound <> nil)
        end // if (opts[optindex - 1].Value <> #0)
        else
        begin
          s := LongOptAllowed(lof, opts[optindex - 1].Name);

          if (s <> '') then
            FParent.OnUnallowedLongoptFound(s, opts[optindex - 1].Name);

          lof.Add(opts[optindex - 1].Name);

          if (FOnOptionFound <> nil) then
          begin
            FOnOptionFound(opts[optindex - 1].Name, OptArg);
          end; // if (FOnOptionFound <> nil)
        end; // else (if (opts[optindex - 1].Value <> #0))

        par := TMRParam.Create;
        par.LongOption := opts[optindex - 1].Name;
        par.ShortOption := opts[optindex - 1].Value;
        par.Value := OptArg;
        FParams.Add(par);
      end
      else
      begin
        lindex := LongOptIndex(c, opts);
        ch := ShortOptAllowed(sof, c);

        if (ch <> #0) then
        begin
          FParent.OnUnallowedShortoptFound(ch, c);
        end // if (Pos(opts[optindex - 1].Value, sof) > 0)
        else
        begin
          if (lindex > -1) then
          begin
            s := LongOptAllowed(lof, opts[lindex].Name);

            if (s <> '') then
              FParent.OnUnallowedLongoptFound(s, opts[lindex].Name);
          end; // if (lindex > -1)
        end;

        if (Pos(c, sof) = 0) then
          sof := sof + c;

        if (FOnShortOptionFound <> nil) then
        begin
          FOnShortOptionFound(c, OptArg);
        end; // if (FOnOptionFound <> nil)

        par := TMRParam.Create;
        s := '';

        if (lindex > -1) then
        begin
          s := opts[lindex].Name;
          lof.Add(s);
        end;

        par.LongOption := s;
        par.ShortOption := c;
        par.Value := OptArg;
        FParams.Add(par);
      end;
    end; // if (FOnOptionFound <> nil)
  until (c = EndOfOptions);

  while (OptInd <= Paramcount) do
  begin
    aRemainingOptions.Add(ParamStr(OptInd));
    OptInd := OptInd + 1;
  end; // while (OptInd <= Paramcount)

  lof.Free;
end;

procedure TMRLongOpts.Reset;
var
  i: Integer;
begin
  for i := 0 to FOptions.Count - 1 do
  begin
    TMRLongOpt(FOptions.Objects[i]).Free;
  end; // for i := 0 to FOptions.Count - 1

  FOptions.Clear;
  FShortOpts.Clear;

  for i := 0 to FParams.Count - 1 do
  begin
    TMRParam(FParams[i]).Free;
  end; // for i := 0 to FParams.Count - 1

  FParams.Clear;
end;

{ TMRParam }

procedure TMRParam.SetLongOption(const AValue: string);
begin
  if FLongOption=AValue then exit;
  FLongOption:=AValue;
end;

procedure TMRParam.SetShortOption(const AValue: Char);
begin
  if FShortOption=AValue then exit;
  FShortOption:=AValue;
end;

procedure TMRParam.SetValue(const AValue: string);
begin
  if FValue=AValue then exit;
  FValue:=AValue;
end;

constructor TMRParam.Create;
begin

end;

destructor TMRParam.Destroy;
begin
  inherited Destroy;
end;

function TMRParam.OptionHasValue: Boolean;
begin
  Result := (FValue <> '');
end;

function TMRParam.ValueAsBoolean: Boolean;
var
  b: Boolean;
  i: Integer;
  res: Boolean;
  s: string;
begin
  s := FShortOption;

  if (s = #0) then
  begin
    s := FLongOption;
  end; // if (s = #0)

  if (not OptionHasValue) then
  begin
    raise EMROptionHasNoValue(Format('Option ''%s'' has no value', [s]));
    exit;
  end; // if (not OptionHasValue)

  if (UpperCase(FValue) = 'TRUE') then
  begin
    res := True;
    b := True;
  end // if (UpperCase(FValue) = 'TRUE')
  else
  if (UpperCase(FValue) = 'FALSE') then
  begin
    res := False;
    b := True;
  end // if (UpperCase(FValue) = 'FALSE')
  else
  begin
    try
      i := StrToInt(FValue);
      res := Boolean(i);
      b := True;
    except
      b := False;
    end;
  end;

  if (b) then
  begin
    Result := res;
  end // if (b)
  else
    raise EMROptionIsNotThatType(Format('Value of option ''%s'' is not a boolean', [s]));
end;

function TMRParam.ValueAsExtended: Extended;
begin
  Result := StrToFloat(FValue);
end;

function TMRParam.ValueAsInt64: Int64;
begin
  Result := StrToInt64(FValue);
end;

function TMRParam.ValueAsInteger: Longint;
var
  b: Boolean;
  i: Integer;
  s: string;
begin
  s := FShortOption;

  if (s = #0) then
  begin
    s := FLongOption;
  end; // if (s = #0)

  try
    if (not OptionHasValue) then
    begin
      raise EMROptionHasNoValue(Format('Option ''%s'' has no value', [s]));
      exit;
    end; // if (not OptionHasValue)

    i := StrToInt(FValue);
    b := True;
  except
    b := False;
  end;

  if (b) then
  begin
    Result := i;
  end // if (b)
  else
    raise EMROptionIsNotThatType(Format('Value of option ''%s'' is not an integer', [s]));
end;

end.

