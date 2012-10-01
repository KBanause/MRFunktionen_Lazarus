unit MRFunktionen_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, getopts;

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
    FShortOptExcl: TMRStrArray;
    FShortOpts: TStringList;

    function LongOptIndex(ShortOpt: Char; LongOpts: TMRLongoptions): Integer;
    function ShortOptAllowed(ShortOptsFound: string; newShortOpt: Char): Char;
    function LongOptAllowed(LongOptsFound: TStringList; newLongOpt: string): string;
    procedure AddShortOption(aShortOpt: Char; aArgumentType: TMRArgumentType; IgnoreExistingShortOpt: Boolean); overload;
    //procedure SetParamExclusions;
  protected
    //procedure OnLongOptFound(Option: string; aValue: string); virtual;
    //procedure OnShortOptFound(Option: Char; aValue: string); virtual;
    procedure OnUnallowedLongoptFound(Param1, Param2: string); virtual;
    procedure OnUnallowedShortoptFound(Param1, Param2: Char); virtual;
    //procedure SetLongOptExclusions(var ExclusionArray: TMRExclusionArray); virtual;
    //procedure SetShotOptExclusions(var ExclusionArray: TMRStrArray); virtual;
  public
    constructor Create;
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

//procedure TMRLongOpts.SetParamExclusions;
//begin
  //SetLongOptExclusions(FLongOptExcl);
  //SetShotOptExclusions(FShortOptExcl);
//end;

//procedure TMRLongOpts.OnLongOptFound(Option: string; aValue: string);
//begin
//end;

//procedure TMRLongOpts.OnShortOptFound(Option: Char; aValue: string);
//begin

//end;

procedure TMRLongOpts.OnUnallowedLongoptFound(Param1, Param2: string);
begin

end;

procedure TMRLongOpts.OnUnallowedShortoptFound(Param1, Param2: Char);
begin

end;

//procedure TMRLongOpts.SetLongOptExclusions(var ExclusionArray: TMRExclusionArray
  //);
//var
  //i: Integer;
//begin
  //for i := 0 to High(ExclusionArray) do
  //begin
    //SetLength(ExclusionArray[i], 0);
  //end; // for i := 0 to High(ExclusionArray)

  //SetLength(ExclusionArray, 0);
//end;

//procedure TMRLongOpts.SetShotOptExclusions(var ExclusionArray: TMRStrArray);
//begin
  //SetLength(ExclusionArray, 0);
//end;

constructor TMRLongOpts.Create;
begin
  FShortOpts := TStringList.Create;
  FOptions := TStringList.Create;

  FLongOptExcl := nil;
  FOnOptionFound := nil;
  FOnShortOptionFound := nil;
  FParams := TList.Create;
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
            OnUnallowedShortoptFound(ch, opts[optindex - 1].Value);

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
            OnUnallowedLongoptFound(s, opts[optindex - 1].Name);

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
          OnUnallowedShortoptFound(ch, c);
        end // if (Pos(opts[optindex - 1].Value, sof) > 0)
        else
        begin
          if (lindex > -1) then
          begin
            s := LongOptAllowed(lof, opts[lindex].Name);

            if (s <> '') then
              OnUnallowedLongoptFound(s, opts[lindex].Name);
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

