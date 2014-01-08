uses
  windows, winsock, ActiveX;

type
  NET_API_STATUS    = DWORD;
  TStringArray      = array of WideString;

  USER_INFO_1 = record
    usri1_name: LPWSTR;
    usri1_password: LPWSTR;
    usri1_password_age: DWORD;
    usri1_priv: DWORD;
    usri1_home_dir: LPWSTR;
    usri1_comment: LPWSTR;
    usri1_flags: DWORD;
    usri1_script_path: LPWSTR;
  end;
  lpUSER_INFO_1 = ^USER_INFO_1;
  PUserInfo1 = lpUSER_INFO_1;

  TUserInfo3 = record
    usri3_name: LPWSTR;
    usri3_password: LPWSTR;
    usri3_password_age: DWORD;
    usri3_priv: DWORD;
    usri3_home_dir: LPWSTR;
    usri3_comment: LPWSTR;
    usri3_flags: DWORD;
    usri3_script_path: LPWSTR;
    usri3_auth_flags: DWORD;
    usri3_full_name: LPWSTR;
    usri3_usr_comment: LPWSTR;
    usri3_parms: LPWSTR;
    usri3_workstations: LPWSTR;
    usri3_last_logon: DWORD;
    usri3_last_logoff: DWORD;
    usri3_acct_expires: DWORD;
    usri3_max_storage: DWORD;
    usri3_units_per_week: DWORD;
    usri3_logon_hours: PBYTE;
    usri3_bad_pw_count: DWORD;
    usri3_num_logons: DWORD;
    usri3_logon_server: LPWSTR;
    usri3_country_code: DWORD;
    usri3_code_page: DWORD;
    usri3_user_id: DWORD;
    usri3_primary_group_id: DWORD;
    usri3_profile: LPWSTR;
    usri3_home_dir_drive: LPWSTR;
    usri3_password_expired: DWORD;
  end;
  PUserInfo3 = ^TUserInfo3;

const
  NERR_Success      = 0;
  MAX_PREFERRED_LENGTH = DWORD(-1);
  netapi32lib       = 'netapi32.dll';

  UF_SCRIPT                          = $0001;
  UF_ACCOUNTDISABLE                  = $0002;
  UF_HOMEDIR_REQUIRED                = $0008;
  UF_LOCKOUT                         = $0010;
  UF_PASSWD_NOTREQD                  = $0020;
  UF_PASSWD_CANT_CHANGE              = $0040;
  UF_ENCRYPTED_TEXT_PASSWORD_ALLOWED = $0080;
  UF_DONT_EXPIRE_PASSWD = $10000;

function NetUserEnum(servername: LPCWSTR; level: DWORD; filter: DWORD; var
  buf: Pointer; prefmaxlen: DWORD; var entriesred: DWORD; var totalentries:
  DWORD; resumehandle: PDWORD): NET_API_STATUS; stdcall; external netapi32lib name 'NetUserEnum';

function NetUserGetInfo(servername: LPCWSTR; username: LPCWSTR; level: DWORD;
  var bufptr: Pointer): NET_API_STATUS; stdcall; external netapi32lib name 'NetUserGetInfo';

function NetApiBufferFree(Buffer: LPVOID): NET_API_STATUS; stdcall; external netapi32lib name 'NetApiBufferFree';

function GetUserInfo(const Server, User: WideString; var ret: TUserInfo3): NET_API_STATUS;
var
  ui3               : PUserInfo3;
  NetError          : DWORD;
begin
  ui3 := nil;
  NetError := 0;
  if User <> '' then
  begin
    try
      NetError := NetUserGetInfo(PWideChar(Server), PWideChar(User), 3,
        Pointer(ui3));
      if NetError = NERR_SUCCESS then
      begin
        with ret do
        begin
          usri3_flags := 0;
          usri3_name := SysAllocString(ui3^.usri3_name);
          usri3_full_name := SysAllocString(ui3^.usri3_full_name);
          usri3_comment := SysAllocString(ui3^.usri3_comment);
          usri3_home_dir := SysAllocString(ui3^.usri3_home_dir);
          usri3_script_path := SysAllocString(ui3^.usri3_script_path);

          usri3_password_age := ui3^.usri3_password_age;
          usri3_last_logon := ui3^.usri3_last_logon;
          usri3_num_logons := ui3^.usri3_num_logons;
          usri3_acct_expires := ui3^.usri3_acct_expires;
          usri3_max_storage := ui3^.usri3_max_storage;
          usri3_user_id := ui3^.usri3_user_id;
          usri3_bad_pw_count := ui3^.usri3_bad_pw_count;
          usri3_password_expired := ui3^.usri3_password_expired;

          if (DWORD(ui3^.usri3_flags and UF_PASSWD_CANT_CHANGE) =
            UF_PASSWD_CANT_CHANGE) then
            usri3_flags := usri3_flags or UF_PASSWD_CANT_CHANGE;
          if (DWORD(ui3^.usri3_flags and UF_DONT_EXPIRE_PASSWD) =
            UF_DONT_EXPIRE_PASSWD) then
            usri3_flags := usri3_flags or UF_DONT_EXPIRE_PASSWD;
          if (DWORD(ui3^.usri3_flags and UF_ACCOUNTDISABLE) =
            UF_ACCOUNTDISABLE) then
            usri3_flags := usri3_flags or UF_ACCOUNTDISABLE;
        end;
      end;
    finally
      NetApiBufferFree(ui3);
    end;
  end;
  result := NetError;
end;

function EnumUsers(const Server: WideString; filter: DWORD): TStringArray;
var
  ui1, pWork        : Pointer;
  EntriesRead, EntriesTotal: DWORD;
  NetError          : DWORD;
  Loop              : Integer;
begin
  ui1 := nil;
  pWork := nil;
  if Server <> '' then
  begin
    try
      NetError := NetUserEnum(PWideChar(Server), 0, filter, ui1,
        MAX_PREFERRED_LENGTH, EntriesRead, EntriesTotal, nil);
      if (NetError = NERR_SUCCESS) and (EntriesRead > 0) then
      begin
        pWork := ui1;
        SetLength(Result, EntriesRead);

        for Loop := 0 to EntriesRead - 1 do
        begin
          Result[Loop] := PUserInfo1(ui1)^.usri1_name;
          Inc(PtrUInt(ui1), sizeof(Pointer));
        end;
      end;
    finally
      NetApiBufferFree(pWork);
    end;
  end;
end;

procedure GetUserNamesOnSystem(List: TStrings);
begin
  GetUserNamesOnSystem(List, False);
end;

procedure GetUserNamesOnSystem(List: TStrings; WithDisabledUsers: Boolean);
var
  buf: array[0..63]of Char;
  GInitData: TWSAData;
  i: LongInt;
  s: string;
  sa: TStringArray;
  sl: TStringList;
  hname: String;
  ui3: TUserInfo3;
begin
  if (List is TSTringList) then
    sl := TStringList(List)
  else
    sl := TSTringList.Create;

  sl.Clear;
  WSAStartup($101, GInitData);
  i := gethostname(buf, SizeOf(buf));

  if (i <> 0) then
  begin
    i := WSAGetLastError;
    ShowMessage(SysErrorMessage(i));
    WSACleanup;
    Exit;
  end; // if (i <> 0)

  s := buf;
  hname := s;
  sa := EnumUsers(hname, FILTER_NORMAL_ACCOUNT);

  for i := 0 to High(sa) do
  begin
    sl.Add(sa[i]);
  end; // for

  WSACleanup;
  sl.Sort;

  if (not WithDisabledUsers) then
  begin
    for i := sl.Count - 1 downto 0 do
    begin
      GetUserInfo(hname, sl[i], ui3);

      if (ui3.usri3_flags and UF_ACCOUNTDISABLE = UF_ACCOUNTDISABLE) then
      begin
        sl.Delete(i);
      end; // if (ui3.usri3_flags and UF_ACCOUNTDISABLE = UF_ACCOUNTDISABLE)
    end; // for i := sl.Count - 1 downto 0
  end; // if (not WithDisabledUsers)

  if (sl <> List) then
  begin
    List.Assign(sl);
    sl.Free;
  end; // if (sl <> List)
end;

function GetProcessID(Exename: string): DWORD;
var
  hProcSnap                   : THandle;
  pe32                        : TProcessEntry32;
begin
  result := 0;
  hProcSnap := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
  if hProcSnap <> INVALID_HANDLE_VALUE then
  begin
    pe32.dwSize := SizeOf(ProcessEntry32);
    if Process32First(hProcSnap, pe32) = true then
    begin
      while Process32Next(hProcSnap, pe32) = true do
      begin
        if pos(Exename, pe32.szExeFile) <> 0 then
          result := pe32.th32ProcessID;
      end;
    end;
    CloseHandle(hProcSnap);
  end;
end;

function GetProcessHandleFromID(ID: DWORD): THandle;
begin
  result := OpenProcess(SYNCHRONIZE, False, ID);
  CloseHandle(result);
end;

function ProgrammIsRunning(ExeFile: string): Boolean;
begin
  Result := (GetProcessHandleFromID(GetProcessID(ExeFile)) > 0);
end;
