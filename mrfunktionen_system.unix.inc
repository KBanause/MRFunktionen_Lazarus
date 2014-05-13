uses
  Unix, BaseUnix;

procedure GetUserNamesOnSystem(List: TStrings);
begin
  GetUserNamesOnSystem(List, False);
end;

procedure GetUserNamesOnSystem(List: TStrings; WithDisabledUsers: Boolean);
var
  sl: TStringList;
  sa: TStrArray;
  sb: TStrArray;
  i: Integer;
begin
  if (List is TStringList) then
    sl := TStringList(List)
  else
    sl := TStringList.Create;

  sl.Clear;
  sl.LoadFromFile('/etc/passwd');

  for i := sl.Count - 1 downto 0 do
  begin
    Explode(sl[i], ':', sa);
    Explode(sa[High(sa)], '/', sb);

    if (Pos('sh', sb[High(sb)]) > 0) then
      sl[i] := sa[0]
    else
      sl.Delete(i);
  end; //  i := sl.Count - 1 downto 0

  sl.Sort;

  if (sl <> List) then
  begin
    List.Assign(sl);
    sl.Free;
  end; // if (sl <> List)
end;
