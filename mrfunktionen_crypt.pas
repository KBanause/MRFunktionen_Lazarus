unit MRFunktionen_Crypt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function AESDecode(AText: string; Key: string): string;
function AESDecode(AText: string; Key: string; AsBase64: Boolean): string;
function AESEncode(AText: string; Key: string): string;
function AESEncode(AText: string; Key: string; AsBase64: Boolean): string;

implementation

uses
  DCPrijndael, DCPsha256;

function AESDecode(AText: string; Key: string): string;
begin
  Result := AESDecode(AText, Key, False);
end;

function AESDecode(AText: string; Key: string; AsBase64: Boolean): string;
var
  dcp: TDCP_rijndael;
  enc: TMemoryStream;
  plain: TMemoryStream;
  s: String;
begin
  dcp := TDCP_rijndael.Create(nil);
  dcp.InitStr(key, TDCP_sha256);
  plain := TMemoryStream.Create;
  enc := TMemoryStream.Create;
  enc.Write(AText[1], Length(AText));
  s := '';

  if (AsBase64) then
  begin
    enc.Position := 0;
    DecodeBase64Stream(enc, plain);
    enc.Position := 0;
    enc.Size := 0;
    plain.Position := 0;
    enc.CopyFrom(plain, plain.Size);
    plain.Position := 0;
    plain.Size := 0;
  end; // if (AsBase64)

  enc.Position := 0;
  dcp.DecryptStream(enc, plain, enc.Size);
  dcp.Burn;
  dcp.Free;
  enc.Free;
  plain.Position := 0;
  SetLength(s, plain.Size);
  plain.Read(s[1], plain.Size);
  plain.Free;
  Result := s;
end;

function AESEncode(AText: string; Key: string): string;
begin
  Result := AESEncode(AText, Key, False);
end;

function AESEncode(AText: string; Key: string; AsBase64: Boolean): string;
var
  dcp: TDCP_rijndael;
  enc: TMemoryStream;
  plain: TMemoryStream;
  s: String;
begin
  dcp := TDCP_rijndael.Create(nil);
  dcp.InitStr(Key, TDCP_sha256);
  plain := TMemoryStream.Create;
  enc := TMemoryStream.Create;
  plain.Write(AText[1], Length(AText));
  plain.Position := 0;
  dcp.EncryptStream(plain, enc, plain.Size);
  dcp.Burn;
  dcp.Free;
  plain.Free;
  enc.Position := 0;
  plain := TMemoryStream.Create;

  if (AsBase64) then
    EncodeBase64Stream(enc, plain)
  else
    plain.CopyFrom(enc, enc.Size);

  s := '';
  SetLength(s, plain.Size);
  plain.Position := 0;
  plain.Read(s[1], plain.Size);
  enc.Free;
  plain.Free;
  Result := s;
end;

end.

