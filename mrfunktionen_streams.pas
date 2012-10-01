unit MRFunktionen_Streams;

{$mode objfpc}{$H+}

interface

uses
  Classes, base64;

procedure DecodeAESStream(SrcStream, DstStream: TStream; Key: string);
procedure DecodeAESStream(SrcStream, DstStream: TStream; Key: string; AsBase64: Boolean);
procedure DecodeBase64Stream(B64Stream: TStream; DestStream: TStream);
procedure DecodeBase64Stream(B64Stream: TStream; DestStream: TStream; AMode: TBase64DecodingMode);
procedure EncodeAESStream(SrcStream, DstStream: TStream; Key: string);
procedure EncodeAESStream(SrcStream, DstStream: TStream; Key: string; AsBase64: Boolean);
procedure EncodeBase64Stream(SrcStream: TStream; B64Stream: TStream);
procedure GUnZipStream(SrcStream, DstStream: TStream);
procedure GZipStream(SrcStream, DstStream: TStream);

implementation

uses
  SysUtils, zstream, DCPrijndael, DCPsha256;

procedure DecodeAESStream(SrcStream, DstStream: TStream; Key: string);
begin
  DecodeAESStream(SrcStream, DstStream, Key, False);
end;

procedure DecodeAESStream(SrcStream, DstStream: TStream; Key: string;
  AsBase64: Boolean);
var
  dcp: TDCP_rijndael;
  ms: TMemoryStream;
begin
  dcp := TDCP_rijndael.Create(nil);
  dcp.InitStr(Key, TDCP_sha256);

  if (AsBase64) then
  begin
    ms := TMemoryStream.Create;
    DecodeBase64Stream(SrcStream, ms);
    ms.Position := 0;
    dcp.DecryptStream(ms, DstStream, ms.Size);
    ms.Free;
  end // if (AsBase64)
  else
    dcp.DecryptStream(SrcStream, DstStream, SrcStream.Size - SrcStream.Position);

  dcp.Burn;
  dcp.Free;
end;

procedure DecodeBase64Stream(B64Stream: TStream; DestStream: TStream);
begin
  DecodeBase64Stream(B64Stream, DestStream, bdmMIME);
end;

procedure DecodeBase64Stream(B64Stream: TStream; DestStream: TStream;
  AMode: TBase64DecodingMode);
var
  b64dec: TBase64DecodingStream;
  buffer: array[0..2047]of Byte;
  read: LongInt;
begin
  b64dec := TBase64DecodingStream.Create(B64Stream, AMode);
  read := b64dec.Read(buffer, SizeOf(buffer));

  while (read > 0) do
  begin
    DestStream.Write(buffer, read);
    read := b64dec.Read(buffer, SizeOf(buffer));
  end; // while (read > 0)

  b64dec.Free;
end;

procedure EncodeAESStream(SrcStream, DstStream: TStream; Key: string);
begin
  EncodeAESStream(SrcStream, DstStream, Key, False);
end;

procedure EncodeAESStream(SrcStream, DstStream: TStream; Key: string;
  AsBase64: Boolean);
var
  dcp: TDCP_rijndael;
  ms: TMemoryStream;
begin
  dcp := TDCP_rijndael.Create(nil);
  dcp.InitStr(Key, TDCP_sha256);

  if (AsBase64) then
  begin
    ms := TMemoryStream.Create;
    dcp.EncryptStream(SrcStream, ms, SrcStream.Size - SrcStream.Position);
    ms.Position := 0;
    EncodeBase64Stream(ms, DstStream);
    ms.Free;
  end // if (AsBase64)
  else
    dcp.EncryptStream(SrcStream, DstStream, SrcStream.Size - SrcStream.Position);

  dcp.Burn;
  dcp.Free;
end;

procedure EncodeBase64Stream(SrcStream: TStream; B64Stream: TStream);
var
  b64enc: TBase64EncodingStream;
  buffer: array[0..2047]of Byte;
  read: LongInt;
begin
  b64enc := TBase64EncodingStream.Create(B64Stream);
  read := SrcStream.Read(buffer, SizeOf(buffer));

  while (read > 0) do
  begin
    b64enc.Write(buffer, read);
    read := SrcStream.Read(buffer, SizeOf(buffer));
  end; // while (read > 0)

  b64enc.Free;
end;

procedure GUnZipStream(SrcStream, DstStream: TStream);
var
  buffer: array[0..2047]of Byte;
  read: LongInt;
  z: Tdecompressionstream;
begin
  z := Tdecompressionstream.create(SrcStream, False);
  read := z.read(buffer, SizeOf(buffer));

  while (read > 0) do
  begin
    DstStream.Write(buffer, read);
    read := z.read(buffer, SizeOf(buffer));
  end; // while (read > 0)

  z.Free;
end;

procedure GZipStream(SrcStream, DstStream: TStream);
var
  z: Tcompressionstream;
begin
  z := Tcompressionstream.create(clmax, DstStream, False);
  z.CopyFrom(SrcStream, SrcStream.Size - SrcStream.Position);
  z.Free;
end;

end.

