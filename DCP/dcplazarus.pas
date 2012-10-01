{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DCPlazarus; 

interface

uses
    DCPbase64, DCPblockciphers, DCPconst, DCPcrypt2, DCPreg, DCPblowfish, 
  DCPcast128, DCPcast256, DCPdes, DCPgost, DCPice, DCPidea, DCPmars, 
  DCPmisty1, DCPrc2, DCPrc4, DCPrc5, DCPrc6, DCPrijndael, DCPserpent, DCPtea, 
  DCPtwofish, DCPhaval, DCPmd4, DCPmd5, DCPripemd128, DCPripemd160, DCPsha1, 
  DCPsha256, DCPsha512, DCPtiger, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DCPreg', @DCPreg.Register); 
end; 

initialization
  RegisterPackage('DCPlazarus', @Register); 
end.
