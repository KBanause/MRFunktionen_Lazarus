{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit virtualtreeslcl;

interface

uses
  VirtualTrees, VirtualDrawTree, VirtualStringTree, VTHeaderPopup, VTRegister, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('VTRegister', @VTRegister.Register);
end;

initialization
  RegisterPackage('virtualtreeslcl', @Register);
end.
