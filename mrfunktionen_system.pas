unit MRFunktionen_System; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, MRFunktionen;

{$IFDEF WINDOWS}
function GetProcessID(Exename: string): DWORD;
function GetProcessHandleFromID(ID: DWORD): THandle;
function ProgrammIsRunning(ExeFile: string): Boolean;
{$ENDIF}

procedure GetUserNamesOnSystem(List: TStrings);
procedure GetUserNamesOnSystem(List: TStrings; WithDisabledUsers: Boolean);

implementation

{$IFDEF WINDOWS}
  {$I mrfunktionen_system.win32.inc}

{$ENDIF WINDOWS}
{$IFDEF UNIX}
  {$I mrfunktionen_system.unix.inc}
{$ENDIF UNIX}

end.

