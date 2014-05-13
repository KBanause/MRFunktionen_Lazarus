unit MRFunktionen_CRT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, crt, windows;

//procedure ConsoleEnd;
function StdOutRedirected: Boolean;

implementation

//procedure ConsoleEnd;
//begin
  //if WhereX > 1 then
    //WriteLn;

  //TextColor(White);
  //TextBackground(Black);
  //Write('Press any key');

  //repeat
  //until (KeyPressed);

  //ReadKey;
  //TextColor(LightGray);
  //TextBackground(Black);
//end;

function StdOutRedirected: Boolean;
var
  HConsoleOutput: LongWord;
begin
  HConsoleOutput := GetStdHandle(STD_OUTPUT_HANDLE);

  Result := (HConsoleOutput <> 7);
end;

end.

