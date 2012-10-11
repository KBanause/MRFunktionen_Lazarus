unit MRFunktionen_Errors;

interface

uses Classes, SysUtils;

type
  EMRComponentDuplicate = class(Exception)
  public
    constructor Create(Caller: TComponent);
  end;

  EMRThreadNotAllowed = class(Exception);
  EMRArrayIndexOutOfBound = class(ERangeError);
  EMRArrayIndexNotFound = class(Exception);
  EMRArrayIndexIsInteger = class(Exception);

implementation

{ EMRComponentDuplicate }

constructor EMRComponentDuplicate.Create(Caller: TComponent);
var
  s                           : string;
begin
  s := 'Komponente "' + Caller.ClassName + '" existiert bereits und darf nur einmal verwendet werden.';

  inherited Create(s);
end;

end.
