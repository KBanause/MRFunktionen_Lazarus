unit MRSetFormPosition;
(************************************************)
(*TMRSetFormPos Version 1.0 (20.10.2006)        *)
(*                                              *)
(*Copyright ® 2006 by Marc Rasmussen            *)
(*E-Mail: marc@myrasmussen.de                   *)
(*- - - - - - - - - - - - - - - - - - - - - - - *)
(*Ich übernehme keine Haftung für etwaige       *)
(*Schäden, die durch diese Komponente verursacht*)
(*werden.                                       *)
(*- - - - - - - - - - - - - - - - - - - - - - - *)
(*Diese Komponente ist FREEWARE.                *)
(*Alle Rechte vorbehalten                       *)
(************************************************)

interface

uses Windows, Classes, Forms;

type
  TMRMakeVisibleEvent = procedure(Sender: TForm; const fullyvisible: Boolean; var docorrect: Boolean)of object;
  TMRFormPos = (mrfpAsDesigned,
    mrfpBottomLeft, mrfpBottomCenter, mrfpBottomRight,
    mrfpCenterLeft, mrfpCenter, mrfpCenterRight,
    mrfpTopLeft, mrfpTopCenter, mrfpTopRight, mrfpMainFormCenter);

  TMRSetFormPos = class(TComponent)
  private
    FFormPosition: TMRFormPos;
    FOnSetPosition: TNotifyEvent;
    loadings: boolean;
    FOnMakeVisible: TMRMakeVisibleEvent;

    procedure SetFormPosition(const Value: TMRFormPos);
    { private-Deklarationen }
  protected
    { protected-Deklarationen }
    procedure Loaded; override;
  public
    { public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetPosition;
    procedure MakeVisible;
  published
    { published-Deklarationen }
    property FormPosition: TMRFormPos read FFormPosition write SetFormPosition default mrfpAsDesigned;
    property OnSetPosition: TNotifyEvent read FOnSetPosition write FOnSetPosition;
    property OnMakeVisible: TMRMakeVisibleEvent read FOnMakeVisible write FOnMakeVisible;
  end;

implementation

uses MRFunktionen, MRFunktionen_Errors, Types;

{ TMRSetFormPos }

constructor TMRSetFormPos.Create(AOwner: TComponent);
var
  owne                        : TComponent;
  FOwnerForm                  : TForm;
  i                           : Integer;
begin
  FOnSetPosition := nil;
  FOnMakeVisible := nil;

  if AOwner is TForm then
    FOwnerForm := TForm(AOwner)
  else
  begin
    owne := AOwner.Owner;

    while (not (owne is TForm)) do
      owne := owne.Owner;

    FOwnerForm := TForm(owne);
  end;

  for i := 0 to FOwnerForm.ComponentCount - 1 do
  begin
    if FOwnerForm.Components[i] is TMRSetFormPos then
      raise EMRComponentDuplicate.Create(Self);
  end; // for i := 0 to FOwnerForm.ComponentCount - 1

  inherited;

  FFormPosition := mrfpAsDesigned;
  FOnSetPosition := nil;
  FOnMakeVisible := nil;
  loadings := False;
end;

destructor TMRSetFormPos.Destroy;
begin

  inherited;
end;

procedure TMRSetFormPos.Loaded;
begin
  loadings := True;
  inherited;

  if not (csDesigning in ComponentState) and (Owner is TForm) then
    SetPosition;

  loadings := False;
end;

procedure TMRSetFormPos.MakeVisible;
var
  h: Integer;
  MonitorMouse: TMonitor;
  MousePosition: TPoint;
  myOwner: TForm;
  PositionOwner: TPoint;
  w: Integer;
  x: Integer;
  y: Integer;
  fullyvisible: Boolean;
  docorrect: Boolean;
begin
  if (not (csDesigning in ComponentState) and (Owner is TForm)) then
  begin
    myOwner := TForm(Owner);
    x := myOwner.Left;
    y := myOwner.Top;
    w := myowner.Width;
    h := myowner.Height;
    PositionOwner.X := x;
    PositionOwner.Y := y;
    GetCursorPos(MousePosition);
    MonitorMouse := Screen.MonitorFromPoint(MousePosition);
    fullyvisible := True;
    docorrect := True;

    if (Assigned(FOnMakeVisible)) then
    begin
      FOnMakeVisible(myowner, fullyvisible, docorrect);
    end; // if (Assigned(FOnMakeVisible))

    if (docorrect) then
    begin
      if (MonitorMouse.WorkareaRect.Right < (x + w)) then
      begin
        myowner.Left := MonitorMouse.WorkareaRect.Right - w - 1;
        fullyvisible := False;
      end;

      if (MonitorMouse.WorkareaRect.Bottom < (y + h)) then
      begin
        myowner.Top := MonitorMouse.WorkareaRect.Bottom - h - 1;
        fullyvisible := False;
      end;
    end; // if (docorrect)
  end; // if (not (csDesigning in ComponentState) and (Owner is TForm))
end;

procedure TMRSetFormPos.SetFormPosition(const Value: TMRFormPos);
begin
  if Value <> FFormPosition then
  begin
    FFormPosition := Value;
    SetPosition;
  end;
end;

procedure TMRSetFormPos.SetPosition;
var
  MonitorMouse                : TMonitor;
  MousePosition               : TPoint;
  myOwner                     : TForm;
  PositionOwner               : TPoint;
  pt: TPoint;
  x                           : Integer;
  y                           : Integer;
begin
  if not (csDesigning in ComponentState) and (Owner is TForm) then
  begin
    myOwner := TForm(Owner);
    x := myOwner.Left;
    y := myOwner.Top;
    PositionOwner.X := x;
    PositionOwner.Y := y;
    GetCursorPos(MousePosition);
//    MonitorOwner := Screen.MonitorFromPoint(PositionOwner);
    MonitorMouse := Screen.MonitorFromPoint(MousePosition);

    with MonitorMouse.WorkareaRect do
    begin
      case FFormPosition of
        mrfpBottomLeft:
          begin
            x := Left + 1;
            y := Bottom - myOwner.Height;
          end;
        mrfpBottomCenter:
          begin
            x := (Left + Right) div 2 - (myOwner.Width div 2);
            y := MonitorMouse.WorkareaRect.Bottom - myOwner.Height;
          end;
        mrfpBottomRight:
          begin
            x := Right - myOwner.Width;
            y := MonitorMouse.WorkareaRect.Bottom - myOwner.Height;
          end;
        mrfpCenterLeft:
          begin
            x := Left + 1;
            y := (Top + Bottom) div 2 - (myOwner.Height div 2);
          end;
        mrfpCenter:
          begin
            x := (Left + Right) div 2 - (myOwner.Width div 2);
            y := (Top + Bottom) div 2 - (myOwner.Height div 2);
          end;
        mrfpCenterRight:
          begin
            x := Right - myOwner.Width;
            y := (Top + Bottom) div 2 - (myOwner.Height div 2);
          end;
        mrfpTopLeft:
          begin
            x := Left + 1;
            y := Top + 1;
          end;
        mrfpTopCenter:
          begin
            x := (Left + Right) div 2 - (myOwner.Width div 2);
            y := Top + 1;
          end;
        mrfpTopRight:
          begin
            x := Right - myOwner.Width;
            y := Top + 1;
          end;
        mrfpMainFormCenter:
          begin
            pt.X := Application.MainForm.Left;
            pt.Y := Application.MainForm.Top;

            x := (pt.X + Application.MainForm.Width) div 2 - (myOwner.Width div 2);
            y := (pt.Y + Application.MainForm.Height) div 2 - (myOwner.Height div 2);
          end;
      end;
    end;

    myOwner.Left := x;
    myOwner.Top := y;

    if (Assigned(FOnSetPosition)) and (not loadings) then
      FOnSetPosition(Self);
  end;
end;

end.
