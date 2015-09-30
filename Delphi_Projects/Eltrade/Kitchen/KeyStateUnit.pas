unit KeyStateUnit;

interface

uses Classes, Forms, Windows, SysUtils;

type
  TKeyState = class(TObject)
  private
    KeyState       : TKeyboardState;
    LastChangeTime : TDateTime;
    function GetNumLock : boolean;
    function GetCapsLock : boolean;
    function IsNT : boolean;
    function State(Ctrl: Word) : boolean;
    procedure SetState(Ctrl: word; stOn: boolean);
    procedure SetNumLock(stOn: Boolean);
    procedure SetCapsLock(stOn: Boolean);
  public
    property NumLock  : Boolean read GetNumLock write SetNumLock;
    property CapsLock : Boolean read GetCapsLock write SetCapsLock;
    function GetKeyboardLayoutPOS(SetToDefault: Boolean): String;
    function ChangeActiveLayOutPOS: String;
  end;

implementation

uses  Dialogs;

procedure TKeyState.SetNumLock(stOn: Boolean);
begin
 SetState(vk_NumLock, stOn);
end;

function TKeyState.GetNumLock: boolean;
begin
 GetKeyboardState(KeyState);
 Result:=State(vk_NumLock);
end;

procedure TKeyState.SetCapsLock(stOn: Boolean);
begin
 SetState(vk_Capital, stOn);
end;

function TKeyState.GetCapsLock: boolean;
begin
 GetKeyboardState(KeyState);
 Result := State(vk_Capital);
end;

function TKeyState.IsNT: boolean;
begin
 Result := (GetVersion<$80000000);
end;

function TKeyState.State(Ctrl: Word): boolean;
begin
 Result := ((KeyState[Ctrl] and 1)=1);
end;

procedure TKeyState.SetState(Ctrl: word; stOn: boolean);
begin
 GetKeyboardState(KeyState);
 { Toggle KeyState if changed }
 if (State(ctrl) or stOn) then
  begin
   { Toggle KeyState SystemWide }
   keybd_event(Ctrl, 0, 0, 0);
   keybd_event(Ctrl, 0, KEYEVENTF_KEYUP, 0);
  end;
 { if not Windows NT this has to be done. }
 if not IsNT then
  begin
   Application.ProcessMessages; { Has to be here. Otherwise Win95 lose control. }
   { Set KeyState }
   KeyState[Ctrl]:=Byte(stOn);
   SetKeyboardState(KeyState);
  end;
end;

function TKeyState.GetKeyboardLayOutPOS(SetToDefault: Boolean): String;
var LayOutName : Pchar;
    S1, S2    : String;
begin
 GetMem(LayOutName, 20);
 try
  GetKeyboardLayoutName(LayOutName);
  S1 :=  StrPas(LayOutName);
 finally
  FreeMem(LayOutName);
 end;

 GetMem(LayOutName, 255);
 try
  VerLanguageName(GetKeyboardLayout(0), LayOutName, 255);
  S2 :=  StrPas(LayOutName);
 finally
  FreeMem(LayOutName);
 end;
 result := '';

// if LayOutName = '00000409' then Result := 'EN';
 if S1 = '00020402' then Result := 'BP'
 else
 if S1 = '00000402' then Result := 'BG'
 else
 Result := UpperCase(Copy(S2,1,2));
end;

function TKeyState.ChangeActiveLayOutPOS : String;
begin
 if Now - LastChangeTime >EncodeTime(0,0,0,400) then
  begin
   LastChangeTime := Now;
   ActivateKeyboardLayout(HKL_NEXT, $100 {KLF_SETFORPROCESS});
  end; 
 Result := GetKeyboardLayoutPOS(false);
end;

end.
