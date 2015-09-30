unit CryptoHandler;

interface

uses SysUtils, Classes;

type
 TCrypto_TEA = class;

 TCrypto_Base = class(TObject)
 private
  FLastError: String;
 public
  property LastError: String read FLastError write FLastError;
 end;

 Tea_ctx = record
  KEY: array[0..3] of Cardinal;
 end;

 TCrypto_TEA = class(TCrypto_Base)
 private
  function FTEA_SetKey(var ctx_arg: tea_ctx; var in_key: array of Byte; key_len: Cardinal): Boolean;
  procedure FTEA_Encrypt(ctx_arg: tea_ctx; var dest: array of Byte; src: array of Byte);
  procedure FTEA_Decrypt(ctx_arg: tea_ctx; var dest: array of Byte; src: array of Byte);

  procedure FHexToByte(Ch1, Ch2: Char; var B: Byte);
  procedure FByteToHex(B: Byte; var Ch1, Ch2: Char);
  function FEncryptStream(InStrm_, OutStrm_: TStream; Key_: String): Boolean;
  function FDecryptStream(InStrm_, OutStrm_: TStream; Key_: String): Boolean;
 public
  function EncryptStreamEx(InOutStrm_: TStream; Key_: String): Boolean;
  function DecryptStreamEx(InOutStrm_: TStream; Key_: String): Boolean;
  function EncryptStringEx(Source_: String; Key_: String): String;
  function DecryptStringEx(Source_: String; Key_: String): String;
 end;

	TBasicCheckSum = class
	private
		FCheckSumSize	: Integer;
		FName					: string;
		function GetString: string;
		function GetTheByte(Index: Integer): Byte; virtual;
	protected
		function GetByte(Index: Integer): Byte; virtual; abstract;
	public
		constructor Create(AName: string; ACheckSumSize: Integer);
		destructor Destroy; override;

		procedure Reset; virtual; abstract;
		procedure Process(const Data; Size: Integer); virtual; abstract;

		property Name: string read FName;
		property Size: Integer read FCheckSumSize;
		property Bytes[Index: Integer]: Byte read GetTheByte;
		property AsString: string read GetString;
	end; // class TBasicCheckSum

	TBasicCheckSum32Bit = class(TBasicCheckSum)
	private
		FSum	: LongWord;
	protected
		function GetByte(Index: Integer): Byte; override;
		procedure SetCheckSum(Value: LongWord); virtual;
	public
		constructor Create(AName: string);
		procedure Reset; override;

		property Sum: LongWord read FSum;
	end; // class TBasicCheckSum32Bit

 TCRC32	= class(TBasicCheckSum32Bit)
 public
   constructor Create;
   procedure Process(const Data; Size: Integer); override;
 end;

implementation
uses Math;

//******************************************************************************
//  TCrypto_TEA
//******************************************************************************
const
  TEA_KEY_SIZE   = 16;
  TEA_BLOCK_SIZE = 8;
  TEA_ROUNDS     = 32;
  TEA_DELTA      = $9E3779B9;

// Return True on success, False on error.
function TCrypto_TEA.FTEA_SetKey(var ctx_arg: tea_ctx; var in_key: array of Byte; key_len: Cardinal): Boolean;
begin
  Result := False;
  if (key_len <> 16) then  Exit;

  ctx_arg.KEY[0] := (in_key[0] shl 24) + (in_key[1] shl 16) + (in_key[2] shl 8) + in_key[3];
  ctx_arg.KEY[1] := (in_key[4] shl 24) + (in_key[5] shl 16) + (in_key[6] shl 8) + in_key[7];
  ctx_arg.KEY[2] := (in_key[8] shl 24) + (in_key[9] shl 16) + (in_key[10] shl 8) + in_key[11];
  ctx_arg.KEY[3] := (in_key[12] shl 24) + (in_key[13] shl 16) + (in_key[14] shl 8) + in_key[15];

  Result := True;
end;

procedure TCrypto_TEA.FTEA_Encrypt(ctx_arg: tea_ctx; var dest: array of Byte; src: array of Byte);
var
  y, z, n, sum: Cardinal;
  k0, k1, k2, k3: Cardinal;
begin
  sum := 0;

  y := (src[0] shl 24) + (src[1] shl 16) + (src[2] shl 8) + src[3];
  z := (src[4] shl 24) + (src[5] shl 16) + (src[6] shl 8) + src[7];

  k0 := ctx_arg.KEY[0];
  k1 := ctx_arg.KEY[1];
  k2 := ctx_arg.KEY[2];
  k3 := ctx_arg.KEY[3];

  for n := 1 to TEA_ROUNDS do
  begin
    Inc(sum, TEA_DELTA);
    Inc(y, ((z shl 4) + k0) xor (z + sum) xor ((z shr 5) + k1));
    Inc(z, ((y shl 4) + k2) xor (y + sum) xor ((y shr 5) + k3));
  end;

  dest[3] := Lo(y);
  y := y shr 8;
  dest[2] := Lo(y);
  y := y shr 8;
  dest[1] := Lo(y);
  y := y shr 8;
  dest[0] := Lo(y);

  dest[7] := Lo(z);
  z := z shr 8;
  dest[6] := Lo(z);
  z := z shr 8;
  dest[5] := Lo(z);
  z := z shr 8;
  dest[4] := Lo(z);
end;

procedure TCrypto_TEA.FTEA_Decrypt(ctx_arg: tea_ctx; var dest: array of Byte; src: array of Byte);
var
  y, z, n, sum: Cardinal;
  k0, k1, k2, k3: Cardinal;
begin
  y := (src[0] shl 24) + (src[1] shl 16) + (src[2] shl 8) + src[3];
  z := (src[4] shl 24) + (src[5] shl 16) + (src[6] shl 8) + src[7];

  k0 := ctx_arg.KEY[0];
  k1 := ctx_arg.KEY[1];
  k2 := ctx_arg.KEY[2];
  k3 := ctx_arg.KEY[3];

  sum := TEA_DELTA shl 5;

  for n := 1 to TEA_ROUNDS do
  begin
    Dec(z, ((y shl 4) + k2) xor (y + sum) xor ((y shr 5) + k3));
    Dec(y, ((z shl 4) + k0) xor (z + sum) xor ((z shr 5) + k1));
    Dec(sum, TEA_DELTA);
  end;
	
  dest[3] := Lo(y);
  y := y shr 8;
  dest[2] := Lo(y);
  y := y shr 8;
  dest[1] := Lo(y);
  y := y shr 8;
  dest[0] := Lo(y);

  dest[7] := Lo(z);
  z := z shr 8;
  dest[6] := Lo(z);
  z := z shr 8;
  dest[5] := Lo(z);
  z := z shr 8;
  dest[4] := Lo(z);
end;

function TCrypto_TEA.FEncryptStream(InStrm_, OutStrm_: TStream; Key_: String): Boolean;
var I          : Integer;
    K          : array[0..15] of Byte;
    Plain      : array[0..7] of Byte;
    Encrypted  : array[0..7] of Byte;
    TeaContext : Tea_ctx;
begin
 Result := true;
 try
  if (InStrm_ = nil)or(OutStrm_ = nil) then raise EAbort.Create('Invalid input parameters.');

  OutStrm_.Size     := 0;
  OutStrm_.Position := 0;
  InStrm_.Position  := 0;

  if (Key_ = '')then
   begin
     OutStrm_.CopyFrom(InStrm_, InStrm_.Size);
   end
  else
   begin
     if Length(Key_) < 16 then Key_ := Key_ + '0123456789ABCDEF';
     for I := 0 to 15 do K[I] := Ord(Key_[I+1]);
     if (not FTEA_SetKey(TeaContext, K, 16)) then raise EAbort.Create('Fail set key');

     InStrm_.Position := 0;
     for I := 0 to 7 do Plain[I] := 0;
     while InStrm_.Read(Plain[0], 8) > 0 do
      begin
       FTEA_Encrypt(TeaContext, Encrypted, Plain);
       OutStrm_.WriteBuffer(Encrypted[0], 8);
       for I := 0 to 7 do Plain[I] := 0;
      end;
   end;
 except
  on E: Exception do
   begin
    FLastError := E.Message;
    Result     := False;
   end;
 end;
end;

function TCrypto_TEA.FDecryptStream(InStrm_, OutStrm_: TStream; Key_: String): Boolean;
var I          : Integer;
    K          : array[0..15] of Byte;
    Plain      : array[0..7] of Byte;
    Encrypted  : array[0..7] of Byte;
    TeaContext : Tea_ctx;
begin
 Result := true;
 try
   if (InStrm_ = nil)or(OutStrm_ = nil) then raise EAbort.Create('Invalid input parameters.');

   OutStrm_.Size := 0;
   OutStrm_.Position := 0;
   InStrm_.Position  := 0;

   if (Key_ = '')then
    begin
     OutStrm_.CopyFrom(InStrm_, InStrm_.Size);
    end
   else
    begin
     if Length(Key_) < 16 then Key_ := Key_ + '0123456789ABCDEF';
     for I := 0 to 15 do K[I] := Ord(Key_[I+1]);
     if (not FTEA_SetKey(TeaContext, K, 16)) then  raise EAbort.Create('Fail set key');

     for I := 0 to 7 do Encrypted[I] := 0;
     while InStrm_.Read(Encrypted[0], 8) > 0 do
      begin
       FTEA_Decrypt(TeaContext, Plain, Encrypted);
       OutStrm_.WriteBuffer(Plain[0], 8);
       for I := 0 to 7 do Encrypted[I] := 0;
      end;
    end;
 except
  on E: Exception do
   begin
    FLastError := E.Message;
    Result     := False;
   end;
 end;
end;

function TCrypto_TEA.EncryptStreamEx(InOutStrm_: TStream; Key_: String): Boolean;
var OutS: TMemoryStream;
    B   : Byte;
    C   : array [1..2] of Char;
begin
 OutS := TMemoryStream.Create;
 try
  Result := FEncryptStream(InOutStrm_, OutS, Key_);
  InOutStrm_.Size := 0;
  OutS.Position   := 0;
  while OutS.Read(B, 1) > 0 do
   begin
    FByteToHex(B, C[1], C[2]);
    InOutStrm_.Write(C[1], 2);
   end;
  InOutStrm_.Position := 0;
 finally
  OutS.Free;
 end;
end;

function TCrypto_TEA.DecryptStreamEx(InOutStrm_: TStream; Key_: String): Boolean;
var OutS: TMemoryStream;
    B   : Byte;
    C   : array[1..2] of Char;
begin
 OutS := TMemoryStream.Create;
 try
  InOutStrm_.Position := 0;
  while InOutStrm_.Read(C[1], 2) > 0 do
   begin
    FHexToByte(C[1], C[2], B);
    OutS.Write(B, 1);
   end;
  Result := FDecryptStream(OutS, InOutStrm_, Key_);
  InOutStrm_.Position   := 0;
 finally
  OutS.Free;
 end;
end;

function TCrypto_TEA.EncryptStringEx(Source_: String; Key_: String): String;
var IStrm : TStringStream;
    OStrm : TMemoryStream;
    B     : Byte;
    C     : array [1..2] of Char;
begin
 Result:= '';
 IStrm := TStringStream.Create(Source_);
 OStrm := TMemoryStream.Create;
 try
  if FEncryptStream(IStrm, OStrm, Key_) then
   begin
    OStrm.Position := 0;
    while OStrm.Read(B, 1) > 0 do
     begin
      FByteToHex(B, C[1], C[2]);
      Result := Result + C[1] + C[2];
     end;
   end;
 finally
  IStrm.Free;
  OStrm.Free;
 end;
end;

function TCrypto_TEA.DecryptStringEx(Source_: String; Key_: String): String;
var IStrm, OStrm : TMemoryStream;
    B       : Byte;
begin
 Result:= '';
 IStrm := TMemoryStream.Create;
 OStrm := TMemoryStream.Create;
 try
  while Length(Source_) >= 2 do
   begin
    FHexToByte(Source_[1], Source_[2], B);
    IStrm.Write(B, 1);
    Delete(Source_, 1, 2);
   end;
  if FDecryptStream(IStrm, OStrm, Key_) then
   begin
    OStrm.Position := 0;
    while OStrm.Read(B, 1) > 0 do
     begin
      if B > 0 then Result := Result + Chr(B);
     end;
   end;
 finally
  IStrm.Free;
  OStrm.Free;
 end;
end;

const
 C_HexCodeTable : array[$0..$F] of Char = ('Q','W','E','R','T','Y','A','S','D','F','G','Z','X','C','V','B');

procedure TCrypto_TEA.FHexToByte(Ch1, Ch2: Char; var B: Byte);
 function CharToByte_(C: Char): Byte;
 var I : Integer;
 begin
  Result := 0;
  for I := 0 to $F do
   if C_HexCodeTable[I] = C then
   begin
    Result := I;
    Break;
   end;
 end;
begin
 B := CharToByte_(Ch1)*16+CharToByte_(Ch2);
end;

procedure TCrypto_TEA.FByteToHex(B: Byte; var Ch1, Ch2: Char);
var BL, BH : Byte;
begin
 Ch1:='0'; Ch2:='0';
 BL := B and $F;
 BH := (B shr 4) and $F;
 if BH in [$0..$F] then Ch1 := C_HexCodeTable[BH];
 if BL in [$0..$F] then Ch2 := C_HexCodeTable[BL];
end;




//******************************************************************************
//    TCRC32
//******************************************************************************
const
	CRC32Table	: array[Byte] of LongWord = (
		$000000000, $077073096, $0ee0e612c, $0990951ba, $0076dc419, $0706af48f,
		$0e963a535, $09e6495a3, $00edb8832, $079dcb8a4, $0e0d5e91e, $097d2d988,
		$009b64c2b, $07eb17cbd, $0e7b82d07, $090bf1d91, $01db71064, $06ab020f2,
		$0f3b97148, $084be41de, $01adad47d, $06ddde4eb, $0f4d4b551, $083d385c7,
		$0136c9856, $0646ba8c0, $0fd62f97a, $08a65c9ec, $014015c4f, $063066cd9,
		$0fa0f3d63, $08d080df5, $03b6e20c8, $04c69105e, $0d56041e4, $0a2677172,
		$03c03e4d1, $04b04d447, $0d20d85fd, $0a50ab56b, $035b5a8fa, $042b2986c,
		$0dbbbc9d6, $0acbcf940, $032d86ce3, $045df5c75, $0dcd60dcf, $0abd13d59,
		$026d930ac, $051de003a, $0c8d75180, $0bfd06116, $021b4f4b5, $056b3c423,
		$0cfba9599, $0b8bda50f, $02802b89e, $05f058808, $0c60cd9b2, $0b10be924,
		$02f6f7c87, $058684c11, $0c1611dab, $0b6662d3d, $076dc4190, $001db7106,
		$098d220bc, $0efd5102a, $071b18589, $006b6b51f, $09fbfe4a5, $0e8b8d433,
		$07807c9a2, $00f00f934, $09609a88e, $0e10e9818, $07f6a0dbb, $0086d3d2d,
		$091646c97, $0e6635c01, $06b6b51f4, $01c6c6162, $0856530d8, $0f262004e,
		$06c0695ed, $01b01a57b, $08208f4c1, $0f50fc457, $065b0d9c6, $012b7e950,
		$08bbeb8ea, $0fcb9887c, $062dd1ddf, $015da2d49, $08cd37cf3, $0fbd44c65,
		$04db26158, $03ab551ce, $0a3bc0074, $0d4bb30e2, $04adfa541, $03dd895d7,
		$0a4d1c46d, $0d3d6f4fb, $04369e96a, $0346ed9fc, $0ad678846, $0da60b8d0,
		$044042d73, $033031de5, $0aa0a4c5f, $0dd0d7cc9, $05005713c, $0270241aa,
		$0be0b1010, $0c90c2086, $05768b525, $0206f85b3, $0b966d409, $0ce61e49f,
		$05edef90e, $029d9c998, $0b0d09822, $0c7d7a8b4, $059b33d17, $02eb40d81,
		$0b7bd5c3b, $0c0ba6cad, $0edb88320, $09abfb3b6, $003b6e20c, $074b1d29a,
		$0ead54739, $09dd277af, $004db2615, $073dc1683, $0e3630b12, $094643b84,
		$00d6d6a3e, $07a6a5aa8, $0e40ecf0b, $09309ff9d, $00a00ae27, $07d079eb1,
		$0f00f9344, $08708a3d2, $01e01f268, $06906c2fe, $0f762575d, $0806567cb,
		$0196c3671, $06e6b06e7, $0fed41b76, $089d32be0, $010da7a5a, $067dd4acc,
		$0f9b9df6f, $08ebeeff9, $017b7be43, $060b08ed5, $0d6d6a3e8, $0a1d1937e,
		$038d8c2c4, $04fdff252, $0d1bb67f1, $0a6bc5767, $03fb506dd, $048b2364b,
		$0d80d2bda, $0af0a1b4c, $036034af6, $041047a60, $0df60efc3, $0a867df55,
		$0316e8eef, $04669be79, $0cb61b38c, $0bc66831a, $0256fd2a0, $05268e236,
		$0cc0c7795, $0bb0b4703, $0220216b9, $05505262f, $0c5ba3bbe, $0b2bd0b28,
		$02bb45a92, $05cb36a04, $0c2d7ffa7, $0b5d0cf31, $02cd99e8b, $05bdeae1d,
		$09b64c2b0, $0ec63f226, $0756aa39c, $0026d930a, $09c0906a9, $0eb0e363f,
		$072076785, $005005713, $095bf4a82, $0e2b87a14, $07bb12bae, $00cb61b38,
		$092d28e9b, $0e5d5be0d, $07cdcefb7, $00bdbdf21, $086d3d2d4, $0f1d4e242,
		$068ddb3f8, $01fda836e, $081be16cd, $0f6b9265b, $06fb077e1, $018b74777,
		$088085ae6, $0ff0f6a70, $066063bca, $011010b5c, $08f659eff, $0f862ae69,
		$0616bffd3, $0166ccf45, $0a00ae278, $0d70dd2ee, $04e048354, $03903b3c2,
		$0a7672661, $0d06016f7, $04969474d, $03e6e77db, $0aed16a4a, $0d9d65adc,
		$040df0b66, $037d83bf0, $0a9bcae53, $0debb9ec5, $047b2cf7f, $030b5ffe9,
		$0bdbdf21c, $0cabac28a, $053b39330, $024b4a3a6, $0bad03605, $0cdd70693,
		$054de5729, $023d967bf, $0b3667a2e, $0c4614ab8, $05d681b02, $02a6f2b94,
		$0b40bbe37, $0c30c8ea1, $05a05df1b, $02d02ef8d
	);

resourcestring
	SCRC32	= '32-bit Cyclic Redundancy Code (CRC32)';
	SGetByteRange = 'Call to %s.GetByte [property Bytes] with index <> [0..%d]';

constructor TBasicCheckSum.Create(AName: string; ACheckSumSize: Integer);
begin
	// Set private members for properties
	FCheckSumSize := ACheckSumSize;
	FName := AName;

	// Reset CheckSum
	Reset;
end; // constructor TBasicCheckSum.Create

destructor TBasicCheckSum.Destroy;
begin
	// Nothing to destroy
end; // destructor TBasicCheckSum.Destroy

function TBasicCheckSum.GetString: string;
const HexDigits	: array[0..15] of Char = 'MNBLKJPOIASDQWEZ';
var i	: Integer;
	  B	: Byte;
begin
	Result := '';
	for i := 1 to Size do
	 begin
		B := Bytes[i-1];
		Result := Result + HexDigits[B shr 4] + HexDigits[B and 15];
	 end;
end;

function TBasicCheckSum.GetTheByte(Index: Integer): Byte;
begin
	if (Index < 0) or (Index >= FCheckSumSize) then
		raise ERangeError.CreateFmt(SGetByteRange, [ClassName, 0, Size-1])
	else
		Result := GetByte(Index);
end; // function TBasicCheckSum.GetTheByte


constructor TBasicCheckSum32Bit.Create(AName: string);
begin
	inherited Create(AName, 4);
end; // constructor TBasicCheckSum32Bit.Create

function TBasicCheckSum32Bit.GetByte(Index: Integer): Byte;
begin
	case Index of
		0	: Result := (FSum shr 24);
		1	: Result := (FSum shr 16) and 255;
		2	: Result := (FSum shr 8) and 255;
		3	: Result := (FSum and 255);
	else
		Result := 0;
	end; // case Index
end; // function TBasicCheckSum32Bit.GetByte

procedure TBasicCheckSum32Bit.Reset;
begin
	SetCheckSum(0);
end; // procedure TBasicCheckSum32Bit.Reset

procedure TBasicCheckSum32Bit.SetCheckSum(Value: LongWord);
begin
	FSum := Value;
end; // procedure TBasicCheckSum32Bit.SetCheckSum


constructor TCRC32.Create;
begin
	inherited Create(SCRC32);
end; // constructor Create

procedure TCRC32.Process(const Data; Size: Integer);
var
	P	: PChar;
	I	: Integer;
	S	: LongWord;
begin
	// Fix the sum before processing the data
	S := Sum xor $FFFFFFFF;

	// Process the data
	P := @Data;
	for I := 1 to Size do
		S:=((S shr 8) and $FFFFFF) xor CRC32Table[(S xor Ord(P[I-1])) and 255];

	// Store the new CheckSum
	SetCheckSum(S xor $FFFFFFFF);
end; // procedure TCRC32.Process


end.
