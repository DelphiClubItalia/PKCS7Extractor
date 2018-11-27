unit PKCS7Extractor;

{***************************************************************************}
{                                                                           }
{     PKCS#7 Extractor library for Delphi                                   }
{     Version 1.0.0.0 released November, 22nd 2018                          }
{                                                                           }
{     Copyright (C) 2018 Delphi Club Italia                                 }
{                        http://www.delphiclubitalia.it                     }
{                                                                           }
{     Original authors:                                                     }
{         Christian Cristofori         pkcs7reader@christiancristofori.it   }
{         Giancarlo Oneglio                   giancarlo.oneglio@gmail.com   }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the GNU Lesser General Public License, Version 3;         }
{  you may not use this file except in compliance with the License.         }
{                                                                           }
{  This is free software: you can redistribute it and/or modify it under    }
{  the terms of the GNU Lesser General Public License as published by the   }
{  Free Software Foundation, either version 3 of the License, or (at your   }
{  option) any later version.                                               }
{                                                                           }
{  This is distributed in the hope that it will be useful, but WITHOUT      }
{  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or    }
{  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public      }
{  License for more details.                                                }
{                                                                           }
{  You should have received a copy of the GNU Lesser General Public         }
{  License along with this software.                                        }
{  If not, see <http://www.gnu.org/licenses/>.                              }
{                                                                           }
{***************************************************************************}

interface

uses Classes;

const
  LIBEAY32_LIBRARY = 'libeay32.dll';

function GetFolder: String;
procedure SetFolder(const Value: String);
function Load: Boolean;
function Loaded: Boolean;
procedure Unload;
function GetVersion: String;

type
  PBIO_METHOD       = Pointer;

  PSTACK            = Pointer;

  CRYPTO_EX_DATA    = record
    sk              : PSTACK;
    dummy           : Integer
  end;

  PBIO              = ^BIO;
  BIO               = record
    method          : PBIO_METHOD;
    callback        : Pointer;
    cb_arg          : PAnsiChar;
    init,
    shutdown,
    flags,
    retry_reasonm,
    num             : Integer;
    ptr             : Pointer;
    next_bio,
    prev_bio        : PBIO;
    references      : Integer;
    num_read,
    num_write       : LongWord;
    ex_data         : CRYPTO_EX_DATA
  end;

  PASN1_OBJECT      = ^ASN1_OBJECT;
  ASN1_OBJECT       = record
    sn,
    ln              : PAnsiChar;
    nid,
    length          : Integer;
    data            : PAnsiChar;
    flags           : Integer
  end;

  PKCS7_union       = record
    ptr             : PAnsiChar
  end;

  PPKCS7            = ^PKCS7;
  PKCS7             = record
    asn1            : PAnsiChar;
    length          : LongInt;
    state,
    detached        : Integer;
    _type           : PASN1_OBJECT;
    d               : PKCS7_union
  end;

  PX509_STORE       = Pointer;

  TVerificationOption = (
    voNone,       // Does not verify at all
    voVerify,     // Verifies only
    voSignature   // Full signature verification
    );

  TPKCS7Message         = class
  private
    fBIO,
    fData,
    fOutput         : PBIO;
    fPKCS7          : PPKCS7;
    fSignerStack    : PSTACK;
    fStore          : PX509_STORE;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function LoadFromStream(Stream: TStream): Boolean;
    function LoadFromFile(const Filename: String): Boolean;

    function Verify(const Verify: TVerificationOption = voNone): Boolean;

    function SaveToStream(Stream: TStream): Boolean;
    function SaveToFile(const Filename: String): Boolean;
  end;

// Tries to guess destination filename of a .*.p7m file
function GuessFilename(const Filename: String): String;

// Extracts data from PKCS#7 message stream to a stream.
function Extract(InStream, OutStream: TStream; const Verification: TVerificationOption = voNone): Boolean; overload;

// Utility function: does the above operation from a file to another by
// specifing the filenames.
function Extract(const InFilename, OutFilename: String; const Verification: TVerificationOption = voNone): Boolean; overload;

// Utility function: doest the above operation from a file to another by specifing
// the filename, this function tries to guess the destination filename by removing
// the final extension, if another extension is present it will do the job.
function Extract(const Filename: String; const Verification: TVerificationOption = voNone): String; overload;

// Utility function to extract the content from a stream directly to a String.
function Extract(Stream: TStream; var S: String; const Verification: TVerificationOption = voNone): Boolean; overload;

// Utility function to extract the content from a file directly to a String.
function ExtractToString(const Filename: String; var S: String; const Verification: TVerificationOption = voNone): Boolean; overload;

function Verify(Stream: TStream; const Verification: TVerificationOption = voNone): Boolean; overload;

function Verify(const Filename: String; const Verification: TVerificationOption = voNone): Boolean; overload;

implementation

uses Windows, SysUtils;

// ================================================== LIBEAY32.DLL MICRO WRAPPER

const
  BIO_CTRL_RESET    = 1;

  PKCS7_NOVERIFY    = $20;

  NID_PKCS7_SIGNED  = 22;

  LIBEAY_OK         = 1;
  LIBEAY_FAILURE    = 0;

type
  PPPKCS7           = ^PPKCS7;

  PPEM_PASSWORD_CB  = Pointer;

  PSTACK_OF_X509    = Pointer;

const
  LIBEAY32_FUNCTIONS_COUNT = 21;

type
  TLibeay32Functions  = packed record
    case Integer of
      0: (
    {00}    OpenSSL_add_all_digests   : procedure cdecl;

    {01}    sk_num                    : function(const x: PSTACK): Integer cdecl; // OpenSSL 0.9.6

    {02}    BIO_s_mem                 : function: PBIO_METHOD cdecl; // OpenSSL pre-0.9.6
    {03}    BIO_new                   : function(t: PBIO_METHOD): PBIO cdecl; // OpenSSL pre-0.9.6
    {04}    BIO_new_mem_buf           : function(buf : Pointer; len : Integer) : PBIO cdecl; // introduced OpenSSL 0.9.6
    {05}    BIO_ctrl                  : function(b: PBIO; c: Integer; l: LongInt; a: Pointer): LongInt cdecl; // OpenSSL pre-0.9.6
    {06}    BIO_read                  : function(b: PBIO; d: Pointer; l: Integer): Integer cdecl; // OpenSSL pre-0.9.6
    {07}    BIO_write                 : function(b: pBIO; const p: Pointer; l: Integer): Integer cdecl; // OpenSSL pre-0.9.6
    {08}    BIO_pop                   : function(b: PBIO): PBIO cdecl; // OpenSSL pre-0.9.6
    {09}    BIO_free                  : function(b: PBIO): Integer cdecl; // OpenSSL pre-0.9.6

    {10}    OBJ_obj2nid               : function(const o: PASN1_OBJECT): Integer cdecl; // OpenSSL pre-0.9.6

    {11}    d2i_PKCS7_bio             : function(b: PBIO; p: PPKCS7): PPKCS7 cdecl; // OpenSSL pre-0.9.6
    {12}    PEM_read_bio_PKCS7        : function(b: PBIO; x: PPPKCS7; c: PPEM_PASSWORD_CB; u: Pointer): PPKCS7 cdecl; // OpenSSL pre-0.9.6
    {13}    SMIME_read_PKCS7          : function(b: PBIO; var c: PBIO): PPKCS7 cdecl; // OpenSSL pre-0.9.6
    {14}    PKCS7_ctrl                : function(p: PPKCS7; c, l: Integer; a: PAnsiChar): Integer cdecl; // OpenSSL pre-0.9.6
    {15}    PKCS7_get_signer_info     : function(p: PPKCS7): PSTACK cdecl; // OpenSSL pre-0.9.6
    {16}    PKCS7_dataInit            : function(p: PPKCS7; b: PBIO): PBIO cdecl; // OpenSSL pre-0.9.6
    {17}    PKCS7_free                : procedure(p: PPKCS7) cdecl; // OpenSSL pre-0.9.6

    {18}    PKCS7_verify              : function(p: PPKCS7; c: PSTACK_OF_X509; s: PX509_STORE; i, o: PBIO; f: Integer): Integer cdecl; // OpenSSL pre-0.9.6
        // TODO: see PKCS7_dataVerify

    {19}    X509_STORE_new            : function: PX509_STORE cdecl; // OpenSSL pre-0.9.6
    {20}    X509_STORE_free           : procedure(s: pX509_STORE) cdecl; // OpenSSL pre-0.9.6
      );
      1: (Functions                   : Array[0..LIBEAY32_FUNCTIONS_COUNT - 1] of Pointer)
  end;

var
  mFolder     : String = '';
  mFunctions  : TLibeay32Functions;

function Load: Boolean;
var
  hLibeay32: HMODULE;
begin
  // Loads library
  hLibeay32 := GetModuleHandle(LIBEAY32_LIBRARY);
  if hLibeay32 = 0 then hLibeay32 := LoadLibrary(PChar(mFolder + LIBEAY32_LIBRARY));
  if hLibeay32 = 0 then hLibeay32 := LoadLibrary(LIBEAY32_LIBRARY);

  // Gets functions pointers
  @mFunctions.OpenSSL_add_all_digests := GetProcAddress(hLibeay32, 'OPENSSL_add_all_algorithms_noconf'); // introduced OpenSSL 0.9.7
  if @mFunctions.OpenSSL_add_all_digests = nil then
    @mFunctions.OpenSSL_add_all_digests := GetProcAddress(hLibeay32, 'OpenSSL_add_all_digests'); // OpenSSL pre-0.9.6
  // TODO: see OPENSSL_init_crypto from 1.1.0

  @mFunctions.sk_num := GetProcAddress(hLibeay32, 'sk_num');

  @mFunctions.BIO_s_mem := GetProcAddress(hLibeay32, 'BIO_s_mem');
  @mFunctions.BIO_new := GetProcAddress(hLibeay32, 'BIO_new');
  @mFunctions.BIO_new_mem_buf := GetProcAddress(hLibeay32, 'BIO_new_mem_buf');
  @mFunctions.BIO_ctrl := GetProcAddress(hLibeay32, 'BIO_ctrl');
  @mFunctions.BIO_read := GetProcAddress(hLibeay32, 'BIO_read');
  @mFunctions.BIO_write := GetProcAddress(hLibeay32, 'BIO_write');

  @mFunctions.BIO_pop := GetProcAddress(hLibeay32, 'BIO_pop');
  @mFunctions.BIO_free := GetProcAddress(hLibeay32, 'BIO_free');

  @mFunctions.OBJ_obj2nid := GetProcAddress(hLibeay32, 'OBJ_obj2nid');

  @mFunctions.X509_STORE_new := GetProcAddress(hLibeay32, 'X509_STORE_new');
  @mFunctions.X509_STORE_free := GetProcAddress(hLibeay32, 'X509_STORE_free');

  @mFunctions.d2i_PKCS7_bio := GetProcAddress(hLibeay32, 'd2i_PKCS7_bio');
  @mFunctions.PEM_read_bio_PKCS7 := GetProcAddress(hLibeay32, 'PEM_read_bio_PKCS7');
  @mFunctions.SMIME_read_PKCS7 := GetProcAddress(hLibeay32, 'SMIME_read_PKCS7');
  @mFunctions.PKCS7_ctrl := GetProcAddress(hLibeay32, 'PKCS7_ctrl');
  @mFunctions.PKCS7_get_signer_info := GetProcAddress(hLibeay32, 'PKCS7_get_signer_info');
  @mFunctions.PKCS7_dataInit := GetProcAddress(hLibeay32, 'PKCS7_dataInit');
  @mFunctions.PKCS7_free := GetProcAddress(hLibeay32, 'PKCS7_free');
  @mFunctions.PKCS7_verify := GetProcAddress(hLibeay32, 'PKCS7_verify');

  Result := Loaded();
  if Result then
    mFunctions.OpenSSL_add_all_digests()
  else
    Unload
end;

procedure SetFolder(const Value: String);
begin
  mFolder := IncludeTrailingPathDelimiter(Value)
end;

procedure Unload;
begin
  FillChar(mFunctions, SizeOf(TLibeay32Functions), 0)
end;

function GetVersion: String;
var
  hLibeay32: HMODULE;
  SSLeay_version: function(t: Integer): PAnsiChar cdecl;
begin
  Result := '';
  hLibeay32 := GetModuleHandle(LIBEAY32_LIBRARY);
  if hLibeay32 = 0 then Exit;
  @SSLeay_version := GetProcAddress(hLibeay32, 'SSLeay_version'); // OpenSSL pre-0.9.6
  if @SSLeay_version <> nil then
    Result := String(SSLeay_version(0))
end;

function GetFolder: String;
var
  hLibeay32: THandle;
  szFilename: Array[0..MAX_PATH] of Char;
begin
  Result := mFolder;
  hLibeay32 := GetModuleHandle(LIBEAY32_LIBRARY);
  if hLibeay32 = 0 then Exit;
  FillChar(szFilename, SizeOf(szFilename), 0);
  if GetModuleFileName(hLibeay32, szFilename, MAX_PATH) > 0 then
    Result := IncludeTrailingPathDelimiter(ExtractFilePath(szFilename))
end;

function Loaded(): Boolean;
var
  I: Integer;
begin
  Result := GetModuleHandle(LIBEAY32_LIBRARY) > 0;
  for I := 0 to LIBEAY32_FUNCTIONS_COUNT - 1 do
    if mFunctions.Functions[I] = nil then begin
      Result := False;
      Break
    end
end;

// =========================================================== LIBRARY FUNCTIONS

function Extract(InStream, OutStream: TStream; const Verification: TVerificationOption): Boolean;
begin
  Result := False;
  if Loaded() or Load() then
    with TPKCS7Message.Create() do try
      Result := LoadFromStream(InStream) and Verify(Verification) and SaveToStream(OutStream)
    finally
      Free()
    end
end;

function Extract(const InFilename, OutFilename: String; const Verification: TVerificationOption): Boolean;
begin
  Result := False;
  if Loaded() or Load() then
    with TPKCS7Message.Create() do try
      Result := LoadFromFile(InFilename) and Verify(Verification) and SaveToFile(OutFilename)
    finally
      Free()
    end
end;

function Extract(const Filename: String; const Verification: TVerificationOption): String;
begin
  Result := GuessFilename(Filename);
  if (Result = Filename) or (not Extract(Filename, Result)) then
    Result := ''
end;

function Extract(Stream: TStream; var S: String; const Verification: TVerificationOption): Boolean;
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  Result := Extract(Stream, StringStream, Verification);
  S := StringStream.DataString;
  StringStream.Free()
end;

function ExtractToString(const Filename: String; var S: String; const Verification: TVerificationOption): Boolean;
var
  InStream: TFileStream;
  OutStream: TStringStream;
begin
  Result := False;
  if not FileExists(Filename) then Exit;
  try
    InStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite)
  except
    Exit
  end;
  try
    InStream.Position := 0;
    OutStream := TStringStream.Create('');
    Result := Extract(InStream, OutStream, Verification);
    S := OutStream.DataString;
    OutStream.Free()
  finally
    InStream.Free()
  end
end;

function GuessFilename(const Filename: String): String;
var
  S: String;
begin
  Result := Filename;
  S := ChangeFileExt(Filename, '');
  if Length(ExtractFileExt(S)) > 0 then
    Result := S
end;

function Verify(Stream: TStream; const Verification: TVerificationOption): Boolean;
var
  P: Int64;
begin
  Result := False;
  if Stream = nil then Exit;
  P := Stream.Position;
  try
    if Loaded() or Load() then
      with TPKCS7Message.Create() do try
        Result := LoadFromStream(Stream) and Verify(Verification)
      finally
        Free()
      end
  finally
    Stream.Position := P
  end
end;

function Verify(const Filename: String; const Verification: TVerificationOption): Boolean;
begin
  Result := False;
  if Loaded() or Load() then
    with TPKCS7Message.Create() do try
      Result := LoadFromFile(Filename) and Verify(Verification)
    finally
      Free()
    end
end;

{ TPKCS7Message }

procedure TPKCS7Message.Clear;
begin
  if Loaded() then begin
    if fBIO <> nil then
      mFunctions.BIO_free(fBIO);
    // Don't dare to use BIO_free() on fData!
    if fData <> nil then
      mFunctions.BIO_pop(fData);
    if fPKCS7 <> nil then
      mFunctions.PKCS7_free(fPKCS7);
    if fStore <> nil then
      mFunctions.X509_STORE_free(fStore);
  end;
  fBIO := nil;
  fData := nil;
  fPKCS7 := nil;
  fStore := nil;
end;

constructor TPKCS7Message.Create;
begin
  inherited Create();
  fBIO := nil;
  fData := nil;
  fPKCS7 := nil;
  fStore := nil;
end;

destructor TPKCS7Message.Destroy;
begin
  Clear();
  inherited Destroy()
end;

function TPKCS7Message.LoadFromFile(const Filename: String): Boolean;
var
  FStream: TFileStream;
begin
  Result := False;
  if not FileExists(Filename) then
    Exit;
  try
    FStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite)
  except
    Exit
  end;
  try
    FStream.Position := 0;
    Result := LoadFromStream(FStream)
  finally
    FStream.Free()
  end
end;

function TPKCS7Message.LoadFromStream(Stream: TStream): Boolean;
var
  Buffer      : Array of Byte;
  iDataLength : Integer;
begin
  // Default return value
  Result := False;
  // Clears class instance variables
  Clear();
  // Parameters check
  if Stream = nil then Exit;
  if not Loaded() then Exit;

  // Verify data length
  iDataLength := Stream.Size - Stream.Position;
  if iDataLength <= 0 then Exit;

  // Allocate memory
  SetLength(Buffer, iDataLength);

  // Read from stream
  if Stream.Read(Buffer[0], iDataLength) = iDataLength then begin
    // Create BIO from this memory
    fBIO := mFunctions.BIO_new_mem_buf(Buffer, iDataLength);

    if fBIO <> nil then begin
      // Tries to read this as a .P7M file.
      fPKCS7 := mFunctions.d2i_PKCS7_bio(fBIO, nil);

      // If it failed, tries again as a .PEM file.
      if fPKCS7 = nil then begin
        // Rewinds BIO to BOF.
        if mFunctions.BIO_ctrl(fBIO, BIO_CTRL_RESET, 0, nil) = 1 then
          fPKCS7 := mFunctions.PEM_read_bio_PKCS7(fBIO, nil, nil, nil)
      end;

      // If failed again, tries as a S/MIME message.
      if fPKCS7 = nil then begin
        // Rewinds BIO to BOF.
        if mFunctions.BIO_ctrl(fBIO, BIO_CTRL_RESET, 0, nil) = 1 then
          fPKCS7 := mFunctions.SMIME_read_PKCS7(fBIO, fData)
      end
    end;
    Result := fPKCS7 <> nil
  end;

  if not Result then
    Clear()
end;

function TPKCS7Message.SaveToFile(const Filename: String): Boolean;
var
  FStream : TFileStream;
  M       : Word;
begin
  // Default return value
  Result := False;
  // File open mode
  M := fmOpenReadWrite or fmShareDenyWrite;
  if not FileExists(Filename) then M := M or fmCreate;
  // Opens file stream
  try
    FStream := TFileStream.Create(Filename, M)
  except
    Exit
  end;
  try
    // Saves to output stream
    FStream.Size := 0;
    Result := SaveToStream(FStream)
  finally
    FStream.Free()
  end
end;

function TPKCS7Message.SaveToStream(Stream: TStream): Boolean;
var
  Buffer  : Array of Byte;
begin
  // Default return value
  Result := False;
  // Parameters check
  if (fOutput = nil) or (Stream = nil) then Exit;
  if not Loaded() then Exit;

  // Allocates memory
  SetLength(Buffer, fOutput^.num_write);

  // If we do have any data
  if fOutput^.num_write > 0 then begin

    // Copies data from BIO to memory buffer.
    if mFunctions.BIO_read(fOutput, Buffer, fOutput^.num_write) <> fOutput^.num_write then
      Exit;

    // Copies data from memory to stream.
    if Stream.Write(Buffer[0], fOutput^.num_write) <> fOutput^.num_write then
      Exit
  end;

  // If we reach this point we copied the data or the data is empty.
  Result := True
end;

function TPKCS7Message.Verify(const Verify: TVerificationOption): Boolean;
var
  iRead, iTotalRead, iWritten, iTotalWritten, iFlags: Integer;
  Buffer                                            : Array[0..$1000] of Byte;
  tempBIO                                           : PBIO;
begin
  // Default return value
  Result := False;
  // Initializes what is needed
  if not Loaded() then Exit;
  if fStore = nil then
    fStore := mFunctions.X509_STORE_new();
  if fStore = nil then Exit;

  // Check that the given data was signed using PKCS#7.
  // This is a small verification also on Verification = voNone
  if mFunctions.OBJ_obj2nid(fPKCS7._type) <> NID_PKCS7_SIGNED then Exit;
  if (Pointer(mFunctions.PKCS7_ctrl(fPKCS7, 2, 0, nil)) <> nil) and (fData = nil) then Exit;

  // Reads data about signer and checks that at least one is present.
  // This is a small verification also on Verification = voNone
  fSignerStack := mFunctions.PKCS7_get_signer_info(fPKCS7);
  if (fSignerStack = nil) or (mFunctions.sk_num(fSignerStack) = 0) then Exit;

  // Creates memory structure for output data
  fOutput := mFunctions.BIO_new(mFunctions.BIO_s_mem());
  if fOutput <> nil then try

    if Verify = voNone then begin

      // Writes out extracted data.
      tempBIO := mFunctions.PKCS7_dataInit(fPKCS7, fData);
      if tempBIO <> nil then try
        // Copies data from buffer to output BIO
        iTotalRead := 0;
        iTotalWritten := 0;
        repeat
          iRead := mFunctions.BIO_read(tempBIO, @Buffer, SizeOf(Buffer));
          iTotalRead := iTotalRead + iRead;
          if iRead > 0 then begin
            iWritten := mFunctions.BIO_write(fOutput, @Buffer, iRead);
            iTotalWritten := iTotalWritten + iWritten;
          end;
        until iRead <= 0;

        Result := iTotalRead = iTotalWritten
      finally
        mFunctions.BIO_free(tempBIO)
      end;

    end else begin

      // Verifies PKCS7 while extracting it.
      iFlags := 0;
      if Verify = voVerify then
        iFlags := PKCS7_NOVERIFY;
      Result := mFunctions.PKCS7_verify(fPKCS7, nil, fStore, nil, fOutput, iFlags) = LIBEAY_OK;

    end
  finally
    if not Result then begin
      mFunctions.BIO_free(fOutput);
      fOutput := nil
    end
  end;

end;

initialization
  FillChar(mFunctions, SizeOf(TLibeay32Functions), 0);

finalization

end.
