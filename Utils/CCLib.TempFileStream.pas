unit CCLib.TempFileStream;

{***************************************************************************}
{                                                                           }
{   Christian Cristofori's Library for Delphi                               }
{                                                                           }
{   Copyright (C) Christian Cristofori, 1998-2020.                          }
{                                                                           }
{***************************************************************************}

interface

uses
  Classes;

type
  TTempFileSteam = class(THandleStream)
  private
    FFilename: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property FileName: String read FFilename;
  end;

implementation

uses
  Windows, SysUtils;

{ TTempFileSteam }

constructor TTempFileSteam.Create;
var
  hFile: THandle;
  szFolder, szFile: Array[0..MAX_PATH] of Char;
begin
  FillChar(szFolder, SizeOf(szFolder), 0);
  if GetTempPath(MAX_PATH - 1, szFolder) = 0 then
    FillChar(szFolder, SizeOf(szFolder), 0);
  FillChar(szFile, SizeOf(szFile), 0);
  hFile := INVALID_HANDLE_VALUE;
  if GetTempFileName(szFolder, 'CCL', 0, szFile) > 0 then
    hFile := Windows.CreateFile(szFile, GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS,
      FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_RANDOM_ACCESS or FILE_FLAG_DELETE_ON_CLOSE, 0);
  if hFile = INVALID_HANDLE_VALUE then
    raise Exception.Create('Unable to create temporary file.');
  FFilename := szFile;
  inherited Create(hFile)
end;

destructor TTempFileSteam.Destroy;
begin
  CloseHandle(Handle);
  inherited Destroy
end;

end.
