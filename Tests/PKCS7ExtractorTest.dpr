program PKCS7ExtractorTest;

{$APPTYPE CONSOLE}

{***************************************************************************}
{                                                                           }
{     PKCS#7 Extractor for Delphi Unit Tests                                }
{     Version 1.3.0.0 released June, 15th 2021                              }
{                                                                           }
{     Copyright (C) 2018 Delphi Club Italia                                 }
{                        http://www.delphiclubitalia.it                     }
{                                                                           }
{     Original authors:                                                     }
{         Christian Cristofori              github@christiancristofori.it   }
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

uses
  Windows,
  SysUtils,
  Classes,
  PKCS7Extractor in '..\Source\PKCS7Extractor.pas';

const
  ARCHITECTURE = {$IFDEF WIN64}'x64'{$ELSE}'x86'{$ENDIF};

var
  paramList: Boolean;
  paramVersions: TStringList;
  currentFolder: String;

function GetTempFolder: String;
var
  dwLength: Cardinal;
  szPath: PChar;
begin
  Result := '';
  dwLength := GetTempPath(0, nil);
  if dwLength = 0 then Exit;
  Inc(dwLength);
  GetMem(szPath, dwLength * SizeOf(Char));
  try
    FillChar(szPath^, dwLength * SizeOf(Char), 0);
    if GetTempPath(dwLength, szPath) > 0 then
      Result := IncludeTrailingPathDelimiter(szPath)
  finally
    FreeMem(szPath, dwLength * SizeOf(Char))
  end
end;

function GetTempFile(const AFolder, APrefix: String): String;
const
  ALPHA = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
var
  S, sFolder: String;
  szFile: PChar;
  szPrefix: Array[0..3] of Char;
  I: Integer;
begin
  Result := '';
  // Checks given folder
  sFolder := IncludeTrailingPathDelimiter(AFolder);
  if (not DirectoryExists(sFolder)) and (not ForceDirectories(sFolder)) then begin
    if Length(AFolder) > 0 then Exit;
    // If no folder was given, checks for the TEMP folder
    sFolder := GetTempFolder;
    if (not DirectoryExists(sFolder)) and (not ForceDirectories(sFolder)) then Exit
  end;
  // Generates a random prefix
  FillChar(szPrefix, Length(szPrefix) * SizeOf(Char), 0);
  Randomize;
  for I := 0 to 2 do
    szPrefix[I] := ALPHA[Random(Length(ALPHA)) + 1];
  S := APrefix;
  for I := Length(S) downto 1 do
    if Pos(S[I], ALPHA) = 0 then
      Delete(S, I, 1);
  if Length(S) > 3 then
    Delete(S, 4, MaxInt);
  for I := 1 to Length(S) do
    szPrefix[I - 1] := S[I];
  // Generates the filename
  GetMem(szFile, (MAX_PATH + 1) * SizeOf(Char));
  try
    FillChar(szFile^, (MAX_PATH + 1) * SizeOf(Char), 0);
    if GetTempFileName(PChar(sFolder), szPrefix, 0, szFile) > 0 then
      Result := szFile
  finally
    FreeMem(szFile, (MAX_PATH + 1) * SizeOf(Char))
  end
end;

var
  slLibs: TStringList;

procedure SearchLibrary(const F: String);
var
  SearchRec: TSearchRec;
  R: Integer;
  N: String;
begin
  R := FindFirst(IncludeTrailingPathDelimiter(F) + '*', faAnyFile, SearchRec);
  while R = 0 do begin
    if (SearchRec.Attr and faDirectory = faDirectory) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then begin
      N := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(F) + SearchRec.Name);
      if FileExists(N + LIBEAY32_LIBRARY) and ((paramVersions.Count = 0) or (paramVersions.IndexOf(SearchRec.Name) > -1)) then
        slLibs.Add(N);
      SearchLibrary(N)
    end;
    R := FindNext(SearchRec)
  end;
  FindClose(SearchRec);
end;

var
  slTests: TStringList;

procedure SearchTests;
var
  SearchRec: TSearchRec;
  R: Integer;
begin
  R := FindFirst(currentFolder + 'Data\*', faAnyFile - faDirectory, SearchRec);
  while R = 0 do begin
    if FileExists(currentFolder + 'DataCheck\' + SearchRec.Name) then
      slTests.Add(SearchRec.Name);
    R := FindNext(SearchRec)
  end;
  FindClose(SearchRec);
end;

procedure EmptyFolder(const S: String);
var
  SearchRec: TSearchRec;
  R: Integer;
begin
  if Length(S) = 0 then Exit;
  R := FindFirst(IncludeTrailingPathDelimiter(S) + '*', faAnyFile, SearchRec);
  while R = 0 do begin
    if SearchRec.Attr and faDirectory = faDirectory then begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then begin
        EmptyFolder(IncludeTrailingPathDelimiter(S) + SearchRec.Name);
        RemoveDir(IncludeTrailingPathDelimiter(S) + SearchRec.Name)
      end
    end else
      DeleteFile(IncludeTrailingPathDelimiter(S) + SearchRec.Name);
    R := FindNext(SearchRec)
  end;
  FindClose(SearchRec);
end;

const
  VERIFY_STATUS_TO_STRING: Array[TVerifyStatus] of String = ('vsUnknown', 'vsFull', 'vsPartial');
  SIGNATURE_MODE_TO_STRING: Array[TSignatureMode] of String = ('smUnknown', 'smPKCS7', 'smCMS');
var
  tempFile, tempFolder: String;
  msInput, msOutput: TMemoryStream;
  S: String;
  I: Integer;
  V: TVerifyStatus;
  M: TSignatureMode;
begin
  try
    paramList := FindCmdLineSwitch('list', ['/', '-'], True);
    if not paramList then begin
        WriteLn('PKCS#7 Extractor library for Delphi. Copyright (C) Delphi Club Italia, 2018.');
        WriteLn;
        WriteLn('Usage:');
        WriteLn('   PKCS7ExtractorTest.exe [version-folder, ...] [/list]');
        WriteLn;
        WriteLn('Specify the names of one or more version folders inside the OpenSSL to run only those test.');
        WriteLn('Use /list to generate the list of what OpenSSL version where loaded.');
        WriteLn;
        WriteLn('This software requires four sub-folders in the folder it is run:');
        WriteLn('  Data      - put here some PKCS#7 messages files on whose you want to run tests');
        WriteLn('  DataCheck - put here the expected result files from extraction, note that the files');
        WriteLn('              needs to have the same filename and extension (thus including added extension');
        WriteLn('              that the source files in the Data folder.');
        WriteLn;
        WriteLn('  [Tests will be run only on files that have a correspondence in the two above folders!]');
        WriteLn;
        WriteLn('  OpenSSL   - put in any subfolder the OpenSSL libraries. Tests will be executed for every');
        WriteLn('              different copy of the libraries found.');
        WriteLn;
        WriteLn('  [This folder is separated in two folders: "x64" and "x32", program will work on folder');
        WriteLn('   compatible with compilatio architecture]');
        WriteLn;
        WriteLn('  Temp      - a temporary folder the program uses for saving files. This folder gets created');
        WriteLn('              automatically every time you run this program.');
        WriteLn;
        WriteLn('  [DO NOT PUT FILES INTO THIS FOLDER, THEY WILL BE DELETED]');
        WriteLn;
    end;
    tempFile := GetTempFile('', 'p7m');
    currentFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
    tempFolder := currentFolder + 'Temp\';
    if (not FileExists(tempFolder)) and (not ForceDirectories(tempFolder)) then begin
      WriteLn(Format('WARNING: inaccessible folder "%s", PKCS7Extract(Filename) will not be tested.', [tempFolder]));
      tempFolder := '';
    end;
    EmptyFolder(tempFolder);
    slLibs := TStringList.Create;
    slTests := TStringList.Create;
    try
      Write('Searching for libraries... ');
      paramVersions := TStringList.Create;
      try
        if (not paramList) and (ParamCount > 0) then
          for I := 1 to ParamCount do
            paramVersions.Add(ParamStr(I));
        SearchLibrary(currentFolder + 'OpenSSL\' + ARCHITECTURE);
      finally
        paramVersions.Free
      end;
      if slLibs.Count = 0 then begin
        WriteLn('no libraries found!');
        WriteLn(Format('Unable to continue: please be sure to put at least one libeay32.dll into folder "%sOpenSSL\" or in a subfolder.', [currentFolder]));
        Exit
      end else
        WriteLn(Format('%d libraries found.', [slLibs.Count]));

      if paramList then begin
        I := 0;
        while slLibs.Count > 0 do begin
          PKCS7Extractor.Unload;
          FreeLibrary(GetModuleHandle(LIBEAY32_LIBRARY));
          PKCS7Extractor.SetFolder(slLibs[0]);
          if PKCS7Extractor.Loaded then begin
            Inc(I);
            slTests.Add(PKCS7Extractor.GetVersion)
          end;
          slLibs.Delete(0)
        end;
        slTests.Sorted := False;
        slTests.Sorted := True;
        while slTests.Count > 0 do begin
          WriteLn(slTests[0]);
          slTests.Delete(0)
        end;
        WriteLn(Format('%d libraries successfully loaded.', [I]));
        Exit
      end;

      Write('Searching for test data... ');
      SearchTests;
      if slTests.Count = 0 then begin
        WriteLn('no test data found!');
        WriteLn(Format('Unable to continue: please be sure to put at least one file into folder "%sData\" and the result file with the same filename in the "%sDataCheck\" folder.', [currentFolder, currentFolder]));
        Exit
      end else
        WriteLn(Format('%d test(s) will be run.', [slTests.Count]));

      WriteLn('Testing PKCS7Extractor.GuessFilename(Filename)...');
      for I := 0 to slTests.Count - 1 do begin
        S := PKCS7Extractor.GuessFilename(slTests[I]);
        if S = slTests[I] then
          S := 'failed.'
        else
          S := Format('"%s"', [S]);
        WriteLn(Format('  testing with file "%s"... %s', [slTests[I], S]))
      end;

      msInput := TMemoryStream.Create;
      msOutput := TMemoryStream.Create;
      try
        while slLibs.Count > 0 do begin

          WriteLn;
          WriteLn(Format('Testing with library located at "%s".', [slLibs[0]]));

          PKCS7Extractor.Unload;
          FreeLibrary(GetModuleHandle(LIBEAY32_LIBRARY));
          PKCS7Extractor.SetFolder(slLibs[0]);
          slLibs.Delete(0);
          Write('Testing PKCS7Extractor.Load... ');
          if PKCS7Extractor.Load then
            WriteLn('successfully loaded.')
          else
            WriteLn('NOT LOADED!');

          WriteLn(Format('Testing PKCS7LibraryLocation: "%s"', [PKCS7Extractor.GetFolder]));
          WriteLn(Format('Testing PKCS7LibraryVersion: "%s"', [PKCS7Extractor.GetVersion]));

          WriteLn('Testing PKCS7Extractor.Verify(Stream)...');
          for I := 0 to slTests.Count - 1 do begin
            Write(Format('  testing with file "%s"... ', [slTests[I]]));
            msInput.Clear;
            try
              msInput.LoadFromFile(currentFolder + 'Data\' + slTests[I]);
              msInput.Position := 0
            except
              WriteLn('unable to load input file.');
              Continue
            end;
            V := PKCS7Extractor.Verify(msInput);
            if V in [vsFull, vsPartial] then
              WriteLn(Format('data corresponds to a valid PKCS#7 message file. (%s)', [VERIFY_STATUS_TO_STRING[V]]))
            else
              WriteLn('not a valid PKCS#7 message file.');
          end;

          WriteLn('Testing PKCS7Extractor.Verify(Filename)...');
          for I := 0 to slTests.Count - 1 do begin
            Write(Format('  testing with file "%s"... ', [slTests[I]]));
            V := PKCS7Extractor.Verify(currentFolder + 'Data\' + slTests[I]);
            if V in [vsFull, vsPartial] then
              WriteLn(Format('data corresponds to a valid PKCS#7 message file. (%s)', [VERIFY_STATUS_TO_STRING[V]]))
            else
              WriteLn('not a valid PKCS#7 message file.');
          end;

          WriteLn('Testing PKCS7Extractor.SignatureMode(Stream)...');
          for I := 0 to slTests.Count - 1 do begin
            Write(Format('  testing with file "%s"... ', [slTests[I]]));
            msInput.Clear;
            try
              msInput.LoadFromFile(currentFolder + 'Data\' + slTests[I]);
              msInput.Position := 0
            except
              WriteLn('unable to load input file.');
              Continue
            end;
            M := PKCS7Extractor.SignatureMode(msInput);
            if M in [smPKCS7, smCMS] then
              WriteLn(Format('data corresponds to a valid PKCS#7 message file. (%s)', [SIGNATURE_MODE_TO_STRING[M]]))
            else
              WriteLn('not a valid PKCS#7 message file.');
          end;

          WriteLn('Testing PKCS7Extractor.SignatureMode(Filename)...');
          for I := 0 to slTests.Count - 1 do begin
            Write(Format('  testing with file "%s"... ', [slTests[I]]));
            M := PKCS7Extractor.SignatureMode(currentFolder + 'Data\' + slTests[I]);
            if M in [smPKCS7, smCMS] then
              WriteLn(Format('data corresponds to a valid PKCS#7 message file. (%s)', [SIGNATURE_MODE_TO_STRING[M]]))
            else
              WriteLn('not a valid PKCS#7 message file.');
          end;

          WriteLn('Testing PKCS7Extractor.Extract(Stream, Stream)...');
          for I := 0 to slTests.Count - 1 do begin
            Write(Format('  testing with file "%s"... ', [slTests[I]]));
            msInput.Clear;
            try
              msInput.LoadFromFile(currentFolder + 'Data\' + slTests[I]);
              msInput.Position := 0
            except
              WriteLn('unable to load input file.');
              Continue
            end;

            msOutput.Clear;
            if PKCS7Extractor.Extract(msInput, msOutput) then begin
              msInput.Clear;
              try
                msInput.LoadFromFile(currentFolder + 'DataCheck\' + slTests[I])
              except
                WriteLn('file has been extracted but can''t load check file for comparision.');
                Continue
              end;
              if (msInput.Size = msOutput.Size) and CompareMem(msInput.Memory, msOutput.Memory, msInput.Size) then
                WriteLn('success.')
              else
                WriteLn('extracted data does not correspond to check file content.')
            end else
              WriteLn('not a valid PKCS#7 message file.')
          end;

          WriteLn('Testing PKCS7Extractor.Extract(Filename, Filename)...');
          for I := 0 to slTests.Count - 1 do begin
            Write(Format('  testing with file "%s"... ', [slTests[I]]));
            if PKCS7Extractor.Extract(currentFolder + 'Data\' + slTests[I], tempFile) then begin
              msInput.Clear;
              try
                msInput.LoadFromFile(currentFolder + 'DataCheck\' + slTests[I])
              except
                WriteLn('file has been extracted but can''t load check file for comparision.');
                Continue
              end;
              msOutput.Clear;
              try
                msOutput.LoadFromFile(tempFile)
              except
                WriteLn('file has been extracted but can''t load temporary output file for comparision.');
                Continue
              end;
              if (msInput.Size = msOutput.Size) and CompareMem(msInput.Memory, msOutput.Memory, msInput.Size) then
                WriteLn('success.')
              else
                WriteLn('extracted data does not correspond to check file content.')
            end else
              WriteLn('not a valid PKCS#7 message file.')
          end;

          if Length(tempFolder) > 0 then begin
            WriteLn('Testing PKCS7Extractor.Extract(Filename)...');
            for I := 0 to slTests.Count - 1 do begin
              EmptyFolder(tempFolder);
              Write(Format('  testing with file "%s"... ', [slTests[I]]));
              if CopyFile(PChar(currentFolder + 'Data\' + slTests[I]), PChar(tempFolder + slTests[I]), True) then begin
                try
                  S := PKCS7Extractor.Extract(tempFolder + slTests[I]);
                  if Length(S) > 0 then begin
                    try
                      msInput.Clear;
                      try
                        msInput.LoadFromFile(currentFolder + 'DataCheck\' + slTests[I])
                      except
                        WriteLn('file has been extracted but can''t load check file for comparision.');
                        Continue
                      end;
                      msOutput.Clear;
                      try
                        msOutput.LoadFromFile(S)
                      except
                        WriteLn('file has been extracted but can''t load temporary output file for comparision.');
                        Continue
                      end;
                      if (msInput.Size = msOutput.Size) and CompareMem(msInput.Memory, msOutput.Memory, msInput.Size) then
                        WriteLn('success.')
                      else
                        WriteLn('extracted data does not correspond to check file content.')
                    finally
                      DeleteFile(S)
                    end
                  end else
                    WriteLn('not a valid PKCS#7 message file or destination filename can''t be guessed.')
                finally
                  DeleteFile(tempFolder + slTests[I])
                end
              end else
                WriteLn('failed copying file itno temp folder.')
            end;
            EmptyFolder(tempFolder)
          end;

          WriteLn('Testing PKCS7Extractor.Extract(Stream, var String)...');
          for I := 0 to slTests.Count - 1 do begin
            Write(Format('  testing with file "%s"... ', [slTests[I]]));
            msInput.Clear;
            try
              msInput.LoadFromFile(currentFolder + 'Data\' + slTests[I]);
              msInput.Position := 0
            except
              WriteLn('unable to load input file.');
              Continue
            end;
            if PKCS7Extractor.Extract(msInput, S) then
              WriteLn(Format('success (string length %d characters).', [Length(S)]))
            else
              WriteLn('extracted data does not correspond to check file content.')
          end;

          WriteLn('Testing PKCS7Extractor.PKCS7ExtractToString(String, var String)...');
          for I := 0 to slTests.Count - 1 do begin
            Write(Format('  testing with file "%s"... ', [slTests[I]]));
            if PKCS7Extractor.ExtractToString(currentFolder + 'Data\' + slTests[I], S) then
              WriteLn(Format('success (string length %d characters).', [Length(S)]))
            else
              WriteLn('extracted data does not correspond to check file content.')
          end

        end
      finally
        msInput.Free;
        msOutput.Free
      end
    finally
      if FileExists(tempFile) then
        DeleteFile(tempFile);
      slLibs.Free;
      slTests.Free;
      EmptyFolder(tempFolder)
    end
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message)
  end;
{$IFDEF DEBUG}
  WriteLn;
  Write('Press [ENTER] key to continue... ');
  ReadLn
{$ENDIF}
end.
