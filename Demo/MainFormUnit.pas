unit MainFormUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{***************************************************************************}
{                                                                           }
{     PKCS#7 Extractor for Delphi Demo Application                          }
{     Version 1.2.0.0 released March, 2nd 2020                              }
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

interface

uses
  Forms, Classes, Dialogs, Controls, ExtCtrls, StdCtrls, Menus, Graphics,
  PKCS7Extractor;

type
  TFormMain = class(TForm)
    bExtract: TButton;
    odInput: TOpenDialog;
    sdOutput: TSaveDialog;
    memoSource: TMemo;
    popupEmpty: TPopupMenu;
    panelLibrary: TPanel;
    pnlSubHeader: TPanel;
    pnlHeader: TPanel;
    imgDCILogo: TImage;
    lblTitle: TLabel;
    labelLocation: TLabel;
    labelVersion: TLabel;
    bBrowse: TButton;
    labelLocationL: TLabel;
    labelVersionL: TLabel;
    lblVerification: TLabel;
    lblSignatureMode: TLabel;
    lblSignatureModeValue: TLabel;
    lblVerificationValue: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ChangeLibrary(Sender: TObject);
    procedure ExtractPKCS7(Sender: TObject);
    procedure GoToWebsite(Sender: TObject);
  private
    procedure RefreshPanel;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
{$IFnDEF FPC}
  ShellAPI,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  Windows, SysUtils, ShlObj, ActiveX;

{$IFDEF FPC}
type
  BFFCALLBACK = function(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
  TFNBFFCallBack = type BFFCALLBACK;
{$ENDIF}

var
  mInitialFolder: String = ''; // NOT THREAD SAFE! (WHO CARES HERE?)

function lpfnBrowseProc(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
begin
  Result := 0;
  if (Wnd <> 0) and (uMsg = BFFM_INITIALIZED) then
    SendMessage(Wnd, BFFM_SETSELECTION, WPARAM(True), Windows.LPARAM(Pointer(mInitialFolder)))
end;

function BrowseForFolder(const AHandle: THandle; const AMessage: String; var AFolder: String): Boolean;
var
  BrowseInfo: TBrowseInfo;
  pidlStart, pidlSelected: PItemIDList;
  Malloc: IMalloc;
  szDisplayNameBuff: Array[0..MAX_PATH] of Char;
begin
  Result := False;
  mInitialFolder := AFolder;
  SHGetSpecialFolderLocation(AHandle, 0, pidlStart);
  try
    SHGetMalloc(Malloc);

    StrPCopy(szDisplayNameBuff, AMessage);
    FillChar(BrowseInfo, SizeOf(TBrowseInfo), #0);
    BrowseInfo.hwndOwner := AHandle;
    BrowseInfo.pidlRoot := pidlStart;
    BrowseInfo.pszDisplayName := szDisplayNameBuff;
    BrowseInfo.lpfn := TFNBFFCallBack(@lpfnBrowseProc);
    BrowseInfo.lParam := 0;
    BrowseInfo.lpszTitle := PChar(AMessage);
    BrowseInfo.ulFlags := $00000051;

    pidlSelected := SHBrowseForFolder(BrowseInfo);
    if pidlSelected <> nil then begin
      if SHGetPathFromIDList(pidlSelected, BrowseInfo.pszDisplayName) then
        AFolder := StrPas(BrowseInfo.pszDisplayName);
      Malloc.Free(pidlSelected);
      Result := True
    end
  finally
    Malloc.Free(pidlStart)
  end
end;

procedure TFormMain.ChangeLibrary(Sender: TObject);
var
  F: String;
begin
  F := labelLocation.Caption;
  if not DirectoryExists(F) then
    F := ExtractFilePath(Application.ExeName);
  if BrowseForFolder(Handle, 'Select a folder to load libeay32.dll from.', F) and DirectoryExists(F) then begin
    if GetModuleHandle('libeay32') > 0 then
      Application.MessageBox('WARNING: unloading a DLL can be dangerous if you use multiple libraries to manage it''s functions.'#13#10 +
        'This is a controlled example and we don''t have any other library refering to this module in memory or to it''s functions, but '+
        'you should always pay attention because references are not checked again and this will lead to serious crashes.', 'WARNING', MB_ICONEXCLAMATION);
    PKCS7Extractor.Unload;
    FreeLibrary(GetModuleHandle(LIBEAY32_LIBRARY));
    PKCS7Extractor.SetFolder(F);
    PKCS7Extractor.Load
  end;
  RefreshPanel
end;

procedure TFormMain.ExtractPKCS7(Sender: TObject);
const
  SIGNATURE_MODE: Array[TSignatureMode] of String = ('(unknown)', 'PKCS#7', 'CMS');
  VERIFY_STATUS: Array[TVerifyStatus] of String = ('(unknown/invalid)', 'full verification', 'partial verification');
var
  sFolder, sFile, sExtension, S: String;
begin
  sFolder := GetCurrentDir;
  try
    memoSource.Lines.Clear;
    lblSignatureModeValue.Caption := SIGNATURE_MODE[smUnknown];
    lblVerificationValue.Caption := '(idle)';
    if not PKCS7Extractor.Load then begin
      Application.MessageBox('Library Libeay32.dll has not been found or an error occurre while loading.', 'Extraction report', MB_ICONEXCLAMATION);
      Exit
    end;
    RefreshPanel;
    if not odInput.Execute then
      Exit;
    if Verify(odInput.FileName) = vsUnknown then begin
      Application.MessageBox('Selected file is not a supported PKCS#7 message file.', 'Extraction report', MB_ICONEXCLAMATION);
      lblVerificationValue.Caption := VERIFY_STATUS[vsUnknown];
      Exit
    end;
    sFile := ExpandFilename(ChangeFileExt(odInput.FileName, ''));
    if FileExists(sFile) then
      case Application.MessageBox(PChar(Format('File "%s" alredy exists, do you want to overwrite?', [sFile])), 'File already exists.', MB_ICONQUESTION or MB_YESNOCANCEL) of
        ID_NO: begin
          sExtension := ExtractFileExt(sFile);
          if Copy(sExtension, 1, 1) = '.' then
            Delete(sExtension, 1, 1);
          sExtension := UpperCase(sExtension);
          sdOutput.InitialDir := ExtractFilePath(sFile);
          sdOutput.FileName := ExtractFileName(sFile);
          sdOutput.Filter := 'Any file (*.*)|*.*';
          if Length(sExtension) > 0 then
            sdOutput.Filter := Format('%s file (*.%s)|*.%s|Any file (*.*)|*.*', [sExtension, sExtension, sExtension]);
          sdOutput.DefaultExt := sExtension;
          if not sdOutput.Execute then
            Exit;
          sFile := sdOutput.FileName
        end;
        ID_CANCEL: Exit
      end;
    lblVerificationValue.Caption := VERIFY_STATUS[vsUnknown];
    if Extract(odInput.FileName, sFile) then begin
      lblSignatureModeValue.Caption := SIGNATURE_MODE[SignatureMode(odInput.FileName)];
      S := '';
      if ExtractToString(odInput.FileName, S) then
        memoSource.Lines.Text := S;
      lblVerificationValue.Caption := VERIFY_STATUS[Verify(odInput.FileName)];
      Application.MessageBox(PChar(Format('Content of the PKCS#7 message file has been extracted and saved successfully to file "%s".', [sFile])), 'Extraction report', MB_ICONINFORMATION);
    end else begin
      Application.MessageBox('Error while extracting data from PKCS#7 message file.', 'Extraction report', MB_ICONEXCLAMATION)
    end
  finally
    SetCurrentDir(sFolder)
  end
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  memoSource.Lines.Clear;
  memoSource.Font.Name := 'Courier New';
  PKCS7Extractor.Load;
  RefreshPanel
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Application.Terminate
  else
    if Key = VK_F1 then
      GoToWebsite(Sender)
end;

procedure TFormMain.GoToWebsite(Sender: TObject);
begin
{$IFDEF FPC}
  OpenURL('http://www.delphiclubitalia.it')
{$ELSE}
  ShellExecute(0, 'open', 'http://www.delphiclubitalia.it', nil, nil, SW_SHOWMAXIMIZED)
{$ENDIF}
end;

procedure TFormMain.RefreshPanel;
var
  S: String;
begin
  S := PKCS7Extractor.GetFolder;
  if Length(S) = 0 then
    S := '(unknown)';
  labelLocation.Caption := S;
  S := PKCS7Extractor.GetVersion;
  if Length(S) = 0 then
    S := '(unknown)';
  labelVersion.Caption := S
end;

end.
