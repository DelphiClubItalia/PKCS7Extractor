program PKCS7ExtractorDemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFDEF FPC}
  Interfaces,
  {$ENDIF }
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {FormMain},
  PKCS7Extractor in '..\Source\PKCS7Extractor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
