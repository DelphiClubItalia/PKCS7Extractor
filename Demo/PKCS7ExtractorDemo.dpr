program PKCS7ExtractorDemo;

uses
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {FormMain},
  PKCS7Extractor in '..\Source\PKCS7Extractor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
