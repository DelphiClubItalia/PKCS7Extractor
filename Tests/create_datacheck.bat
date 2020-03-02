@ECHO OFF
CLS
IF NOT EXIST .\OpenSSL\OpenSSL.exe (
	ECHO Unable to find .\OpenSSL\OpenSSL.exe
	GOTO TheEnd
)
.\OpenSSL\OpenSSL.exe help >NUL 2>&1
IF %ERRORLEVEL% NEQ 0 (
	ECHO Unable to execute .\OpenSSL\OpenSSL.exe, please check!
	GOTO TheEnd
)
.\OpenSSL\OpenSSL.exe version
ECHO.
REM Faccio l'operazione per tutti i file .p7m che trovo nella sottocartella Data
FOR /R ".\Data" %%f IN (*.p7m) DO (
	REM Se lo stesso file esiste nella cartella DataCheck lo elimino
	DEL /F /Q ".\DataCheck\%%~nf%%~xf" >NUL 2>&1
	REM Tento di estrarlo come S/MIME
	.\OpenSSL\OpenSSL.EXE smime -verify -noverify -in ".\Data\%%~nf%%~xf" -inform DER -out ".\DataCheck\%%~nf%%~xf" >NUL 2>&1
	REM Se il file è vuoto (lunghezza zero) lo elimino
	FOR /F %%a in (".\DataCheck\%%~nf%%~xf") DO (
		IF %%~za EQU 0 (
			DEL /F /Q %%a >NUL 2>&1
		)
	)
	REM Se il file non esiste tento di estrarlo come CMS
	IF NOT EXIST ".\DataCheck\%%~nf%%~xf" (
		.\OpenSSL\OpenSSL.EXE cms -verify -noverify -in ".\Data\%%~nf%%~xf" -inform DER -out ".\DataCheck\%%~nf%%~xf" >NUL 2>&1
	)
	REM Se il file è vuoto (lunghezza zero) lo elimino
	FOR /F %%a in (".\DataCheck\%%~nf%%~xf") DO (
		IF %%~za EQU 0 (
			DEL /F /Q %%a >NUL 2>&1
		)
	)
	REM Metto in output il risultato
	IF EXIST ".\DataCheck\%%~nf%%~xf" (
		ECHO %%~nf%%~xf extracted correctly
	) ELSE (
		ECHO.
		ECHO Could not extract %%~nf%%~xf
		ECHO.
	)
)
:TheEnd