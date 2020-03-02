[english version - versione inglese](README.md)

![Libreria PKCS#7 Extractor per Delphi / Free Pascal / Lazarus](Documentation/pkcs7extractory_logo.png)
# Libreria PKCS#7 Extractor per Delphi / Free Pascal / Lazarus

Questo progetto è nato dalla necessità di estrarre semplicemente il contenuto dei file .P7M in alcune applicazioni Delphi.\
Dal 1 gennaio 2019 gli sviluppatori italiani hanno dovuto affrontare il nuovo sistema di Fatturazione Elettronica, divenuto obbligatorio per Legge.\
Le Fatture Elettroniche sono esenzialmente rappresentante in file XML che vengono firmate digitalmente utilizzando la modalità CAdES-BEM.\
Gli sviluppatori che gestiscono fatture devono quindi generare questi file XML ed essere in grado di rileggerli, ma quando si tratta di una fattura firmata digitalmente inglobata in un file .P7M molti di loro hanno trovato la soluzione poco ortodossa di rimuovere l'envelope trovando il tag di apertura e di chiusura XML all'interno del file e copiando quella parte.\
Abbiamo pensato che questo non potesse essere il modo professionale di gestire questa operazione, quindi abbiamo iniziato a studiare e siamo finiti a creare questo progetto.

## Indice

  - [Libreria PKCS#7 Extractor per Delphi / Free Pascal / Lazarus](#libreria-pkcs7-extractor-per-delphi-free-pascal-lazarus)
  - [Indice](#indice)
  - [File inclusi](#file-inclusi)
     - [PKCS7Extractor.pas](#pkcs7extractorpas)
       - [Che cosa è e come funziona](#che-cosa-e-e-come-funziona)
       - [Funzioni esportate](#funzioni-esportate)
       - [Classi esportate](#classi-esportate)
         - [Evento OnStreamCreate](#evento-onstreamcreate)
  - [Applicazione demo](#applicazione-demo)
  - [Unit tests](#unit-tests)
    - [Salvare un report dei test](#salvare-un-report-dei-test)
    - [Ottenere una lista delle librerie OpenSSL](#ottenere-una-lista-delle-librerie-openssl)
    - [Eseguire i test solo su alcune versioni di OpenSSL](#eseguire-i-test-solo-su-alcune-versioni-di-openssl)
  - [File necessari](#file-necessari)
  - [Cronologia versioni](#cronologia-versioni)
  - [Compatibilità con i compilatori](#compatibilità-con-i-compilatori)
  - [Progetti per il futuro](#progetti-per-il-futuro)
  - [Ringraziamenti](#ringraziamenti)
  - [Commenti](#commenti)

## File inclusi

### [PKCS7Extractor.pas](Source/PKCS7Extractor.pas)
### Che cosa è e come funziona
Questa è la unit principale con cui gli sviluppatori dovranno confrontarsi, esporta alcune utili funzioni per gestire i messaggi PKCS#7.

### Funzioni esportate
Ogni funzione esportata ha una sua storia così lunga che a raccontarla nessuno ci crederebbe. Siamo chiari, non è stato per nulla facile, ma da ora dovrebbe esserlo. Iniziamo con il descrivere le funzioni esportate per il wrapper delle funzioni di [OpenSSL](https://www.openssl.org).

```delphi
function Load: Boolean;
```
Richiamare la funzione `Load` per inizializzare il wrapper, questo causerà se necessario il caricamento della libreria. Se è stata precedentemente specificata una cartella valida utilizzando la procedure `SetFolder`, verrà tentato di caricare la libreria dalla cartella specificata.\
Il valore ritornato indica se il caricamento e/o collegamento della libreria è avvenuto con successo ed implica che tutte le funzioni necessarie siano state tornare.\
Chiamare questa funzione più volte non ha alcun effetto pratico a meno che non venga scaricata prima la libreria.

```delphi
function Loaded: Boolean;
```
Ritorna se il wrapper è attualmente inizializzato correttamente.

```delphi
procedure Unload;
```
La procedure `Unload` causa la deinizializzazione del wrapper ma, come detto prima, la libreria rimarrà residente in memoria.

```delphi
procedure SetFolder(const S: String);
```
Richiamare `SetFolder` prima di `Load` per impostare una cartella specifica da cui caricare la libreria. Si notic che qualora la libreria fosse già stata caricata da questo wrapper o da qualsiasi altro componente o classe, le chiamate a questa procedura non avranno alcun effetto pratico.\
Se viene specificata una cartella non valida o una cartella in cui non si trovi la libreria, verranno seguiti i normali percorsi utilizzati dal sistema operativo per il caricamento della libreria.\
Non ha alcun effetto nel caso la libreria sia linkata staticamente nell'eseguibile.

```delphi
function GetFolder: String;
```
La funzione `GetFolder` ritorna il percorso assoluto da cui la libreria è attualmente caricata in memoria oppure una stringa vuota se la libreria non è presente in memoria.\
Questo potrebbe differire dalla cartella specificata utilizzando `SetFolder` qualora la cartella specificata non sia valida o non contenga la libreria o ancora qualora la libreria fosse già precedentemente caricata.

```delphi
function GetVersion: String;
```
La funzione `GetVersion` ritorna semplicemente la stringa contenente la versione della libreria attualmente caricata in memoria o una stringa vuota qualora il wrapper non sia inizializzato.\
La funzione corrispondente è stata introdotta in [OpenSSL](https://www.openssl.org) dalla versione 0.9.7, qyuindi questa funzione ritornerà una stringa vuota con versioni precedenti.

```delphi
function GetErrorCode: Cardinal;
```
Ritorna un codice numerico che rappresenta l'ultimo errore interno della libreria OpenSSL.

```delphi
function GetError(const ErrorCode: Cardinal): String;
```
Ritorna la descrizione testuale per un dato codice di errore.

```delphi
function GetError: String;
```
Ritorna la descrizione testuale per l'ultimo errore interno della libreria OpenSSL.\
\
Prima di continuare, è necessario prendere visione di due tipi indispensabili.

```delphi
  TVerifyStatus = (vsUnknown, vsFull, vsPartial);
```

|Valore|Descrizione|
|--|--|
|`vsUnknown`|Nessun dato è stato caricato o i dati non erano validi.|
|`vsPartial`|I dati e la struttura dell'envelope sono stati verificati.|
|`vsFull`|E' stata eseguita una verifica completa dei dati.|

```delphi
  TSignatureMode = (smUnknown, smPKCS7, smCMS);
```

|Valore|Descrizione|
|--|--|
|`smUnknown`|Nessun dato è stato caricato o i dati non erano validi.|
|`smPKCS7`|I dati sono stati caricati ed erano imbustati con funzioni PKCS#7.|
|`smCMS`|I dati sono stati caricati ed erano imbustati con funzioni CMS.|

Quelle che seguono sono le funzioni di utilità.

```delphi
function Extract(InStream, OutStream: TStream): Boolean;
```
Dato un messaggio PKCS#7 presente in uno stream, ritorna il contenuto estratto ad un altro stream.\
Ritorna se l'operazione è andata a buon fine o meno.\
Si prega di notare che `InStream.Position` deve essere impostata al punto di inizio del messaggio PKCS#7 prima di richiamare questa funzione, perché la stessa abbia successo. La proprietà `Position` sarà variata al termine dell'esecuzione di questa funzione, anche se la stessa non ha prodotto un risultato positivo.\
I dati risultati - se ve ne sono - sono aggiunti in coda al contenuto corrente dello stream di output, è quindi necessario fornire uno stream vuoto prima di richiamare questa funzione per ottenere solamente il contenuto del messaggio estratto.

```delphi
function Extract(InFilename, OutFilename: String): Boolean;
```
Funzione parallela che estrae i dati da un file PKCS#7 specificato con nome file ad un altro file. Ritorna se l'operazione è avvenuta correttamente o meno.

```delphi
function Extract(Filename: String): Boolean;
```
Estrae il contenuto di un file contenente un messaggio PKCS#7 usando come file di output un nome di file calcolato automaticamente. Ritorna se l'operazione è avvenuta correttamente o meno. Si tenga conto che se il nome di file di destinazione non può essere calcolato, l'operazione fallirà.\
Per vedere come il file viene calcolato si legga della funzione `GuessFilename` qui sotto.

```delphi
function GuessFilename(Filename: String): String;
```
Prova a calcolare il nome di file di destinazione di un dato file originale, il che significa rimuovere l'estensione aggiunta (solitamente ".p7m") dal nome di file.\
Per verificare che l'estensione sia stata aggiunta invece che sostituita, se nel file risultante non dovesse esserci un'altra estensione, la funzione fallità e ritornerà il nome del file originale.

```delphi
function Verify(Stream: TStream): TVerifyStatus;
```
Funzione necessaria a verificare se un dato stream contiene o meno un messaggio PKCS#7 valido. Come per `Extract(TStream, TStream)` ci si ricordi che la proprietà `Position` dello stream di input deve essere impostato all'inizio del messaggio PKCS#7 prima di essere passato a questa funzione.\
A differenza di `Extract(TStream, TStream)` comunque, questa funzione manterrà il valore della proprietà `Position` al suo valore corrente.

```delphi
function Verify(Filename: String): TVerifyStatus;
```
Verifica semplicemente che un dato file contenga un messaggio PKCS#7 valido.

```delphi
function Extract(Stream: TStream; var S: String): Boolean;
```
Funzione per ottenere il contenuto di un messaggio PKCS#7 estratto da uno stream direttamente in una stringa. Se l'operazione fallisse il valore tornato sarebbe `False` e sarà tornata uns stringa vuota.

```delphi
function ExtractToString(Filename: String; var S: String): Boolean;
```
Funzione per ottenere il contenuto di un messaggio PKCS#7 estratto da un file direttamente in una stringa. Se l'operazione fallisse il valore tornato sarebbe `False` e sarà tornata uns stringa vuota.

```delphi
function SignatureMode(Stream: TStream): TSignatureMode; overload;
```

Ritorna il tipo di funzioni crittografiche utilizzate per imbustare dei dati. Preserva il valore della proprietà `Position` dello stream.

```delphi
function SignatureMode(const Filename: String): TSignatureMode; overload;
```

Ritorna il tipo di funzioni crittografiche utilizzate per imbustare un file.

### Classi esportate

Quasta unit esporta una classe chiamata `TPKCS7Message` che è il cuore pulsante dove il lavoro viene svolto. Qualora si preferisse lavorare con questa, ecco come funziona. Si informa che a differenza delle suddette funzioni rapide, questa classe non tenta di caricare la libreria da sola, quindi bisogna sempre ricordare di richiamare `Load()` prima di utilizzarla.

```delphi
constructor TPKCS7Message.Create;
```

Crea un'istanza della classe.

```delphi
procedure TPKCS7Message.Clear;
```

Pulisce tutte le variabili locali cancellando eventuali dati caricati.

```delphi
function TPKCS7Message.LoadFromStream(Stream: TStream): Boolean;
```

Carica un messaggio PKCS#7 da uno stream, ritorna `True` al successo.

```delphi
function TPKCS7Message.LoadFromFile(Filename: String): Boolean;
```

Carica un messaggio PKCS#7 da un file, ritorna `True` al successo.

```delphi
function TPKCS7Message.SaveToStream(Stream: TStream): Boolean;
```

Salva il contenuto precedentemente caricato con `LoadFromStream` o `LoadFromFile` su uno stream, ritorna `True` al successo.

```delphi
function TPKCS7Message.SaveToFile(Filename: String): Boolean;
```

Salva il contenuto precedenemtente caricato con `LoadFromStream` o `LoadFromFile` su un file, ritorna `True` al successo.

```delphi
property SignatureMode: TSignatureMode;
```

Proprietà in sola lettura che rappresenta il tipo di funzioni crittografiche utilizzate per imbustare i dati.

```delphi
property VerifyStatus: TVerifyStatus;
```

Proprietà in sola lettura che rappresenta il tipo di verifica che è stata effettuata sui dati.

#### Evento OnStreamCreate

La classe estrae tutto il contenuto dai dati e li memorizza durante l'uso delle funzioni `LoadFromStream` o `LoadFromFile`.
Si è scelto di memorizzare i dati in memoria RAM utilizzando un `TMemoryStream`, visto che i dati per cui era inizialmente destinata questa classe dovevano essere di dimensioni ridotte.
Tuttavia è possibile che ci sia la necessità di reindirizzare i dati estratti altrove, per questo motivo è stato definito un evento, scatenato nel momento in cui la classe ha bisogno di allocare lo spazio dove memorizzare i dati.
In questo modo è possibile redirigere i dati a qualsiasi discendente di `TStream` che sia scrivibile.

```delphi
property OnStreamCreate: TStreamCreateEvent;
```

Per esempio potremo definire nella nostra Form un evento con cui andare a creare un file temporaneo.

```delphi
type
  TMyForm = class(TForm)
    ...
    procedure FormCreate(Sender: TObject);
  private
    FPKCS7Message: TPKCS7Message;
  protected
    procedure PKCS7Stream(Sender: TObject; var AStream: TStream);
    ...

...
procedure TMyForm.PKCS7Stream(Sender: TObject; var AStream: TStream);
begin
  AStream := TFileStream.Create('temp.tmp', fmCreate or fmOpenReadWrite or fmShareDenyWrite)
end;

...

procedure TMyForm.FormCreate(Sender: TObject);
begin
  FPKCS7Message := TPKCS7Message.Create;
  FPKCS7Message.OnStreamCreate := PKCS7Stream;
...
end;
...
```

La classe si occuperà di richiamare il `Free` dello stream creato quando questo non sarà più necessario.

ADDENDUM: nella cartella "Utils" potete trovare il file [CCLib.TempFileStream.pas](Utils/CCLib.TempFileStream.pas) che non fa parte di questa libreria ma è liberamente disponibile.
Esporta una semplice classe discendente da `TStream` che crea un file temporaneo con nome casuale nella cartella temporanea di sistema, impedendone l'accesso ad altre applicazioni.
Questo file sarà eliminato automaticamente quando verrà richiamato il metodo `Free` della classe.

## Applicazione demo

L'applicazione demo contenuto nella cartella `\Demo` è un semplice estrattore che permette inoltre di cambiare la cartella da cui le librerie [OpenSSL](https://www.openssl.org) vengono caricate e selezionare un file, estrarlo verso un file di destinazione e visualizzare il suo contenuto in un controllo `TMemo`.

![Schermata della applicazione demo](Documentation/pkcs7extractory_demo.png)

## Unit tests

La unit tests contenuta nella cartella `\Tests` è un progetto di un'applicazione console che esegue tutta una serie di test sulla libreria e sul wrapper.

Il software richiede l'esistenza di alcune cartelle e file all'interno della stessa cartella in cui l'eseguibile viene lanciato, ecco una lista delle cartelle e che cosa devono contenere.

| Nome cartella | Obbligatoria? | Descrizione |
|--|--|--|
| Data | SI | Questa cartella deve contenere una serie di file sui quali i test saranno eseguiti. |
| DataCheck  | SI | In questa cartella devono essere contenuti dei file con i risultati attesi dall'estrazione dei file nella cartella "Data". Ogni file deve avere esattamente lo stesso nome file ed estensione del file sorgente. **I test verranno eseguiti solamente sui file che hanno corrispondenza nelle due cartelle.**
| OpenSSL\x32  | NO* | Questa cartella deve contenere delle sottocartelle ognuna delle quali deve contenere le librerie di [OpenSSL](https://www.openssl.org) in forma binaria. I test verranno eseguiti per ogni cartella trovata. **Inserire qui solamente librerie compilate a 32-bit** |
| OpenSSL\x64  | NO* | Stessa cosa di cui sopra, ma per le librerie 64-bit. |
| Temp  | NO** | Questa cartella è utilizzata mentre i test vengono eseguiti per salvare alcuni file. **Non inserire alcun file in questa cartella**, questa cartella verrà eliminata ogni volta che vengono eseguiti i test. |

###   
||Addendum|
|---------------|--|
|*               | Questa cartella è richiesta solamente se l'applicazione demo è compilata per questa architettura. |
|**               | Questa cartella verrà creata automaticamente dal software ogni volta che viene eseguito. |

### Salvare un report dei test
Per salvare un report dei test su un file di testo per una migliore analisi - o per smettere di perder tempo a scorrere la console - torna utile la redirezione dell'output.

     PKCS7ExtractorTest.exe >report.txt
Genererà un file chiamato "report.txt" contenente tutte le informazioni necessarie.
### Ottenere una lista delle librerie OpenSSL
Per ottenere una lista delle librerie [OpenSSL](https://www.openssl.org) presenti nelle cartelle `.\OpenSSL\x32` e/o `.\OpenSSL\x64` è possibile eseguire il programma di testo come:

     PKCS7ExtractorTest.exe /list

Questo causerà l'output delle sole versioni delle librerie che il programma è in grado di caricare. Per salvare questa lista su un file di testo, è possibile sfruttare la ridirezione dell'output eseguendo il comando:

     PKCS7ExtractorTest.exe /list >output.txt
Questo genererà un file chiamato "output.txt".

### Eseguire i test solo su alcune versioni di OpenSSL
Avendo una lunga lista di versioni diverse di [OpenSSL](https://www.openssl.org) nella cartella `.\OpenSSL` perdavamo molto tempo eseguendo i test, per questa ragione abbiamo implementato la possibilità di specificare quali versioni eseguire.\
E' sufficiente eseguire l'eseguibile specificando i nomi delle sottocartelle da cui caricare, ad esempio:

     PKCS7ExtractorTest.exe 0.9.6 openssl-0.9.8x-i386-win32 "openssl ultima"
Eseguirà i test solamente con le librerie presenti nella cartelle chiamate "0.9.6", "openssl-0.9.8x-i386-win32" e "openssl ultima".

## File necessari

Il progetto richiede le librerie [GNU Win32/64](http://gnuwin32.sourceforge.net/packages/openssl.htm) binarie per Windows.\
La versione minima supportata dovrebbe essere la 0.9.6 rilasciata il 24 settembre 2000. Non siamo in grado di testare librerie precedenti a tale versione.\
Con la versione 1.1.0 le librerie [OpenSSL](https://www.openssl.org) hanno avuto una riscrittura importante e stiamo attualmente lavorando per capire come rendere questa librerie compatibile con entrambe le versioni. Non garantiamo che questa libreria sarà in futuro compatibile con tali versioni.

Questa unit è stata testata con le seguenti versioni binarie:

| Versione | Data | x86 | x64 | Note |
|---------|----------|-----|-----|-------|
| 0.9.6 | 24 Sep 2000 | :white_check_mark: | *N/A* |  |
| 0.9.6b | 9 Jul 2001 | :white_check_mark: | *N/A* |  |
| 0.9.6i | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.6j | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| ~~0.9.6k~~ | ~~30 Sep 2003~~ | ~~NO~~ | *N/A* | **STIAMO ATTUALMENTE INVESTIGANDO SU QUESTO PROBLEMA** |
| 0.9.6l | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| ~~0.9.6m~~ | ~~17 Mar 2004~~ | ~~NO~~ | *N/A* | **STIAMO ATTUALMENTE INVESTIGANDO SU QUESTO PROBLEMA** |
| 0.9.7 | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.7a | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.7b | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.7c | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.7d | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.7e | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.7f | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.7g | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.7h | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.7i | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| ~~0.9.7j~~ | ~~04 May 2006~~ | ~~NO~~ | *N/A* | **STIAMO ATTUALMENTE INVESTIGANDO SU QUESTO PROBLEMA** |
| 0.9.7k | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.7l | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| ~~0.9.7m~~ | ~~23 Feb 2007~~ | ~~NO~~ | *N/A* | **STIAMO ATTUALMENTE INVESTIGANDO SU QUESTO PROBLEMA** |
| 0.9.8 | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.8a | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.8b | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.8c | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.8d | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.8e | 23 Feb 2007 | :white_check_mark: | *N/A* |  |
| 0.9.8f | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.8g | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.8h | 28 May 2008 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8h | 28 May 2008 | :white_check_mark: | *N/A* | Indy / IntraWeb Edition |
| 0.9.8i | 15 Sep 2008 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8j | 07 Jan 2009 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8k | 25 Mar 2009 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8l | 5 Nov 2009 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8l | 5 Nov 2009 | :white_check_mark: | *N/A* | Indy Backport |
| 0.9.8m | 25 Feb 2010 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8n | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.8o | 01 Jun 2010 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8p | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.8q | 2 Dec 2010 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8r | 8 Feb 2011 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8r | 8 Feb 2011 | :white_check_mark: | :white_check_mark: | rev.2 |
| 0.9.8s | 4 Jan 2012 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8t | 18 Jan 2012 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8u | 12 Mar 2012 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8v | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.8w | 23 Apr 2012 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8x | 10 May 2012 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8y | 5 Feb 2013 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8za | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 0.9.8zb | 6 Aug 2014 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8zc | 15 Oct 2014 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8zd | 8 Jan 2015 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8ze | 15 Jan 2015 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8zf | 19 Mar 2015 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8zg | 11 Jun 2015 | :white_check_mark: | :white_check_mark: |  |
| 0.9.8zh | 3 Dec 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0 | 29 Mar 2010 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0a | 1 Jun 2010 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0b | ??? | ??? | ??? | searching fo this |
| 1.0.0c | 2 Dec 2010 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0d | 8 Feb 2011 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0d | 8 Feb 2011 | :white_check_mark: | :white_check_mark: | rev.2 |
| 1.0.0e | 6 Sep 2011 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0f | 4 Jan 2012 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0g | 18 Jan 2012 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0h | 12 Mar 2012 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0i | 19 Apr 2012 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0j | 10 May 2012 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0k | 5 Feb 2013 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0l | 6 Jan 2014 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0m | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 1.0.0n | 6 Aug 2014 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0o | 15 Oct 2014 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0p | 8 Jan 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0q | 15 Jan 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0r | 19 Mar 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0s | 11 Jun 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.0t | 3 Dec 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1 | 14 Mar 2012 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1a | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 1.0.1b | 26 Apr 2012 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1c | 10 May 2012 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1d | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 1.0.1e | 11 Feb 2013 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1f | 6 Jan 2014 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1g | 7 Apr 2014 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1h | 5 Jun 2014 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1i | 6 Aug 2014 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1j | 15 Oct 2014 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1k | 8 Jan 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1l | 15 Jan 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1m | 19 Mar 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1o | 12 Jun 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1p | 9 Jul 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1q | 3 Dec 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1r | 28 Jan 2016 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1s | 1 Mar 2016 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1t | 3 May 2016 | :white_check_mark: | :white_check_mark: |  |
| 1.0.1u | 22 Sep 2016 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2 | 22 Jan 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2a | 19 Mar 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2b | ??? | ??? | ??? | siamo alla ricerca di questa versione |
| 1.0.2c | 12 Jun 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2d | 9 Jul 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2e | 3 Dec 2015 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2f | 28 Jan 2016 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2g | 1 Mar 2016 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2h | 3 May 2016 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2i | 22 Sep 2016 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2j | 26 Sep 2016 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2k | 26 Jan 2017 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2l | 25 May 2017 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2m | 2 Nov 2017 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2n | 7 Dec 2017 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2o | 27 Mar 2018 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2p | 14 Aug 2018 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2q | 20 Nov 2018 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2r | 26 Feb 2019 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2s | 28 May 2019 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2t | 10 Sep 2019 | :white_check_mark: | :white_check_mark: |  |
| 1.0.2u | 20 Dec 2019 | :white_check_mark: | :white_check_mark: |  |

La maggior parte di queste versioni binarie è stata trovata su [indy.fulgan.com](https://indy.fulgan.com/SSL/Archive).\
Siamo attualmente alla ricerca delle versioni binarie che ci mancano quindi fateci sapere qualora doveste trovarne qualcosa, saremo ben lieti di aggiungerle ai nostri test.\
Ogni informazioni riguardante problemi di compatibilità sarà benvenuto.
Si è deciso di ignorare le versioni 1.1.0+ da questa tabella, così come le versioni beta che non sono adatte alla distribuzione.

## Cronologia versioni

- Versione 1.2.0.0 rilasciata il 2 marzo 2020
  - aggiunta la gestione dei file firmati utilizzando le funzioni di crittografia CMS (richiede librerie OpenSSL 0.9.8h o successive)
  - ora compatibile con Lazarus / Free Pascal
  - il sorgente non è compatibile con compilatori precedenti a Delphi 6
  - tutta l'elaborazione è stata sposta in LoadFromStream, utilizza meno variabili di classe
  - TPKCS7Message.Verify è stata sostituita dalla proprietà TPKCS7Message.VerifyStatus
  - nuova proprietà TPKCS7Message.SignatureMode
  - nuove funzioni veloci SignatureMode(Stream) and SignatureMode(String)
  - nuovo evento TPKCS7Message.OnStreamCreate che consente di modificare la classe TStream da utilizzare
  - Extract(Stream, S) e ExtractToString ora gestiscono i BOM e le codifiche (Delphi 2009+)
  - aggiunte le funzioni GetErrorCode, GetError(Cardinal) e GetError
  - non è più necessario specificare le opzioni di verifica
  - non lascia un file vuoto quando si crea un nuovo file con SaveToFile e fallisce
  - spostate tutte le noiose definizioni delle strutture nell'implementation
  - evitato hint del compilatore Delphi (ne persistono alcuni molto sciocchi su Free Pascal / Lazarus)
  - aggiunto il file [CCLib.TempFileStream.pas](Utils/CCLib.TempFileStream.pas)
  - testato con le librerie OpenSSL versioni 1.0.2q, 1.0.2r, 1.0.2s, 1.0.2t e 1.0.2u 
  - risolti bug minori e riscritto parte di codice, rimosso lo zucchero sintattico
  - aggiunto file batch per la creazione automatica dei risultati attesi dei test
  - aggiornati README.md, LEGGIMI.md e lo screenshot della demo
  - ripristinato file dell'icona DCI nella cartella della demo

- Versione 1.0.0.0 rilasciata il 27 novembre 2018
  - primo rilascio pubblico
  - correzione di errori minori
  - pulizia del codice ed aggiunta commenti
  - provato con alcune versioni in più di OpenSSL

- Versione 0.9.5.0 - 26 novembre 2018
  - correzione errori

- Versione 0.9.3.0 - 25 novembre 2018
  - la verifica supporta ora la verifica della firma
  - aggiunto il parametro per la verifica ad ogni funzione

- Versione 0.9.0.0 - 25 novembre 2018
  - completamente modificata la struttura della libreria
  - ora è necessario un solo file
  - eliminate le due classi statiche `Libeay32Wrapper` e `PKCS7Message`
  - introdotta una classe `TPKCS7Message`, le funzioni rapide usano tale classe

- Versione 0.8.2.0 - 24 novembre 2018
  - rimossa `PKCS7LibraryLocation` da `PKCS7Extractor.pas` utilizzare `Libeay32Wrapper.GetLibraryFolder`
  - rimossa `PKCS7LibraryVersion` da `PKCS7Extractor.pas` utilizzare `Libeay32Wrapper.SSLeay_version`

- Versione 0.8.0.0 - 24 novembre 2018
  - aggiunta compatibilità x64

- Versione 0.7.0.0 - 23 novembre 2018
  - rimossa la funzione `Loaded` da `Libeay32Wrapper`, utilizzare `Load`
  - riscritta completamente la funzione `Load` in `Libeay32Wrapper`
  - aggiunta funzione `GetLibraryFolder` per avere la posizione della libreria caricata
  - `SSLeay_version` non è più una semplice replica della funzione omonima esportata da [OpenSSL](https://www.openssl.org) ma una funzione più utile che ritorna una stringa contenente la versione

- Versione 0.5.0.0 - 23 novembre 2018
  - abbandonata la dipendenza dal progetto [Indy](https://www.indyproject.org), utilizza ora un proprio wrapper minimale
  - rimossa la funzione `PKCS7Check` da `PKCS7Extractor.pas` utilizzare le funzioni `Libeay32Wrapper.Load` e `.Loaded`
  - aggiunto il file `Libeay32Wrapper.pas`

- Versione 0.2.1.0 - 22 novembre 2018
  - aggiunta la funzione `PKCS7GuessFilename`
  - la funzione `PKCS7Extract(Filename)` ritorna ora il nome di destinazione o una stringa vuota
  - compatibile con Delphi 2007

- Versione 0.2.0.0 - 22 novembre 2018
  - aggiunto `PKCS7ExtractorTest.dpr`
  - aggiunta la funzione `PKCS7Check`
  - aggiunta la funzione `PKCS7LibraryLocation`
  - aggiunta la funzione `PKCS7LibraryVersion`
  - aggiunta la funzione `IsPKCS7`

- Versione 0.1.1.0 - 21 novembre 2018
  - risolta mancante inizializzazione della Libeay32.dll

- Versione 0.1.0.0 - 20 novembre 2018
  - prima versione funzionante

## Compatibilità con i compilatori

Vorremmo essere in grado di raggiungere la piena compatiblità con ogni compilatore Delphi (magari anche con [Free Pascal](https://www.freepascal.org)), ma abbiamo bisogno che altri ci aiutino a testare il codice con compilatori di cui non abbiamo la licenza. Per ogni compilatore non indicato come compatibile stiamo aspettando che qualcuno si metta in contatto con noi per inviarci i test compilati con quel compilatore.

|     | Compilatore | S.O. | Architettura | Versione | Tester/Note |
|:---:|-------------|:----:|:------------:|---------:|-------------|
| :x: | [Borland](https://www.embarcadero.com) Delphi 1 | :mount_fuji: | *N/A* |  | No 32-bit support. |
| :x: | [Borland](https://www.embarcadero.com) Delphi 2 | :mount_fuji: | x86 | 1.2.0.0 | Christian Cristofori |
| :x: | [Borland](https://www.embarcadero.com) Delphi 3 | :mount_fuji: | x86 | 1.2.0.0 | Non *dovrebbe* funzionare. |
| :x: | [Borland](https://www.embarcadero.com) Delphi 4 | :mount_fuji: | x86 | 1.2.0.0 | Non *dovrebbe* funzionare. |
| :x: | [Borland](https://www.embarcadero.com) Delphi 5 | :mount_fuji: | x86 | 1.2.0.0 | Non *dovrebbe* funzionare. |
| :white_check_mark: | [Borland](https://www.embarcadero.com) Delphi 6 | :mount_fuji: | x86 |  |  |
| :white_check_mark: | [Borland](https://www.embarcadero.com) Delphi 7 | :mount_fuji: | x86 | 1.2.0.0 | Marcello Gribaudo |
| :thought_balloon: | [Borland](https://www.embarcadero.com) Delphi 2005 | :mount_fuji: | x86 |  |  |
| :thought_balloon: | [Borland](https://www.embarcadero.com) Delphi 2006 | :mount_fuji: | x86 |  |  |
| :thought_balloon: | Turbo Delphi 2006 | :mount_fuji: | x86 |  |  |
| :white_check_mark: | [CodeGear](https://www.embarcadero.com) Delphi 2007 | :mount_fuji: | x86 | 1.2.0.0 | Christian Cristofori |
| :thought_balloon: | [Embarcadero](https://www.embarcadero.com) Delphi 2009 | :mount_fuji: | x86 |  |  |
| :thought_balloon: | [Embarcadero](https://www.embarcadero.com) Delphi 2010 | :mount_fuji: | x86 |  |  |
| :thought_balloon: | [Embarcadero](https://www.embarcadero.com) Delphi XE | :mount_fuji: | x86 |  |  |
| :thought_balloon: | [Embarcadero](https://www.embarcadero.com) Delphi XE2 | :mount_fuji: | x86 |  |  |
| :thought_balloon: |  | :mount_fuji: | x64 |  |  |
| :thought_balloon: | [Embarcadero](https://www.embarcadero.com) Delphi XE3 | :mount_fuji: | x86 |  |  |
| :thought_balloon: |  | :mount_fuji: | x64 |  |  |
| :thought_balloon: | [Embarcadero](https://www.embarcadero.com) Delphi XE4 | :mount_fuji: | x86 |  |  |
| :thought_balloon: |  | :mount_fuji: | x64 |  |  |
| :thought_balloon: | [Embarcadero](https://www.embarcadero.com) Delphi XE5 | :mount_fuji: | x86 |  |  |
| :thought_balloon: |  | :mount_fuji: | x64 |  |  |
| :thought_balloon: | [Embarcadero](https://www.embarcadero.com) Delphi XE6 | :mount_fuji: | x86 |  |  |
| :thought_balloon: |  | :mount_fuji: | x64 |  |  |
| :white_check_mark: | [Embarcadero](https://www.embarcadero.com) Delphi XE7 | :mount_fuji: | x86 | 1.2.0.0 | Diego Rigoni |
| :white_check_mark: |  | :mount_fuji: | x64 | 1.0.0.0 | Diego Rigoni |
| :thought_balloon: | [Embarcadero](https://www.embarcadero.com) Delphi XE8 | :mount_fuji: | x86 |  |  |
| :thought_balloon: |  | :mount_fuji: | x64 |  |  |
| :thought_balloon: | [Embarcadero](https://www.embarcadero.com) Delphi 10 Seattle | :mount_fuji: | x86 |  |  |
| :thought_balloon: |  | :mount_fuji: | x64 |  |  |
| :white_check_mark: | [Embarcadero](https://www.embarcadero.com) Delphi 10.1 Berlin | :mount_fuji: | x86 | 1.2.0.0 | Gianni Giorgetti |
| :white_check_mark: |  | :mount_fuji: | x64 | 1.0.0.0 | Christian Cristofori |
| :white_check_mark: | [Embarcadero](https://www.embarcadero.com) Delphi 10.2 Tokyo | :mount_fuji: | x86 | 1.0.0.0 | Christian Cristofori |
| :white_check_mark: |  | :mount_fuji: | x64 | 1.0.0.0 | Christian Cristofori |
| :white_check_mark: | [Embarcadero](https://www.embarcadero.com) Delphi 10.3 Rio | :mount_fuji: | x86 | 1.2.0.0 | Christian Cristofori |
| :white_check_mark: |  | :mount_fuji: | x64 | 1.2.0.0 | Christian Cristofori |
| :white_check_mark: | [Lazarus](https://www.lazarus-ide.org) 2.0.6 [Free Pascal](https://freepascal.org) 3.0.4 | :mount_fuji: | x86 | 1.2.0.0 | Christian Cristofori |
| :thought_balloon: |  | :mount_fuji: | x64 |  |  |
| :x: |  | :penguin: | x86 | \*1.1.0.0 | Christian Cristofori |
| :thought_balloon: |  | :penguin: | x64 |  |  |

\*: la versione 1.1.0.0 non è mai stata più che una banale versione alpha.

## Progetti per il futuro

- Gestione della firma per verificarne la validità.
- Gestione dello store dei certificati per selezionare quali CA considerare autoritative.
- Rendere questa unit compatibile con ogni versione di Delphi.
- Ottenere i risultati dei test in tutte le versioni di Delphi, gestire una rete di volontari in grado di fornire nuovi test per ogni nuova release.
- Compatibilità con ogni possibile versione di [OpenSSL](https://www.openssl.org).

## Ringraziamenti

- [Delphi Club Italia](http://www.delphiclubitalia.it) - [Pagina Facebook](https://www.facebook.com/groups/delphiclubitalia)
- [Christian Cristofori](https://github.com/zizzo81)
- [Giancarlo Oneglio](http://www.onix.it)
- [Diego Rigoni](mailto:diego@gdpitalia.com)
- [Gianni Giorgetti](https://www.g3cube.net)
- [Marcello Gribaudo](mailto:marcello.gribaudo@opigi.com)

## Commenti
Qualsiasi suggerimento, contributo o commento sarà veramente apprezzato.
