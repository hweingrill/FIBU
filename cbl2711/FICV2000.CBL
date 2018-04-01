      $SET LINKCOUNT"450" ANS85 BOUND NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FIBUCONV.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KONSOLD  ASSIGN "XIBUKONS.DAT"
                           ORGANIZATION RELATIVE, ACCESS DYNAMIC
                           RELATIVE KEY WH-KEY
                           FILE STATUS WF-STATUS.
           SELECT BUCHOLD  ASSIGN "XIBUZEIL.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           LOCK MODE EXCLUSIVE
                           RECORD KEY OB-KEY
                           ALTERNATE KEY IS OB-JKEY
                           FILE STATUS WF-STATUS.
           SELECT OPOLD    ASSIGN "XIBUOPES.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           LOCK MODE EXCLUSIVE
                           RECORD KEY AO-KEY
                           ALTERNATE KEY AO-AKEY DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT AUSOLD   ASSIGN "XIBUAUSZ.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           LOCK MODE EXCLUSIVE
                           RECORD KEY OZ-KEY
                           FILE STATUS WF-STATUS.
           SELECT ZINSOLD  ASSIGN TO "XIBUZINS.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY OI-BZEIL
                           ALTERNATE KEY OI-KEY DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT ZAHLOLD  ASSIGN TO "XIBUZAHL.DAT"
                           ORGANIZATION INDEXED ACCESS DYNAMIC
                           RECORD KEY ZO-KEY
                           ALTERNATE RECORD ZO-AKEY
                           FILE STATUS IS WF-STATUS.
           SELECT DEBOLD   ASSIGN WN-DEBI
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY OD-KEY
                           ALTERNATE KEY OD-MCODE DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT KREDOLD  ASSIGN WN-KRED
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY OR-KEY
                           ALTERNATE KEY OR-MCODE DUPLICATES
                           FILE STATUS WF-STATUS.
      *-----------------------------------------------> neuen Dateien <-
           SELECT KONSTANT ASSIGN WN-KONS
                           ORGANIZATION RELATIVE, ACCESS DYNAMIC
                           RELATIVE KEY WH-KEY
                           FILE STATUS WF-STATUS.
           SELECT OPDATEI  ASSIGN WN-OPES
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY OP-KEY
                           ALTERNATE KEY OP-AKEY DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT BUCHZEIL ASSIGN WN-BZEI
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY BZ-KEY
                           ALTERNATE KEY IS BZ-JKEY
                           FILE STATUS WF-STATUS.
           SELECT AUSZIFF  ASSIGN WN-AUSZ
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY AZ-KEY
                           FILE STATUS WF-STATUS.
           SELECT ZINSEN   ASSIGN TO "FIBUZINS.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY ZI-BZEIL
                           ALTERNATE KEY ZI-KEY DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT ZAHLPOS  ASSIGN TO "FIBUZAHL.DAT"
                           ORGANIZATION INDEXED ACCESS DYNAMIC
                           RECORD KEY ZP-KEY
                           ALTERNATE RECORD ZP-AKEY
                           FILE STATUS IS WF-STATUS.
           SELECT DEBITOR  ASSIGN WN-DEBI
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY DE-KEY
                           ALTERNATE KEY DE-MCODE DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT KREDIT   ASSIGN WN-KRED
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY KR-KEY
                           ALTERNATE KEY KR-MCODE DUPLICATES
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
      ************************************* Unternehmerstammdaten FIBU *
       FD  KONSOLD      external       LABEL RECORD STANDARD.
      *------------------------------------------------> WH-Key = 1 <-
       01  OK-SATZ.
           03  OK-NUM                  PIC 999     COMP-3.
           03  OK-FIRMA                PIC X(22).
           03  OK-UST                  PIC 99V99   COMP    OCCURS 6.
           03  OK-JONUM                PIC 9(6)    COMP.
           03  OK-PER                  PIC 9(6)    COMP.
           03  OK-ZTAG                 PIC 9(6)    COMP.
        02 OK-REST-1.
           03  OK-ZPZ                  PIC 999V99  COMP.
           03  OK-SPERRE               PIC X(6).
           03  OK-ORT                  PIC X(15).
           03  OK-MERK                 PIC XXXX.
           03  OK-KST                  PIC 99      COMP.
           03  OK-ANR.
               05 OK-AN                PIC X(9)            OCCURS 7.
           03  OK-TABK.
               05 OK-KONDIT            PIC 9(13)   COMP-3  OCCURS 9.
      *------------> Schleppsummenspeicher WH-KEY = 2  user + 1 (1-5) <-
       01  OK-BSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  OK-BUDAT                PIC 9(6)     COMP.
           03  OK-JOKEY                PIC X(5).
           03  OK-SASUM.
               05 OK-DEBSUM            PIC S9(9)V99 COMP  OCCURS 6.
               05 OK-KREDSUM           PIC S9(9)V99 COMP  OCCURS 6.
               05 OK-JSOLL             PIC S9(9)V99 COMP.
               05 OK-JHABEN            PIC S9(9)V99 COMP.
               05 OK-VST               PIC S9(9)V99 COMP.
               05 OK-MWST              PIC S9(9)V99 COMP.
               05 OK-MWSTBMG           PIC S9(9)V99 COMP  OCCURS 9.
               05 OK-SKTOERL           PIC S9(9)V99 COMP  OCCURS 2.
               05 OK-SKTOAUF           PIC S9(9)V99 COMP  OCCURS 6.
               05 OK-SKTOVST           PIC S9(9)V99 COMP.
               05 OK-REST              PIC S9(9)V99 COMP  OCCURS 2.
           03  OK-RS REDEFINES
               OK-SASUM                PIC S9(9)V99 COMP  OCCURS 36.
           03  OK-SPERR                PIC 99       COMP.
      *---------------------------------> zwingende Konten WH-KEY = 9 <-
       01  OK-TSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  OK-KTONR                PIC 9(6)     COMP   OCCURS 20.
           03  OK-WIJ                            OCCURS 4.
               05 OK-WKZ               PIC 99       COMP.
               05 OK-WBEG              PIC 9(6)     COMP.
               05 OK-WEND              PIC 9(6)     COMP.
           03  OK-ABDAT                PIC 9(6)     COMP.
           03  OK-EURO                 PIC 9(8)     COMP.
           03  OK-FILLER               PIC 99       COMP.
           03  OK-DRU                  PIC 99       COMP.
      *-----------------------------------------> Symbole WH-KEY = 10 <-
       01  OK-MSATZ.
           03  FILLER                  PIC X(5).
           03  OK-TS.
               05 OK-SYM               PIC XX            OCCURS 20.
               05 OK-SKZ               PIC 9999    COMP  OCCURS 20.
      *-----------------------------> Druckerseinstellung WH-KEY = 11 <-
       01  KOD-SATZ.
           03  FILLER                  PIC X(2).
           03  KOD-VERZ                PIC X(36).
           03  KOD-APAR                PIC X(45).
           03  KOD-BPAR                PIC X(45).
      *---------------------------------------------> Druckerparamter <-
       01  OK-PSATZ.
           03  FILLER                  PIC XX.
           03  KOP-BEZ                 PIC X(26).
           03  KOP-TX.
               05 KOP-STG              PIC X(10)     OCCURS 10.
      *-----------------------> WH-KEY = 68 Konten fÅr GetrÑnkeabgabe <-
       01  OK-GSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  OK-GKTO                 PIC 9(6)     COMP   OCCURS 32.
           03  OK-GPZ                  PIC 99V9999  COMP   OCCURS 32.
      *---------------------> WH-KEY = 69/67 Konten fÅr Ustverprobung <-
       01  OK-USATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  OK-UKTO                 PIC 9(6)     COMP   OCCURS 40.
           03  OK-UKZ                  PIC 99       COMP   OCCURS 40.
      *-----------------> WH-KEY = 70 eigene Adresse fÅr BankÅberwsg. <-
       01  OK-FSATZ.
           03  FILLER                  PIC X(18).
           03  OK-FBEZ                 PIC X(110).
      *--------------------------------------> Hausbanken Key 71 - 79 <-
       01  OKB-SATZ.
           03  OKB-NUM                  PIC 999      COMP-3.
           03  OKB-KTONR                PIC X(15).
           03  OKB-BEZ                  PIC X(29).
           03  OKB-ART                  PIC 99       COMP.
           03  OKB-BLZ                  PIC 9(5)     COMP.
           03  OKB-FIBKTO               PIC 9(6)     COMP.
      ************************************* Journal- u. Buchungszeilen *
       FD  BUCHOLD      external       RECORD VARYING FROM 68 TO 118
                                       DEPENDING ON WZ-BUCH.
       01  OB-SATZ.
           03  OB-KEY.
               05 OB-KTONR             PIC 9(6)      COMP.
               05 OB-BUDAT             PIC 9(6)      COMP.
               05 OB-LFD               PIC 99        COMP-X.
           03  OB-JKEY.
               05 OB-USE               PIC 99        COMP.
               05 OB-SEITE             PIC 9(7)      COMP-X.
           03  OB-BELNR                PIC 9(6)      COMP.
           03  OB-BELDAT               PIC 9(6)      COMP.
        02 OB-REST.
           03  OB-REFNR                PIC 9(7)      COMP.
           03  OB-SYM                  PIC 99        COMP.
           03  OB-GGKTO                PIC 9(6)      COMP.
           03  OB-SH                   PIC 9         COMP.
           03  OB-BUBET                PIC S9(9)V99  COMP-3.
           03  OB-Z                    PIC 9         COMP.
           03  OB-VM                   PIC 9         COMP.
           03  OB-MW                   PIC S9(7)V99  COMP-3.
           03  OB-U1                   PIC 99        COMP.
           03  OB-B1                   PIC S9(9)V99  COMP-3.
           03  OB-U2                   PIC 99        COMP.
           03  OB-B2                   PIC S9(9)V99  COMP-3.
           03  OB-KSTNR                PIC 9(5)      COMP.
           03  OB-KURS                 PIC 9(7)V9999 COMP-3.
           03  OB-KOND                 PIC 9(13)     COMP.
           03  OB-OPLFD                PIC 99        COMP-X.
           03  OB-TX                   PIC X(50).
      *----------------------------------------> Jahresabschlu·summen <-
       01  OB-ASATZ.
           03  FILLER                  PIC X(50).
           03  OB-SOLL                 PIC S9(9)V99  COMP-3.
           03  OB-HABEN                PIC S9(9)V99  COMP-3.
           03  OB-EBIL                 PIC S9(9)V99  COMP-3.
      ***************************************************** OP - Datei *
       FD  OPOLD        external       LABEL RECORD STANDARD.
       01  AO-SATZ.
           03  AO-AKEY.
               05 AO-SA                PIC 99         COMP.
               05 AO-KEY.
                  07 AO-DK             PIC 99         COMP.
                  07 AO-KTONR          PIC 9(6)       COMP.
                  07 AO-VALDAT         PIC 9(6)       COMP.
                  07 AO-LFD            PIC 99         COMP-X.
           03  AO-REF                  PIC 9(7)       COMP.
           03  AO-RENUM                PIC 9(7)       COMP.
           03  AO-REDAT                PIC 9(6)       COMP.
           03  AO-SYM                  PIC 99         COMP.
           03  AO-ZUSTAND              PIC 9          COMP.
           03  AO-KOND                 PIC 9(13)      COMP.
           03  AO-MW                   PIC S9(7)V99   COMP-3.
           03  AO-U1                   PIC 99         COMP.
           03  AO-B1                   PIC S9(9)V99   COMP-3.
           03  AO-U2                   PIC 99         COMP.
           03  AO-B2                   PIC S9(9)V99   COMP-3.
           03  AO-REBETRAG             PIC S9(9)V99   COMP-3.
           03  AO-ZABETRAG             PIC S9(9)V99   COMP-3.
           03  AO-SKTOBAS              PIC S9(9)V99   COMP-3.
           03  AO-ZADAT                PIC 9(6)       COMP.
           03  AO-ZPBET                PIC S9(9)V99   COMP-3.
           03  AO-ZPSKTO               PIC S9(7)V99   COMP-3.
           03  AO-FW-KURS              PIC 9(4)V9999  COMP.
           03  AO-MSTUFE               PIC 9          COMP.
           03  AO-MAHNDAT              PIC 9(6)       COMP.
           03  AO-ZESS                 PIC 99         COMP.
           03  AO-FREMDNR              PIC X(15).
      ********************************************* Auszifferungsdatei *
       FD  AUSOLD       external       LABEL RECORD STANDARD
                                       RECORD VARYING FROM 20 TO 657
                                       DEPENDING ON WZ-AUSZ.
       01  OZ-SATZ.
           03  OZ-KEY.
               05 OZ-SA                PIC 99        COMP.
               05 OZ-TKEY.
                  07 OZ-KTONR          PIC 9(6)      COMP.
                  07 OZ-BUDAT          PIC 9(6)      COMP.
                  07 OZ-LFD            PIC 99        COMP-X.
         02    OZ-REST.
           03  OZ-RETAB       OCCURS 50.
               05 OZ-OPNR              PIC 9(7)      COMP.
               05 OZ-OPSYM             PIC 99        COMP.
               05 OZ-OPBET             PIC S9(8)V99  COMP.
               05 OZ-OPSKTO            PIC S9(7)V99  COMP.
      **************************************************** Zinsendatei *
       FD  ZINSOLD                      LABEL RECORD STANDARD.
       01  OI-SATZ.
           03  OI-KEY.
               05 OI-NUM               PIC 9(6)       COMP.
               05 OI-SA                PIC 99         COMP.
               05 OI-VALDAT            PIC 9(6)       COMP.
         02 OI-REST.
           03  OI-BELDAT               PIC 9(6)       COMP.
           03  OI-BELNR                PIC 9(7)       COMP.
           03  OI-SYM                  PIC 99         COMP.
           03  OI-BET                  PIC S9(7)V99   COMP.
           03  OI-BZEIL.
               05 OI-USE               PIC 99         COMP.
               05 OI-SEITE             PIC 9(7)       COMP-X.
      ********************************************* Zahlungspositionen *
       FD  ZAHLOLD      external       label record standard.
       01  ZO-SATZ.
           03  ZO-AKEY.
               05 ZO-BAKEY             PIC 999        COMP-3.
               05 ZO-KEY.
                  07 ZO-DK             PIC 99         COMP.
                  07 ZO-KTONR          PIC 9(6)       COMP.
                  07 ZO-VALDAT         PIC 9(6)       COMP.
                  07 ZO-LFD            PIC 99         COMP.
         02 ZO-REST.
           03  ZO-BETRAG               PIC S9(7)V99   COMP.
           03  ZO-SKTO                 PIC S9(7)V99   COMP.
           03  ZO-EMPFG                PIC X(52).
           03  ZO-BLZ                  PIC 9(5)       COMP.
           03  ZO-VERWEND              PIC X(60).
           03  ZO-DATUM                PIC 9(6)       COMP.
           03  ZO-REDAT                PIC 9(6)       COMP.
           03  ZO-BAKTO                PIC X(15).
           03  ZO-BABEZ                PIC X(30).
      ******************************************************************
       FD  DEBOLD       external       LABEL RECORDS STANDARD.
       01  OD-SATZ.
           03  OD-KEY.
               05 OD-KTONR             PIC 9(6)      COMP.
           03  OD-MCODE                PIC X(8).
           03  OD-KTOART               PIC 99        COMP.
           03  OD-OPKZ                 PIC 99        COMP.
           03  OD-ANREDE               PIC 99        COMP.
           03  OD-SOLL                 PIC S9(9)V99  COMP-3.
           03  OD-HABEN                PIC S9(9)V99  COMP-3.
           03  OD-UMSATZ               PIC S9(9)V99  COMP-3.
           03  OD-KOND                 PIC 9(13)     COMP.
           03  OD-FWKZ                 PIC 99        COMP.
           03  OD-MAHNT                PIC 999       COMP.
           03  OD-MAHNDAT              PIC 9(6)      COMP.
           03  OD-BEZ                  PIC X(110).
           03  OD-TEL                  PIC X(35).
      *****************************************************************
       FD  KREDOLD      external       LABEL RECORD STANDARD.
       01  OR-SATZ.
           03  OR-KEY.
               05  OR-KTONR            PIC 9(7)      COMP.
           03  OR-MCODE                PIC X(8).
           03  OR-KTOART               PIC 99        COMP.
           03  OR-OPKZ                 PIC 99        COMP.
           03  OR-SOLL                 PIC S9(9)V99  COMP-3.
           03  OR-HABEN                PIC S9(9)V99  COMP-3.
           03  OR-UMSATZ               PIC S9(9)V99  COMP-3.
           03  OR-UEWTRKZ              PIC 99        COMP.
           03  OR-KOND                 PIC 9(13)     COMP.
           03  OR-FWKZ                 PIC 99        COMP.
           03  OR-ZUS                  PIC 99        COMP.
           03  OR-BANKKTO              PIC X(13).
           03  OR-BANKLTZ              PIC X(6).
           03  OR-BEZ                  PIC X(110).
           03  OR-BANK                 PIC X(24).
      ************************************* Unternehmerstammdaten FIBU *
       FD  KONSTANT     external       LABEL RECORD STANDARD.
      *------------------------------------------------> WH-Key = 1 <-
       01  KO-SATZ.
           03  KO-NUM                  PIC 999     COMP-3.
           03  KO-FIRMA                PIC X(22).
           03  KO-UST                  PIC 99V99   COMP    OCCURS 6.
           03  KO-JONUM                PIC 9(6)    COMP.
           03  KO-PER                  PIC 9(8)    COMP.
           03  KO-ZTAG                 PIC 9(8)    COMP.
        02 KO-REST-1.
           03  KO-ZPZ                  PIC 999V99  COMP.
           03  KO-SPERRE               PIC X(6).
           03  KO-ORT                  PIC X(15).
           03  KO-MERK                 PIC XXXX.
           03  KO-KST                  PIC 99      COMP.
           03  KO-ANR.
               05 KO-AN                PIC X(9)            OCCURS 7.
           03  KO-TABK.
               05 KO-KONDIT            PIC 9(13)   COMP-3  OCCURS 9.
      *------------> Schleppsummenspeicher WH-KEY = 2  user + 1 (1-5) <-
       01  KO-BSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  KO-BUDAT                PIC 9(8)     COMP.
           03  KO-JOKEY                PIC X(5).
           03  KO-SASUM.
               05 KO-DEBSUM            PIC S9(9)V99 COMP  OCCURS 6.
               05 KO-KREDSUM           PIC S9(9)V99 COMP  OCCURS 6.
               05 KO-JSOLL             PIC S9(9)V99 COMP.
               05 KO-JHABEN            PIC S9(9)V99 COMP.
               05 KO-VST               PIC S9(9)V99 COMP.
               05 KO-MWST              PIC S9(9)V99 COMP.
               05 KO-MWSTBMG           PIC S9(9)V99 COMP  OCCURS 9.
               05 KO-SKTOERL           PIC S9(9)V99 COMP.
               05 KO-KRDIF             PIC S9(9)V99 COMP.
               05 KO-SKTOAUF           PIC S9(9)V99 COMP  OCCURS 6.
               05 KO-SKTOVST           PIC S9(9)V99 COMP.
               05 KO-IGMUST            PIC S9(9)V99 COMP  OCCURS 2.
           03  KO-RS REDEFINES
               KO-SASUM                PIC S9(9)V99 COMP  OCCURS 36.
           03  KO-SPERR                PIC 99       COMP.
      *---------------------------------> zwingende Konten WH-KEY = 9 <-
       01  KO-TSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  KO-KTONR                PIC 9(6)     COMP   OCCURS 20.
           03  KO-WIJ                            OCCURS 4.
               05 KO-WKZ               PIC 99       COMP.
               05 KO-WBEG              PIC 9(8)     COMP.
               05 KO-WEND              PIC 9(8)     COMP.
           03  KO-ABDAT                PIC 9(8)     COMP.
           03  KO-EURO                 PIC 9(8)     COMP.
           03  KO-FILLER               PIC 99       COMP.
           03  KO-DRU                  PIC 99       COMP.
      *-----------------------------------------> Symbole WH-KEY = 10 <-
       01  KO-MSATZ.
           03  FILLER                  PIC X(5).
           03  KO-TS.
               05 KO-SYM               PIC XX            OCCURS 20.
               05 KO-SKZ               PIC 9999    COMP  OCCURS 20.
      *-----------------------------> Druckerseinstellung WH-KEY = 11 <-
       01  KOD-SATZ.
           03  FILLER                  PIC X(2).
           03  KOD-VERZ                PIC X(36).
           03  KOD-APAR                PIC X(45).
           03  KOD-BPAR                PIC X(45).
      *---------------------------------------------> Druckerparamter <-
       01  KO-PSATZ.
           03  FILLER                  PIC XX.
           03  KOP-BEZ                 PIC X(26).
           03  KOP-TX.
               05 KOP-STG              PIC X(10)     OCCURS 10.
      *-----------------------> WH-KEY = 68 Konten fÅr GetrÑnkeabgabe <-
       01  KO-GSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  KO-GKTO                 PIC 9(6)     COMP   OCCURS 32.
           03  KO-GPZ                  PIC 99V9999  COMP   OCCURS 32.
      *---------------------> WH-KEY = 69/67 Konten fÅr Ustverprobung <-
       01  KO-USATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  KO-UKTO                 PIC 9(6)     COMP   OCCURS 40.
           03  KO-UKZ                  PIC 99       COMP   OCCURS 40.
      *-----------------> WH-KEY = 70 eigene Adresse fÅr BankÅberwsg. <-
       01  KO-FSATZ.
           03  FILLER                  PIC X(18).
           03  KO-FBEZ                 PIC X(110).
      *--------------------------------------> Hausbanken Key 71 - 79 <-
       01  KB-SATZ.
           03  KB-NUM                  PIC 999       COMP-3.
           03  KB-KTONR                PIC X(15).
           03  KB-BEZ                  PIC X(29).
           03  KB-ART                  PIC 99        COMP.
           03  KB-BLZ                  PIC 9(5)      COMP.
           03  KB-FIBKTO               PIC 9(6)      COMP.
      *----------------------------------------------> FremdwÑhrungen <-
       01  KO-WSATZ.                                *> 200 Bytes frei
           03  FILLER                  PIC 999       COMP-3.
           03  KW-FRWTAB.
               05 KW-SYM               PIC XXX             OCCURS 20.
               05 KW-KURS              PIC 9(4)V9(7) COMP  OCCURS 20.
      ************************************* Journal- u. Buchungszeilen *
       FD  BUCHZEIL     external       RECORD VARYING FROM 68 TO 115
                                       DEPENDING ON WZ-BUCH.
       01  BZ-SATZ.
           03  BZ-KEY.
               05 BZ-KTONR             PIC 9(6)      COMP.
               05 BZ-BUDAT             PIC 9(8)      COMP.
               05 BZ-LFD               PIC 9999      COMP.
           03  BZ-JKEY.
               05 BZ-USE               PIC 99        COMP.
               05 BZ-SEITE             PIC 9(7)      COMP-X.
           03  BZ-BELNR                PIC 9(6)      COMP.
           03  BZ-BELDAT               PIC 9(8)      COMP.
           03  BZ-REFNR                PIC 9(7)      COMP.
           03  BZ-SYM                  PIC 99        COMP.
           03  BZ-GGKTO                PIC 9(6)      COMP.
           03  BZ-SH                   PIC 9         COMP.
           03  BZ-BUBET                PIC S9(9)V99  COMP.
           03  BZ-Z                    PIC 9         COMP.
           03  BZ-VM                   PIC 9         COMP.
           03  BZ-MW                   PIC S9(7)V99  COMP.
           03  BZ-U1                   PIC 99        COMP.
           03  BZ-B1                   PIC S9(9)V99  COMP.
           03  BZ-U2                   PIC 99        COMP.
           03  BZ-B2                   PIC S9(9)V99  COMP.
           03  BZ-KSTNR                PIC 9(5)      COMP.
           03  BZ-KOND                 PIC 9(13)     COMP.
           03  BZ-OPLFD                PIC 9999      COMP.
           03  BZ-FWBET                PIC S9(9)V99  COMP.
           03  BZ-KURS                 PIC 9(4)V9(7) COMP.
           03  BZ-FSYM                 PIC XXX.
           03  BZ-TX                   PIC X(40).
      *----------------------------------------> Jahresabschlu·summen <-
       01  BZ-ASATZ.
           03  FILLER                  PIC X(52).
           03  BZ-SOLL                 PIC S9(9)V99  COMP.
           03  BZ-HABEN                PIC S9(9)V99  COMP.
           03  BZ-EBIL                 PIC S9(9)V99  COMP.
      ***************************************************** OP - Datei *
       FD  OPDATEI      external       LABEL RECORD STANDARD.
       01  OP-SATZ.
           03  OP-AKEY.
               05 OP-SA                PIC 99         COMP.
               05 OP-KEY.
                  07 OP-DK             PIC 99         COMP.
                  07 OP-KTONR          PIC 9(6)       COMP.
                  07 OP-VALDAT         PIC 9(8)       COMP.
                  07 OP-LFD            PIC 9999       COMP.
           03  OP-REF                  PIC 9(7)       COMP.
           03  OP-RENUM                PIC 9(7)       COMP.
           03  OP-REDAT                PIC 9(8)       COMP.
           03  OP-SYM                  PIC 99         COMP.
           03  OP-ZUSTAND              PIC 99         COMP.
           03  OP-KOND                 PIC 9(13)      COMP.
           03  OP-MW                   PIC S9(7)V99   COMP.
           03  OP-U1                   PIC 99         COMP.
           03  OP-B1                   PIC S9(9)V99   COMP.
           03  OP-U2                   PIC 99         COMP.
           03  OP-B2                   PIC S9(9)V99   COMP.
           03  OP-REBETRAG             PIC S9(9)V99   COMP.
           03  OP-ZABETRAG             PIC S9(9)V99   COMP.
           03  OP-SKTOBAS              PIC S9(9)V99   COMP.
           03  OP-ZADAT                PIC 9(6)       COMP.
           03  OP-ZPBET                PIC S9(9)V99   COMP.
           03  OP-ZPSKTO               PIC S9(7)V99   COMP.
           03  OP-KURS                 PIC 9(4)V9(7)  COMP.
           03  OP-FSYM                 PIC XXX.              *> WÑhrung
           03  OP-MSTUFE               PIC 99         COMP.
           03  OP-MAHNDAT              PIC 9(6)       COMP.
           03  OP-ZESS                 PIC 99         COMP.
           03  OP-FREMDNR              PIC X(15).
      ********************************************* Auszifferungsdatei *
       FD  AUSZIFF      external       LABEL RECORD STANDARD
                                       RECORD VARYING FROM 20 TO 1060
                                       DEPENDING ON WZ-AUSZ.
       01  AZ-SATZ.
           03  AZ-KEY.
               05 AZ-SA                PIC 99        COMP.
               05 AZ-TKEY.
                  07 AZ-KTONR          PIC 9(6)      COMP.
                  07 AZ-BUDAT          PIC 9(8)      COMP.
                  07 AZ-LFD            PIC 9999      COMP.
           03  AZ-RETAB       OCCURS 50.
               05 AZ-OPNR              PIC 9(7)      COMP.
               05 AZ-OPSYM             PIC 99        COMP.
               05 AZ-OPBET             PIC S9(8)V99  COMP.
               05 AZ-OPSKTO            PIC S9(7)V99  COMP.
               05 AZ-OPFSYM            PIC XXX.
               05 AZ-KURS              PIC 9(5)V9(7) COMP.
      ******************************************************************
       FD  DEBITOR      external       LABEL RECORDS STANDARD.
       01  DE-SATZ.
           03  DE-KEY.
               05 DE-KTONR             PIC 9(6)      COMP.
           03  DE-MCODE                PIC X(8).
           03  DE-KTOART               PIC 99        COMP.
           03  DE-OPKZ                 PIC 99        COMP.
           03  DE-ANREDE               PIC 99        COMP.
           03  DE-SOLL                 PIC S9(9)V99  COMP-3.
           03  DE-HABEN                PIC S9(9)V99  COMP-3.
           03  DE-UMSATZ               PIC S9(9)V99  COMP-3.
           03  DE-KOND                 PIC 9(13)     COMP.
           03  DE-FWKZ                 PIC 99        COMP.
           03  DE-MAHNT                PIC 999       COMP.
           03  DE-MAHNDAT              PIC 9(6)      COMP.
           03  DE-BEZ                  PIC X(110).
           03  DE-TEL                  PIC X(35).
      *****************************************************************
       FD  KREDIT       external       LABEL RECORD STANDARD.
       01  KR-SATZ.
           03  KR-KEY.
               05  KR-KTONR            PIC 9(7)      COMP.
           03  KR-MCODE                PIC X(8).
           03  KR-KTOART               PIC 99        COMP.
           03  KR-OPKZ                 PIC 99        COMP.
           03  KR-SOLL                 PIC S9(9)V99  COMP-3.
           03  KR-HABEN                PIC S9(9)V99  COMP-3.
           03  KR-UMSATZ               PIC S9(9)V99  COMP-3.
           03  KR-UEWTRKZ              PIC 99        COMP.
           03  KR-KOND                 PIC 9(13)     COMP.
           03  KR-FWKZ                 PIC 99        COMP.
           03  KR-ZUS                  PIC 99        COMP.
           03  KR-BANKKTO              PIC X(13).
           03  KR-BANKLTZ              PIC X(6).
           03  KR-BEZ                  PIC X(110).
           03  KR-BANK                 PIC X(24).
      ********************************************* Zahlungspositionen *
       FD  ZAHLPOS      external       LABEL RECORD STANDARD.
       01  ZP-SATZ.
           03  ZP-AKEY.
               05 ZP-BAKEY             PIC 9999       COMP.
               05 ZP-KEY.
                  07 ZP-DK             PIC 99         COMP.
                  07 ZP-KTONR          PIC 9(6)       COMP.
                  07 ZP-VALDAT         PIC 9(8)       COMP.
                  07 ZP-LFD            PIC 9999       COMP.
         02 ZP-REST.
           03  ZP-BETRAG               PIC S9(7)V99   COMP.
           03  ZP-SKTO                 PIC S9(7)V99   COMP.
           03  ZP-EMPFG                PIC X(52).
           03  ZP-BLZ                  PIC 9(5)       COMP.
           03  ZP-VERWEND              PIC X(60).
           03  ZP-DATUM                PIC 9(8)       COMP.
           03  ZP-REDAT                PIC 9(6)       COMP.
           03  ZP-BAKTO                PIC X(15).
           03  ZP-BABEZ                PIC X(30).
      **************************************************** Zinsendatei *
       FD  ZINSEN                      LABEL RECORD STANDARD.
       01  ZI-SATZ.
           03  ZI-KEY.
               05 ZI-NUM               PIC 9(6)       COMP.
               05 ZI-SA                PIC 99         COMP.
               05 ZI-VALDAT            PIC 9(8)       COMP.
         02 ZI-REST.
           03  ZI-BELDAT               PIC 9(6)       COMP.
           03  ZI-BELNR                PIC 9(7)       COMP.
           03  ZI-SYM                  PIC 99         COMP.
           03  ZI-BET                  PIC S9(7)V99   COMP.
           03  ZI-BZEIL.
               05 ZI-USE               PIC 99         COMP.
               05 ZI-SEITE             PIC 9(7)       COMP-X.
      **************************************************************-***
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  REGISTER.
           03  COP-STA                 PIC X(2)    COMP-5.
           03  WH-ANZ                  PIC 9(6).
           03  WH-HW                   PIC 99.
       COPY FIBUEXT.CPY.
       DECL-K SECTION.         USE AFTER ERROR PROCEDURE ON KONSTANT.
       A.  CALL "CADECL" USING "FIBUKONS.DAT" WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CREG TO WH-CREG.
           MOVE WL-CALL TO WH-CALL.
           MOVE 9 TO WH-HW.
           CALL "CAUP" USING "1301012580" WH-CREG.
           IF WL-CA = 50 PERFORM KURSE
                         MOVE 9 TO WH-PG GO X.
           PERFORM CONVBZ.
           PERFORM CONVOP.
           PERFORM CONVAZ.
           PERFORM CONVZI.
           PERFORM CONVZP.
           PERFORM CONVKO.
      *    PERFORM CONVDE.
      *    PERFORM CONVKR.
           MOVE WH-CREG TO WL-CREG.
           MOVE WH-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
       X.  MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       CONVBZ SECTION.
           open input buchzeil.
           if wf-status = "00" go Y.
           PERFORM EURO-JN.
           CALL "CBL_COPY_FILE" USING "FIBUZEIL.DAT " "XIBUZEIL.DAT "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           CALL "CBL_COPY_FILE" USING "FIBUZEIL.IDX " "XIBUZEIL.IDX "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Buchungszeilen:" with FOREGROUND-COLOR 10 AT 0515.
           OPEN OUTPUT BUCHZEIL.
           OPEN INPUT BUCHOLD.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO OB-SATZ.
           READ BUCHOLD NEXT AT END GO X.
           INITIALIZE BZ-SATZ.
           MOVE OB-KTONR TO BZ-KTONR
           MOVE OB-BUDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO BZ-BUDAT.
           MOVE OB-LFD TO BZ-LFD.
           MOVE OB-JKEY TO BZ-JKEY.
           MOVE OB-BELNR TO BZ-BELNR.
           MOVE OB-BELDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO BZ-BELDAT.
           MOVE OB-REFNR TO BZ-REFNR.
           MOVE OB-SYM TO BZ-SYM.
           MOVE OB-GGKTO TO BZ-GGKTO.
           MOVE OB-SH TO BZ-SH.
           IF OB-SYM > 98 MOVE OB-SOLL TO BZ-SOLL
                          MOVE OB-HABEN TO BZ-HABEN
                          MOVE OB-EBIL TO BZ-EBIL
                          GO G.
           MOVE OB-BUBET TO BZ-BUBET.
           MOVE OB-Z TO BZ-Z.
           MOVE OB-VM TO BZ-VM.
           MOVE OB-MW TO BZ-MW.
           MOVE OB-U1 TO BZ-U1.
           MOVE OB-B1 TO BZ-B1.
           MOVE OB-U2 TO BZ-U2.
           MOVE OB-B2 TO BZ-B2.
           MOVE OB-KSTNR TO BZ-KSTNR.
           MOVE OB-KOND TO BZ-KOND.
           MOVE OB-OPLFD TO BZ-OPLFD.
           IF BZ-OPLFD = 255 MOVE 999 TO BZ-OPLFD.
           MOVE OB-BUBET TO BZ-FWBET.
       G.  MOVE 100 TO BZ-KURS.
           COMPUTE BZ-KURS = BZ-KURS / 13,7603.
           MOVE "Ats" TO BZ-FSYM.
           MOVE OB-TX TO BZ-TX.
           INSPECT BZ-TX REPLACING ALL LOW-VALUES BY SPACE.
           PERFORM VARYING WZ-BUCH FROM 115 BY -1 UNTIL WZ-BUCH = 75
               OR BZ-SATZ(WZ-BUCH:1) NOT = SPACE CONTINUE.
           MOVE WZ-BUCH TO WH-ANZ.
           WRITE BZ-SATZ INVALID STOP RUN.
           DISPLAY VDU-DATUM AT 0543 " " BZ-KTONR.
           GO C.
       X.  CLOSE BUCHOLD.
       Y.  CLOSE BUCHZEIL.
       Z.  EXIT.
      ******************************************************************
       CONVOP SECTION.
       A.  open input OPDATEI.
           if wf-status = "00" go Y.
           PERFORM EURO-JN.
           CALL "CBL_COPY_FILE" USING "FIBUOPES.DAT " "XIBUOPES.DAT "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           CALL "CBL_COPY_FILE" USING "FIBUOPES.IDX " "XIBUOPES.IDX "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Offen Posten: " with FOREGROUND-COLOR 11 AT 0615.
           OPEN OUTPUT OPDATEI.
           OPEN INPUT OPOLD.
           MOVE 0 TO WH-ANZ.
       C.  READ OPOLD NEXT AT END GO X.
           MOVE AO-SA TO OP-SA.
           MOVE AO-DK TO OP-DK.
           MOVE AO-KTONR TO OP-KTONR.
           MOVE AO-VALDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO OP-VALDAT.
           MOVE AO-REF TO OP-REF.
           MOVE AO-LFD TO OP-LFD.
           MOVE AO-RENUM TO OP-RENUM.
           MOVE AO-REDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO OP-REDAT.
           MOVE AO-RENUM TO OP-RENUM.
           MOVE AO-SYM TO OP-SYM.
           MOVE AO-ZUSTAND TO OP-ZUSTAND.
           MOVE AO-KOND TO OP-KOND.
           MOVE AO-MW TO OP-MW.
           MOVE AO-U1 TO OP-U1.
           MOVE AO-B1 TO OP-B1.
           MOVE AO-U2 TO OP-U2.
           MOVE AO-B2 TO OP-B2.
           MOVE AO-REBETRAG TO OP-REBETRAG.
           MOVE AO-ZABETRAG TO OP-ZABETRAG.
           MOVE AO-SKTOBAS TO OP-SKTOBAS.
           MOVE AO-ZADAT TO OP-ZADAT.
           MOVE AO-ZPBET TO OP-ZPBET.
           MOVE AO-ZPSKTO TO OP-ZPSKTO.
           MOVE 100 TO OP-KURS.
           MOVE "Ats" TO OP-FSYM.
      *    IF WH-HW = 1 COMPUTE OP-KURS = OP-KURS / 13,7603
      *                 COMPUTE OP-B1 = OP-B1 / 13,7603
      *                 COMPUTE OP-B2 = OP-B2 / 13,7603
      *                 COMPUTE OP-MW = OP-MW / 13,7603.
           MOVE AO-MSTUFE TO OP-MSTUFE.
           MOVE AO-MAHNDAT TO OP-MAHNDAT.
           MOVE AO-ZESS TO OP-ZESS.
           MOVE AO-FREMDNR TO OP-FREMDNR.
           WRITE OP-SATZ INVALID STOP RUN.
           ADD 1 TO WH-ANZ.
           DISPLAY VDU-DATUM AT 0643 " " WH-ANZ.
           GO C.
       X.  CLOSE OPOLD.
       Y.  CLOSE OPDATEI.
       Z.  EXIT.
      ******************************************************************
       CONVAZ SECTION.
       A.  open input AUSZIFF.
           if wf-status = "00" go Y.
           PERFORM EURO-JN.
           CALL "CBL_COPY_FILE" USING "FIBUAUSZ.DAT " "XIBUAUSZ.DAT "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           CALL "CBL_COPY_FILE" USING "FIBUAUSZ.IDX " "XIBUAUSZ.IDX "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Auszifferung:  " with FOREGROUND-COLOR 12 AT 0715.
           OPEN OUTPUT AUSZIFF.
           OPEN INPUT AUSOLD.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO OZ-SATZ.
           READ AUSOLD NEXT AT END GO X.
           MOVE OZ-SA TO AZ-SA.
           MOVE OZ-KTONR TO AZ-KTONR.
           MOVE OZ-BUDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO AZ-BUDAT.
           MOVE OZ-LFD TO AZ-LFD.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 50
               or OZ-RETAB(WX) = LOW-VALUE
                   MOVE OZ-OPNR(WX) TO AZ-OPNR(WX)
                   MOVE OZ-OPSYM(WX) TO AZ-OPSYM(WX)
                   MOVE OZ-OPBET(WX) TO AZ-OPBET(WX)
                   MOVE OZ-OPSKTO(WX) TO AZ-OPSKTO(WX)
                   MOVE "Ats" TO AZ-OPFSYM(WX)
                   MOVE "100" TO AZ-KURS(WX).
      *    PERFORM VARYING WZ-AUSZ FROM 50 BY -1 UNTIL WZ-AUSZ = 1
      *        OR AZ-RETAB(WZ-AUSZ) NOT = LOW-VALUE CONTINUE.
           COMPUTE WZ-AUSZ = (WX - 1) * 13 + 10.
           IF WZ-AUSZ < 23 GO C.
           WRITE AZ-SATZ INVALID STOP RUN.
           ADD 1 TO WH-ANZ.
           DISPLAY VDU-DATUM AT 0743 " " WH-ANZ.
           GO C.
       X.  CLOSE AUSOLD.
       Y.  CLOSE AUSZIFF.
       Z.  EXIT.
      ******************************************************************
       CONVZI SECTION.
       A.  open input ZINSEN.
           if wf-status = "00" go Y.
           PERFORM EURO-JN.
           CALL "CBL_COPY_FILE" USING "FIBUZINS.DAT " "XIBUZINS.DAT "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           CALL "CBL_COPY_FILE" USING "FIBUZINS.IDX " "XIBUZINS.IDX "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Zinsendatei:  " with FOREGROUND-COLOR 13 AT 0815.
           OPEN OUTPUT ZINSEN.
           OPEN INPUT ZINSOLD.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO OI-SATZ.
           READ ZINSOLD NEXT AT END GO X.
           MOVE OI-NUM TO ZI-NUM.
           MOVE OI-SA TO ZI-SA.
           MOVE OI-VALDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO ZI-VALDAT.
           MOVE OI-REST TO ZI-REST.
           ADD 1 TO WZ-AUSZ.
           WRITE ZI-SATZ INVALID STOP RUN.
           ADD 1 TO WH-ANZ.
           DISPLAY VDU-DATUM AT 0843 " " WH-ANZ.
           GO C.
       X.  CLOSE ZINSOLD.
       Y.  CLOSE ZINSEN.
       Z.  EXIT.
      ******************************************************************
       CONVZP SECTION.
       A.  open input ZAHLPOS.
           if wf-status = "00" go Y.
           PERFORM EURO-JN.
           CALL "CBL_COPY_FILE" USING "FIBUZAHL.DAT " "XIBUZAHL.DAT "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           CALL "CBL_COPY_FILE" USING "FIBUZAHL.IDX " "XIBUZAHL.IDX "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Zahlungsdatei: " with FOREGROUND-COLOR 14 AT 0915.
           OPEN OUTPUT ZAHLPOS.
           OPEN INPUT ZAHLOLD.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUES TO ZO-SATZ.
           READ ZAHLOLD NEXT AT END GO X.
           MOVE ZO-BAKEY TO ZP-BAKEY.
           MOVE ZO-DK TO ZP-DK.
           MOVE ZO-KTONR TO ZP-KTONR.
           MOVE ZO-VALDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO ZP-VALDAT.
           MOVE ZO-LFD TO ZP-LFD.
           MOVE ZO-REST TO ZP-REST.
           MOVE ZO-DATUM TO WC-DATUM.
           CALL "CAUP" USING "04DATCV" WH-CREG.
           MOVE WX-DATUM TO ZP-DATUM.
           MOVE ZO-REDAT TO ZP-REDAT.
           MOVE ZO-BAKTO TO ZP-BAKTO.
           MOVE ZO-BABEZ TO ZP-BABEZ.
           ADD 1 TO WZ-AUSZ.
           WRITE ZP-SATZ INVALID STOP RUN.
           ADD 1 TO WH-ANZ.
           DISPLAY VDU-DATUM AT 0943 " " WH-ANZ.
           GO C.
       X.  CLOSE ZAHLOLD.
       Y.  CLOSE ZAHLPOS.
       Z.  EXIT.
      ************************************************ Konstantendatei *
       CONVKO SECTION.
       A.  open input KONSTANT.
           MOVE 1 TO WH-KEY.
           READ KONSTANT.
           MOVE KO-PER TO WZ-DATUM.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WZ-DATUM not = 0 go y.
           PERFORM EURO-JN.
           CLOSE KONSTANT.
           CALL "CBL_COPY_FILE" USING "FIBUKONS.DAT " "XIBUKONS.DAT "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Konstantendatei: " with FOREGROUND-COLOR 14 AT 0915.
           OPEN OUTPUT KONSTANT.
           OPEN INPUT KONSOLD.
           MOVE 0 TO WH-ANZ.
       C.  MOVE LOW-VALUE TO KO-SATZ.
           READ KONSOLD NEXT AT END GO X.
           MOVE OK-SATZ TO KO-SATZ.
           EVALUATE WH-KEY
               WHEN 1 MOVE OK-PER TO WC-DATUM
                      CALL "CAUP" USING "04DATDREH" WH-CREG
                      MOVE WX-DATUM TO KO-PER
                      MOVE OK-ZTAG TO WC-DATUM
                      CALL "CAUP" USING "04DATDREH" WH-CREG
                      MOVE WX-DATUM TO KO-ZTAG
                      MOVE OK-REST-1 TO KO-REST-1
               WHEN 2
               WHEN 3
               WHEN 4
               WHEN 5
               WHEN 6
               WHEN 7
               WHEN 8 MOVE OK-BUDAT TO WC-DATUM
                      CALL "CAUP" USING "04DATDREH" WH-CREG
                      MOVE WX-DATUM TO KO-BUDAT
                      MOVE OK-JOKEY TO KO-JOKEY
                      MOVE OK-SASUM TO KO-SASUM
                      MOVE OK-SPERR TO KO-SPERR
               WHEN 9 MOVE OK-ABDAT TO WC-DATUM
                      CALL "CAUP" USING "04DATDREH" WH-CREG
                      MOVE WX-DATUM TO KO-ABDAT
                      MOVE 0 TO KO-EURO
                      PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 4
                          MOVE OK-WBEG(WX) TO WC-DATUM
                          CALL "CAUP" USING "04DATDREH" WH-CREG
                          MOVE WX-DATUM TO KO-WBEG(WX)
                          MOVE OK-WEND(WX) TO WC-DATUM
                          CALL "CAUP" USING "04DATDREH" WH-CREG
                          MOVE WX-DATUM TO KO-WEND(WX)
                          MOVE OK-WKZ(WX) TO KO-WKZ(WX).
           WRITE KO-SATZ.
           ADD 1 TO WH-ANZ.
           DISPLAY WH-ANZ AT 0943.
           GO C.
       X.  CLOSE KONSOLD.
       Y.  CLOSE KONSTANT.
       Z.  EXIT.
      ******************************************************************
       CONVKR SECTION.
       A.  open input KREDIT.
           if wf-status = "00" go Y.
           PERFORM EURO-JN.
           CALL "CBL_COPY_FILE" USING "FIBUKRED.DAT " "XIBUKRED.DAT "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           CALL "CBL_COPY_FILE" USING "FIBUKRED.IDX " "XIBUKRED.IDX "
                                RETURNING COP-STA.
           IF COP-STA not = 0 go z.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Lieferanten.:  " with FOREGROUND-COLOR 15 AT 1015.
           OPEN OUTPUT KREDIT.
           OPEN INPUT KREDOLD.
           MOVE 0 TO WH-ANZ.
       C.  READ KREDOLD NEXT AT END GO X.
           MOVE OR-SATZ TO KR-SATZ.
      *    MOVE SPACE TO KR-FSYM.
           WRITE KR-SATZ INVALID STOP RUN.
           ADD 1 TO WH-ANZ.
           DISPLAY KR-BEZ(1:10) AT 0743 " " WH-ANZ.
           GO C.
       X.  CLOSE KREDOLD.
       Y.  CLOSE KREDIT.
       Z.  EXIT.
      ******************************************************************
       CONVDE SECTION.
       A.  open input DEBITOR.
           if wf-status = "00" go Y.
           PERFORM EURO-JN.
           CALL "CBL_COPY_FILE" USING "FIBUDEBI.DAT " "XIBUDEBI.DAT "
                                RETURNING COP-STA.
           IF COP-STA not = 0 stop run.
           CALL "CBL_COPY_FILE" USING "FIBUDEBI.IDX " "XIBUDEBI.IDX "
                                RETURNING COP-STA.
           IF COP-STA not = 0 stop run.
           DISPLAY "Konvertierung - EURO/2000" with highlight AT 0328.
           DISPLAY "Kunden......:  " with FOREGROUND-COLOR 15 AT 1115.
           OPEN OUTPUT DEBITOR.
           OPEN INPUT DEBOLD.
           MOVE 0 TO WH-ANZ.
       C.  READ DEBOLD NEXT AT END GO X.
           MOVE OD-SATZ TO DE-SATZ.
      *    MOVE SPACE TO DE-FSYM.
           WRITE DE-SATZ INVALID STOP RUN.
           ADD 1 TO WH-ANZ.
           DISPLAY DE-BEZ(1:10) AT 1143 " " WH-ANZ.
           GO C.
       X.  CLOSE DEBOLD.
       Y.  CLOSE DEBITOR.
       Z.  EXIT.
      ******************************************************************
       EURO-JN SECTION.
       A.  GO Z.
           IF WH-HW not = 9 GO Z.
           DISPLAY "HauswÑhrung: " with highlight AT 0310.
           DISPLAY "<ret>= Ats, 1+<ret>= Eur, <esc>= Stop" AT 2301.
           CALL "CAUP" USING "1003231001" WH-CREG.
           IF ESC STOP RUN.
           IF not RET or WH-WERT > 1 GO A.
           MOVE WH-WERT TO WH-HW.
           IF WH-HW = 0 DISPLAY "Ats" with highlight AT VDU-LP
                    ELSE DISPLAY "Eur" with highlight AT VDU-LP.
       Z.  EXIT.
      ******************************************************************
       KURSE SECTION.
       A.  DISPLAY "Op- Konvertierung" AT 0530.
           MOVE LOW-VALUE TO OP-SATZ.
           START OPDATEI KEY not < OP-KEY INVALID GO Z.
       C.  READ OPDATEI NEXT AT END GO Z.
           IF WM-DATUM < 20000000;
               IF OP-REDAT < 1000000 ADD 19000000 TO OP-REDAT.
           IF OP-REDAT < WE-EURO
               EVALUATE OP-FSYM
                   WHEN "   "
                   WHEN "Ats" MOVE 100,00 TO OP-KURS
                              MOVE "Ats" TO OP-FSYM
                   WHEN "Eur" MOVE 1376,03 TO OP-KURS
               end-evaluate
           else EVALUATE OP-FSYM
                   WHEN "   "
                   WHEN "Ats" MOVE 7,2672835 TO OP-KURS
                   WHEN "Eur" MOVE 100,00 TO OP-KURS.
           REWRITE OP-SATZ.
           DISPLAY OP-KTONR AT 2020.
           GO C.
       Z.  EXIT.
