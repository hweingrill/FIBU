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
               05 KO-FREIUST           PIC S9(9)V99 COMP  OCCURS 2.
           03  KO-RS REDEFINES
               KO-SASUM                PIC S9(9)V99 COMP  OCCURS 38.
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
               05 KO-SYM               PIC XX              OCCURS 30.
               05 KO-SKZ               PIC 9(4)    COMP-X  OCCURS 30.
           03  KO-TR-SR                PIC 9(6)    COMP    OCCURS  3.
           03  KO-TSR REDEFINES
               KO-TR-SR.
               05 KO-NFUST             PIC 9(6)    COMP.
               05 KO-ERLSR             PIC 9(6)    COMP.
               05 KO-ERLTR             PIC 9(6)    COMP.
           03  KOM-NEU                 PIC XXX.
      *    03  KO-ARUFREI              PIC 9(6)    COMP.       *> 10
      *    03  KO-TRUFREI              PIC 9(6)    COMP.       *> 11
      *    03  KO-SRUFREI              PIC 9(6)    COMP.       *> 12
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
      *-----------------------> WH-KEY = 68 Konten fr Getr„nkeabgabe <-
       01  KO-GSATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  KO-GKTO                 PIC 9(6)     COMP   OCCURS 32.
           03  KO-GPZ                  PIC 99V9999  COMP   OCCURS 32.
      *---------------------> WH-KEY = 69/67 Konten fr Ustverprobung <-
       01  KO-USATZ.
           03  FILLER                  PIC 999      COMP-3.
           03  KO-UKTO                 PIC 9(6)     COMP   OCCURS 40.
           03  KO-UKZ                  PIC 99       COMP   OCCURS 40.
      *-----------------> WH-KEY = 70 eigene Adresse fr Bankberwsg. <-
       01  KO-FSATZ.
           03  FILLER                  PIC X(18).
           03  KO-FBEZ                 PIC X(110).
           03  KO-FSACHB               PIC X(25).
           03  KO-FTEL                 PIC X(20).
      *--------------------------------------> Hausbanken Key 71 - 79 <-
       01  KB-SATZ.
           03  KB-NUM                  PIC 999       COMP-3.
           03  KB-KTONR                PIC X(15).
           03  KB-BEZ                  PIC X(29).
           03  KB-ART                  PIC 99        COMP.
           03  KB-BLZ                  PIC 9(5)      COMP.
           03  KB-FIBKTO               PIC 9(6)      COMP.
           03  KB-KENNUNG              PIC XX.
           03  KB-KONTROL              PIC 9(6)      COMP.
           03  KB-OHNEVW               PIC 99        COMP.
           03  KB-KTOBEZ               PIC X(35)    OCCURS 2.
           03  KB-ATBEZ                PIC X(10).
           03  KB-ATBIC                PIC X(25).
      *----------------------------------------------> Fremdw„hrungen <-
       01  KO-WSATZ.                                *> 200 Bytes frei
           03  FILLER                  PIC 999       COMP-3.
           03  KW-FRWTAB.
               05 KW-SYM               PIC XXX             OCCURS 20.
               05 KW-KURS              PIC 9(4)V9(7) COMP  OCCURS 20.
      **************************************** Kostenstellenbewegungen *
       FD  KOSTEN       external       LABEL RECORD STANDARD
                                       RECORD VARYING FROM 20 TO 310
                                       DEPENDING ON WZ-KOST.
       01  KS-SATZ.
           03  KS-KEY.
               05 KS-SA                PIC 99        COMP.
               05 KS-TKEY.
                  07 KS-KTONR          PIC 9(6)      COMP.
                  07 KS-BUDAT          PIC 9(8)      COMP.
                  07 KS-LFD            PIC 9999      COMP.
           03  KS-RETAB       OCCURS 50.
               05 KS-OPSYM             PIC 99        COMP.
               05 KS-OPBET             PIC S9(8)V99  COMP.
      ***************************** Buchs„tze f. Fibuueberleitung FAKT *
       FD  BUCHUNG      external       LABEL RECORD STANDARD.
       01  BU-SATZ.
           03  BU-KEY                  PIC 9(6)     COMP.
           03  BU-RKEY.
               05 BU-SA                PIC X.
               05 BU-REN               PIC 9(7)     COMP.
           03  BU-KTONR                PIC 9(6)     COMP.
           03  BU-KOND                 PIC 9(13)    COMP.
           03  BU-DAT                  PIC 9(6)     COMP.
           03  BU-SY                   PIC 99       COMP.
           03  BU-GK                   PIC 9(6)     COMP.
           03  BU-Z                    PIC 9        COMP.
           03  BU-VM                   PIC 9        COMP.
           03  BU-RF                   PIC 9(7)     COMP.
           03  BU-U1                   PIC 9        COMP.
           03  BU-U2                   PIC 9        COMP.
           03  BU-MW                   PIC S9(7)V99 COMP-3.
           03  BU-B1                   PIC S9(9)V99 COMP-3.
           03  BU-B2                   PIC S9(9)V99 COMP-3.
           03  BU-SH                   PIC S9       COMP-3.
           03  BU-BET                  PIC S9(9)V99 COMP-3.
           03  BU-KST                  PIC S9(5)    COMP-3.
           03  BU-TX                   PIC X(40).
           03  BU-SKTOBAS              PIC S9(9)V99 COMP-3.
           03  BU-VALDAT               PIC 9(6)     COMP.
      *--------------------------------------------> fr Fremdw„hrung <-
           03  BU-FWBET                PIC S9(9)V99  COMP.
           03  BU-KURS                 PIC 9(4)V9(7) COMP.
           03  BU-FSYM                 PIC XXX.
           03  BU-SKTO                 PIC 99        COMP.
           03  FILLER                  PIC X(53).
      *------------------ BU-SA= "K" ---------------> Kontenbernahme <-
       01  BU-KSATZ.
           03  FILLER                  PIC X(19).
           03  BU-MCODE                PIC X(8).
           03  BU-ANREDE               PIC 99       COMP.
           03  BU-BEZ                  PIC X(110).
           03  BU-TEL                  PIC X(35).
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
           03  BZ-BELNR                PIC 9(6)      COMP-X.
           03  BZ-BELDAT               PIC 9(8)      COMP.
           03  BZ-REFNR                PIC 9(7)      COMP.
           03  BZ-SYM                  PIC 99        COMP.
           03  BZ-GGKTO                PIC 9(6)      COMP.
           03  BZ-SH                   PIC 9         COMP.
           03  BZ-BUBET                PIC S9(9)V99  COMP.
           03  BZ-Z                    PIC 9         COMP.
           03  BZ-VM                   PIC 99        COMP.
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
      *----------------------------------------> Jahresabschluásummen <-
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
           03  OP-FSYM                 PIC XXX.              *> W„hrung
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
      ******************************************************************
       FD  SACHBUCH     external       LABEL RECORD STANDARD.
       01  SA-SATZ.
           03  SA-KEY.
               05 SA-KTONR             PIC 9(7)      COMP.
           03  SA-BEZ.
               05 SA-TX                PIC X(25)     OCCURS 2.
           03  SA-MCODE                PIC X(8).
           03  SA-KTOART               PIC 99        COMP.
           03  SA-SH                   PIC 99        COMP.
           03  SA-SOLL                 PIC S9(9)V99  COMP-3.
           03  SA-HABEN                PIC S9(9)V99  COMP-3.
           03  SA-EBIL                 PIC S9(9)V99  COMP-3.
           03  SA-KER-RES              PIC 99        COMP.
           03  SA-KER-NR               PIC 999       COMP.
           03  SA-BTNT                 PIC 99        COMP.
           03  SA-VSTKZ                PIC 99        COMP.
           03  SA-MENGENEB             PIC S9(9)V99  COMP-3.
           03  SA-MENGE-JAHR           PIC S9(9)V99  COMP-3.
      ******************************************************************
       FD  KERMODEL                    LABEL RECORD STANDARD.
       01  KM-SATZ.
           03  KM-KEY.
               05 KM-ZEIL              PIC 999V9      COMP.
           03  KM-ART                  PIC 99         COMP.
           03  KM-PERIODE              PIC 9(6)       COMP.   *> JJJJMM
           03  KM-TSUM.
               05 KM-SUMMEN            PIC S9(9)V99    COMP  OCCURS 25.
           03  KM-TEXT                 PIC X(25).
           03  KM-RECH                 PIC X.
           03  KM-PZ                   PIC X.
           03  KM-FILLER               PIC X(3).
