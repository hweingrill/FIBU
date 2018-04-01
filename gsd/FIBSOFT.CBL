      $SET LINKCOUNT "480" ANS85 AUTOLOCK BOUND NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBLIST.
      ******************************************************************
      *        Journalende mit autom. Sammelbuchungen
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY FIBUSEC.CPY.
           SELECT NACHKALK ASSIGN TO WN-NAKAL
                           ORGANIZATION INDEXED ACCESS DYNAMIC
                           RECORD KEY NA-KEY
                           ALTERNATE RECORD NA-PGKUNR DUPLICATES
                           ALTERNATE RECORD NA-VKEY =
                                            NA-VKF NA-KEY DUPLICATES
                           FILE STATUS IS WF-STATUS.
           SELECT KOPF     ASSIGN TO WN-LFSNAM
                           ORGANIZATION IS INDEXED, ACCESS IS DYNAMIC
                           RECORD KEY IS LK-KEY
                           ALTERNATE LK-AKEY =
                                   LK-SA LK-STATUS LK-REKTO LK-NUM
                           ALTERNATE LK-BKEY = LK-SA LK-REKTO LK-NUM
                           ALTERNATE LK-REKEY DUPLICATES
                           FILE STATUS IS WF-STATUS.
           SELECT DRUCKER  ASSIGN TO PRINTER WH-DRUNAM
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT EXCELIS  ASSIGN TO PRINTER WH-FIBNAM
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
       COPY \WB\SG\SPIFDLFK.CPY.
       COPY \WB\SG\SPIFDNAK.CPY.
      ******************************************************************
       FD  DRUCKER                     LABEL RECORD STANDARD.
       01  DRA-SATZ.
           03  FILLER                  PIC XXXX.
           03  DRA-STR.
               05 DRA-KTONR            PIC ZZZZZZ,Z-.
               05 DRA-BELNR            PIC ZZZZZZ-.
               05 FILLER               PIC X.
               05 DRA-BELDAT           PIC X(8).
               05 DRA-REFNR            PIC ZZZZZZ-.
               05 DRA-SYM              PIC XX.
               05 DRA-GGKTO            PIC ZZZZZZ,Z-.
               05 DRA-TX               PIC X(40).
               05 DRA-UST              PIC ZZ-.
               05 DRA-MWSKT            PIC ZZZZZ.ZZZ,ZZ-.
               05 DRA-SOLL             PIC ZZ.ZZZ.ZZZ,ZZ-.
               05 DRA-HABEN            PIC ZZ.ZZZ.ZZZ,ZZ-.
           03  FILLER                  PIC X.
       01  DRP-SATZ.
           03  FILLER                  PIC X(9).
           03  DRP-STR.
               05 DRP-KTONR            PIC ZZ.ZZ9,9-.
               05 DRP-BEZ              PIC X(51).
               05 DRP-KZ               PIC XX.
               05 DRP-SOLL             PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRP-HABEN            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRP-SALDO            PIC ZZZ.ZZZ.ZZ9,99-.
       01  DRO-SATZ.
           03  FILLER                  PIC X(18).
           03  DRO-DAT                 PIC X(9).
           03  DRO-SYM                 PIC XXX.
           03  DRO-REFNR               PIC ZZZZZZ9-.
           03  DRO-REBET               PIC ZZZZZ.ZZ9,99-.
           03  DRO-ZABET               PIC ZZZZZ.ZZ9,99-.
           03  DRO-OFFEN               PIC ZZZZZ.ZZ9,99-.
           03  FILLER                  PIC XX.
           03  DRO-FAELLIG             PIC X(9).
           03  DRO-ZAHL                PIC XXX.
       01  DRS-SATZ.
           03  FILLER                  PIC X(4).
           03  DRS-STR.
               05 DRS-KTONR            PIC 99.999,9-.
               05 DRS-BEZ.
                  07 DRS-TX            PIC X(38).
                  07 DRS-EB            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRS-MSOLL            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRS-MHABEN           PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRS-GSOLL            PIC ZZZ.ZZZ.ZZ9,99-.
               05 DRS-GHABEN           PIC ZZZ.ZZZ.ZZ9,99-.
      ******************************************************************
       FD  EXCELIS                     LABEL RECORD OMITTED.
       01  EXC-SATZ                    PIC X(200).
       01  EXC-ASATZ.
           03  EXC-KTONR               PIC ZZZZ.
           03  EXC-TA                  PIC X.
           03  EXC-BEZ                 PIC X(50).
           03  EXC-TB                  PIC X.
           03  EXC-MBET                PIC ZZZZZZZZ9,99-.
           03  EXC-TC                  PIC X.
           03  EXC-GBET                PIC ZZZZZZZZ9,99-.
           03  EXC-TD                  PIC X.
           03  EXC-KER                 PIC ZZ9.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WK-DSTG.
               05 WK-STG               PIC X(10)     OCCURS 10.
           03  WM-FIN                  PIC 99        COMP.
           03  WH-P1                   PIC 99        COMP.
           03  WH-P2                   PIC 99        COMP.
           03  WX-PRNO                 PIC 99        COMP-X.
           03  WX-PRSTAT               PIC 99        COMP-X.
           03  WZ-SEITE                PIC 99        COMP   VALUE ZERO.
           03  WZ-SCHALT               PIC 99        COMP   VALUE ZERO.
           03  WZ-ZEILEN               PIC 99        COMP   VALUE ZERO.
           03  WH-DRUNAM               PIC X(12)     VALUE   "LPT1:".
           03  WH-FIBNAM               PIC X(12).
           03  WM-FIBEX                PIC 9         COMP   VALUE ZERO.
           03  WM-OPEN                 PIC 9         COMP   VALUE ZERO.
           03  WH-SIZE                 PIC 99        COMP   VALUE 62.
           03  WM-ANZ                  PIC 9(5).
           03  WM-SUB                  PIC 9(5).
           03  WM-ZAHL                 PIC 9(5).
           03  WD-DIS                  PIC ZZZZZZZ9.
           03  WD-BET                  PIC ZZZZZZZ9,99-.
           03  WH-MOD                  PIC 99        COMP.
           03  WH-VON                  PIC 9(6)      COMP.
           03  WH-BIS                  PIC 9(6)      COMP.
           03  WH-VAR                  PIC 99        COMP.
           03  WM-ART                  PIC 99        COMP.
           03  WM-LOE                  PIC 99        COMP.
           03  WB                      PIC 99        COMP.
           03  WP                      PIC 99        COMP.
           03  WE                      PIC 99        COMP.
           03  WI                      PIC 999       COMP.
           03  IX                      PIC 99        COMP.
           03  WG                      PIC 99        COMP.
           03  WV-BISJ                 PIC 9(5)      COMP.
           03  WS-REG.
               05 WZ-SOLL              PIC S9(9)V99  COMP  OCCURS 4.
               05 WZ-HABEN             PIC S9(9)V99  COMP  OCCURS 4.
               05 WG-SOLL              PIC S9(9)V99  COMP  OCCURS 4.
               05 WG-HABEN             PIC S9(9)V99  COMP  OCCURS 4.
           03  WM-TEIL                 PIC S9(9)V99  COMP.
           03  WM-SOLL                 PIC S9(9)V99  COMP.
           03  WM-HABEN                PIC S9(9)V99  COMP.
           03  WM-UMSATZ               PIC S9(9)V99  COMP.
           03  WM-EBIL                 PIC S9(9)V99  COMP.
           03  WV-SOLL                 PIC S9(9)V99  COMP.
           03  WV-HABEN                PIC S9(9)V99  COMP.
           03  WV-EBIL                 PIC S9(9)V99  COMP.
           03  WS-OPS                  PIC S9(9)V99  COMP.
           03  WM-EUR                  PIC S9(9)V99  COMP.
           03  WS-ART                  PIC 9(5)      COMP   OCCURS 3.
           03  wj-zeil                 pic 9(6)      COMP-x.
           03  WV-KTONR                PIC 9(6)      COMP.
           03  WV-VDAT                 PIC 9(6).
           03  WV-BDAT                 PIC 9(6).
           03  WH-BKEY.
               05 WH-KTONR             PIC 9(6)      COMP.
               05 WH-BUDAT             PIC 9(8)      COMP.
               05 WH-LFD               PIC 9999      COMP.
           03  WH-KAT                  PIC 9         COMP.
           03  WK-JS                   PIC 9(7).
           03  WR-JS REDEFINES WK-JS.
               05 WK-SEITE             PIC 99999.
               05 WK-ZEIL              PIC 99.
           03  WV-SEITE                PIC 99999.
           03  wx-seite                pic 9(6)      COMP.
           03  WV-ABDAT                PIC 9(8)      COMP.
           03  WV-BISDAT               PIC 9(8)      COMP.
           03  WV-SALDAT               PIC 9(8)      COMP.
           03  WV-WBEG                 PIC 9(8)      COMP.
           03  WV-WEND                 PIC 9(8)      COMP.
           03  WV-WKZ                  PIC 99        COMP.
           03  WH-BZSATZ               PIC X(120).
      *-----------------------------------------------> Anzeigefelder <-
           03  WD-JONUM                PIC Z.ZZ9-.
           03  WD-SALDO                PIC ZZ.ZZZ.ZZ9,99-.
           03  WD-UMSATZ               PIC ZZZ.ZZZ.ZZ9,99-.
           03  WD-NUM                  PIC ZZZZZ9.
           03  WD-KTO                  PIC ZZZZ9,9.
           03  WD-KZ                   PIC 9.
           03  WD-PZ                   PIC 99.
      *------------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WH-VALUTA               PIC 9(6)      COMP.
           03  WA-TAGE                 PIC 9(7)      COMP.
           03  WB-TAGE                 PIC 9(8)      COMP.
           03  WH-TAG                  PIC S999      COMP.
           03  WN-LFSNAM               PIC X(25).
           03  WN-NAKAL                PIC X(25).
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           PERFORM PERFORM UEB-OPS.
           MOVE WH-CREG TO WL-CREG.
       Z.  STOP RUN.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
       Z.  EXIT.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" USING "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       NO-REC SECTION.
       A.  DISPLAY "keine Daten vorhanden," AT 2401.
           PERFORM WEITER.
           MOVE 07 TO WX-TASTE.
       Z.  EXIT.
      ******************************************************************
       NODATA SECTION.
       A.  DISPLAY "** keine Daten mehr **" with highlight blink
               AT 2458.
           SET FINE TO TRUE.
       Z.  EXIT.
      ******************************************************* Drucker *
       DRUCK SECTION.
       A.  IF WM-DRU = 1 MOVE DRA-STR TO DRA-SATZ(4:).
       C.  WRITE DRA-SATZ AFTER WZ-SCHALT.
           IF WF-STATUS = 27 GO C.
           MOVE SPACE TO DRA-SATZ.
           ADD WZ-SCHALT TO WZ-ZEILEN.
           MOVE 1 TO WZ-SCHALT.
       Z.  EXIT.
      ****************************** Druckerrueckstellung auf 10/Zoll *
       END-DRU SECTION.
       A.  IF WM-OPEN = 0 GO Z.
           IF WM-DRU = 0 MOVE X"1B210000" TO DRA-SATZ(1:4).
       B.  WRITE DRA-SATZ BEFORE PAGE.
           IF WF-STATUS = 27 GO B.
           MOVE SPACE TO DRA-SATZ.
           CLOSE DRUCKER.
           MOVE 0 TO WM-OPEN.
       Z.  EXIT.
      ******************************************************************
       BEG-DRU SECTION.
       A.  IF WM-OPEN > 0 GO Z.
           MOVE 1 TO WM-OPEN.
           OPEN EXTEND DRUCKER.
       C.  MOVE 0 TO WZ-ZEILEN WZ-SCHALT.
           IF WM-FIN = 1 GO Z.                          *> Finanzexport
           IF WM-DRU = 1 MOVE WE-STG(WH-P1 - 10) TO DRA-SATZ
                    else MOVE WK-STG(WH-P1) TO DRA-SATZ.
       D.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO D.
           IF WM-DRU = 1 GO X.
           MOVE WK-STG(WH-P2) TO DRA-SATZ.
       E.  WRITE DRA-SATZ AFTER 0.
           IF WF-STATUS = 27 GO E.
       X.  MOVE SPACE TO DRA-SATZ.
       Z.  EXIT.
      **************************************** OP-Liste Personenkonten *
       KREDOPS SECTION.
       A.  CALL "CAUP" USING "07070805690000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " offene Posten Lieferanten "
               with highlight AT VDU-LP.
           MOVE "LIEF-OPS.LST" TO WH-DRUNAM.
           ADD 245 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-DRUNAM with highlight AT VDU-LP.
           MOVE 0 TO WH-MOD.
       C.  ADD 133 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           IF WH-MOD = 1 DISPLAY "  numerisch   " AT VDU-LP
                    else DISPLAY " alphabetisch " AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<#>= Start alle, <ret>= Start fÑllige, <esc>= Abbruc
      -        "h <       >" AT VDU-LP.
           MOVE 0 TO WE.
           IF WH-MOD = 1 DISPLAY "< >= Listung alphabetisch" AT 2401
                    else DISPLAY "< >= Listung numerisch" AT 2401.
           CALL "CAUP" USING "1003586007" WH-CREG.
           IF ESC GO X.
           IF KIST MOVE 1 TO WE.
           IF AUF; IF WH-MOD = 1 MOVE 0 TO WH-MOD
                   else MOVE 1 TO WH-MOD.
           MOVE 0 TO WG.
      *    PERFORM OB-LFTEINZ.
           IF not RET AND not KIST GO C.
           MOVE 0 TO WS-ART(1) WS-ART(2) WS-ART(3) WZ-SEITE
               WM-SOLL WM-HABEN WZ-ZEILEN WG-SOLL(1) WG-HABEN(1).
       E.  ADD 403 VDU-ECK GIVING VDU-LP.
           IF WV-KTONR not = 0 MOVE 1 TO WE GO H.
           MOVE 0 TO KR-KTONR WB.
           IF WH-MOD = 1 START KREDIT KEY not < KR-KEY INVALID GO Z
                         end-start GO G.
           MOVE LOW-VALUES TO KR-MCODE.
           START KREDIT KEY not < KR-MCODE INVALID GO Z.
       G.  READ KREDIT NEXT NO LOCK AT END GO Q.
           IF ZUGRIF PERFORM BESETZT GO G.
       H.  SUBTRACT KR-HABEN FROM KR-SOLL GIVING WM-TEIL.
           MOVE KR-KTOART TO WF.
           IF WF not = WG ADD 1 TO WS-ART(WF + 1) GO G.
      *-------------------------------------> wenn Saldo dann drucken <-
           MOVE 0 TO WS-OPS OP-REF OP-LFD WK-ZEIL OP-VALDAT.
           MOVE KR-KTONR TO OP-KTONR.
           MOVE 3 TO OP-DK.
           START OPDATEI KEY not < OP-KEY INVALID GO G.
       J.  READ OPDATEI NEXT NO LOCK AT END GO P.
           IF ZUGRIF PERFORM BESETZT GO J.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           IF WH-WERT = 0 GO J.
           IF OP-KTONR not = KR-KTONR GO P.
      *----------------------------------------> FÑlligkeit ermitteln <-
           MOVE OP-KOND TO WH-KK.
           IF OP-KOND = 0 MOVE KR-KOND TO WH-KK.
           IF WH-TGN = 0 MOVE 30 TO WH-TGN.
           MOVE WH-TGN TO WH-TAG.
           PERFORM FAELLIG.
           IF WE = 0; IF WB-TAGE > WM-DATUM GO J.
           MOVE WB-TAGE TO WS-DATUM.
           PERFORM KREOPKOPF.
           MOVE OP-VALDAT TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRO-DAT.
           MOVE WE-SYM(OP-SYM + 1) TO DRO-SYM.
           MOVE OP-REF TO DRO-REFNR.
           MOVE OP-REBETRAG TO DRO-REBET.
           MOVE OP-ZABETRAG TO DRO-ZABET.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           ADD WH-WERT TO WS-OPS.
           MOVE WH-WERT TO DRO-OFFEN.
           MOVE WS-DATUM TO WC-DATUM.
           PERFORM DATDREH.
           MOVE VDU-DATUM TO DRO-FAELLIG.
           IF OP-ZPBET not = 0 MOVE " Za" TO DRO-ZAHL.
           PERFORM DRUCK.
           IF WK-ZEIL = 0 MOVE 1 TO WK-ZEIL
                     else MOVE 2 TO WK-ZEIL.
           GO J.
       P.  SUBTRACT KR-SOLL FROM KR-HABEN GIVING WM-TEIL.
           IF WS-OPS not = 0 AND WK-ZEIL = 2 OR WM-TEIL not = 0
               OR WM-TEIL not = WS-OPS
               PERFORM KREOPKOPF
               MOVE "Summe OP's" TO DRP-STR(50:)
               MOVE WS-OPS TO DRO-OFFEN
               IF WS-OPS not = WM-TEIL AND WE = 1
                   MOVE "abw" TO DRO-FAELLIG end-if
               PERFORM DRUCK.
           MOVE 2 TO WZ-SCHALT.
           IF WV-KTONR = 0 GO G.
       Q.  IF WG not = 2 ADD 1 TO WG
               IF WS-ART(WG + 1) not = 0 GO E else GO Q.
           IF WM-SOLL not = 0 OR WM-HABEN not = 0
               MOVE ALL "ƒ" TO DRP-STR(1:)
               PERFORM DRUCK
               MOVE "S u m m e n:" TO DRP-STR(10:15)
               MOVE WM-SOLL TO DRP-SOLL
               MOVE WM-HABEN TO DRP-HABEN
               SUBTRACT WM-SOLL FROM WM-HABEN GIVING DRP-SALDO
               PERFORM DRUCK
               MOVE ALL "=" TO DRP-STR(63:) PERFORM DRUCK
               MOVE 2 TO WZ-SCHALT
               MOVE 0 TO WM-SOLL WM-HABEN.
       W.
      *    PERFORM LISDAT.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ***************************************** Kreitoren-OP Kopfdruck *
       KREOPKOPF SECTION.
       A.
       Z.  EXIT.
      ********************************************** Listung OP-Kunden *
       UEB-OPS SECTION.
       A.  CALL "CAUP" USING "07070805690000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " offene Posten " with highlight AT VDU-LP.
           MOVE "OP-SOFTL.TXT" TO WH-FIBNAM.
           ADD 245 VDU-ECK GIVING VDU-LP.
           DISPLAY WH-FIBNAM with highlight AT VDU-LP.
           MOVE 0 TO WH-MOD WG WH-VON.
           MOVE 999999 TO WH-BIS.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<ret>= Start alle, <esc>= Abbruch < >" AT VDU-LP.
           MOVE 0 TO WE.
           CALL "CAUP" USING "1003380000" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           DELETE FILE EXCELIS.
           OPEN EXTEND EXCELIS.
           MOVE "\FAKT\sg\SPILFKOP.DAT" TO WN-LFSNAM.
           MOVE "\FAKT\sg\SPINAKAL.DAT" TO WN-NAKAL.
           MOVE WN-BUEB(9:2) TO WN-LFSNAM(7:2) WN-NAKAL(7:2).
           open input kopf.
           open input nachkalk.
           MOVE "firma;anwendung;programm;benutzer;buchsatznr;lfdnr;kate
      -        "gorie;nummer;belegdatum;bezugsnummer;" TO EXC-SATZ
           MOVE "belegnummer;valutadatum;faelldatum;symbol;varn1;buchper
      -        "iode;bemerkung;soll;haben;mehrwertsteuer;tage1;skonto1;t
      -        "age2;skonto2;nettotage" TO EXC-SATZ(95:).
           WRITE EXC-SATZ.
           MOVE SPACE TO EXC-SATZ.
       E.  ADD 403 VDU-ECK GIVING VDU-LP.
           MOVE 0 TO DE-KTONR WB WM-ANZ WM-ZAHL.
           MOVE WH-VON TO DE-KTONR.
           START DEBITOR KEY not < DE-KEY INVALID GO Z.
       G.  READ DEBITOR NEXT IGNORE LOCK AT END GO K.
       H.  SUBTRACT DE-HABEN FROM DE-SOLL GIVING WM-TEIL.
           IF WM-TEIL = 0 GO G.
           MOVE 0 TO WM-ZAHL WM-SOLL WM-HABEN.
      *-------------------------------------> wenn Saldo dann drucken <-
           MOVE 0 TO WS-OPS OP-REF OP-LFD OP-VALDAT WK-ZEIL.
           MOVE DE-KTONR TO OP-KTONR.
           MOVE 2 TO OP-DK WE.
           START OPDATEI KEY not < OP-KEY INVALID GO G.
       J.  READ OPDATEI NEXT IGNORE LOCK AT END GO K.
           IF OP-KTONR not = DE-KTONR
                SUBTRACT WM-HABEN FROM WM-SOLL GIVING WM-UMSATZ
                IF WM-UMSATZ not = WM-TEIL PERFORM WEITER
                end-if
                GO G.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           IF WH-WERT = 0 GO J.
           IF OP-KOND = 0 MOVE DE-KOND TO OP-KOND.
           MOVE DE-KTONR TO WV-KTONR.
           IF WN-BUEB(9:2) = "BG" ADD 900000 TO WV-KTONR.
           PERFORM EXC-AUSGABE.
           GO J.
      *-------------------------------------------> nun die Kred-OP`s <-
       K.  MOVE 0 TO KR-KTONR WB.
           START KREDIT KEY not < KR-KEY INVALID GO Z.
       M.  READ KREDIT NEXT IGNORE LOCK AT END GO W.
           SUBTRACT KR-HABEN FROM KR-SOLL GIVING WM-TEIL.
           MOVE KR-KTONR TO OP-KTONR.
           MOVE 3 TO OP-DK WE.
           MOVE 0 TO WS-OPS OP-REF OP-LFD WK-ZEIL OP-VALDAT.
           START OPDATEI KEY not < OP-KEY INVALID GO W.
       P.  READ OPDATEI NEXT IGNORE LOCK AT END GO W.
           IF OP-KTONR not = KR-KTONR GO M.
           SUBTRACT OP-ZABETRAG FROM OP-REBETRAG GIVING WH-WERT.
           IF WH-WERT = 0 GO P.
           IF OP-KOND = 0 MOVE KR-KOND TO OP-KOND.
           MOVE KR-KTONR TO WV-KTONR.
           PERFORM EXC-AUSGABE.
           GO P.
       W.  CLOSE EXCELIS.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       EXC-AUSGABE SECTION.
       A.  IF WM-ZAHL = 0 MOVE 0 TO WM-SUB
                          ADD 1 TO WM-ANZ WM-ZAHL.
      *----------------------------------------> FÑlligkeit ermitteln <-
           IF OP-VALDAT(1:3)= "0  " DELETE OPDATEI
                                    MOVE OP-REDAT TO OP-VALDAT
                                    WRITE OP-SATZ.
           IF OP-VALDAT = 0 MOVE OP-REDAT TO OP-VALDAT.
           MOVE OP-KOND TO WH-KK.
           IF WH-TGN = 0 MOVE 30 TO WH-TGN.
           MOVE WH-TGN TO WH-TAG.
           PERFORM FAELLIG.
           MOVE WB-TAGE TO WS-DATUM.
           EVALUATE WN-BUEB(9:2)
               WHEN "SG"
               WHEN "BG" MOVE "FIRMA1" TO EXC-SATZ
               WHEN "TR" MOVE "FIRMA2" TO EXC-SATZ
               WHEN "GR" MOVE "FIRMA3" TO EXC-SATZ
               WHEN "VW" MOVE "FIRMA5" TO EXC-SATZ
               WHEN "OA" MOVE "FIRMA6" TO EXC-SATZ
               WHEN "WZ" MOVE "FIRMA7" TO EXC-SATZ
               WHEN "WW" MOVE "FIRMA8" TO EXC-SATZ.
           MOVE ";" TO EXC-SATZ(7:).
           MOVE "FIB" TO EXC-SATZ(8:).
           MOVE WN-BUEB(9:2) TO EXC-SATZ(11:).
           MOVE ";UEBFIB;UEBFIB;" TO EXC-SATZ(13:).
           MOVE 28 TO WI.
           MOVE WM-ANZ TO WD-DIS.
           PERFORM ANF-NUM.
           ADD 1 TO WM-SUB.
           MOVE WM-SUB TO WD-DIS.
           PERFORM ANF-NUM.
           MOVE WE TO WD-DIS.
           PERFORM ANF-NUM.
           COMPUTE WD-DIS = WV-KTONR / 10.
           PERFORM ANF-NUM.
           MOVE OP-REDAT TO WD-DIS.
           PERFORM ANF-NUM.
           MOVE OP-REF TO WD-DIS.
           PERFORM ANF-NUM.
           MOVE OP-RENUM TO WD-DIS.
           PERFORM ANF-NUM.
           MOVE OP-VALDAT TO WD-DIS.
           PERFORM ANF-NUM.
           MOVE WS-DATUM TO WD-DIS.
           PERFORM ANF-NUM.
           MOVE WE-SYM(OP-SYM + 1) TO EXC-SATZ(WI:).
           move 0 to na-aufnum.
           if op-sym > 21 perform ab-nr.
           move na-aufnum to wd-dis.
           ADD 3 TO WI.
           MOVE ";" TO EXC-SATZ(WI:).
           ADD 1 TO WI.
           PERFORM ANF-NUM.
           IF WN-BUEB(9:5) = "FIBWZ" MOVE 20040331 TO WD-DIS
                                else MOVE 20040229 TO WD-DIS.
           PERFORM ANF-NUM.
           MOVE "Uebernahme" TO EXC-SATZ(WI:).
           ADD 10 TO WI.
           MOVE ";" TO EXC-SATZ(WI:).
           ADD 1 TO WI.
           MOVE OP-REBETRAG TO WD-BET.
           ADD OP-REBETRAG TO WM-SOLL.
           PERFORM ANF-BET.
           MOVE OP-ZABETRAG TO WD-BET.
           ADD OP-ZABETRAG TO WM-HABEN.
           PERFORM ANF-BET.
           MOVE OP-MW TO WD-BET.
           PERFORM ANF-BET.
           MOVE WH-SK1 TO WD-BET.
           PERFORM ANF-BET.
           MOVE WH-TG1 TO WD-DIS.
           PERFORM ANF-NUM.
           MOVE WH-SK2 TO WD-BET.
           PERFORM ANF-BET.
           MOVE WH-TG2 TO WD-DIS.
           PERFORM ANF-NUM.
           MOVE WH-TGN TO WD-DIS.
           PERFORM ANF-NUM.
           ADD -1 TO WI.
           MOVE " " TO EXC-SATZ(WI:).
           DISPLAY EXC-SATZ(1:80) AT 2001.
           WRITE EXC-SATZ.
           MOVE SPACE TO EXC-SATZ.
       Z.  EXIT.
      ********************************************** numerisch anfÅgen *
       ANF-NUM SECTION.
       A.  PERFORM VARYING IX FROM 1 BY 1 UNTIL IX = 8 or
               WD-DIS(IX:1) not = SPACE CONTINUE.
           PERFORM VARYING WI FROM WI BY 1 UNTIL IX = 9
               MOVE WD-DIS(IX:1) TO EXC-SATZ(WI:)
               ADD 1 TO IX.
           MOVE ";" TO EXC-SATZ(WI:).
           ADD 1 TO WI.
       Z.  EXIT.
      ********************************************** numerisch anfÅgen *
       ANF-BET SECTION.
       A.  PERFORM VARYING IX FROM 1 BY 1 UNTIL IX = 11 or
               WD-BET(IX:1) not = SPACE CONTINUE.
           PERFORM VARYING WI FROM WI BY 1 UNTIL IX = 13
               MOVE WD-BET(IX:1) TO EXC-SATZ(WI:)
               ADD 1 TO IX.
           MOVE ";" TO EXC-SATZ(WI:).
           ADD 1 TO WI.
       Z.  EXIT.
      ******************************************** Auftrags-Nr. suchen *
       AB-NR SECTION.
      *    03  NA-DATUM                PIC 9(6)      COMP.
       a.  move 0 to na-aufnum.
           if we = 3 go z.
           move op-renum to na-renum.
           move 0 to na-bl.
           start nachkalk key not < na-key invalid go z.
           read nachkalk invalid move 0 to na-aufnum.
           move na-lfnum to lk-num.
           move "L" to lk-sa.
           read kopf invalid move 0 to na-aufnum.
           if lk-rekto = de-ktonr go z.
           move 0 to na-aufnum.
       Z.  EXIT.
      ***************************************** Ermittlung Valutadatum *
       FAELLIG SECTION.
       A.  COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(OP-VALDAT).
           ADD WH-TAG TO WA-TAGE GIVING WB-TAGE.
           COMPUTE WB-TAGE = FUNCTION DATE-OF-INTEGER(WB-TAGE).
           MOVE WB-TAGE TO WH-VALUTA WC-DATUM WZ-DATUM.
       Z.  EXIT.
