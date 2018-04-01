      $SET LINKCOUNT "320" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FIBREG.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.  PC.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY FIBUSEC.CPY.
           SELECT EALAGER  ASSIGN TO "FIBULAG.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY EA-KEY
                           ALTERNATE KEY EA-JKEY DUPLICATES
                           FILE STATUS WF-STATUS.
           SELECT ZEIL     ASSIGN TO "ZEIL.DAT"
                           ORGANIZATION RECORD SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT FBBUSATZ ASSIGN "FBBUSATZ.DAT"
                           ORGANIZATION INDEXED, ACCESS DYNAMIC
                           RECORD KEY FB-KEY
                           ALTERNATE KEY FB-RKEY DUPLICATES
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ************************ ASCII-BuchsÑtze f. FibuÅberleitung FAKT *
       FD  FBBUSATZ     external       LABEL RECORD STANDARD.
       01  FB-SATZ.
           03  FB-KEY                  PIC 9(6).
           03  FB-RKEY.
               05 FB-SA                PIC X.
               05 FB-REN               PIC 9(7).
           03  FB-KTONR                PIC 9(6).
           03  FB-KOND                 PIC 9(13).
           03  FB-DAT                  PIC 9(6).
           03  FB-SY                   PIC 99.
           03  FB-GK                   PIC 9(6).
           03  FB-Z                    PIC 9.
           03  FB-VM                   PIC 9.
           03  FB-RF                   PIC 9(7).
           03  FB-U1                   PIC 9.
           03  FB-U2                   PIC 9.
           03  FB-MW                   PIC S9(7)V99.
           03  FB-B1                   PIC S9(9)V99.
           03  FB-B2                   PIC S9(9)V99.
           03  FB-SH                   PIC S9.
           03  FB-BET                  PIC S9(9)V99.
           03  FB-KST                  PIC S9(5).
           03  FB-TX                   PIC X(40).
           03  FB-SKTOBAS              PIC S9(9)V99.
           03  FB-VALDAT               PIC 9(6).
      *--------------------------------------------> fÅr FremdwÑhrung <-
           03  FB-FWBET                PIC S9(9)V99.
           03  FB-KURS                 PIC 9(4)V9(7).
           03  FB-FSYM                 PIC XXX.
      *------------------ FB-SA= "K" ---------------> KontenÅbernahme <-
       01  FB-KSATZ.
           03  FILLER                  PIC X(19).
           03  FB-MCODE                PIC X(8).
           03  FB-ANREDE               PIC 99.
           03  FB-BEZ                  PIC X(110).
           03  FB-TEL                  PIC X(35).
      ************************************* Journal- u. Buchungszeilen *
       FD  EALAGER      external       RECORD VARYING FROM 75 TO 115
                                       DEPENDING ON WZ-BUCH.
       01  EA-SATZ.
           03  EA-KEY.
               05 EA-KTONR             PIC 9(6)      COMP.
               05 EA-BUDAT             PIC 9(8)      COMP.
               05 EA-LFD               PIC 9999      COMP.
           03  EA-JKEY.
               05 EA-USE               PIC 99        COMP.
               05 EA-SEITE             PIC 9(7)      COMP-X.
           03  EA-BELNR                PIC 9(6)      COMP.
           03  EA-BELDAT               PIC 9(8)      COMP.
           03  EA-REFNR                PIC 9(7)      COMP.
           03  EA-SYM                  PIC 99        COMP.
           03  EA-GGKTO                PIC 9(6)      COMP.
           03  EA-SH                   PIC 9         COMP.
           03  EA-BUBET                PIC S9(9)V99  COMP.
           03  EA-Z                    PIC 9         COMP.
           03  EA-VM                   PIC 9         COMP.
           03  EA-MW                   PIC S9(7)V99  COMP.
           03  EA-U1                   PIC 99        COMP.
           03  EA-B1                   PIC S9(9)V99  COMP.
           03  EA-U2                   PIC 99        COMP.
           03  EA-B2                   PIC S9(9)V99  COMP.
           03  EA-KSTNR                PIC 9(5)      COMP.
           03  EA-KOND                 PIC 9(13)     COMP.
           03  EA-OPLFD                PIC 9999      COMP.
           03  EA-FWBET                PIC S9(9)V99  COMP.
           03  EA-KURS                 PIC 9(4)V9(7) COMP.
           03  EA-FSYM                 PIC XXX.
           03  EA-TX                   PIC X(40).
      *----------------------------------------> Jahresabschlu·summen <-
       01  EA-ASATZ.
           03  FILLER                  PIC X(52).
           03  EA-SOLL                 PIC S9(9)V99  COMP.
           03  EA-HABEN                PIC S9(9)V99  COMP.
           03  EA-EBIL                 PIC S9(9)V99  COMP.
      ************************************* Journal- u. Buchungszeilen *
       FD      ZEIL     external       RECORD VARYING FROM 75 TO 115
                                       DEPENDING ON WZ-BUCH.
       01  ZZ-SATZ.
           03  ZZ-KEY.
               05 ZZ-KTONR             PIC 9(6)      COMP.
               05 ZZ-BUDAT             PIC 9(8)      COMP.
               05 ZZ-LFD               PIC 9999      COMP.
           03  ZZ-JKEY.
               05 ZZ-USE               PIC 99        COMP.
               05 ZZ-SEITE             PIC 9(7)      COMP-X.
           03  ZZ-BELNR                PIC 9(6)      COMP.
           03  ZZ-BELDAT               PIC 9(8)      COMP.
           03  ZZ-REFNR                PIC 9(7)      COMP.
           03  ZZ-SYM                  PIC 99        COMP.
           03  ZZ-GGKTO                PIC 9(6)      COMP.
           03  ZZ-SH                   PIC 9         COMP.
           03  ZZ-BUBET                PIC S9(9)V99  COMP.
           03  ZZ-Z                    PIC 9         COMP.
           03  ZZ-VM                   PIC 9         COMP.
           03  ZZ-MW                   PIC S9(7)V99  COMP.
           03  ZZ-U1                   PIC 99        COMP.
           03  ZZ-B1                   PIC S9(9)V99  COMP.
           03  ZZ-U2                   PIC 99        COMP.
           03  ZZ-B2                   PIC S9(9)V99  COMP.
           03  ZZ-KSTNR                PIC 9(5)      COMP.
           03  ZZ-KOND                 PIC 9(13)     COMP.
           03  ZZ-OPLFD                PIC 9999      COMP.
           03  ZZ-FWBET                PIC S9(9)V99  COMP.
           03  ZZ-KURS                 PIC 9(4)V9(7) COMP.
           03  ZZ-FSYM                 PIC XXX.
           03  ZZ-TX                   PIC X(40).
      *----------------------------------------> Jahresabschlu·summen <-
       01  ZZ-ASATZ.
           03  FILLER                  PIC X(52).
           03  ZZ-SOLL                 PIC S9(9)V99  COMP.
           03  ZZ-HABEN                PIC S9(9)V99  COMP.
           03  ZZ-EBIL                 PIC S9(9)V99  COMP.
      *01  ZZ-SATZ.
      *    03  ZZ-KEY.
      *        05 ZZ-KTONR             PIC 9(6)      COMP.
      *        05 ZZ-BUDAT             PIC 9(8)      COMP.
      *        05 ZZ-LFD               PIC 9999      COMP.
      *    03  ZZ-JKEY.
      *        05 ZZ-USE               PIC 99        COMP.
      *        05 ZZ-SEITE             PIC 9(7)      COMP-X.
      *    03  ZZ-BELNR                PIC 9(6)      COMP.
      *    03  ZZ-BELDAT               PIC 9(8)      COMP.
      *    03  ZZ-REFNR                PIC 9(7)      COMP.
      *    03  ZZ-SYM                  PIC 99        COMP.
      *    03  ZZ-GGKTO                PIC 9(6)      COMP.
      *    03  ZZ-SH                   PIC 9         COMP.
      *    03  ZZ-BUBET                PIC S9(9)V99  COMP.
      *    03  ZZ-Z                    PIC 9         COMP.
      *    03  ZZ-VM                   PIC 9         COMP.
      *    03  ZZ-MW                   PIC S9(7)V99  COMP.
      *    03  ZZ-U1                   PIC 99        COMP.
      *    03  ZZ-B1                   PIC S9(9)V99  COMP.
      *    03  ZZ-U2                   PIC 99        COMP.
      *    03  ZZ-B2                   PIC S9(9)V99  COMP.
      *    03  ZZ-KSTNR                PIC 9(5)      COMP.
      *    03  ZZ-KOND                 PIC 9(13)     COMP.
      *    03  ZZ-OPLFD                PIC 9999      COMP.
      *    03  ZZ-FWBET                PIC S9(9)V99  COMP.
      *    03  ZZ-KURS                 PIC 9(4)V9(7) COMP.
      *    03  ZZ-FSYM                 PIC XXX.
      *    03  ZZ-TX                   PIC X(40).
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WH-REG.
           03  WX-J                    PIC 9(6).
           03  WA-USE                  PIC 99        COMP.
           03  WB-USE                  PIC 99        COMP.
           03  WH-MOD                  PIC 99        COMP.
           03  WD-SEITE                PIC ZZZZ.ZZZ-.
           03  WD-USE                  PIC Z9-.
           03  WM-BUCH                 PIC 9(5).
           03  WH-SEITE                PIC 9(7)      COMP-X.
           03  WD-KZ                   PIC 9.
           03  WK-CODE                 PIC 9999.
      *----------------------------------------------> Command-Line <-
           03  RESULT                  PIC 99         COMP-X.
           03  FUNKT                   PIC 99         COMP-X VALUE 35.
           03  PARAM.
               05 SUB                  PIC 99         COMP-X VALUE 0.
               05 PAR                  PIC X(6)       VALUE  SPACE.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       DECL-G SECTION.         USE AFTER ERROR PROCEDURE ON EALAGER.
       A.  CALL "CADECL" USING "FIBULAG.DAT " WH-CREG.
       Z.  EXIT.
       END DECLARATIVES.
      *****************************************************************
       STEUER SECTION.
       A.  DISPLAY SPACE UPON CRT.
           MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           IF WL-CA = 10 PERFORM FB-UMWAND GO W.
           IF WL-CA = 50 PERFORM LAGERN GO W.
           IF WL-CA = 60 PERFORM STAMKONTROL GO Z.
           MOVE 9 TO WH-KEY.
       C.  READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF GO C.
           ADD 50 TO KO-WKZ(1).
           ADD 50 TO KO-WKZ(2).
           ADD 50 TO KO-WKZ(3).
           ADD 50 TO KO-WKZ(4).
           MOVE KO-WKZ(1) TO KO-WKZ(1).
           MOVE KO-WKZ(2) TO KO-WKZ(2).
           MOVE KO-WKZ(3) TO KO-WKZ(3).
           MOVE KO-WKZ(4) TO KO-WKZ(4).
           CALL "CAUP" USING "0707050570000" WH-CREG.
       D.  ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY "Reorg FIBUZEIL.DAT" with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "<esc>= Abbruch, Code+<Einfg>= Start <    >"
               AT VDU-LP.
           CALL "CAUP" USING "1003404004" WH-CREG.
           IF ESC GO X.
           IF not EINF GO D.
           COMPUTE WK-CODE = WH-DATUM + 4711.
           IF WH-NUM not = WK-CODE GO D.
           CLOSE DEBITOR KREDIT SACHBUCH KONSTANT BUCHZEIL OPDATEI.
           DISPLAY LOW-VALUE AT 2020.
           DISPLAY "ren fibuzeil.dat zeil.dat" UPON COMMAND-LINE.
           CALL X"91" USING RESULT FUNKT PARAM.
           IF RESULT NOT = 0 DISPLAY "Fehler!!!" at 2112 GO X.
           IF RETURN-CODE NOT = 0 GO X.
           OPEN INPUT ZEIL.
           IF WF-STATUS > "00" GO Z.
           MOVE "FIBUZEIL.DAT" TO WN-BZEI.
           OPEN OUTPUT BUCHZEIL.
           MOVE 0 TO WX-J.
       G.  MOVE SPACE TO BZ-TX.
           READ ZEIL NEXT AT END GO Q.
           MOVE ZZ-SATZ TO BZ-SATZ.
      *---------------------------------------------------------------
           ADD 1 TO WX-J.
           DISPLAY BZ-SEITE AT 0952 " " WX-J.
           MOVE 1 TO WX.
           PERFORM VARYING WY FROM 4 BY -1 UNTIL WY = 1
               IF KO-WBEG(WY) NOT = 0 AND BZ-BUDAT >= KO-WBEG(WY)
                   MOVE WY TO WX.
      *    IF BZ-USE = 0 OR > 40 MOVE KO-WKZ(WX) TO BZ-USE.
           WRITE BZ-SATZ invalid display "Fehler: " at 2112
               bz-ktonr " " bz-budat "  " PERFORM weiter.
           GO G.
       Q.  CLOSE ZEIL.
           DISPLAY "del zeil.*" UPON COMMAND-LINE.
           CALL X"91" USING RESULT FUNKT PARAM.
       W.  CLOSE BUCHZEIL.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  STOP RUN.
      *********************************** Journalzeilen aus- einlagern *
       LAGERN SECTION.
       A.  CALL "CAUP" USING "0710100560000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Aus-/Einlagerung " with highlight AT VDU-LP.
           OPEN I-O EALAGER.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Code: <    >, +<Entf>= aus-, <Einfg>= einlagern"
               AT VDU-LP.
           CALL "CAUP" USING "1003104004" WH-CREG.
           IF ESC GO X.
           IF NOT ENTF AND NOT EINF GO C.
           IF ENTF MOVE 1 TO WH-MOD.
           IF EINF MOVE 0 TO WH-MOD.
           IF WH-NUM NOT = 1174 GO A.
       E.  ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY "von User:     auf User:   " AT VDU-LP.
           CALL "CAUP" USING "1004132002" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           MOVE WH-NUM TO WA-USE WD-USE.
           DISPLAY WD-USE with highlight AT VDU-LP.
       G.  CALL "CAUP" USING "1004272002" WH-CREG.
           IF ESC GO X.
           IF WOLI GO E.
           IF NOT RET GO G.
           MOVE WH-NUM TO WB-USE WD-USE.
           DISPLAY WD-USE with highlight AT VDU-LP.
      *--------------------------------> eigentliche Aus-/Einlagerung <-
           IF WH-MOD = 0 PERFORM EINLAGERN
                    else PERFORM AUSLAGERN.
       X.  CLOSE EALAGER.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " with highlight AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       AUSLAGERN SECTION.
       A.  MOVE LOW-VALUE TO BZ-SATZ.
           MOVE WA-USE TO BZ-USE.
           START BUCHZEIL KEY NOT < BZ-JKEY.
       C.  READ BUCHZEIL NEXT AT END GO X.
           IF ZUGRIF GO C.
           IF BZ-USE NOT = WA-USE GO X.
      *    if bz-seite > 8000000 subtract 8000000 from bz-seite.
           MOVE BZ-SATZ TO EA-SATZ.
           MOVE WB-USE TO EA-USE.
           WRITE EA-SATZ INVALID STOP RUN.
           ADD 440 VDU-ECK GIVING VDU-LP.
           MOVE EA-SEITE TO WD-SEITE.
           DISPLAY WD-SEITE with highlight AT VDU-LP.
           DELETE BUCHZEIL.
           GO C.
       X.  DISPLAY "Auslagerung beendet!" with highlight AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      ******************************************************************
       EINLAGERN SECTION.
       A.  MOVE LOW-VALUE TO EA-SATZ.
           PERFORM LAST-JOUR.
           MOVE WA-USE TO EA-USE.
           START EALAGER KEY NOT < EA-JKEY.
       C.  READ EALAGER NEXT AT END GO X.
           IF ZUGRIF GO C.
           IF EA-USE NOT = WA-USE GO X.
           MOVE EA-SATZ TO BZ-SATZ.
           MOVE WB-USE TO BZ-USE.
           ADD 2 TO WH-SEITE.
       E.  MOVE WH-SEITE TO BZ-SEITE.
           WRITE BZ-SATZ INVALID ADD 1 TO BZ-LFD GO E.
           ADD 440 VDU-ECK GIVING VDU-LP.
           MOVE BZ-SEITE TO WD-SEITE.
           DISPLAY WD-SEITE with highlight AT VDU-LP.
           DELETE EALAGER.
           GO C.
       X.  DISPLAY "Einlagerung beendet!" with highlight AT 2401.
           PERFORM WEITER.
       Z.  EXIT.
      ************************************* letzte Journalnummer holen *
       LAST-JOUR SECTION.
       A.  MOVE LOW-VALUE TO BZ-SATZ.
           MOVE 0 TO WH-SEITE.
           MOVE WB-USE TO BZ-USE.
           MOVE 9999999 TO BZ-SEITE.
           START BUCHZEIL KEY < BZ-JKEY INVALID GO Z.
       C.  READ BUCHZEIL PREVIOUS AT END GO Z.
           IF ZUGRIF GO C.
           MOVE BZ-SEITE TO WH-SEITE.
       Z.  EXIT.
      ************************************ Kontrolle ob Konto vorhanden
       STAMKONTROL SECTION.
       A.  CALL "CAUP" USING "0707100360000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " KontoprÅfung " AT VDU-LP.
           MOVE 0 TO BZ-SEITE.
           MOVE WE-WKZ TO BZ-USE.
           START BUCHZEIL KEY NOT < BZ-JKEY INVALID GO X.
       C.  READ BUCHZEIL NEXT AT END GO X.
           MOVE BZ-KTONR TO WD-KZ DE-KTONR KR-KTONR SA-KTONR.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY BZ-KTONR with highlight AT VDU-LP " " BZ-SEITE.
           EVALUATE WD-KZ
               WHEN 0 READ SACHBUCH INVALID GO W
               WHEN 1 READ SACHBUCH INVALID GO W
               WHEN 2 READ DEBITOR  INVALID GO W
               WHEN 3 READ KREDIT   INVALID GO W.
           GO C.
       W.  ADD 230 VDU-ECK GIVING VDU-LP.
           DISPLAY BZ-KTONR with highlight AT VDU-LP.
           PERFORM WEITER.
           GO C.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************************************** Umwandlung ASCII in COMP *
       FB-UMWAND SECTION.
       A.  OPEN INPUT FBBUSATZ.
           IF WF-STATUS = "35" GO Z.
       C.  READ FBBUSATZ NEXT AT END GO Z.
           IF ZUGRIF GO C.
           MOVE FB-KEY TO BU-KEY.
           MOVE FB-SA TO BU-SA.
           MOVE FB-REN TO BU-REN.
           MOVE FB-KTONR TO BU-KTONR.
           MOVE FB-KOND TO BU-KOND.
           IF FB-SA = "K" GO G.
           MOVE FB-DAT TO BU-DAT.
           MOVE FB-SY TO BU-SY.
           MOVE FB-GK TO BU-GK.
           MOVE FB-Z TO BU-Z.
           MOVE FB-VM TO BU-VM.
           MOVE FB-RF TO BU-RF.
           MOVE FB-U1 TO BU-U1.
           MOVE FB-U2 TO BU-U2.
           MOVE FB-MW TO BU-MW.
           MOVE FB-B1 TO BU-B1.
           MOVE FB-B2 TO BU-B2.
           MOVE FB-SH TO BU-SH.
           MOVE FB-BET TO BU-BET.
           MOVE FB-KST TO BU-KST.
           MOVE FB-TX TO BU-TX.
           MOVE FB-SKTOBAS TO BU-SKTOBAS.
           MOVE FB-VALDAT TO BU-VALDAT.
           MOVE FB-FWBET TO BU-FWBET.
           MOVE FB-KURS TO BU-KURS.
           MOVE FB-FSYM TO BU-FSYM.
           WRITE BU-SATZ INVALID NEXT SENTENCE.
           GO C.
      *------------------ FB-SA= "K" ---------------> KontenÅbernahme <-
       G.  MOVE FB-MCODE TO BU-MCODE.
           MOVE FB-ANREDE TO BU-ANREDE.
           MOVE FB-BEZ TO BU-BEZ.
           MOVE FB-TEL TO BU-TEL.
           WRITE BU-SATZ INVALID NEXT SENTENCE.
           GO C.
       Z.  EXIT.
