      $SET LINKCOUNT"640" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBSOFT.
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BZLTAB   ASSIGN TO PRINTER "BZLBIC.DAT"
                           ORGANISATION LINE SEQUENTIAL
                           FILE STATUS WF-STATUS.
       COPY FIBUSEC.CPY.
           SELECT EXCELIS  ASSIGN TO WH-FIBNAM
                           ORGANIZATION LINE SEQUENTIAL
                           FILE STATUS WF-STATUS.
           SELECT EXCEKUN  ASSIGN TO PRINTER WH-FIBNAM
                           FILE STATUS WF-STATUS.
           SELECT EXCEOPE  ASSIGN TO PRINTER WH-OP-NAM
                           FILE STATUS WF-STATUS.
           SELECT EXCSACH  ASSIGN TO PRINTER WH-SA-NAM
                           FILE STATUS WF-STATUS.
       DATA DIVISION.
       FILE SECTION.
      ********************************************* BZL-Liste (STUZZA) *
       FD  BZLTAB                      LABEL RECORD STANDARD.
       01  BL-SATZ                     PIC X(128).
       COPY FIBUFD.CPY.
      ******************************* Excel-BuchungsdatenÅbergabedatei *
       FD  EXCELIS                     LABEL RECORD STANDARD.
       01  EXC-SATZ                    PIC X(312).
      ************************************************* öbernahme OP's *
       FD  EXCEOPE                     LABEL RECORD STANDARD.
       01  EXO-SATZ.
           03  EXO-FIRMA               PIC 9.          *> Firma
           03  EXO-A                   PIC X.
           03  EXO-KTONR               PIC 9(6).       *> Konto-Nr.
           03  EXO-B                   PIC X.
           03  EXO-IBELNR              PIC 9(6).
           03  EXO-C                   PIC X.
           03  EXO-FBELNR              PIC 9(9).
           03  EXO-D                   PIC X.
           03  EXO-BUSYM               PIC XX.
           03  EXO-E                   PIC X.
           03  EXO-BELDAT              PIC 9(8).
           03  EXO-G                   PIC X.
           03  EXO-VALDAT              PIC 9(8).
           03  EXO-H                   PIC X.
           03  EXO-NTAGE               PIC 999.
           03  EXO-I                   PIC X.
           03  EXO-TG1                 PIC 99.
           03  EXO-J                   PIC X.
           03  EXO-SKTO1               PIC 99,9.
           03  EXO-K                   PIC X.
           03  EXO-TG2                 PIC 99.
           03  EXO-L                   PIC X.
           03  EXO-SKTO2               PIC 99,9.
           03  EXO-M                   PIC X.
           03  EXO-FAELLIG             PIC 9(8).
           03  EXO-N                   PIC X.
           03  EXO-MSTUFE              PIC 9.
           03  EXO-O                   PIC X.
           03  EXO-SPERRE              PIC X.
           03  EXO-P                   PIC X.
           03  EXO-USTSATZ             PIC XX.
           03  EXO-Q                   PIC X.
           03  EXO-OFFEN               PIC -9(6)V99.
           03  EXO-R                   PIC X.
           03  EXO-KAT                 PIC 9.
      ************************************** Excel-KundenÅbergabedatei *
       FD  EXCEKUN                     LABEL RECORD STANDARD.
       01  EXK-SATZ.
           03  EXK-KAT                 PIC 9.
           03  EXK-A                   PIC X.
           03  EXK-KTONR               PIC ZZZZZ9.
           03  EXK-B                   PIC X.
           03  EXK-MCODE               PIC X(15).
           03  EXK-C                   PIC X.
           03  EXK-GRP                 PIC 9.
           03  EXK-D                   PIC X.
           03  EXK-OPKZ                PIC 9.
           03  EXK-E                   PIC X.
           03  EXK-ANR                 PIC X(10).
           03  EXK-F                   PIC X.
           03  EXK-PZ1                 PIC 99,9.
           03  EXK-J                   PIC X.
           03  EXK-TG1                 PIC 999.
           03  EXK-K                   PIC X.
           03  EXK-PZ2                 PIC 99,9.
           03  EXK-L                   PIC X.
           03  EXK-TG2                 PIC 999.
           03  EXK-M                   PIC X.
           03  EXK-TGN                 PIC 999.
           03  EXK-N                   PIC X.
           03  EXK-MAHNT               PIC ZZZ.
           03  EXK-O                   PIC X.
           03  EXK-MAHNDAT             PIC ZZZZZZZZ.
           03  EXK-P                   PIC X.
           03  EXK-TEL                 PIC X(35).
           03  EXK-Q                   PIC X.
           03  EXK-NAME1               PIC X(35).
           03  EXK-R                   PIC X.
           03  EXK-NAME2               PIC X(35).
           03  EXK-S                   PIC X.
           03  EXK-NAME3               PIC X(35).
           03  EXK-T                   PIC X.
           03  EXK-STR                 PIC X(35).
           03  EXK-U                   PIC X.
           03  EXK-PLZL                PIC X(10).
           03  EXK-V                   PIC X.
           03  EXK-ORT                 PIC X(35).
           03  EXK-V1                  PIC X.
           03  EXK-EZG                 PIC X.
           03  EXK-W                   PIC X.
           03  EXK-LANDKZ              PIC XX.
           03  EXK-W                   PIC X.
           03  EXK-IBAN                PIC X(30).
           03  EXK-X                   PIC X.
           03  EXK-BIC                 PIC X(30).
           03  EXK-Y                   PIC X.
           03  EXK-BKNAME              PIC X(30).
           03  EXK-Z                   PIC X.
      ****************************************** Excel-Sachkontendatei *
       FD  EXCSACH                     LABEL RECORD STANDARD.
       01  EXS-SATZ.
           03  EXS-FIRMA               PIC 9.
           03  EXS-A                   PIC X.
           03  EXS-KTONR               PIC ZZZZZ9.
           03  EXS-B                   PIC X.
           03  EXS-MCODE               PIC X(15).
           03  EXS-C                   PIC X.
           03  EXS-KTXT1               PIC X(35).
           03  EXS-D                   PIC X.
           03  EXS-KTXT2               PIC X(35).
           03  EXS-E                   PIC X.
           03  EXS-KTOART              PIC X.
           03  EXS-F                   PIC X.
           03  EXS-KZMV                PIC X.           *> K -  M - V
           03  EXS-G                   PIC X.
           03  EXS-USTSATZ             PIC 99.
           03  EXS-H                   PIC X.
           03  EXS-KER                 PIC 9999.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-ECK                  PIC 9999.
           03  FILLER REDEFINES WL-ECK.
               05  WL-AZ               PIC 99.
               05  WL-VL               PIC 99.
           03  WL-GROSS                PIC 9999.
           03  FILLER REDEFINES WL-GROSS.
               05  WL-VP               PIC 99.
               05  WL-SZ               PIC 99.
           03  WL-KO                   PIC 99.
           03  WL-MA                   PIC 9.
           03  WL-ATTR                 PIC XX.
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WB-BZL                  PIC ZZ999.
           03  WB-KEN                  PIC X(15).
           03  WB-BIC                  PIC X(15).
           03  WB-NAME                 PIC X(50).
           03  WH-DRUNAM               PIC X(12).
           03  WH-FIBNAM               PIC X(25).
           03  WH-OP-NAM               PIC X(25).
           03  WH-SA-NAM               PIC X(25).
           03  WM-OPEN                 PIC 9         COMP.
           03  WM-FIRMA                PIC 9.
           03  WI                      PIC 999        COMP.
           03  WE                      PIC 999        COMP.
           03  IX                      PIC 9999       COMP.
           03  WD-DIS                  PIC ZZZZZZZ9.
           03  WD-BET                  PIC -ZZZZZZZ9,99.
           03  WD-KZ                   PIC 9.
      *------------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WH-VALUTA               PIC 9(6)      COMP.
           03  WA-TAGE                 PIC 9(8)      COMP.
           03  WB-TAGE                 PIC 9(8)      COMP.
           03  WH-TAG                  PIC S999      COMP.
       COPY FIBUEXT.CPY.
       COPY FIBUDECL.CPY.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           MOVE " - FIBU - Softline -" TO WK-GEB.
           CALL "CAUP" USING "06KOPF" WH-CREG.
           CALL "CAUP" USING "27101005400000C" WH-CREG.
       C.  ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Firma:     " AT VDU-LP.
           DISPLAY "<ESC>= Abbruch, <ret>= Firmen-Nr." AT 2301.
           CALL "CAUP" USING "1003101001" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           MOVE WH-NUM TO WM-FIRMA.
           MOVE "\FIRMA1\PERSONEN.DAT" TO WH-FIBNAM.
           MOVE "\FIRMA1\OPOSTEN.DAT" TO WH-OP-NAM.
           MOVE "\FIRMA1\SACHBUCH.DAT" TO WH-SA-NAM.
           MOVE WH-MCODE(1:1) TO WH-FIBNAM(7:1).
           MOVE WH-MCODE(1:1) TO WH-OP-NAM(7:1).
           MOVE WH-MCODE(1:1) TO WH-SA-NAM(7:1).
           PERFORM SOFT-SACH.
           PERFORM SOFT-KUND.
           PERFORM SOFT-KRED.
           PERFORM SOFT-OP.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE WH-CREG TO WL-CREG.
       Z.  EXIT PROGRAM.
      ******************************************************************
       BESETZT SECTION.
       A.  DISPLAY "Record - besetzt" AT 2401.
       Z.  EXIT.
      ******************************************************************
       WEITER SECTION.
       A.  DISPLAY " weiter mit <ret>: " AT 0000.
           MOVE SPACE TO WH-X.
           ACCEPT WH-X AT 0000.
           CALL "CAUP" using "1324012480000" WH-CREG.
       Z.  EXIT.
      *****************************************************************
       DATDREH SECTION.
       A.  MOVE WC-TAG  TO WZ-TAG VDU-JAHR.
           MOVE WC-MONAT TO WZ-MONAT VDU-MONAT.
           MOVE WC-JAHR TO WZ-JAHR VDU-TAG.
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
       A.  MOVE 1 TO IX.
           IF WD-BET(1:1) = "-" MOVE "-" TO EXC-SATZ(WI:1)
               ADD 1 TO WI
               MOVE 2 TO IX.
           PERFORM VARYING IX FROM IX BY 1 UNTIL IX = 11 or
               WD-BET(IX:1) not = SPACE CONTINUE.
           PERFORM VARYING WI FROM WI BY 1 UNTIL IX = 13
               MOVE WD-BET(IX:1) TO EXC-SATZ(WI:)
               ADD 1 TO IX.
           MOVE ";" TO EXC-SATZ(WI:).
           ADD 1 TO WI.
       Z.  EXIT.
      ***************************************** Ermittlung Valutadatum *
       FAELLIG SECTION.
       A.  COMPUTE WA-TAGE = FUNCTION INTEGER-OF-DATE(WB-TAGE).
           ADD WH-TAG TO WA-TAGE GIVING WB-TAGE.
           COMPUTE WB-TAGE = FUNCTION DATE-OF-INTEGER(WB-TAGE).
           MOVE WB-TAGE TO WH-VALUTA WC-DATUM WZ-DATUM.
       Z.  EXIT.
      *********************************** Lieferanten an Fibu Softline *
       SOFT-KRED SECTION.
       A.  OPEN EXTEND EXCEKUN.
           IF WF-STATUS = 05
               MOVE "Kat;Kto-Nr.;Matchcode;Grp.;OP-Kz;Anr.;%-Sk1;Tg-1;%-
      -             "Sk2;TG-2;TG-N;Mahn-Tg;Mahn-dat.;Anmerkg.;Name1;Name
      -             "2;Name3;Stra·e;Plzl;Ort;Einzug" TO EXC-SATZ
               MOVE ";Landeskz;Iban;Bic;Bank" TO EXC-SATZ(133:)
               INSPECT EXC-SATZ REPLACING ALL ";" BY X"09"
               WRITE EXK-SATZ FROM EXC-SATZ
               MOVE SPACE TO EXK-SATZ EXC-SATZ.
       C.  INITIALIZE KR-SATZ.
           ADD 312 VDU-ECK GIVING VDU-LP.
           DISPLAY "Kreditoren" with highlight AT VDU-LP.
           OPEN INPUT BANKDAT.
           START KREDIT KEY not < KR-KEY INVALID STOP RUN.
       E.  READ KREDIT NEXT IGNORE LOCK AT END GO X.
           MOVE ALL x"09" TO EXK-SATZ.
           COMPUTE EXK-KTONR = KR-KTONR / 10 + 200000.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY EXK-KTONR AT VDU-LP " " KR-BEZ(1:25).
           MOVE KR-KTONR TO EXK-KAT.
           MOVE KR-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO EXK-MCODE.
           MOVE KR-KOND TO WH-KK.
           MOVE WH-SK1 TO EXK-PZ1.
           MOVE WH-TG1 TO EXK-TG1.
           MOVE WH-SK2 TO EXK-PZ2.
           MOVE WH-TG2 TO EXK-TG2.
           MOVE WH-TGN TO EXK-TGN.
           MOVE SPACE TO EXK-TEL.
           MOVE 0 TO EXK-MAHNT EXK-MAHNDAT.
           MOVE KR-KTOART TO EXK-GRP.
           MOVE SPACE TO EXK-ANR.
           IF KR-OPKZ > 19 COMPUTE EXK-OPKZ = KR-OPKZ - 20
           else MOVE KR-OPKZ TO EXK-OPKZ.
           MOVE SPACE TO WT-ADR.
           MOVE KR-BEZ TO WT-BEZ.
           INSPECT KR-BEZ REPLACING ALL X"00" BY " ".
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WR-ADR(1) TO EXK-NAME1.
           MOVE WR-ADR(2) TO EXK-NAME2.
           MOVE SPACE TO EXK-PLZL.
           MOVE SPACE TO EXK-ORT EXK-STR EXK-NAME3 EXK-LANDKZ.
           PERFORM HOL-PLZL.
           IF WY > 0 MOVE WR-ADR(WY - 1) TO EXK-STR.
           IF WR-ADR(3) not = space and WR-ADR(3) not = EXK-STR
              MOVE WR-ADR(3) TO EXK-NAME3.
           MOVE " " TO EXK-EZG.
           MOVE SPACE TO EXK-IBAN EXK-BIC EXK-BKNAME.
           MOVE KR-KTONR TO BK-KTONR WD-KZ.
           MOVE " " TO EXK-EZG.
           PERFORM EINZUG.
           WRITE EXK-SATZ.
           GO E.
       X.  CLOSE BANKDAT.
       Z.  EXIT.
      ************************************** Sachbuch an Fibu Softline *
       SOFT-SACH SECTION.
       A.  OPEN EXTEND EXCSACH.
           IF WF-STATUS = 05
               MOVE "Firma;Kontonummer;Matchcode;Kontotext1;Kontotext2;K
      -             "ontoart;KzMwstVst;Ustsatz;Kercode" TO EXC-SATZ
               INSPECT EXC-SATZ REPLACING ALL ";" BY X"09"
               WRITE EXS-SATZ FROM EXC-SATZ
               MOVE SPACE TO EXS-SATZ EXC-SATZ.
       C.  INITIALIZE SA-SATZ.
           ADD 312 VDU-ECK GIVING VDU-LP.
           DISPLAY "Sachbuch  " with highlight AT VDU-LP.
           START SACHBUCH KEY not < SA-KEY INVALID STOP RUN.
       E.  READ SACHBUCH NEXT IGNORE LOCK AT END GO X.
           MOVE ALL x"09" TO EXS-SATZ.
           COMPUTE EXS-KTONR = SA-KTONR / 10 + 200000.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY EXS-KTONR AT VDU-LP " " SA-BEZ(1:25).
           MOVE WM-FIRMA TO EXS-FIRMA.
           MOVE SA-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO EXS-MCODE.
           MOVE SA-TX(1) TO EXS-KTXT1.
           MOVE SA-TX(2) TO EXS-KTXT2.
           IF SA-KTOART = 0 MOVE "B" TO EXS-KTOART
                       else MOVE "E" TO EXS-KTOART.
           MOVE "K" TO EXS-KZMV.
           MOVE 0 TO EXS-USTSATZ.
           MOVE SA-KER-NR TO EXS-KER.
           WRITE EXS-SATZ.
           GO E.
       X.
       Z.  EXIT.
      ************************************** Adressen an Fibu Softline *
       SOFT-KUND SECTION.
       A.  OPEN EXTEND EXCEKUN.
           IF WF-STATUS = 05
               MOVE "Kat;Kto-Nr.;Matchcode;Grp.;OP-Kz;Anr.;%-Sk1;Tg-1;%-
      -             "Sk2;TG-2;TG-N;Mahn-Tg;Mahn-dat.;Anmerkg.;Name1;Name
      -             "2;Name3;Stra·e;Plzl;Ort;Einzug" TO EXC-SATZ
               MOVE ";Landeskz;Iban;Bic;Bank" TO EXC-SATZ(133:)
               INSPECT EXC-SATZ REPLACING ALL ";" BY X"09"
               WRITE EXK-SATZ FROM EXC-SATZ
               MOVE SPACE TO EXK-SATZ EXC-SATZ.
       C.  INITIALIZE DE-SATZ.
           ADD 312 VDU-ECK GIVING VDU-LP.
           DISPLAY "Debitoren " with highlight AT VDU-LP.
           OPEN INPUT BANKDAT.
           START DEBITOR KEY not < DE-KEY INVALID STOP RUN.
       E.  READ DEBITOR NEXT IGNORE LOCK AT END GO X.
           MOVE ALL x"09" TO EXK-SATZ.
           COMPUTE EXK-KTONR = DE-KTONR / 10 + 200000.
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY EXK-KTONR AT VDU-LP " " DE-BEZ(1:25).
           MOVE DE-KTONR TO EXK-KAT.
           MOVE DE-MCODE TO WT-TX.
           CALL "CAUP" USING "21CONV08" WH-CREG.
           MOVE WT-TX TO EXK-MCODE.
           MOVE DE-KOND TO WH-KK.
           MOVE WH-SK1 TO EXK-PZ1.
           MOVE WH-TG1 TO EXK-TG1.
           MOVE WH-SK2 TO EXK-PZ2.
           MOVE WH-TG2 TO EXK-TG2.
           MOVE WH-TGN TO EXK-TGN.
           MOVE DE-TEL TO EXK-TEL.
           MOVE DE-MAHNT TO EXK-MAHNT.
           MOVE DE-MAHNDAT TO EXK-MAHNDAT.
           MOVE DE-KTOART TO EXK-GRP.
           MOVE WT-TXT(DE-ANREDE + 1) TO EXK-ANR.
           IF DE-OPKZ > 19 COMPUTE EXK-OPKZ = DE-OPKZ - 20
           else MOVE DE-OPKZ TO EXK-OPKZ.
           MOVE SPACE TO WT-ADR.
           MOVE DE-BEZ TO WT-BEZ.
           INSPECT DE-BEZ REPLACING ALL X"00" BY " ".
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
           MOVE WR-ADR(1) TO EXK-NAME1.
           MOVE WR-ADR(2) TO EXK-NAME2.
           MOVE SPACE TO EXK-PLZL.
           MOVE SPACE TO EXK-ORT EXK-STR EXK-NAME3.
           PERFORM HOL-PLZL.
           IF WY > 0 MOVE WR-ADR(WY - 1) TO EXK-STR.
           IF WR-ADR(3) not = space and WR-ADR(3) not = EXK-STR
              MOVE WR-ADR(3) TO EXK-NAME3.
           MOVE " " TO EXK-EZG.
           MOVE SPACE TO EXK-IBAN EXK-BIC EXK-BKNAME.
           IF DE-OPKZ > 19 MOVE DE-KTONR TO BK-KTONR WD-KZ
               MOVE "J" TO EXK-EZG
               PERFORM EINZUG.
           WRITE EXK-SATZ.
           GO E.
       X.  CLOSE BANKDAT.
       Z.  EXIT.
      **************************** Postleitzhal und LÑnderkennzeichen *
       HOL-PLZL SECTION.
       A.  MOVE 0 TO WY.
           PERFORM VARYING WZ FROM 1 BY 1 UNTIL WZ > 6
               EVALUATE TRUE
                   WHEN WR-ADR(WZ)(1:2) = "A-" MOVE " " TO EXK-LANDKZ
                                               PERFORM PLZL-ORT
                   WHEN WR-ADR(WZ)(1:2) = "D-" MOVE "DE" TO EXK-LANDKZ
                                               PERFORM PLZL-ORT
                   WHEN WR-ADR(WZ)(1:2) = "H-" MOVE "HU" TO EXK-LANDKZ
                                               PERFORM PLZL-ORT
                   WHEN WR-ADR(WZ)(1:3) = "SL-" MOVE "SL" TO EXK-LANDKZ
                                               PERFORM PLZL-ORT
                   WHEN WR-ADR(WZ)(1:3) = "HU-" MOVE "HU" TO EXK-LANDKZ
                                               PERFORM PLZL-ORT
                   WHEN WR-ADR(WZ)(1:4) = "SLO-" MOVE "SL" TO EXK-LANDKZ
                                               PERFORM PLZL-ORT
                   WHEN WR-ADR(WZ)(1:4) is numeric
                        PERFORM PLZL-ORT
                        MOVE " " TO EXK-LANDKZ.
       Z.  EXIT.
      ******************************************************************
       EINZUG SECTION.
       A.  MOVE WD-KZ TO BK-DK.
           READ BANKDAT INVALID GO Z.
           MOVE BK-IBAN TO EXK-IBAN.
           MOVE BK-BIC TO EXK-BIC.
           MOVE BK-BANK TO EXK-BKNAME.
           IF BK-BIC = SPACE MOVE BK-IBAN(5:5) TO BK-BANKLTZ.
           OPEN INPUT BZLTAB.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 1172 or
               BL-SATZ(1:5) = BK-BANKLTZ(1:5) or
               BL-SATZ(1:3) = BK-BANKLTZ(1:3)
               READ BZLTAB AT END MOVE 1172 TO IX
               end-read.
           CLOSE BZLTAB.
           IF IX > 1171 GO Z.
           UNSTRING BL-SATZ DELIMITED BY ";" INTO WB-BZL WB-KEN WB-BIC
               WB-NAME.
           INSPECT WB-NAME REPLACING ALL "‰" by "Ñ"
                     ALL "ˆ" by "î" ALL "¸" by "Å"
                     ALL "ﬂ" by "·" ALL "ƒ" by "é"
                     ALL "÷" by "ô" ALL"‹"  by "ö".
           MOVE WB-NAME TO EXK-BKNAME.
           IF BK-BIC = SPACE MOVE WB-BIC TO EXK-BIC.
       Z.  EXIT.
      ***************************************** PLZL und ORT abstellen *
       PLZL-ORT SECTION.
       A.  MOVE WR-ADR(WZ) TO WT-TX.
           MOVE WZ TO WY.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               or WT-TX(WX:1) = SPACE or LOW-VALUES MOVE WX TO WZ.
           MOVE WT-TX(1:WZ) TO EXK-PLZL.
           ADD 2 TO WZ.
           MOVE WT-TX(WZ:) TO EXK-ORT.
       Z.  EXIT.
      ***************************************** PLZL und ORT abstellen *
       PLZL-ORT-2 SECTION.
       A.  MOVE WR-ADR(WZ) TO WT-TX.
           MOVE WZ TO WY.
           PERFORM VARYING WX FROM 3 BY 1 UNTIL WX > 8
               IF WT-TX(WX:1) = SPACE or LOW-VALUES MOVE WX TO WZ.
           ADD -1 WZ GIVING WX.
           IF WT-TX(1:4) is NUMERIC MOVE WT-TX(1:WX) TO EXK-PLZL.
           ADD 1 TO WZ.
           MOVE WT-TX(WZ:) TO EXK-ORT.
       Z.  EXIT.
      ***************************************** PLZL und ORT abstellen *
       PLZL-ORT-3 SECTION.
       A.  MOVE WR-ADR(WZ) TO WT-TX.
           MOVE WZ TO WY.
           PERFORM VARYING WX FROM 4 BY 1 UNTIL WX > 8
               IF WT-TX(WX:1) = SPACE or LOW-VALUES MOVE WX TO WZ.
           ADD -1 WZ GIVING WX.
           IF WT-TX(1:4) is NUMERIC MOVE WT-TX(1:WX) TO EXK-PLZL.
           ADD 1 TO WZ.
           MOVE WT-TX(WZ:) TO EXK-ORT.
       Z.  EXIT.
      ***************************************** PLZL und ORT abstellen *
       PLZL-ORT-4 SECTION.
       A.  MOVE WR-ADR(WZ) TO WT-TX.
           MOVE WZ TO WY.
           PERFORM VARYING WX FROM 5 BY 1 UNTIL WX > 9
               IF WT-TX(WX:1) = SPACE or LOW-VALUES MOVE WX TO WZ.
           ADD -1 WZ GIVING WX.
           IF WT-TX(1:4) is NUMERIC MOVE WT-TX(1:WX) TO EXK-PLZL.
           ADD 1 TO WZ.
           MOVE WT-TX(WZ:) TO EXK-ORT.
       Z.  EXIT.
      ****************************************** OP-öbernahme Softline *
       SOFT-OP SECTION.
       A.  OPEN EXTEND EXCEOPE.
           IF WF-STATUS not = 05 GO C.
           MOVE "Firma;Kontonummer;Belegnummer;Femdbelegnummer;BuSymbol;
      -         "Belegdatum;Valuatadatum;Nettotage;Tage1;Skonto1;Tage2;S
      -         "konto2;FÑlligkeit;Mahnstufe;" TO EXC-SATZ.
           MOVE "Sperre;Ustsatz;Betrag offen;Kategorie"
                TO EXC-SATZ(139:).
           INSPECT EXC-SATZ REPLACING ALL ";" BY X"09".
           WRITE EXC-SATZ.
           MOVE SPACE TO EXO-SATZ EXC-SATZ.
       C.  INITIALIZE OP-SATZ.
           ADD 312 VDU-ECK GIVING VDU-LP.
           DISPLAY "offene Posten" with highlight AT VDU-LP.
           MOVE 2 TO OP-SA OP-DK.
           START OPDATEI KEY not < OP-KEY INVALID STOP RUN.
       E.  READ OPDATEI NEXT IGNORE LOCK AT END GO Z.
           COMPUTE WH-WERT = OP-REBETRAG - OP-ZABETRAG.
           IF WH-WERT = 0 GO E.
           IF OP-VALDAT < 20040000 GO E.
           MOVE ALL x"09" TO EXO-SATZ.
           MOVE WH-WERT TO EXO-OFFEN.
           MOVE WH-NUM TO EXO-FIRMA.
           MOVE OP-KTONR TO EXO-KAT WD-KZ.
           COMPUTE EXO-KTONR = OP-KTONR / 10 + 200000.
           IF WD-KZ = 3 ADD 100000 TO EXO-KTONR.          *> Kreditoren
           ADD 403 VDU-ECK GIVING VDU-LP.
           DISPLAY EXO-KTONR AT VDU-LP.
           MOVE OP-REF TO EXO-IBELNR.
           MOVE OP-RENUM TO EXO-FBELNR.
           MOVE OP-VALDAT TO EXO-VALDAT.
           MOVE OP-REDAT TO EXO-BELDAT.
           MOVE WE-SYM(OP-SYM + 1) TO EXO-BUSYM.
           MOVE OP-KOND TO WH-KK.
           MOVE WH-SK1 TO EXO-SKTO1
           MOVE WH-TG1 TO EXO-TG1.
           MOVE WH-SK2 TO EXO-SKTO2.
           MOVE WH-TG2 TO EXO-TG2.
           MOVE WH-TGN TO EXO-NTAGE.
           MOVE OP-MSTUFE TO EXO-MSTUFE.
      *    MOVE OP-MAHNDAT TO EXO-MAHNDAT.
           MOVE OP-VALDAT TO WB-TAGE.
           MOVE WH-TGN TO WH-TAG.
           PERFORM FAELLIG.
           MOVE WB-TAGE TO EXO-FAELLIG.
           MOVE "N" TO EXO-SPERRE.
           IF OP-FREMDNR not = space MOVE OP-FREMDNR TO EXO-FBELNR.
           EVALUATE OP-U1
               WHEN 0 MOVE 00 TO EXO-USTSATZ
               WHEN 1 MOVE 10 TO EXO-USTSATZ
               WHEN 2 MOVE 20 TO EXO-USTSATZ
               WHEN 3 MOVE 00 TO EXO-USTSATZ
               WHEN 4 MOVE 12 TO EXO-USTSATZ
               WHEN 5 MOVE 20 TO EXO-USTSATZ
               WHEN 6 MOVE 10 TO EXO-USTSATZ.
           IF OP-U2 not = 0 MOVE OP-B1 TO EXO-OFFEN.
           MOVE 0 TO WR.
           INSPECT EXO-SATZ TALLYING WR FOR ALL X"09".
           IF WR NOT = 17 PERFORM WEITER.
           WRITE EXO-SATZ.
           IF OP-U2 = 0 GO E.
           MOVE OP-B2 TO EXO-OFFEN.
           EVALUATE OP-U2
               WHEN 1 MOVE 10 TO EXO-USTSATZ
               WHEN 2 MOVE 20 TO EXO-USTSATZ
               WHEN 3 MOVE 00 TO EXO-USTSATZ
               WHEN 4 MOVE 12 TO EXO-USTSATZ
               WHEN 5 MOVE 20 TO EXO-USTSATZ
               WHEN 6 MOVE 10 TO EXO-USTSATZ.
           WRITE EXO-SATZ.
           GO E.
       X.  CLOSE BANKDAT.
       Z.  EXIT.

      *        05 OP-SA                PIC 99         COMP.
      *           07 OP-DK             PIC 99         COMP.

      *    03  OP-U1                   PIC 99         COMP.
      *    03  OP-B1                   PIC S9(9)V99   COMP.
      *    03  OP-U2                   PIC 99         COMP.
      *    03  OP-B2                   PIC S9(9)V99   COMP.
      *    03  OP-REBETRAG             PIC S9(9)V99   COMP.
      *    03  OP-ZABETRAG             PIC S9(9)V99   COMP.
      *
      *    03  EXO-FAELLIG             PIC 9(8).
      *    03  EXO-OFFEN               PIC -9(6)V99.
