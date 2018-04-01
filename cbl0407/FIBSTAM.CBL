      $SET LINKCOUNT"240" ANS85 BOUND AUTOLOCK NOALTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     FIBSTAM.
      ******************************************************************
      *                   Stammdatenverwaltung                         *
      ******************************************************************
       ENVIRONMENT    DIVISION.
       CONFIGURATION   SECTION.
       SOURCE-COMPUTER.     pc.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY FIBUSEC.CPY.
       DATA DIVISION.
       FILE SECTION.
       COPY FIBUFD.CPY.
      ******************************************************************
       WORKING-STORAGE SECTION.
       01  WH-CALL.
           03  WL-CA                   PIC 99.
           03  WL-REST                 PIC 9(13).
       COPY WHCREG.CPY.
       01  WF-REG.
           03  WD-UST                  PIC Z9,99-.
           03  WD-NUM                  PIC ZZZ.ZZ9.
           03  WD-BANK                 PIC ZZ9.
           03  WD-BLZ                  PIC 99.999.
           03  WD-PZ                   PIC 99.
           03  WD-GPZ                  PIC Z9,9999.
           03  WD-POS                  PIC ZZ9.
           03  WD-KZ                   PIC 9.
           03  WD-DIV                  PIC Z9.
           03  WD-KTO                  PIC ZZ999,9-.
           03  WD-KONTR                PIC ZZZZZZ.
           03  WD-KURS                 PIC ZZ.ZZZ,ZZZZ.
           03  WX-ART.
               05 WH-ART               PIC 99      COMP-X.
           03  WD-ART                  PIC Z99.
           03  WH-PX                   PIC XX          OCCURS 2.
           03  IX                      PIC 99      COMP.
           03  WI                      PIC 99      COMP.
           03  WL                      PIC 99      COMP.
           03  WK-VERDAT               PIC 9(6)    COMP.
           03  WT-UTAB.
               05 WK-UKTO              PIC 9(6)    COMP  OCCURS 80.
               05 WK-UKZ               PIC 99      COMP  OCCURS 80.
      *------------------------------------------> Konditionentabelle <-
           03  WH-KK                   PIC 9(13).
           03  WR-KK REDEFINES WH-KK.
               05  WH-SK1              PIC 9V9.
               05  WH-TG1              PIC 999.
               05  WH-SK2              PIC 9V9.
               05  WH-TG2              PIC 999.
               05  WH-TGN              PIC 999.
           03  WD-TG                   PIC ZZ9.
           03  WD-PROZ                 PIC Z9,9.
           03  WX-KZ                   PIC X.
      *------------------------------------------> Bildschirmausgaben <-
           03  VDU-REL                 PIC 9999.
           03  VDR-REL REDEFINES VDU-REL.
               05  VDU-RL              PIC 99.
               05  VDU-RP              PIC 99.
       COPY "FIBUEXT.CPY".
       COPY FIBUDECL.CPY.
       END DECLARATIVES.
      ******************************************************************
       STEUER SECTION.
       A.  MOVE WL-CALL TO WH-CALL.
           MOVE WL-CREG TO WH-CREG.
           EVALUATE WL-CA
               WHEN 33 PERFORM USTSATZ
               WHEN 20 PERFORM SYMBOL
               WHEN 25
               WHEN 26 PERFORM FREMDWHG
               WHEN 12 PERFORM KONDIT
               WHEN 10 PERFORM BANK
               WHEN 55 PERFORM WODRUCK
               WHEN 60 MOVE " Stammdatenverwaltung" TO WK-GEB
                       CALL "CAUP" USING "06KOPF" WH-CREG
                       PERFORM STAMVERW.
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
      ****************************************** Stammdatenverwaltung *
       STAMVERW SECTION.
       A.  CALL "CAUP" USING "0705081662000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Programmwahl " with highlight AT VDU-LP.
       C.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           ADD 307 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 - Sachkonten"          AT VDU-LP
                   "2 - Debitoren"           AT 0014
                   "3 - Kreditoren"          AT 0014
                   "4 - Summenkonten"        AT 0014
                   "5 - Symbole"             AT 0014
                   "6 - Ust-SÑtze"           AT 0014
                   "7 - Banken"              AT 0014
                   "8 - Kopf / Code"         AT 0014
                   "9 - sonst. Konstanten"   AT 0014
                   "10 - Wirtschaftsjahre"   AT 0013
                   "11 - Anredetexte"        AT 0743
                   "12 - Konditionen"        AT 0043
                   "13 - Druckerwahl"        AT 0043
                   "14 - Druckerparameter"   AT 0043
                   "15 - eigene Adresse"     AT 0043
                   "16 - Re-öberleitung"     AT 0043
                   "17 - Umsatzsteuerkonten" AT 0043
                   "18 - GetrÑnkeabgabekonten" AT 0043
                   "19 - Eurobeginn"         AT 0043
                   "20 - WÑhrungen"          AT 0043
                   "bitte wÑhlen Sie: " with highlight AT 1829.
           DISPLAY "<esc/Ende/ret-leer>= Programmende" AT 2301.
           UNLOCK KONSTANT.
           CALL "CAUP" USING "0018472002" WH-CREG.
           IF ESC OR ENDE SET RET TO TRUE
               MOVE 0 TO WH-NUM.
           IF NOT RET GO C.
           MOVE WH-NUM TO WH-PG.
           EVALUATE WH-PG
               WHEN 0 MOVE 9 TO WH-PG GO Z
               WHEN 1 MOVE 49 TO WH-PG
                      MOVE "FIBSACH 20SACHK" TO WT-TX GO Z
               WHEN 2 MOVE 49 TO WH-PG
                      MOVE "FIBDEBIT30DEBIT" TO WT-TX GO Z
               WHEN 3 MOVE 49 TO WH-PG
                      MOVE "FIBKRED 40KREDK" TO WT-TX GO Z
               WHEN 4 PERFORM AUTOKONT
               WHEN 5 PERFORM SYMBOL
               WHEN 7 PERFORM BANK
               WHEN 6
               WHEN 8
               WHEN 9 GO D
               WHEN 10 PERFORM WIJAHR
               WHEN 11 PERFORM ANR
               WHEN 12 PERFORM KONDIT
               WHEN 13 PERFORM WODRUCK
               WHEN 14 PERFORM DRUANL
               WHEN 15 PERFORM ADRESS
               WHEN 16 PERFORM VERZEICH
               WHEN 17 PERFORM USTPROB
               WHEN 18 PERFORM GETRAENK
               WHEN 19 PERFORM EUROBEG
               WHEN 20 PERFORM FREMDWHG.
           GO C.
       D.  MOVE 1 TO WH-KEY.
           READ KONSTANT INVALID KEY GO Z.
           IF ZUGRIF PERFORM BESETZT GO D.
           IF WH-PG = 6 PERFORM USTSATZ GO C.
           IF WH-PG = 9 GO J.
      *------------------------------> énderg. Firmenkopf u. Codewort <-
       F.  MOVE KO-FIRMA TO WT-TX.
           CALL "CAUP" USING "0201270122" WH-CREG.
           MOVE WT-TX TO KO-FIRMA.
           IF KO-FIRMA = SPACE GO F.
           DISPLAY KO-FIRMA AT VDU-LP.
           CALL "CAUP" USING "0705430528000" WH-CREG.
           DISPLAY " Codewort " with highlight AT 0545.
       G.  DISPLAY "Altes Codewort:" AT 0645.
           DISPLAY KO-SPERRE AT 0663.
           MOVE SPACE TO WX-CODE(1) WX-CODE(2).
           DISPLAY "Neues Codewort:" AT 0745.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "1203190106" WH-CREG.
           IF NOT RET GO G.
           MOVE WT-TX TO WX-CODE(1) KO-SPERRE.
       H.  DISPLAY "Kontrolleingabe:" AT 0845.
           MOVE SPACE TO WT-TX.
           CALL "CAUP" USING "1204200106" WH-CREG.
           MOVE WT-TX TO WX-CODE(2).
           IF NOT RET GO H.
           IF WX-CODE(2) NOT = WX-CODE(1)
               DISPLAY "Eingaben nicht gleich" AT 0445 GO G.
       I.  REWRITE KO-SATZ.
           UNLOCK KONSTANT.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           GO C.
      *-----------------------------------------> sonstige Konstanten <-
       J.  CALL "CAUP" USING "0704011045000" WH-CREG.
           DISPLAY " sonst. Konstanten " with highlight AT 0403.
           DISPLAY "letztes Journal.......:" AT 0503.
           DISPLAY "Journaldarstellung....:" AT 0603.
           DISPLAY "Firmensitz............:" AT 0703.
           DISPLAY "Mahnung...............:" AT 0803.
           DISPLAY "K-Auswertung..........:" AT 0903.
           DISPLAY "Journaldruck..........:" AT 1003.
           DISPLAY "Sachkontennummer......:" AT 1103.
           DISPLAY "Periodenende..........:" AT 1203.
           MOVE KO-MERK TO WK-M.
           MOVE KO-JONUM TO WD-NUM.
           DISPLAY WD-NUM with highlight AT 0527.
           MOVE WM-JO TO WD-KZ.
           DISPLAY WD-KZ with highlight AT 0627.
           IF WM-JO = 0 DISPLAY " netto " AT 0629
                  ELSE  DISPLAY " brutto" AT 0629.
           DISPLAY KO-ORT with highlight AT 0727.
           MOVE WM-MA TO WD-KZ.
           DISPLAY WD-KZ with highlight AT 0827.
           IF WM-MA = 0 DISPLAY " ohne Mahnstufen" AT 0000
                   else DISPLAY " mit Nahnstufen " AT 0000.
           MOVE KO-KST TO WD-KZ.
           DISPLAY WD-KZ with highlight AT 0927.
           IF KO-KST = 1 DISPLAY " mit KER" AT 0000
                    else DISPLAY " ohne   " AT 0000.
           ADD 1029 VDU-ECK GIVING VDU-LP.
           PERFORM KOART.
           MOVE WM-SK TO WD-KZ.
           DISPLAY WD-KZ with highlight AT 1127.
           MOVE KO-PER TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           DISPLAY VDU-DATUM with highlight AT 1227.
           MOVE 1 TO WX.
       K.  MOVE KO-JONUM TO WH-WERT WD-NUM.
           DISPLAY WD-NUM with highlight AT 0528.
           DISPLAY "<ret-leer>= keine énderung" AT 2301.
           CALL "CAUP" USING "0005285005" WH-CREG.
           IF ESC OR ENDE GO R.
           IF NOT RET GO K.
           MOVE WH-WERT TO KO-JONUM WD-NUM.
           DISPLAY WD-NUM with highlight AT VDU-LP.
       L.  MOVE WM-JO TO WH-WERT.
           DISPLAY "0 = netto, 1 = brutto" AT 2401.
           CALL "CAUP" USING "0006271001" WH-CREG.
           IF HELP CALL "CAUP" USING "190045004601" WH-CREG GO P.
           IF WOLI GO K.
           IF ESC OR ENDE GO R.
           IF NOT RET GO L.
           IF WH-WERT > 1 GO L.
           MOVE WH-WERT TO WM-JO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 2 TO VDU-P.
           IF WM-JO = 0 DISPLAY " netto " AT VDU-LP
                  ELSE  DISPLAY " brutto" AT VDU-LP.
       M.  MOVE KO-ORT TO WT-TX.
           CALL "CAUP" USING "0207270115" WH-CREG.
           IF WOLI GO L.
           IF ESC OR ENDE GO R.
           IF NOT RET GO M.
           MOVE WT-TX TO KO-ORT.
           IF KO-ORT = SPACE GO M.
           DISPLAY KO-ORT with highlight AT VDU-LP.
       N.  MOVE WM-MA TO WH-WERT.
           DISPLAY "Mahnung: " with highlight AT 2401
               "0= ohne, 1= mit Mahnstufen".
           CALL "CAUP" USING "0008271001" WH-CREG.
           IF WOLI GO M.
           IF ESC OR ENDE GO R.
           IF NOT RET GO N.
           IF WH-WERT > 1 GO N.
           MOVE WH-WERT TO WM-MA WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 2 TO VDU-P.
           IF WM-MA = 0 DISPLAY " ohne Mahnstufen" AT 0000
                   else DISPLAY " mit Nahnstufen " AT 0000.
       O.  MOVE KO-KST TO WH-WERT WD-KZ.
           DISPLAY WD-KZ with highlight AT 0927.
           IF KO-KST = 1 DISPLAY " mit KER" AT 0000
                    else DISPLAY " ohne   " AT 0000.
           DISPLAY "0= ohne, 1= mit K-auswertung" AT 2401.
           CALL "CAUP" USING "0009271001" WH-CREG.
           IF WOLI GO N.
           IF ESC OR ENDE GO R.
           IF SEPO MOVE 1 TO WH-WERT KO-KST.
           IF not RET and not SEPO GO O.
           EVALUATE KO-KST
               WHEN 0 IF WH-WERT > 0 GO O
               WHEN 1 IF WH-WERT > 1 GO O.
           MOVE WH-WERT TO KO-KST WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           IF KO-KST = 1 DISPLAY " mit KER" AT 0000
                    else DISPLAY " ohne   " AT 0000.
       P.  MOVE WM-KO TO WH-WERT.
           DISPLAY "Journalpapier: 0 = 8-Zoll, 1 = 12-Zoll" AT 2401.
           CALL "CAUP" USING "0010271001" WH-CREG.
           IF HELP CALL "CAUP" USING "190049005001" WH-CREG GO P.
           IF WOLI GO O.
           IF ESC OR ENDE GO R.
           IF NOT RET OR WH-WERT > 1 GO P.
           MOVE WH-WERT TO WM-KO.
           PERFORM KOART.
       Q.  MOVE WM-SK TO WH-WERT.
           DISPLAY "Anzahl Stellen Sachkontonummern" AT 2401.
           CALL "CAUP" USING "0011271001" WH-CREG.
           IF HELP CALL "CAUP" USING "190051005201" WH-CREG GO Q.
           IF WOLI GO P.
           IF ESC OR ENDE GO R.
           IF NOT RET GO Q.
           IF WH-WERT > 5 OR < 3 GO Q.
           MOVE WH-WERT TO WM-SK WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       QA. MOVE KO-PER TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE KO-PER TO WZ-DATUM WH-WERT.
           DISPLAY "<ret>= Periode, <>= zurÅck, <esc>= Abbruch"
               AT 2301.
           DISPLAY "Achtung!" with highlight AT 2401 " énderung kann fal
      -        "sche Zinsenabrechnung bewirken!".
           CALL "CAUP" USING "0112276006" WH-CREG.
           IF HELP CALL "CAUP" USING "190053005401" WH-CREG GO QA.
           IF WOLI GO Q.
           IF NOT RET GO QA.
           IF WZ-DATUM = 0 GO QA.
           MOVE WZ-DATUM TO KO-PER.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       R.  DISPLAY "<ret>= speichern, <>= zurÅck, <esc>= Abbruch "
               AT 2301.
           DISPLAY "<Bild- >= speichern u. TR-Konten verwalten"
               AT 2401.
           CALL "CAUP" USING "0023471001" WH-CREG.
           IF ESC GO S.
           IF BAB PERFORM TR-SR GO A.
           IF WOLI GO Q.
           IF NOT RET GO R.
           MOVE WK-M TO KO-MERK.
           REWRITE KO-SATZ.
       S.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
           GO C.
       Z.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           EXIT.
      ******************************************************************
       WODRUCK SECTION.
       A.  CALL "CAUP" USING "0707070730000" WH-CREG.
           MOVE 9 TO WH-KEY.
           MOVE 9 TO WH-PG.
       C.  READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO C.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " wo drucken? " with highlight AT VDU-LP.
           ADD 305 VDU-ECK GIVING VDU-LP.
           DISPLAY "0 = Nadeldrucker" AT VDU-LP
           ADD 425 VDU-ECK GIVING VDU-LP.
           DISPLAY "< >" AT VDU-LP.
           ADD 505 VDU-ECK GIVING VDU-LP.
           DISPLAY "1 = Laserdrucker" AT VDU-LP.
       E.  MOVE KO-DRU TO WD-KZ WH-WERT.
           ADD 426 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           DISPLAY "<ret>= Druck im Angebotsprogramm" AT 2301.
           CALL "CAUP" USING "1004261001" WH-CREG.
           IF ESC GO X.
           IF not RET GO E.
           IF WH-WERT > 1 GO E.
           MOVE WH-WERT TO KO-DRU WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ******************************************************************
       KOART SECTION.
       A.  MOVE WM-KO TO WD-KZ.
           ADD 0727 VDU-ECK GIVING VDU-LP.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 2 TO VDU-LP.
           EVALUATE WM-KO
               WHEN 0 DISPLAY " 8-Zoll " AT VDU-LP
               WHEN 1 DISPLAY " 12-Zoll" AT VDU-LP.
       Z.  EXIT.
     ********************************************* Standardkonditionen *
       KONDIT SECTION.
       A.  CALL "CAUP" USING "0702021332000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Standardkonditionen " with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr. Tage   %  Tage   %  Netto" AT VDU-LP.
           ADD 302 VDU-ECK GIVING VDU-LP.
           DISPLAY ALL "ƒ" with SIZE 30 AT VDU-LP.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 9
               COMPUTE VDU-LP = IX * 100 + 303 + VDU-ECK
               MOVE IX TO WD-KZ
               DISPLAY WD-KZ with highlight AT VDU-LP.
       D.  PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 9
               MOVE WT-KONDIT(IX) TO WH-KK
               PERFORM KONDIS.
           IF WL-CA = 12
               DISPLAY "Kz. +<ret>= öbernahme Kondition < >" AT 2301
               CALL "CAUP" USING "0023341001" WH-CREG
               GO X.
       E.  DISPLAY "<esc>= Abbruch, <ret-leer>= speichern, Nr+<ret>= Zei
      -        "e fÅr énderg. < >" AT 2301.
           CALL "CAUP" USING "0023681001" WH-CREG.
           IF ESC GO Q.
           IF NOT RET GO E.
           IF WH-NUM = 0 GO Q.
           MOVE WH-NUM TO IX.
           MOVE WT-KONDIT(IX) TO WH-KK.
           PERFORM KONDIS.
       G.  COMPUTE VDU-LP = IX * 100 + 307.
           MOVE WH-TG1 TO WH-WERT.
           CALL "CAUP" USING "1000073002" WH-CREG.
           IF ESC OR WOLI GO E.
           IF NOT RET GO G.
           MOVE WH-WERT TO WH-TG1.
           PERFORM KONDIS.
       I.  COMPUTE VDU-LP = IX * 100 + 300.
           MOVE WH-SK1 TO WH-WERT.
           CALL "CAUP" USING "1000112103" WH-CREG.
           IF WOLI GO G.
           IF ESC GO E.
           IF NOT RET GO I.
           MOVE WH-WERT TO WH-SK1.
           PERFORM KONDIS.
       K.  COMPUTE VDU-LP = IX * 100 + 300.
           MOVE WH-TG2 TO WH-WERT.
           CALL "CAUP" USING "1000173002" WH-CREG.
           IF WOLI GO I.
           IF ESC GO E.
           IF NOT RET GO K.
           MOVE WH-WERT TO WH-TG2.
           PERFORM KONDIS.
       M.  COMPUTE VDU-LP = IX * 100 + 300.
           MOVE WH-SK2 TO WH-WERT.
           CALL "CAUP" USING "1000212103" WH-CREG.
           IF WOLI GO K.
           IF ESC GO E.
           IF NOT RET GO I.
           MOVE WH-WERT TO WH-SK2.
           PERFORM KONDIS.
       O.  COMPUTE VDU-LP = IX * 100 + 300.
           MOVE WH-TGN TO WH-WERT.
           CALL "CAUP" USING "1000283002" WH-CREG.
           IF WOLI GO M.
           IF ESC GO E.
           IF NOT RET GO O.
           MOVE WH-WERT TO WH-TGN.
           PERFORM KONDIS.
           MOVE WH-KK TO WT-KONDIT(IX).
           IF IX < 9 ADD 1 TO IX.
           GO E.
      *-----------------------------------------------------> Austieg <-
       Q.  MOVE 1 TO WH-KEY.
           READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO Q.
           IF ESC MOVE KO-TABK TO WH-TABK GO W.
           MOVE WH-TABK TO KO-TABK.
           REWRITE KO-SATZ.
       W.  MOVE KO-TABK TO WH-TABK.
           UNLOCK KONSTANT.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************************************
       KONDIS SECTION.
       A.  COMPUTE VDU-LP = IX * 100 + 307 + VDU-ECK.
           MOVE WH-TG1 TO WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
           ADD 4 TO VDU-LP.
           MOVE WH-SK1 TO WD-PROZ.
           DISPLAY WD-PROZ WITH HIGHLIGHT AT VDU-LP.
           ADD 6 TO VDU-LP.
           MOVE WH-TG2 TO WD-TG.
           DISPLAY WD-TG with highlight AT VDU-LP.
           ADD 4 TO VDU-LP.
           MOVE WH-SK2 TO WD-PROZ.
           DISPLAY WD-PROZ WITH HIGHLIGHT AT VDU-LP.
           ADD 7 TO VDU-LP.
           MOVE WH-TGN TO WD-TG.
           DISPLAY WD-TG WITH HIGHLIGHT AT VDU-LP.
       Z.  EXIT.
      *********************************************** Symbolverwaltung *
       SYMBOL SECTION.
       A.  CALL "CAUP" USING "270418195102012" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Symbolverwaltung " with highlight AT VDU-REL.
           IF WL-CA = 20 GO C.
           MOVE 10 TO WH-KEY.
       B.  READ KONSTANT INVALID STOP RUN.
           IF ZUGRIF PERFORM BESETZT GO B.
           MOVE KO-TS TO WE-TS.
       C.  ADD 202 VDU-ECK GIVING VDU-REL.
           DISPLAY "  Nr. Sy S/H Ust OPs Res  Nr. Sy S/H Ust OPs Res "
               with reverse-video AT VDU-REL.
           ADD 302 VDU-ECK GIVING VDU-REL.
           DISPLAY all "ƒ" with SIZE 49 AT VDU-REL.
      *--------------------------------------> Symboltabelle anzeigen <-
           PERFORM VARYING WH-ART FROM 0 BY 1 UNTIL WH-ART > 29
               ADD 1 WH-ART GIVING WX
               ADD 03 VDU-EL WX GIVING VDU-L
               IF WX < 16 ADD 03 VDU-EP GIVING VDU-P
                     ELSE ADD 27 VDU-EP GIVING VDU-P
                          SUBTRACT 15 FROM VDU-L
               end-if
               MOVE WH-ART TO WD-POS
               DISPLAY WD-POS AT VDU-LP
               ADD 6 TO VDU-P
               DISPLAY WE-SYM(WX) with highlight AT VDU-LP
               MOVE WE-SKZ(WX) TO WE-SYMT
               ADD 4 TO VDU-P
               EVALUATE WE-SH
                   WHEN 0 MOVE "S" TO WX-KZ
                   WHEN 1 MOVE "H" TO WX-KZ
                   WHEN 2 MOVE "B" TO WX-KZ
               end-evaluate
               IF WE-SYM(WX) NOT = SPACE
                   DISPLAY WX-KZ with highlight AT VDU-LP
               end-if
               ADD 4 TO VDU-P
               EVALUATE WE-UST
                   WHEN 0 MOVE "-" TO WX-KZ
                   WHEN 1 MOVE "M" TO WX-KZ
                   WHEN 2 MOVE "V" TO WX-KZ
                   WHEN 3 MOVE "Z" TO WX-KZ
               end-evaluate
               IF WE-SYM(WX) NOT = SPACE
                   DISPLAY WX-KZ with highlight AT VDU-LP
               end-if
               ADD 4 TO VDU-P
               EVALUATE WE-OP
                   WHEN 0 MOVE "N" TO WX-KZ
                   WHEN 1 MOVE "J" TO WX-KZ
                   WHEN 2 MOVE "S" TO WX-KZ
               end-evaluate
               IF WE-SYM(WX) NOT = SPACE
                   DISPLAY WX-KZ with highlight AT VDU-LP
               end-if
               ADD 3 TO VDU-P
               IF WE-DIV NOT = 0 MOVE WE-DIV TO WD-DIV
                   DISPLAY WD-DIV with highlight AT VDU-LP
               end-if.
           IF WL-CA NOT = 20 GO E.
           MOVE VDU-RES TO VDU-LP.
       D.  DISPLAY "<ret>= Symbol, <esc>= Abbruch, <>= zurÅck" AT 2301.
           CALL "CAUP" USING "0000112002" WH-CREG.
           MOVE WH-NUM TO WH-WERT.
           IF WH-NUM > 29 SET ESC TO TRUE.
           IF WOLI OR ESC MOVE 99 TO WH-WERT GO Y.
           IF NOT RET GO D.
           GO Y.
           MOVE 1 TO WX.
       E.  DISPLAY "<esc>= Abbruch, <Ende>= speichern" AT 2301.
           IF WL-CA NOT = 20 DISPLAY ", <ret>= Kz f. Sym, < > = auf, < >
      -        "= ab" AT 0000.
           MOVE WE-SKZ(WX) TO WE-SYMT.
           ADD 03 WX GIVING VDU-RL.
           IF WX < 16 MOVE 08 TO VDU-RP
                 ELSE MOVE 33 TO VDU-RP
                      SUBTRACT 15 FROM VDU-RL.
           MOVE VDU-REL TO VDU-LP.
           MOVE WE-SYM(WX) TO WT-TX.
           CALL "CAUP" USING "1200000102" WH-CREG.
           IF WL-CA = 20 GO X.
           IF ESC GO X.
           IF ENDE GO Q.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO E.
           IF AB GO M.
           IF NOT RET GO E.
           IF WT-TX = SPACE MOVE 0 TO WE-SH WE-UST WE-OP WE-DIV
               MOVE SPACE TO WE-SYM(WX)
               DISPLAY "                 " AT VDU-LP GO M.
           MOVE WT-TX TO WE-SYM(WX).
           ADD VDU-REL VDU-ECK GIVING VDU-LP.
           DISPLAY WE-SYM(WX) with highlight AT VDU-LP.
           ADD 4 TO VDU-REL.
       F.  MOVE VDU-REL TO VDU-LP.
           DISPLAY "0= Sollbuchung, 1= Habenbuchung, 2= Soll- oder Haben
      -        "buchung" AT 2301.
           MOVE WE-SH TO WH-WERT.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF HELP CALL "CAUP" USING "190065006601" WH-CREG GO F.
           IF ESC GO X.
           IF WOLI ADD -4 TO VDU-REL GO E.
           IF ENDE GO Q.
           IF WH-WERT > 2 OR NOT RET GO F.
           MOVE WH-WERT TO WE-SH.
           EVALUATE WE-SH
               WHEN 0 MOVE "S" TO WX-KZ
               WHEN 1 MOVE "H" TO WX-KZ
               WHEN 2 MOVE "B" TO WX-KZ.
           ADD VDU-REL VDU-ECK GIVING VDU-LP.
           DISPLAY WX-KZ with highlight AT VDU-LP.
           ADD 4 TO VDU-REL.
       G.  MOVE VDU-REL TO VDU-LP.
           DISPLAY "0= keine Ust, 1= Mehrwertsteuer, 2= Vorsteuer, 3= Us
      -        "t bei Zahlg." AT 2301.
           MOVE WE-UST TO WH-WERT.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF HELP CALL "CAUP" USING "190067006801" WH-CREG GO G.
           IF ESC GO X.
           IF WOLI ADD -4 TO VDU-REL GO F.
           IF ENDE GO Q.
           IF WH-WERT > 3 or not RET GO G.
           MOVE WH-WERT TO WE-UST.
           EVALUATE WE-UST
               WHEN 0 MOVE "-" TO WX-KZ
               WHEN 1 MOVE "M" TO WX-KZ
               WHEN 2 MOVE "V" TO WX-KZ
               WHEN 3 MOVE "Z" TO WX-KZ.
           ADD VDU-REL VDU-ECK GIVING VDU-LP.
           DISPLAY WX-KZ with highlight AT VDU-LP.
           ADD 4 TO VDU-REL.
       H.  MOVE VDU-REL TO VDU-LP.
           DISPLAY "0= keine OP, 1= mit OP-Bearbeitung, 2= Skonto/OP"
               AT 2301.
           MOVE WE-OP TO WH-WERT.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF HELP CALL "CAUP" USING "190069007001" WH-CREG GO H.
           IF ESC GO X.
           IF WOLI ADD -4 TO VDU-REL GO G.
           IF ENDE GO Q.
           IF WH-WERT > 2 OR NOT RET GO F.
           MOVE WH-WERT TO WE-OP.
           EVALUATE WE-OP
               WHEN 0 MOVE "N" TO WX-KZ
               WHEN 1 MOVE "J" TO WX-KZ
               WHEN 2 MOVE "S" TO WX-KZ
                      IF WE-UST NOT = 0
                          DISPLAY "Ust-Kz nicht erlaubt! "
                               with highlight AT 2401
                          PERFORM WEITER GO H.
           ADD VDU-REL VDU-ECK GIVING VDU-LP.
           DISPLAY WX-KZ with highlight AT VDU-LP.
           MOVE WE-SYMT TO WE-SKZ(WX).
           ADD 3 TO VDU-REL.
       J.  MOVE VDU-REL TO VDU-LP.
           DISPLAY "1= Rechnungs-, 2= Gutschrift-, 3= ZahlungsÅbernahme,
      -        " 4= Diverses" AT 2301.
           DISPLAY "5= Teil-, 6= Storno Teil-, 7= Schlu·rechnung, 8= aut
      -        "om. Umbuchung TR" AT 2401.
           DISPLAY "Ustfrei   19: 10= Rechng., 11= Teilrechng., 12= Schl
      -        "u·rechng., 13= Eing.Re." AT 2501.
           MOVE WE-DIV TO WH-WERT.
           CALL "CAUP" USING "1000002002" WH-CREG.
           IF HELP CALL "CAUP" USING "190071007201" WH-CREG GO J.
           IF ESC GO X.
           IF WOLI ADD -3 TO VDU-REL GO H.
           IF ENDE GO Q.
           IF not RET GO J.
           MOVE WH-WERT TO WE-DIV.
           ADD VDU-REL VDU-ECK GIVING VDU-LP.
           IF WE-DIV NOT = 0 DISPLAY WE-DIV with highlight AT VDU-LP
                        else DISPLAY " " AT VDU-LP.
       M.  MOVE WE-SYMT TO WE-SKZ(WX).
           IF WX < 30 ADD 1 TO WX.
           GO E.
       Q.  MOVE WE-TS TO KO-TS.
           REWRITE KO-SATZ.
       X.  UNLOCK KONSTANT.
       Y.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************************ Wirtschaftsjahr *
       WIJAHR SECTION 70.
       A.  MOVE 9 TO WH-KEY.
           MOVE 0 TO WH-NUM.
           READ KONSTANT INVALID KEY GO Z.
           IF ZUGRIF PERFORM BESETZT GO Z.
           CALL "CAUP" USING "0706200929020" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Wirtschaftsjahre " with highlight AT VDU-LP.
           ADD 204 VDU-ECK GIVING VDU-LP.
           DISPLAY "Abschlu·datum: " AT VDU-LP.
           IF KO-ABDAT(1:2) = SPACE MOVE KO-WEND(WI) TO KO-ABDAT.
           MOVE KO-ABDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           ADD 219 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           ADD 302 VDU-ECK GIVING VDU-LP.
           DISPLAY " WKz.    Beginn      Ende  " with reverse-video
               AT VDU-LP.
       B.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING WI FROM 1 BY 1 UNTIL WI > 4
               OR KO-WBEG(WI) = 0 MOVE KO-WKZ(WI) TO WD-PZ
                   COMPUTE VDU-LP = WI * 100 + 304 + VDU-ECK
                   DISPLAY WD-PZ with highlight AT VDU-LP
                   ADD 5 TO VDU-LP
                   MOVE KO-WBEG(WI) TO WC-DATUM
                   CALL "CAUP" USING "04DATDREH" WH-CREG
                   DISPLAY VDU-DATUM with highlight AT VDU-LP
                   ADD 10 TO VDU-LP
                   MOVE KO-WEND(WI) TO WC-DATUM
                   CALL "CAUP" USING "04DATDREH" WH-CREG
                   DISPLAY VDU-DATUM with highlight AT VDU-LP.
       C.  DISPLAY "<Entf>= Eintrag lîschen, < / >= auf/ab, " AT 2401.
           DISPLAY "letztes Jahr = aktuelles Jahr" with highlight
               AT 0000.
           DISPLAY "<ret>= Jahr, <esc>= Abbruch, < >= Abschlu·datum, <En
      -        "de>= speichern" AT 2301.
           MOVE WI TO WL.
           IF WI < 5 MOVE KO-WKZ(WI) TO WH-WERT.
           COMPUTE VDU-LP = WL * 100 + 304.
      *    IF WH-NUM NOT = 0 MOVE WH-NUM TO WH-WERT
      *        ADD VDU-ECK TO VDU-LP GO D.
           CALL "CAUP" USING "1000042002" WH-CREG.
           IF WORE GO K.
           IF ESC GO X.
           IF ENDE GO W.
           IF ENTF and WI < 5;
               IF WI = 4 or (WI < 4 and KO-WKZ(WI + 1) = 0)
                   MOVE 0 TO KO-WKZ(WI) KO-WBEG(WI) KO-WEND(WI)
                   GO B.
           IF AUF and WI > 1 ADD -1 TO WI GO C.
           IF AB and WI < 5 ADD 1 TO WI GO C.
           IF NOT RET GO C.
           IF WI = 5 PERFORM VARYING WI FROM 1 BY 1 UNTIL WI = 4
                       MOVE KO-WKZ(WI + 1) TO KO-WKZ(WI)
                       MOVE KO-WBEG(WI + 1) TO KO-WBEG(WI)
                       MOVE KO-WEND(WI + 1) TO KO-WEND(WI)
                       MOVE 0 TO KO-WKZ(WI + 1) KO-WBEG(WI + 1)
                             KO-WEND(WI + 1).
       D.  MOVE WH-WERT TO KO-WKZ(WI) WD-PZ.
           COMPUTE VDU-LP = WL * 100 + 304 + VDU-ECK.
           DISPLAY WD-PZ with highlight AT VDU-LP.
           MOVE KO-WBEG(WI) TO WK-VERDAT.
       F.  MOVE KO-WBEG(WI) TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE KO-WBEG(WI) TO WZ-DATUM WH-WERT.
           DISPLAY "<ret>= Beginndatum, <esc>= Abbruch" AT 2301.
           DISPLAY "Achtung!" with highlight AT 2401 " énderung kann fal
      -        "sche Auswertungen bewirken!".
           COMPUTE VDU-LP = WL * 100 + 309.
           CALL "CAUP" USING "1100096006" WH-CREG.
           IF HELP CALL "CAUP" USING "190055005601" WH-CREG GO F.
           IF ESC GO X.
           IF WOLI GO C.
           IF NOT RET GO F.
           IF WZ-DATUM = 0 GO F.
           MOVE WX-DATUM TO KO-WBEG(WI).
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF KO-WBEG(WI) NOT = WK-VERDAT MOVE 0 TO KO-WEND(WI).
       H.  PERFORM BILDJAHR.
           DISPLAY "<ret>= Endedatum, <>= zurÅck, <esc>= Abbruch"
               AT 2301.
           DISPLAY "Achtung!" with highlight AT 2401 " énderung kann fal
      -        "sche Auswertungen bewirken!".
           COMPUTE VDU-LP = WL * 100 + 319.
           CALL "CAUP" USING "1100196006" WH-CREG.
           IF HELP CALL "CAUP" USING "190057005801" WH-CREG GO H.
           IF WOLI GO F.
           IF ESC GO X.
           IF NOT RET GO H.
           IF WZ-DATUM = 0 GO H.
           MOVE WX-DATUM TO KO-WEND(WI).
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF WZ-JAHR not = 0 IF KO-WEND(WI) NOT > KO-WBEG(WI)
              DISPLAY "Daten fehlerhaft" AT 2401 PERFORM WEITER GO F.
           IF WI = 5 CALL "CAUP" USING "17SCROLLF" WH-CREG.
           ADD 1 TO WI.
           GO C.
      *-----------------------------------------------> Abschlu·datum <-
       K.  IF KO-ABDAT(1:2) = SPACE MOVE KO-WEND(WI) TO KO-ABDAT.
           MOVE KO-ABDAT TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           ADD 219 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
       L.  MOVE KO-ABDAT TO WZ-DATUM WH-WERT.
           DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           DISPLAY "Datum des letztes Jahresabschlu·es!" with highlight
               AT 2401.
           CALL "CAUP" USING "1102196006" WH-CREG.
           MOVE 0 TO WH-NUM.
           IF ESC GO C.
           IF NOT RET or WZ-DATUM = 0 GO L.
           MOVE WX-DATUM TO KO-ABDAT.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           GO C.
       W.  REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ************************************* Bilde Ende Wirtschaftsjahr *
       BILDJAHR SECTION 70.
       A.  IF KO-WEND(WI) NOT = 0 GO X.
           MOVE KO-WBEG(WI) TO WZ-DATUM.
           ADD -1 TO WZ-MONAT.
           IF WZ-MONAT = 0 MOVE 12 TO WZ-MONAT
           ELSE ADD 1 TO WZ-JAHR.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE 32 TO WZ-TAG.
       C.  ADD -1 TO WZ-TAG.
           MOVE WZ-DATUM TO WK-VERDAT.
           CALL "CAUP" USING "05DATPRF" WH-CREG.
           IF WZ-DATUM = 0 MOVE WK-VERDAT TO WZ-DATUM GO C.
           MOVE WZ-DATUM TO WK-VERDAT WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           COMPUTE VDU-LP = WL * 100 + 319 + VDU-ECK.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WM-DATUM TO WZ-DATUM.
           CALL "CAUP" USING "03DATUM" WH-CREG.
           MOVE WK-VERDAT TO WZ-DATUM WH-WERT.
           GO Z.
       X.  MOVE KO-WEND(WI) TO WC-DATUM.
           CALL "CAUP" USING "04DATDREH" WH-CREG.
           MOVE KO-WEND(WI) TO WZ-DATUM WH-WERT.
       Z.  EXIT.
      ********************************************************* Anrede *
       ANR SECTION 70.
       A.  MOVE 1 TO WH-KEY.
           READ KONSTANT INVALID GO Y.
           IF ZUGRIF PERFORM BESETZT GO A.
           CALL "CAUP" USING "0706601117020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-REL.
           DISPLAY " Anredetexte " with highlight AT VDU-REL.
           ADD 0203 VDU-ECK GIVING VDU-REL.
           DISPLAY "Nr. T e x t e" AT VDU-REL.
           ADD 99 TO VDU-REL.
           DISPLAY "---------------" AT VDU-REL.
           MOVE 1 TO WX.
       F.  COMPUTE VDU-REL = 304 + WX * 100 + VDU-ECK.
           SUBTRACT 1 FROM WX GIVING WD-KZ.
           DISPLAY WD-KZ AT VDU-REL.
           ADD 3 TO VDU-REL.
           DISPLAY WT-TXT(WX) with highlight AT VDU-REL.
           IF WX < 7 ADD 1 TO WX GO F.
           MOVE 1 TO WX.
       H.  DISPLAY "<ret>= Eingabe, <esc>= Abbruch, <Ende>= Ende, <>= z
      -        "urÅck" AT 2301.
           COMPUTE VDU-LP = WX * 100 + 307.
           MOVE WT-TXT(WX) TO WT-TX.
           CALL "CAUP" USING "1200000109" WH-CREG.
           IF ENDE GO L.
           IF ESC GO Y.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO H.
           IF NOT RET GO H.
           MOVE WT-TX TO WT-TXT(WX).
           COMPUTE VDU-REL = 307 + WX * 100 + VDU-ECK.
           DISPLAY WT-TXT(WX) with highlight AT VDU-REL.
           IF WX < 7 ADD 1 TO WX.
           GO H.
       L.  MOVE 1 TO WX.
           MOVE WH-TABTX TO KO-ANR.
           REWRITE KO-SATZ.
       X.  MOVE KO-AN(WX) TO WT-TXT(WX).
           IF WX < 7 ADD 1 TO WX GO X.
       Y.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ******************************************************************
       USTSATZ SECTION 70.
       A.  CALL "CAUP" USING "0704081017020" WH-CREG.
           DISPLAY " Ust-SÑtze " with highlight AT 0411.
           DISPLAY "Nr.    %-Satz" AT 0510.
           DISPLAY "ƒƒƒƒƒƒƒƒƒƒƒƒƒ" AT 0610.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               ADD 6 WX GIVING VDU-L
               MOVE WX TO WD-KZ
               MOVE 11 TO VDU-P
               DISPLAY WD-KZ AT VDU-LP
               MOVE WT-UST(WX) TO WD-UST
               MOVE 17 TO VDU-P
               DISPLAY WD-UST with highlight AT VDU-LP.
       F.  IF WL-CA = 33 DISPLAY "Weiter mit jeder Taste < >" AT 2301
               CALL "CAUP" USING "0023250000" WH-CREG
               GO X.
           MOVE 1 TO WX.
       H.  DISPLAY "<ret>= Ust-Satz, <esc>= Abbruch, <Ende>= speichern,
      -        "<>= zurÅck" AT 2301.
           IF WX > 4 DISPLAY "Ust-Satz f. innergemeinschaftl. UmsÑtze"
                          with highlight AT 2401.
           ADD 6 WX GIVING VDU-L.
           MOVE WT-UST(WX) TO WH-WERT.
           CALL "CAUP" USING "0000002205" WH-CREG.
           IF ENDE GO L.
           IF ESC GO X.
           IF AUF AND WX NOT = 1 ADD -1 TO WX GO H.
           IF AB GO I.
           IF NOT RET GO H.
           MOVE 17 TO VDU-P.
           MOVE WH-WERT TO WT-UST(WX) WD-UST.
           DISPLAY WD-UST with highlight AT VDU-LP.
       I.  IF WX < 6 ADD 1 TO WX GO H.
       L.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 6
               MOVE WT-UST(WX) TO KO-UST(WX).
           MOVE 1 TO WX.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ************************************************* Rechnungstexte *
       DRUANL SECTION 70.
       A.  CALL "CAUP" USING "0709081334020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Druckerverzeichnis " with highlight AT VDU-LP.
           ADD 202 VDU-ECK GIVING VDU-LP.
           DISPLAY " Nr. Druckerbezeichnung         " with reverse-video
                AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY ALL "ƒ" with SIZE 32 AT VDU-LP.
       B.  CALL "CAUP" USING "16LOEFEN" WH-CREG.
      *---------------------------------------------------> auflisten <-
           MOVE 1 TO WX WR WF.
       C.  ADD 11 WX GIVING WH-KEY.
           MOVE 1 TO WF.
           READ KONSTANT IGNORE LOCK INVALID INITIALIZE KO-PSATZ
               MOVE 0 TO WF.
           COMPUTE VDU-LP = 303 + WX * 100 + VDU-ECK.
           MOVE WX TO WD-KZ.
           DISPLAY WD-KZ AT VDU-LP.
           ADD 3 TO VDU-LP.
           DISPLAY KOP-BEZ with highlight AT VDU-LP.
           IF WR = 1; IF WX < 9 ADD 1 TO WX GO C.
           IF WR = 2 GO E
                     ELSE MOVE 2 TO WR.
       D.  DISPLAY "<esc/Ende>= Ende, <ret>= Nr.:" AT 2301.
           CALL "CAUP" USING "0023311001" WH-CREG.
           IF ESC OR ENDE GO Y.
           IF NOT RET OR WH-NUM = 0 GO D.
           MOVE WH-NUM TO WX.
           GO C.
       E.  PERFORM DRUSTG.
           GO B.
       Y.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      *********************************************** Druckerparameter *
       DRUSTG SECTION 70.
       A.  CALL "CAUP" USING "0706071566000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Druckerparameter " with highlight AT VDU-LP.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr.:     Bezeichnung: " AT VDU-LP.
           MOVE WX TO WD-KZ.
           ADD 5 TO VDU-P.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       C.  ADD 225 VDU-ECK GIVING VDU-LP.
           DISPLAY KOP-BEZ with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               COMPUTE VDU-LP = WX * 100 + 203 + VDU-ECK
               EVALUATE WX
                   WHEN 1 DISPLAY "10-Zeichen/Zoll....:" AT VDU-LP
                   WHEN 2 DISPLAY "12-Zeichen/Zoll....:" AT VDU-LP
                   WHEN 3 DISPLAY "15-Zeichen/Zoll....:" AT VDU-LP
                   WHEN 4 DISPLAY "12-Zoll Formular...:" AT VDU-LP
                   WHEN 5 DISPLAY " 8-Zoll Formular...:" AT VDU-LP
                   WHEN 6 DISPLAY "Breitdruck ein.....:" AT VDU-LP
                   WHEN 7 DISPLAY "Breitdruck aus.....:" AT VDU-LP
                   WHEN 8 DISPLAY "Fettdruck ein......:" AT VDU-LP
                   WHEN 9 DISPLAY "Fettdruck aus......:" AT VDU-LP.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 6
                   MOVE KOP-STG(WX)(WY:1) TO WX-ART
                   COMPUTE VDU-LP = WX * 100 + 220 + VDU-ECK + WY * 4
                   IF WH-ART NOT = 32 MOVE WH-ART TO WD-ART
                   DISPLAY WD-ART with highlight AT VDU-LP.
      *---------------------------------> erstmalig anlegen f. EPSON <-
           IF WF = 0 MOVE SPACE TO KOP-TX
               MOVE "Epson LQ1050+" TO KOP-BEZ
               MOVE x"1B50" TO KOP-STG(1)
               MOVE x"1B4D" TO KOP-STG(2)
               MOVE x"1B57" TO KOP-STG(3)
               MOVE x"1B43000C" TO KOP-STG(4)
               MOVE x"1B430008" TO KOP-STG(5)
               MOVE x"1B5701" TO KOP-STG(6)
               MOVE x"1B5700" TO KOP-STG(7)
               MOVE x"1B45" TO KOP-STG(8)
               MOVE x"1B46" TO KOP-STG(9)
               MOVE 1 TO WF
               WRITE KO-SATZ GO C.
           ADD 1303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Geben Sie die einzelnen Steuerzeichen lt. Druckerhan
      -        "dbuch ein." AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Verwenden Sie jedoch nur die Dezimalzahlen der Steue
      -        "rzeichen!" AT VDU-LP.
       D.  DISPLAY "<esc>= Abbruch, <ret>= Bezeichnung, <ende> = Ende"
               AT 2301.
           DISPLAY "keine Bezeichnung = lîschen" AT 2401.
           MOVE KOP-BEZ TO WT-TX.
           ADD 225 VDU-ECK GIVING VDU-LP.
           DISPLAY KOP-BEZ with highlight AT VDU-LP.
           CALL "CAUP" USING "1202250126" WH-CREG.
           IF ESC GO M.
           IF ENDE GO K.
           IF NOT RET GO D.
           MOVE WT-TX TO KOP-BEZ.
           IF KOP-BEZ = SPACE DELETE KONSTANT INVALID GO M
                              end-delete GO M.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 9
               PERFORM VARYING WY FROM 1 BY 1 UNTIL WY > 6
                   COMPUTE VDU-LP = WX * 100 + 220 + VDU-ECK + WY * 4
                   MOVE KOP-STG(WX)(WY:1) TO WX-ART
                   MOVE WH-ART TO WD-ART WH-WERT
                   IF WH-ART = 32 DISPLAY "    " AT VDU-LP
                   ELSE DISPLAY WD-ART with highlight AT VDU-LP
                   END-IF
                   DISPLAY "<Ende>= Ende, <tab>= lîschen, <>= zurÅck, <
      -                "esc>= Abbruch" AT 2301
                   CALL "CAUP" USING "0000003003" WH-CREG
                   EVALUATE TRUE
                   WHEN TABL DISPLAY "    " AT VDU-LP
                       MOVE SPACE TO WX-ART
                       MOVE WX-ART TO KOP-STG(WX)(WY:1)
                   WHEN RET MOVE WH-WERT TO WH-ART WD-ART
                       MOVE WX-ART TO KOP-STG(WX)(WY:1)
                       IF WH-ART = 32 DISPLAY " " AT VDU-LP
                       ELSE DISPLAY WD-ART with highlight AT VDU-LP
                   WHEN WOLI; IF WY > 1 SUBTRACT 2 FROM WY
                           ELSE MOVE 11 TO WY
                           IF WX > 1 SUBTRACT 2 FROM WX
                           ELSE SUBTRACT 1 FROM WX
                   WHEN ENDE GO K
                   WHEN ESC  GO M
                   WHEN HELP CALL "CAUP" USING "190061006201" WH-CREG
                        SUBTRACT 1 FROM WY
                   WHEN OTHER SUBTRACT 1 FROM WY
                 END-PERFORM
               END-PERFORM.
       K.  REWRITE KO-SATZ.
       M.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ************************************* notwendige Automatikkonten *
       AUTOKONT SECTION 80.
       A.  CALL "CAUP" USING "0702011964000" WH-CREG.
           ADD 103  VDU-ECK GIVING VDU-LP.
           DISPLAY " zwingende Konten " with highlight AT VDU-LP.
           DISPLAY "Vorsteuer.............:" AT 0303
                   "Mehrwertsteuer........:" AT 0403
                   "Skontoerlîs (w).......:" AT 0503
                   "Kursdiffenrenzen......:" AT 0603
                   "Skto.-Aufwand Ust-Kz.1:" AT 0703
                   "Skto.-Aufwand Ust-Kz.2:" AT 0803
                   "Skto.-Aufwand Ust-Kz.3:" AT 0903
                   "Skto.-Aufwand Ust-Kz.4:" AT 1003
                   "Skto.-Aufwand Ust-Kz.5:" AT 1103
                   "Skto.-Aufwand Ust-Kz.6:" AT 1203
                   "Sammelkonto Deb.- 0...:" AT 1303
                   "    ''      Deb.- 1...:" AT 1403
                   "    ''      Deb.- 2...:" AT 1503
                   "Sammelkonto Kred.- 0..:" AT 1603
                   "    ''      Kred.- 1..:" AT 1703
                   "    ''      Kred.- 2..:" AT 1803
                   "diverse Ust-Konten....:" AT 1903.
       C.  MOVE 9 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-TSATZ
               WRITE KO-SATZ GO C.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 16
               MOVE KO-KTONR(IX) TO WH-WERT SA-KTONR
               PERFORM READ-SACH.
           PERFORM DIS-DIV.
           MOVE 1 TO IX.
       E.  MOVE KO-KTONR(IX) TO WH-WERT SA-KTONR.
           PERFORM READ-SACH.
           DISPLAY "<ret>= Kto-Nr., <esc>= Abbruch, <Ende>= Ende"
               AT 2301.
           COMPUTE VDU-LP = IX * 100 + 127.
           CALL "CAUP" USING "1000006006" WH-CREG.
           IF AUF OR WOLI; IF IX > 1 ADD -1 TO IX GO E.
           IF ESC GO X.
           IF ENDE GO R.
           IF NOT RET GO E.
           MOVE WH-WERT TO KO-KTONR(IX) SA-KTONR WD-KZ.
           IF WD-KZ NOT = 0 GO E.
       G.  READ SACHBUCH IGNORE LOCK NOT INVALID GO K.
           CALL "FIBSACH" USING "22KTOAN" WH-CREG.
           IF SA-KTONR = 0 MOVE 0 TO KO-KTONR(IX) GO E.
           GO G.
       K.  DIVIDE 10 INTO KO-KTONR(IX) GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           DISPLAY " " AT 0000 SA-TX(1).
           IF IX < 16 ADD 1 TO IX GO E.
       M.  PERFORM DIV-UST.
           PERFORM DIS-DIV.
           IF WOLI or AUF MOVE 16 TO IX GO E.
       R.  DISPLAY "<ret>= speichern, <>= zurÅck, <esc>= Abbruch "
               AT 2301.
           CALL "CAUP" USING "0023471001" WH-CREG.
           IF ESC UNLOCK KONSTANT GO X.
           IF WOLI GO M.
           IF NOT RET GO R.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ******************************************************************
       DIS-DIV SECTION 80.
       A.  PERFORM VARYING IX FROM 17 BY 1 UNTIL IX > 20
               IF KO-KTONR(IX) not = 0 MOVE 1900 TO VDU-LP
                   COMPUTE VDU-P = IX * 9 - 126 + VDU-ECK
                   DIVIDE 10 INTO KO-KTONR(IX) GIVING WD-KTO
                   DISPLAY WD-KTO(1:7) with highlight AT VDU-LP ",".
       Z.  EXIT.
      ******************************************************************
       DIV-UST SECTION 80.
       A.  CALL "CAUP" USING "271010066500012" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Ust-Sonderkonten " with highlight AT VDU-LP.
           DISPLAY "Innergem. Ust.(S).....:" AT 1112
                   "Innergem. Ust.(H).....:" AT 1212
                   "Ust gem.   19 (S).....:" AT 1312
                   "Ust gem.   19 (H).....:" AT 1412.
           PERFORM VARYING IX FROM 17 BY 1 UNTIL IX > 20
               MOVE KO-KTONR(IX) TO WH-WERT SA-KTONR
               PERFORM READ-SACH.
           MOVE 17 TO IX.
       E.  MOVE KO-KTONR(IX) TO WH-WERT SA-KTONR.
           PERFORM READ-SACH.
           DISPLAY "<ret>= Kto-Nr., <esc>= Abbruch, <Ende>= Ende"
               AT 2301.
           COMPUTE VDU-LP = (IX - 16) * 100 + 127.
           CALL "CAUP" USING "1000006006" WH-CREG.
           IF AUF OR WOLI; IF IX > 17 ADD -1 TO IX GO E
                           else GO X.
           IF ESC GO X.
           IF ENDE GO R.
           IF NOT RET GO E.
           MOVE WH-WERT TO KO-KTONR(IX) SA-KTONR WD-KZ.
           IF WD-KZ NOT = 0 GO E.
       G.  READ SACHBUCH IGNORE LOCK NOT INVALID GO K.
           CALL "FIBSACH" USING "22KTOAN" WH-CREG.
           IF SA-KTONR = 0 MOVE 0 TO KO-KTONR(IX) GO E.
           GO G.
       K.  DIVIDE 10 INTO KO-KTONR(IX) GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           DISPLAY " " AT 0000 SA-TX(1).
           IF IX < 20 ADD 1 TO IX GO E.
       R.  DISPLAY "<ret>= weiter, <>= zurÅck, <esc>= Abbruch < >"
               AT 2301.
           CALL "CAUP" USING "0023450000" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF MOVE 19 TO IX GO E.
           IF NOT RET GO R.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************************** notwendige Konten Teilrechnungslogik *
       TR-SR SECTION 80.
       A.  REWRITE KO-SATZ.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CALL "CAUP" USING "08CLOFEN" WH-CREG.
           CALL "CAUP" USING "271010056500012" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Konten Teilrechnung " with highlight AT VDU-LP.
           DISPLAY "Erlîse Teilrechnung...:" AT 1112
                   "Erlîse Schlu·rechnung.:" AT 1212
                   "Nicht fÑllige Ust/TR..:" AT 1312.
       C.  MOVE 10 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-TSATZ
               WRITE KO-SATZ GO C.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 3
               MOVE KO-TR-SR(IX) TO WH-WERT SA-KTONR
               PERFORM READ-SACH.
           MOVE 1 TO IX.
       E.  MOVE KO-TR-SR(IX) TO WH-WERT SA-KTONR.
           PERFORM READ-SACH.
           DISPLAY "<ret>= Kto-Nr., <esc>= Abbruch, <Ende>= Ende"
               AT 2301.
           COMPUTE VDU-LP = IX * 100 + 127.
           CALL "CAUP" USING "1000006006" WH-CREG.
           IF AUF OR WOLI; IF IX > 1 ADD -1 TO IX GO E.
           IF ESC GO X.
           IF ENDE GO R.
           IF NOT RET GO E.
           MOVE WH-WERT TO KO-TR-SR(IX) SA-KTONR WD-KZ.
           IF WD-KZ NOT = 0 GO E.
       G.  READ SACHBUCH IGNORE LOCK NOT INVALID GO K.
           CALL "FIBSACH" USING "22KTOAN" WH-CREG.
           IF SA-KTONR = 0 MOVE 0 TO KO-TR-SR(IX) GO E.
           GO G.
       K.  DIVIDE 10 INTO KO-TR-SR(IX) GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           DISPLAY " " AT 0000 SA-TX(1).
           IF IX < 3 ADD 1 TO IX GO E.
       R.  DISPLAY "<ret>= speichern, <>= zurÅck, <esc>= Abbruch "
               AT 2301.
           CALL "CAUP" USING "0023471001" WH-CREG.
           IF ESC UNLOCK KONSTANT GO X.
           IF WOLI GO E.
           IF NOT RET GO R.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ************************************************ Sachkonto lesen *
       READ-SACH SECTION 80.
       A.  IF SA-KTONR = 0 GO Z.
           DIVIDE 10 INTO SA-KTONR GIVING WD-KTO.
           COMPUTE VDU-LP = IX * 100 + 127 + VDU-ECK.
           IF IX > 16 COMPUTE VDU-LP = (IX - 16) * 100 + 127 + VDU-ECK.
           DISPLAY WD-KTO with highlight AT VDU-LP.
       C.  READ SACHBUCH IGNORE LOCK INVALID GO Z.
           DISPLAY " " AT 0000 SA-TX(1).
       Z.  EXIT.
      ******************************************************************
       BK-DIS SECTION 80.
       A.  MOVE WX TO WD-BANK.
           COMPUTE VDU-LP = VDU-ECK + 302 + WX * 100.
           DISPLAY WD-BANK AT VDU-LP.
           IF KB-KTONR = SPACE AND KB-BLZ = 0 GO Z.
           ADD 4 TO VDU-LP.
           DISPLAY KB-KTONR with highlight AT VDU-LP.
           ADD 17 TO VDU-LP.
           MOVE KB-BLZ TO WD-BLZ.
           IF KB-BLZ NOT = 0 DISPLAY WD-BLZ with highlight  AT VDU-LP.
           ADD 8 TO VDU-LP.
           DISPLAY KB-BEZ with SIZE 22 highlight AT VDU-LP.
           MOVE KB-ART TO WD-KZ.
           ADD 24 TO VDU-P.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           ADD 3 TO VDU-LP.
           DIVIDE 10 INTO KB-FIBKTO GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           ADD 9 TO VDU-LP.
           DISPLAY KB-KENNUNG with highlight AT VDU-LP.
           ADD 3 TO VDU-LP.
           MOVE KB-KONTROL TO WD-KONTR.
           DISPLAY WD-KONTR with highlight AT VDU-LP.
           ADD 7 TO VDU-LP.
           MOVE KB-OHNEVW TO WD-KZ.
           DISPLAY WD-KZ with highlight foreground-color 4 AT VDU-LP.
       Z.  EXIT.
      ************************************************ Sachkonto lesen *
       BANK-SACH SECTION 80.
       A.  IF KB-FIBKTO = 0 GO Z.
           DIVIDE 10 INTO KB-FIBKTO GIVING WD-KTO.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           DISPLAY "Fibukto-Nr.: " AT 2401
               WD-KTO with highlight AT 0000.
           MOVE KB-FIBKTO TO SA-KTONR.
       C.  READ SACHBUCH IGNORE LOCK INVALID
               MOVE "Konto nicht vorhanden" TO SA-BEZ.
           DISPLAY " " AT 0000 SA-BEZ.
       Z.  EXIT.
      ************************************************* Bankverwaltung *
       BANK SECTION 80.
       A.  CALL "CAUP" USING "0703011379020" WH-CREG.
           ADD 0103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Banken " with highlight AT VDU-LP.
           ADD 203 TO VDU-ECK GIVING VDU-LP.
           DISPLAY "Nr.Konto-Nr.        Banklz. Bankbezeichnung        A
      -        "rt F-Ktonr. KG ZÑhler V" AT VDU-LP.
           ADD 302 TO VDU-ECK GIVING VDU-LP.
           DISPLAY ALL "ƒƒ" WITH SIZE 77 AT VDU-LP.
       B.  CALL "CAUP" USING "16LOEFEN" WH-CREG.
      *---------------------------------------------------> auflisten <-
           MOVE 1 TO WX WR.
       C.  ADD 70 WX GIVING WH-KEY.
           READ KONSTANT IGNORE LOCK INVALID INITIALIZE KB-SATZ.
           PERFORM BK-DIS.
           IF WR = 1; IF WX < 9 ADD 1 TO WX GO C.
           IF WR = 2; IF WL-CA = 10 GO Y
                      ELSE GO K
              ELSE MOVE 2 TO WR.
       D.  DISPLAY "<esc/Ende>= Ende, <ret>= Nr.:" AT 2301.
           CALL "CAUP" USING "0023311001" WH-CREG.
           IF ESC OR ENDE OR RET AND WH-NUM = 0 GO Y.
           IF NOT RET OR WH-NUM = 0 GO D.
           MOVE WH-NUM TO WX.
           GO C.
      *-----------------------------------------> anlegen oder Ñndern <-
       K.  DISPLAY "<ret>= Kontonummer, <esc>= Ende" AT 2301.
           DISPLAY "Keine Bezeichnung = lîschen" AT 2401.
           MOVE KB-KTONR TO WT-TX.
           COMPUTE VDU-LP = WX * 100 + 306.
           CALL "CAUP" USING "1200000115" WH-CREG.
           IF ESC GO D.
           IF NOT RET GO K.
           MOVE WT-TX TO KB-KTONR.
           DISPLAY KB-KTONR with highlight AT VDU-LP.
           IF KB-KTONR = SPACE MOVE 0 TO KB-BLZ KB-ART KB-FIBKTO
               MOVE SPACE TO KB-BEZ
               ADD WH-KEY -70 GIVING KB-NUM
               DELETE KONSTANT INVALID GO B
               end-delete GO B.
       L.  DISPLAY "<ret>= Bankleitzahl, <esc>= Ende, <>= zurÅck"
               AT 2301.
           MOVE KB-BLZ TO WH-WERT.
           COMPUTE VDU-LP = WX * 100 + 300.
           CALL "CAUP" USING "1000235005" WH-CREG.
           IF ESC GO D.
           IF WOLI GO K.
           IF NOT RET GO L.
           MOVE WH-WERT TO KB-BLZ WD-BLZ.
           DISPLAY WD-BLZ with highlight AT VDU-LP.
           ADD 8 TO VDU-LP.
           DISPLAY KB-BEZ with SIZE 22 highlight AT VDU-LP.
       N.  DISPLAY "<ret-leer>= wiederholen, < >= zurÅck" AT 2301.
           MOVE KB-BEZ TO WT-TX.
           COMPUTE VDU-LP = 300 + WX * 100.
           CALL "CAUP" USING "1200310122" WH-CREG.
           IF AUF GO L.
           IF ESC GO D.
           IF NOT RET GO N.
           MOVE WT-TX TO KB-BEZ.
           DISPLAY KB-BEZ with SIZE 22 highlight AT VDU-LP.
       O.  DISPLAY "öberweisungsart 0/1/2, < >= zurÅck" AT 2301.
           DISPLAY "1= neues Formular, 2= Edifact/V3" AT 2401.
           MOVE KB-ART TO WH-WERT.
           COMPUTE VDU-LP = 300 + WX * 100.
           CALL "CAUP" USING "1000551001" WH-CREG.
           IF AUF GO N.
           IF ESC GO D.
           IF NOT RET GO O.
           IF WH-WERT > 2 GO O.
           MOVE WH-WERT TO KB-ART WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       P.  DISPLAY "<ret-leer>= wiederholen, < >= zurÅck" AT 2301.
           DISPLAY "Fibukonto-Nr." AT 2401.
           MOVE KB-FIBKTO TO WH-WERT.
           COMPUTE VDU-LP = 300 + WX * 100.
           CALL "CAUP" USING "1000586006" WH-CREG.
           IF AUF GO O.
           IF ESC GO D.
           IF NOT RET GO P.
           MOVE WH-WERT TO KB-FIBKTO WD-KZ.
           IF WH-WERT = 0 OR WD-KZ > 0 GO P.
           PERFORM BANK-SACH.
      *--------------------------------------> fÅr Raika erforderlich <-
       Q.  DISPLAY "<ret>= Bestandskennung (XX), < >= zurÅck" AT 2301.
           MOVE KB-KENNUNG TO WT-TX.
           COMPUTE VDU-LP = 300 + WX * 100.
           CALL "CAUP" USING "1200670102" WH-CREG.
           IF AUF GO P.
           IF ESC GO D.
           IF NOT RET GO Q.
           MOVE WT-TX TO KB-KENNUNG.
           DISPLAY KB-KENNUNG with highlight AT VDU-LP.
           IF KB-ART = 2 and KB-KENNUNG = SPACE GO Q.
           IF KB-KENNUNG = SPACE MOVE 0 TO WH-WERT
               ADD 3 TO VDU-LP GO S.
       R.  DISPLAY "<ret>= Bestandsnummer, < >= zurÅck" AT 2301.
           MOVE KB-KONTROL TO WH-WERT.
           COMPUTE VDU-LP = 300 + WX * 100.
           CALL "CAUP" USING "1000706006" WH-CREG.
           IF AUF GO Q.
           IF ESC GO D.
           IF NOT RET GO R.
           IF WH-WERT = 0 GO R.
       S.  MOVE WH-WERT TO KB-KONTROL WD-KONTR.
           DISPLAY WD-KONTR with highlight AT VDU-LP.
       T.  DISPLAY "<ret>= V-zweck (0/1), < >= zurÅck" AT 2301.
           DISPLAY "0= normal, 1= Verwendungszweck gesondert" AT 2401.
           MOVE KB-OHNEVW TO WH-WERT.
           COMPUTE VDU-LP = 300 + WX * 100.
           CALL "CAUP" USING "1000771001" WH-CREG.
           IF AUF GO R.
           IF ESC GO D.
           IF NOT RET GO T.
           IF WH-WERT > 1 GO T.
           MOVE WH-WERT TO KB-OHNEVW WD-KZ.
           DISPLAY WD-KZ with highlight foreground-color 4 AT VDU-LP.
      *-----------------------------------------------> Kontowortlaut <-
       U.  CALL "CAUP" USING "271502055000010" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Kontowortlaut " with highlight AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           DISPLAY "Bez: " AT VDU-LP.
           ADD 308 VDU-ECK GIVING VDU-LP.
           DISPLAY KB-KTOBEZ(1) with highlight AT VDU-LP.
           ADD 408 VDU-ECK GIVING VDU-LP.
           DISPLAY KB-KTOBEZ(2) with highlight AT VDU-LP.
       V.  DISPLAY "Kontobezeichung lt. Auszug" with highlight AT 2401.
           DISPLAY "<esc>= Abbruch, <ret>= Bezeichnung, < >= zurÅck"
           MOVE KB-KTOBEZ(1) TO WR-ADR(1).
           MOVE KB-KTOBEZ(2) TO WR-ADR(2).
           CALL "CAUP" USING "1203080235" WH-CREG.
           EVALUATE TRUE
               WHEN AUF CALL "CAUP" USING "08CLOFEN" WH-CREG GO T
               WHEN ESC CALL "CAUP" USING "08CLOFEN" WH-CREG
               WHEN RET MOVE WR-ADR(1) TO KB-KTOBEZ(1)
                        MOVE WR-ADR(2) TO KB-KTOBEZ(2)
                        CALL "CAUP" USING "08CLOFEN" WH-CREG
               WHEN OTHER GO V.
           ADD 1 TO WX.
           IF WX > 7 CALL "CAUP" USING "17SCROLLF" WH-CREG
               MOVE 8 TO WX.
           ADD WH-KEY -70 GIVING KB-NUM.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
           MOVE 2 TO WR.
           GO D.
       Y.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ************************************ eigene Adresse fÅr öberwsg. *
       ADRESS SECTION 80.
       A.  CALL "CAUP" USING "0710200849000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Adresse Auftraggeber - Bank " with highlight
               AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Bez...:" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Zusatz:" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Stra·e:" AT VDU-LP.
           ADD 100 TO VDU-LP.
           DISPLAY "Ort...:" AT VDU-LP.
       F.  MOVE 70 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-FSATZ.
           IF ZUGRIF PERFORM BESETZT GO F.
       H.  ADD 311 VDU-ECK GIVING VDU-LP.
           MOVE KO-FBEZ TO WT-BEZ.
           PERFORM HOLADR.
           PERFORM VARYING WR FROM 1 BY 1 UNTIL WR > 4
               DISPLAY WR-ADR(WR) with highlight SIZE 32 AT VDU-LP
               ADD 100 TO VDU-LP.
           DISPLAY "<esc>= zurÅck, <ret-leer>= keine énderung" AT 2301.
           CALL "CAUP" USING "1203110432" WH-CREG.
           MOVE WT-BEZ TO KO-FBEZ.
           IF ESC; IF KO-FBEZ = SPACE GO X
                   ELSE GO R.
           IF WR-ADR(1) = SPACE
               DISPLAY "Bezeichnung mu· sein!" AT 2401
               PERFORM WEITER GO H.
       R.  DISPLAY "<tab>= lîschen, <ret>= speichern, <esc>= Abbruch"
               AT 2301.
           CALL "CAUP" USING "0023500000" WH-CREG.
           IF ESC GO X.
           IF WOLI GO H.
           IF TABL GO V.
           IF NOT RET GO R.
       S.  WRITE KO-FSATZ INVALID REWRITE KO-FSATZ.
           IF ZUGRIF PERFORM BESETZT GO S.
           GO X.
       V.  ADD 316 VDU-ECK GIVING VDU-LP.
           DISPLAY "    ** gelîscht **    " with highlight AT VDU-LP.
           DELETE KONSTANT INVALID GO X.
           IF ZUGRIF PERFORM BESETZT GO V.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      *************************************** Adresszerlegung f. Druck *
       HOLADR SECTION 80.
       A.  MOVE SPACE TO WT-ADR.
           MOVE "#" TO WR-BEZ(131).
           UNSTRING WT-BEZ DELIMITED BY "#" INTO WR-ADR(1) WR-ADR(2)
               WR-ADR(3) WR-ADR(4) WR-ADR(5) WR-ADR(6).
       Z.  EXIT.
      ******************************************************************
       VERZEICH SECTION 80.
       A.  CALL "CAUP" USING "0710100353000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " FibudateiverknÅpfung " with highlight AT VDU-LP.
       C.  MOVE 11 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KOD-SATZ
               MOVE WH-KEY TO KO-NUM.
           IF ZUGRIF PERFORM BESETZT GO X.
           IF KOD-VERZ = SPACE MOVE "F:\FIBU\FIBUKONS.DAT" TO KOD-VERZ.
           ADD 203 VDU-ECK GIVING VDU-LP.
           DISPLAY "Verzeichnis:" AT VDU-LP.
           ADD 216 VDU-ECK GIVING VDU-LP.
           DISPLAY KOD-VERZ with highlight AT VDU-LP.
       E.  DISPLAY "<esc>= Abbruch, <ret>= Verzeichnis und Bezeichnung d
      -        ". FIBU-Konstantendatei" AT 2301.
           MOVE KOD-VERZ TO WT-TX.
           CALL "CAUP" USING "1202160135" WH-CREG.
           IF ESC GO X.
           IF NOT RET GO E.
           MOVE WT-TX TO KOD-VERZ.
           DISPLAY KOD-VERZ with highlight AT VDU-LP.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
           MOVE KOD-VERZ TO WN-BUEB.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ******************************************** Bedienungsanleitung *
       U-BED SECTION 90.
       A.  CALL "FIBZUS" USING "9906061162010" WH-CREG.
           CANCEL "FIBZUS".
       Z.  EXIT.
      **************************************************** Ust-Fenster *
       UST-FENSTER SECTION 90.
       A.  CALL "CAUP" USING "0701012279000" WH-CREG.
           ADD 103  VDU-ECK GIVING VDU-LP.
           DISPLAY " UST - Konten " with highlight AT VDU-LP.
           ADD 20 TO VDU-LP.
           DISPLAY "  . Seite " with highlight AT VDU-LP.
           ADD 1 TO VDU-LP.
           DISPLAY WX-KZ with highlight AT VDU-LP.
       Z.  EXIT.
      **************************************** Konten f. Ustverprobung *
       USTPROB SECTION 90.
       A.  MOVE LOW-VALUE TO WT-UTAB.
           MOVE 67 TO WH-KEY.
           READ KONSTANT INVALID GO C.
           IF ZUGRIF PERFORM BESETZT GO A.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 40
               MOVE KO-UKTO(IX) TO WK-UKTO(IX + 40)
               MOVE KO-UKZ(IX) TO WK-UKZ(IX + 40).
       C.  MOVE 69 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-USATZ
               PERFORM U-BED
               MOVE WH-KEY TO KO-NUM
               WRITE KO-SATZ GO C.
           IF ZUGRIF PERFORM BESETZT GO C.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 40
               MOVE KO-UKTO(IX) TO WK-UKTO(IX)
               MOVE KO-UKZ(IX) TO WK-UKZ(IX).
           MOVE "1" TO WX-KZ.
           MOVE 40 TO WI.
           MOVE 0 TO WL.
           PERFORM UST-FENSTER.
       D.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           SUBTRACT 39 FROM WI GIVING IX.
           PERFORM VARYING IX FROM IX BY 1 UNTIL IX > WI
               MOVE WK-UKTO(IX) TO SA-KTONR
               PERFORM UST-SACH.
           IF WL = 0 PERFORM SUCH-LAST
                else MOVE WL TO IX.
       E.  MOVE WK-UKTO(IX) TO WH-WERT SA-KTONR.
           PERFORM UST-SACH.
           MOVE 0 TO WL.
           DISPLAY "<ret>= Kto-Nr., <esc>= Abbruch, < / >= auf/ab, </ >
      -        " = links/rechts" AT 2301.
           DISPLAY "<? + ret>= Hinweis, <Entf>= lîschen, <Ende>= speiche
      -        "rn" AT 2401.
           IF WX-KZ = "1" DISPLAY ", <Bild  >= 2. Seite" AT 0000
                     else DISPLAY ", <Bild  >= 1. Seite" AT 0000.
           IF IX > 40 SUBTRACT 40 FROM IX GIVING WZ
                 else MOVE IX TO WZ.
           COMPUTE VDU-LP = WZ * 100 + 103.
           IF WZ > 20 SUBTRACT 2000 FROM VDU-LP
                      ADD 38 TO VDU-LP.
           CALL "CAUP" USING "1000006006" WH-CREG.
           CALL "CAUP" USING "1323012580" WH-CREG.
           IF WH-MCODE(1:1) = "?" PERFORM U-BED GO E.
           IF WORE; IF IX < 61 ADD 20 TO IX;
                         MOVE IX TO WL
                         IF IX > 40 and WX-KZ = "1"
                             MOVE 40 TO IX GO M.
           IF WOLI; IF IX > 20 ADD -20 TO IX;
                         MOVE IX TO WL
                         IF IX < 41 and WX-KZ = "2" MOVE "1" TO WX-KZ
                              CALL "CAUP" USING "08CLOFEN" WH-CREG
                              MOVE WL TO IX
                              GO E.
           IF BAB; IF IX < 41 MOVE 40 TO IX GO M
                   else MOVE "1" TO WX-KZ
                        CALL "CAUP" USING "08CLOFEN" WH-CREG
                        PERFORM SUCH-LAST
                        GO E.
           IF AUF; IF IX < 41; IF IX > 1 ADD -1 TO IX GO E
                               else GO E
                   else IF IX = 41 MOVE "1" TO WX-KZ
                            CALL "CAUP" USING "08CLOFEN" WH-CREG
                            PERFORM SUCH-LAST GO E
                        else ADD -1 TO IX GO E.
           IF AB GO M.
           IF ESC; IF WX-KZ = "1" GO X
                   else CALL "CAUP" USING "08CLOFEN" WH-CREG GO X.
           IF ENDE GO R.
           IF EINF PERFORM OLDPIC.
           IF ENTF; PERFORM VARYING WX FROM IX BY 1 UNTIL WX > 79
                        MOVE WK-UKTO(WX + 1) TO WK-UKTO(WX)
                        MOVE WK-UKZ(WX + 1) TO WK-UKZ(WX)
                    end-perform
                    MOVE 0 TO WK-UKTO(80) WK-UKZ(80)
                    IF IX < 41 MOVE 40 TO WI
                          else MOVE 80 TO WI
                    end-if MOVE IX TO WL
                           GO D.
           IF NOT RET GO E.
           MOVE WH-WERT TO SA-KTONR WD-KZ.
           IF WD-KZ > 1 or SA-KTONR = 0 GO E.
       G.  READ SACHBUCH IGNORE LOCK NOT INVALID GO I.
           CALL "FIBSACH" USING "22KTOAN" WH-CREG.
           IF SA-KTONR = 0 GO E.
           GO G.
       I.  DIVIDE 10 INTO SA-KTONR GIVING WD-KTO.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 80
              IF SA-KTONR = WK-UKTO(WX) and IX not = WX
                  DISPLAY "Konto doppelt" with highlight blink AT 2501
                  IF WX > 40 DISPLAY " auf Seite 2" AT 0000
                        else DISPLAY " auf Seite 1" AT 0000
                  end-if
                  GO E.
           DISPLAY WD-KTO with highlight AT VDU-LP.
           DISPLAY SA-TX(1) AT 0000.
       K.  DISPLAY "<Ende>= Abbruch, <ret>= Kz, < >= zurÅck" AT 2301.
           DISPLAY "Ust-Kennzeichen lt. Ust-TAbelle" with highlight
               AT 2401.
           MOVE WK-UKZ(IX) TO WH-WERT.
           IF IX > 40 SUBTRACT 40 FROM IX GIVING WZ
                 else MOVE IX TO WZ.
           COMPUTE VDU-LP = WZ * 100 + 137.
           IF WZ > 20 SUBTRACT 2000 FROM VDU-LP
                      ADD 38 TO VDU-LP.
           CALL "CAUP" USING "1000001001" WH-CREG.
           IF WOLI or AUF GO E.
           IF ENDE GO R.
           IF NOT RET GO K.
           MOVE WH-WERT TO WK-UKZ(IX) WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
           MOVE SA-KTONR TO WK-UKTO(IX).
       M.  IF IX = 80 GO E.
           IF IX = 40 MOVE "2" TO WX-KZ
                      MOVE 80 TO WI
                      PERFORM UST-FENSTER
                      GO D.
           ADD 1 TO IX.
           GO E.
       R.  IF WX-KZ = "2" CALL "CAUP" USING "08CLOFEN" WH-CREG.
           MOVE 69 TO WH-KEY.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 40
               MOVE WK-UKTO(WX) TO KO-UKTO(WX)
               MOVE WK-UKZ(WX) TO KO-UKZ(WX).
           REWRITE KO-SATZ.
           MOVE 67 TO WH-KEY KO-NUM.
           MOVE "0" TO WX-KZ.
           PERFORM VARYING WX FROM 41 BY 1 UNTIL WX > 80
               MOVE WK-UKTO(WX) TO KO-UKTO(WX - 40)
               MOVE WK-UKZ(WX) TO KO-UKZ(WX - 40)
               IF WK-UKTO(WX) not = 0 MOVE "9" TO WX-KZ.
           IF WX-KZ = "0" GO X.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ******************************************* altes Bild abstellen *
       OLDPIC SECTION 90.
       A.  SET RET TO TRUE.
           PERFORM UST-SACH.
           IF WK-UKTO(40) = 0 MOVE 39 TO WI GO E.
           MOVE 79 TO WI
           IF WK-UKTO(80) not = 0 DISPLAY "Letzte Eintragung auf Seite
      -            "2 nicht frei" at 2401
                   PERFORM WEITER
                   SET ESC TO TRUE GO Z.
       E.  PERFORM VARYING WX FROM WI BY -1 UNTIL WX < IX
               MOVE WK-UKTO(WX) TO WK-UKTO(WX + 1)
               MOVE WK-UKZ(WX) TO WK-UKZ(WX + 1).
           MOVE 0 TO WK-UKTO(IX) WK-UKZ(IX).
           CALL "CAUP" USING "16CLRFEN" WH-CREG.
           MOVE IX TO WX.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 40
               MOVE WK-UKTO(IX) TO SA-KTONR
               PERFORM UST-SACH.
           MOVE WX TO IX.
           IF WH-NUM = 0 SET EINF TO TRUE.
           IF IX > 40 SUBTRACT 40 FROM IX GIVING WZ
                 else MOVE IX TO WZ.
           COMPUTE VDU-LP = WZ * 100 + 103.
           IF WZ > 20 SUBTRACT 2000 FROM VDU-LP
                      ADD 38 TO VDU-LP.
       Z.  EXIT.
      ************************************* letzten aus Tabelle suchen *
       SUCH-LAST SECTION 90.
       A.  IF WX-KZ = "1" MOVE 40 TO WI
                     else MOVE 80 TO WI.
           SUBTRACT 39 FROM WI GIVING IX.
           PERFORM VARYING WI FROM WI BY -1 UNTIL WI = IX or
               WK-UKTO(WI) not = 0 CONTINUE.
           MOVE WI TO IX.
       Z.  EXIT.
      ************************************************ Sachkonto lesen *
       UST-SACH SECTION 90.
       A.  IF SA-KTONR = 0 GO Z.
           DIVIDE 10 INTO SA-KTONR GIVING WD-KTO.
           IF IX > 40 SUBTRACT 40 FROM IX GIVING WZ
                 else MOVE IX TO WZ.
           COMPUTE VDU-LP = WZ * 100 + 103.
           IF WZ > 20 SUBTRACT 2000 FROM VDU-LP
                      ADD 38 TO VDU-LP.
           ADD VDU-ECK TO VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
       C.  READ SACHBUCH IGNORE LOCK INVALID GO Z.
           DISPLAY SA-TX(1) AT 0000.
           COMPUTE VDU-LP = WZ * 100 + 137.
           IF WZ > 20 SUBTRACT 2000 FROM VDU-LP
                      ADD 38 TO VDU-LP.
           ADD VDU-ECK TO VDU-LP.
           MOVE WK-UKZ(IX) TO WD-KZ.
           DISPLAY WD-KZ with highlight AT VDU-LP.
       Z.  EXIT.
      ***************************** Konten f. GetrÑnkeabgabeermittlung *
       GETRAENK SECTION 90.
       A.  CALL "CAUP" USING "0704011879000" WH-CREG.
           ADD 103  VDU-ECK GIVING VDU-LP.
           DISPLAY " GetrÑnkeabgabe - Konten "
                     with highlight AT VDU-LP.
       C.  MOVE 68 TO WH-KEY.
           READ KONSTANT INVALID INITIALIZE KO-GSATZ
               PERFORM G-BED
               MOVE WH-KEY TO KO-NUM.
           IF ZUGRIF PERFORM BESETZT GO C.
       D.  CALL "CAUP" USING "16CLRFEN" WH-CREG.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 32
               MOVE KO-GKTO(IX) TO SA-KTONR
               PERFORM GST-SACH.
           MOVE 1 TO IX.
       E.  MOVE KO-GKTO(IX) TO WH-WERT SA-KTONR.
           PERFORM GST-SACH.
           DISPLAY "<ret>= Kto-Nr., <esc>= Abbruch, <Ende>= Ende, < / >=
      -        " auf/ab" AT 2301.
           DISPLAY "<? + ret>= Hinweis, <Entf>= lîschen" AT 2401.
           COMPUTE VDU-LP = IX * 100 + 103.
           IF IX > 16 SUBTRACT 1600 FROM VDU-LP
                      ADD 38 TO VDU-LP.
           CALL "CAUP" USING "1000006006" WH-CREG.
           CALL "CAUP" USING "1323012580" WH-CREG.
           IF WH-MCODE(1:1) = "?" PERFORM G-BED GO E.
           IF AUF OR WOLI; IF IX > 1 ADD -1 TO IX GO E.
           IF AB GO M.
           IF ESC GO X.
           IF ENDE GO R.
           IF ENTF MOVE 0 TO KO-UKTO(IX) KO-UKZ(IX) GO D.
           IF NOT RET GO E.
           MOVE WH-WERT TO SA-KTONR WD-KZ.
           IF WD-KZ > 1 or SA-KTONR = 0 GO E.
       G.  READ SACHBUCH IGNORE LOCK NOT INVALID GO I.
           CALL "FIBSACH" USING "22KTOAN" WH-CREG.
           IF SA-KTONR = 0 GO E.
           GO G.
       I.  DIVIDE 10 INTO SA-KTONR GIVING WD-KTO.
           PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 32
              IF SA-KTONR = KO-GKTO(WX) and IX not = WX
                  DISPLAY "Konto doppelt" with highlight blink AT 2501
                  GO E.
           MOVE SA-KTONR TO KO-GKTO(IX).
           DISPLAY WD-KTO with highlight AT VDU-LP.
           DISPLAY SA-TX(1)(1:20) AT 0000.
       K.  DISPLAY "<Ende>= Abbruch, <ret>= Kz, < >= zurÅck" AT 2301.
           DISPLAY "%-Satz f. GetrÑnkeabgabe" with highlight AT 2401.
           MULTIPLY 10000 BY KO-GPZ(IX) GIVING WH-WERT.
           COMPUTE VDU-LP = IX * 100 + 132.
           IF IX > 16 SUBTRACT 1600 FROM VDU-LP
                      ADD 38 TO VDU-LP.
           CALL "CAUP" USING "1000006006" WH-CREG.
           IF WOLI or AUF GO E.
           IF ENDE GO R.
           IF NOT RET GO K.
           DIVIDE 10000 INTO WH-WERT GIVING KO-GPZ(IX) WD-GPZ.
           DISPLAY WD-GPZ with highlight AT VDU-LP "%".
       M.  IF IX < 32 ADD 1 TO IX GO E.
       R.  DISPLAY "<ret>= speichern, </ >= zurÅck, <esc>= Abbruch < >"
               AT 2301.
           DISPLAY "<Entf>= lîschen" AT 2401.
           CALL "CAUP" USING "0023500000" WH-CREG.
           IF ESC UNLOCK KONSTANT GO X.
           IF WOLI or AUF GO E.
           IF ENTF DELETE KONSTANT INVALID GO X
                   end-delete GO X.
           IF NOT RET GO R.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
      ************************************************ Sachkonto lesen *
       GST-SACH SECTION 90.
       A.  IF SA-KTONR = 0 GO Z.
           DIVIDE 10 INTO SA-KTONR GIVING WD-KTO.
           COMPUTE VDU-LP = IX * 100 + 103.
           IF IX > 16 SUBTRACT 1600 FROM VDU-LP
                      ADD 38 TO VDU-LP.
           ADD VDU-ECK TO VDU-LP.
           DISPLAY WD-KTO with highlight AT VDU-LP.
       C.  READ SACHBUCH IGNORE LOCK INVALID GO Z.
           DISPLAY SA-TX(1)(1:20) AT 0000.
           COMPUTE VDU-LP = IX * 100 + 132.
           IF IX > 16 SUBTRACT 1600 FROM VDU-LP
                      ADD 38 TO VDU-LP.
           ADD VDU-ECK TO VDU-LP.
           MOVE KO-GPZ(IX) TO WD-GPZ.
           DISPLAY WD-GPZ with highlight AT VDU-LP "%".
       Z.  EXIT.
      ******************************************** Bedienungsanleitung *
       G-BED SECTION 90.
       A.  CALL "FIBZUS" USING "9906061162030" WH-CREG.
           CANCEL "FIBZUS".
       Z.  EXIT.
      ****************************************************** WÑhrungen *
       FREMDWHG SECTION 90.
       A.  IF WL-CA = 26 CALL "CAUP" USING "0701202227000" WH-CREG
                    else CALL "CAUP" USING "0701402227000" WH-CREG.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " WÑhrungstabelle " with highlight AT VDU-LP.
           IF WL-CA = 25 GO C.
           MOVE 80 TO WH-KEY.
       B.  READ KONSTANT IGNORE LOCK INVALID INITIALIZE KO-WSATZ
               MOVE 80 TO KO-NUM
               MOVE "Ats" TO KW-SYM(1)
               MOVE 100 TO KW-KURS(1)
               MOVE "Eur" TO KW-SYM(2)
               MOVE 1376,0300 TO KW-KURS(2)
               MOVE "Dm " TO KW-SYM(3)
               MOVE 703,5531 TO KW-KURS(3)
               MOVE "Lit" TO KW-SYM(4)
               MOVE 0,7107 TO KW-KURS(4)
               MOVE "Hfl" TO KW-SYM(5)
               MOVE 624,4149 TO KW-KURS(5)
               MOVE "Ffr" TO KW-SYM(6)
               MOVE 209,7744 TO KW-KURS(6)
               MOVE "Bfr" TO KW-SYM(7)
               MOVE 34,1104 TO KW-KURS(7)
               MOVE "Fm" TO KW-SYM(8)
               MOVE 231,4317 TO KW-KURS(8)
               MOVE "Ipf" TO KW-SYM(9)
               MOVE 17,4715 TO KW-KURS(9)
               MOVE "Lfr" TO KW-SYM(10)
               MOVE 34,1104 TO KW-KURS(10)
               MOVE "Esc" TO KW-SYM(11)
               MOVE 6,8630 TO KW-KURS(11)
               MOVE "Pta" TO KW-SYM(12)
               MOVE 8,2699 TO KW-KURS(12).
           MOVE KW-FRWTAB TO WE-FRWTAB.
      *--------------------------------------> Symboltabelle anzeigen <-
       C.  PERFORM VARYING WX FROM 1 BY 1 UNTIL WX > 20
               ADD -1 WX GIVING WD-POS
               COMPUTE VDU-LP = WX * 100 + 103 + VDU-ECK
               DISPLAY WD-POS AT VDU-LP
               ADD 6 TO VDU-P
               IF WF-KURS(WX) not = 0 MOVE WF-KURS(WX) TO WD-KURS
                   DISPLAY WF-SYM(WX) with highlight foreground-color 6
                       AT VDU-LP
                   ADD 5 TO VDU-P
                   DISPLAY WD-KURS with highlight AT VDU-LP.
           MOVE 13 TO WX.
           IF WL-CA = 60 GO E.
           MOVE VDU-RES TO VDU-LP.
       D.  DISPLAY "<ret>= Symbol, <esc>= Abbruch, <>= zurÅck" AT 2301.
           CALL "CAUP" USING "0000002002" WH-CREG.
           MOVE WH-NUM TO WH-WERT.
           IF WH-NUM > 19 SET ESC TO TRUE.
           IF WOLI OR ESC MOVE 99 TO WH-WERT;
               IF WL-CA = 25 GO Z
                        else GO X.
           IF NOT RET GO D.
           GO X.
       E.  DISPLAY "<esc>= Abbruch, <Ende>= speichern" AT 2301.
           IF WL-CA not = 20 DISPLAY ", <ret>= Kz f. Sym, < > = auf, < >
      -        "= ab" AT 0000.
           COMPUTE VDU-LP = WX * 100 + 109.
           MOVE WF-SYM(WX) TO WT-TX.
           CALL "CAUP" USING "1200000103" WH-CREG.
           IF WL-CA = 25 GO X.
           IF WL-CA = 26 GO X.
           IF ESC GO X.
           IF ENDE GO Q.
           IF AUF AND WX not = 13 ADD -1 TO WX GO E.
           IF AB GO M.
           IF NOT RET GO E.
           IF WT-TX = SPACE MOVE 0 TO WF-KURS(WX)
               DISPLAY "                 " AT VDU-LP GO M.
           MOVE WT-TX TO WF-SYM(WX).
           DISPLAY WF-SYM(WX) with highlight foreground-color 6
               AT VDU-LP.
       F.  DISPLAY "<ret>= Kurs, < />= zurÅck, <esc>= Abbruch" AT 2301.
           DISPLAY "Kurs je 100 WÑrungseinheiten" with highlight
               AT 2401.
           COMPUTE VDU-LP = WX * 100 + 114.
           MOVE WF-KURS(WX) TO WH-WERT.
           CALL "CAUP" USING "1000009411" WH-CREG.
           IF ESC GO X.
           IF WOLI or AUF GO E.
           IF not RET GO F.
           IF WH-WERT = 0 GO F.
           MOVE WH-WERT TO WF-KURS(WX) WD-KURS.
           DISPLAY WD-KURS with highlight AT VDU-LP.
       M.  IF WX < 19 ADD 1 TO WX.
           GO E.
       Q.  MOVE WE-FRWTAB TO KW-FRWTAB.
           REWRITE KO-SATZ INVALID WRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
       Z.  EXIT.
      ************************************ Beginn Euro als HauswÑhrung *
       EUROBEG SECTION 90.
       A.  CALL "CAUP" USING "0707100540000" WH-CREG.
           MOVE 9 TO WH-KEY.
           READ KONSTANT INVALID GO X.
           ADD 103 VDU-ECK GIVING VDU-LP.
           DISPLAY " Euro-Datum " with highlight foreground-color 6
                AT VDU-LP.
           ADD 303 VDU-ECK GIVING VDU-LP.
           DISPLAY "Umstellungsdatum auf EURO: " AT VDU-LP.
       B.  IF KO-EURO not = 0 MOVE KO-EURO TO WC-DATUM
                         else MOVE 020101 TO WC-DATUM.
           CALL "CAUP" USING "04DATDR" WH-CREG.
           ADD 330 VDU-ECK GIVING VDU-LP.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           MOVE WZ-DATUM TO WH-WERT.
       C.  DISPLAY "<esc>= Abbruch, <ret>= Datum" AT 2301.
           DISPLAY "Dieses Datum kann nicht mehr geÑndert werden!"
                 with highlight AT 2401.
           CALL "CAUP" USING "1103306006" WH-CREG.
           IF ESC GO X.
           IF not RET GO C.
           DISPLAY VDU-DATUM with highlight AT VDU-LP.
           IF KO-EURO = 0 MOVE WX-DATUM TO KO-EURO
                     else GO X.
           REWRITE KO-SATZ.
       X.  CALL "CAUP" USING "08CLOFEN" WH-CREG.
           UNLOCK KONSTANT.
       Z.  EXIT.
