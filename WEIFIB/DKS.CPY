      ***************************** Bewegungss„tze fr Fibuberleitung *
       FD  FIBUEBER                    LABEL RECORD STANDARD.
       01  BU-BSATZ.
      *------------------------ BU-SA= "B" ---------------> Buchungen <-
           03  BU-SA                   PIC X.
           03  BU-KTONR                PIC X(6).
           03  BU-RENR                 PIC X(7).
           03  BU-DATUM                PIC 9(6).
           03  BU-SYMBOL               PIC 99.      *>RE / GU
           03  BU-GEGENKTO             PIC X(6).
           03  BU-Z                    PIC 9.       *> ohne Verwendg.
           03  BU-VM                   PIC 9.       *>1= Vst./ 2= Mwst.
           03  BU-REFENZNR             PIC X(7).
           03  BU-UST-KZ1              PIC 9.        *>0=0% 1=10% 2=20%
           03  BU-UST-KZ2              PIC 9.        *>0=0% 1=10% 2=20%
           03  BU-SH                   PIC 9.        *>0= SOLL 1= HABEN
           03  BU-UST                  PIC 9(7)V99.  *> ohne Vorzeichen
           03  BU-UST-BASIS1           PIC 9(9)V99.
           03  BU-UST-BASIS2           PIC 9(9)V99.
           03  BU-BETRAG               PIC 9(9)V99.  *>Brutto
           03  BU-MENGE                PIC 9(9)V99.
           03  BU-VALDAT               PIC 9(6).     *>Valutadatum
           03  BU-TX                   PIC X(40).
      *---------------------- BU-SA= "K" -----------> Kontenbernahme <-
       01  BU-KSATZ.
           03  FILLER                  PIC X.
           03  FILLER                  PIC 9(6).
           03  BU-MCODE                PIC X(8).
           03  BU-ADR                  PIC X(30)     OCCURS 4.
           03  BU-BKLZL                PIC 9(9).
           03  FILLER                  PIC X(21).
           03  BU-BKTO                 PIC X(15).
           03  FILLER                  PIC X(15).
