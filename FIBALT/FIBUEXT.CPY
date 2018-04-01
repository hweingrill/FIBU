       01  WE-EXT       external.
           03  WE-FRWTAB.
               05 WF-SYM               PIC XXX             OCCURS 20.
               05 WF-KURS              PIC 9(4)V9(7) COMP  OCCURS 20.
           03  WK-M.
               05 WM-MA                PIC 99        COMP.
               05 WM-KO                PIC 99        COMP.
               05 WM-JO                PIC 99        COMP.
               05 WM-SK                PIC 99        COMP.
           03  WE-KST                  PIC 99        COMP.
           03  WE-KUEB                 PIC 99        COMP.
           03  WE-USE                  PIC 99        COMP.
           03  WE-PER                  PIC 9(8)      COMP.
           03  WE-WKZ                  PIC 99        COMP.
           03  WE-WJAHR                PIC 9(8)      COMP   OCCURS 3.
           03  WE-EURO                 PIC 9(8)      COMP.
           03  WM-DRU                  PIC 99        COMP.
           03  WE-SOLL                 PIC S9(9)V99  COMP.
           03  WE-HABEN                PIC S9(9)V99  COMP.
           03  WE-SALDO                PIC S9(9)V99  COMP.
           03  WE-BELNR                PIC 9(6)      COMP.
           03  WE-GGKTO                PIC 9(6)      COMP.
           03  WE-GB                   PIC 99        COMP.
           03  WK-KTONR                PIC 9(6)      COMP.
           03  WZ-JOUR                 PIC 9999      COMP.
           03  WE-SY                   PIC 99        COMP.
           03  WE-TS.
               05 WE-SYM               PIC XX                OCCURS 30.
               05 WE-SKZ               PIC 9(4)      COMP-X  OCCURS 30.
           03  WX-TS REDEFINES WE-TS.             *> nur f. Umstellg.Ko
               05 WY-SY                PIC XX                OCCURS 20.
               05 WY-SZ                PIC 9999      COMP    OCCURS 20.
               05 FILLER               PIC XX                OCCURS 20.
           03  WE-SYMT                 PIC 9(5).
           03  WR-Y REDEFINES WE-SYMT.
               05 WE-SH                PIC 9.
               05 WE-UST               PIC 9.
               05 WE-OP                PIC 9.
               05 WE-DIV               PIC 99.
                            88 UST-BAR   VALUE 10 THRU 12.
                            88 USTFREI   VALUE 13.
           03  WE-RETAB.
               05 WH-RT                        OCCURS 100.
                  07 WE-OPKEY          PIC X(10).
                  07 WE-REF            PIC 9(7)       COMP.
                  07 WE-REBET          PIC S9(9)V99   COMP.
                  07 WE-SKONTO         PIC S9(9)V99   COMP.
                  07 WE-UKZ            PIC 99         COMP.
                  07 WE-KURS           PIC 9(4)V9(7)  COMP.
           03  WZ-BUCH                 PIC 9999.
           03  WZ-KOST                 PIC 9999.
           03  WZ-AUSZ                 PIC 9999.
           03  WS-BUCH                 PIC 9999.
           03  WN-BUEB                 PIC X(30).
           03  WN-KONS                 PIC X(12).
           03  WN-OPES                 PIC X(12).
           03  WN-DEBI                 PIC X(12).
           03  WN-KRED                 PIC X(12).
           03  WN-SACH                 PIC X(12).
           03  WN-BZEI                 PIC X(12).
           03  WN-AUSZ                 PIC X(12).
           03  WN-KOST                 PIC X(12).
           03  WH-KEY                  PIC 9(5).            *> aus caup
           03  WE-STG                  PIC X(40)     OCCURS 8.
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  WL-CALL                     PIC X(15).
       01  WL-CREG                     PIC X(1152).
       PROCEDURE DIVISION using WL-CALL WL-CREG.
       DECLARATIVES.
