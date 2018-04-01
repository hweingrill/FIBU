      $set noosvs mf ans85

      ****************************************************************
      * Copyright Micro Focus Limited 1991.    All Rights Reserved.  *
      * This demonstration program is provided for use by users of   *
      * Micro Focus products and may be used, modified and           *
      * distributed as part of your application provided that you    *
      * properly acknowledge the copyright of Micro Focus in this    *
      * material.                                                    *
      ****************************************************************

      ************************************************************
      *                                                          *
      *                      FUNKEY.CBL                          *
      *                                                          *
      *    This program demonstrates how to decode function keys *
      *    using the x"af" call.                                 *
      *                                                          *
      ************************************************************
       special-names.
           crt status is key-status.

       working-storage section.
       01 flag                   pic 9(2) comp-x value 1.
       01 user-key-control.
          05 enable-fn-keys      pic 9(2) comp-x value 1.
          05 filler              pic x           value "1".
          05 first-user-key      pic 9(2) comp-x value 1.
          05 number-of-keys      pic 9(2) comp-x value 99.

       01 key-status.
          05 key-type            pic x.
          05 key-code-1          pic 9(2) comp-x.
          05 filler              pic x.
       01 any-data               pic x.
       01 key-code-1-display     pic z9.

       01  funco                 pic x comp-x.
       01  sys-aa.
           03 sys-a              pic x comp-x     occurs 16.

       procedure division.
           perform enable-keys
           display spaces upon crt.

           move 20 to funco.
           call x"a7" using funco sys-aa return-code.
           move 21 to funco.
           move 3 to sys-a(4).
           call x"a7" using funco sys-aa return-code.

       A.  perform accept-function-key
           perform tell-which-key-was-pressed
           GO A.
           perform stop-run.

       enable-keys.
           call x"af" using flag user-key-control.

       accept-function-key.
      *    display spaces upon crt
           display "Press a function key: F1 to F10" at 0505
           accept any-data at 0540.

       tell-which-key-was-pressed.
           evaluate key-type
              when 0 display "You pressed <Enter>" at 0705
              when 1
                   move key-code-1 to key-code-1-display
                   display "You pressed function key" at 0705
                   display key-code-1-display         at 0730
           end-evaluate.

       stop-run.
           stop run.
