       IDENTIFICATION DIVISION.
       PROGRAM-ID. Rnms.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-pay-rec.
           03  ws-fixed-pay.
               05  ws-basic        PIC 9(05) VALUE 10000.
               05  ws-da           PIC 9(05) VALUE 2000.
           03  ws-add-pay.
               05  ws-hra          PIC 9(05) VALUE 1000.
               05  ws-mi           PIC 9(05) VALUE 500.
           03  ws-deduction.
               05  ws-it-ded       PIC 9(05) VALUE 1000.
               05  ws-oth-ded      PIC 9(05) VALUE 500.
               05  ws-pf-ded       PIC 9(05) VALUE 2000.

       66  ws-add-pay-to-ws-deduction      RENAMES ws-fixed-pay
                                              THRU ws-add-pay.
       66  ws-basic-to-ws-mi       RENAMES ws-basic  THRU ws-mi.
       66  ws-da-to-ws-hra         RENAMES ws-da     THRU ws-hra.
       66  ws-da-to-ws-mi          RENAMES ws-da     THRU ws-mi.
       66  ws-da-to-ws-pf-ded      RENAMES ws-da     THRU ws-pf-ded.
       66  ws-fixed-pay-to-ws-hra  RENAMES ws-fixed-pay THRU ws-hra.
       66  ws-hra-to-ws-it-ded     RENAMES ws-hra    THRU ws-it-ded.
       66  ws-hra-to-ws-oth-ded    RENAMES ws-hra    THRU ws-oth-ded.
       66  ws-it-ded-to-ws-pf-ded  RENAMES ws-it-ded THRU ws-pf-ded.
       66  ws-mi-to-ws-pf-ded      RENAMES ws-mi     THRU ws-pf-ded.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Variable regrouping program."

           DISPLAY SPACE
           DISPLAY "Content of the regrouped variables."
           DISPLAY "Full main record content."
           DISPLAY "Record Pay        : [" ws-pay-rec             "]."

           DISPLAY SPACE
           DISPLAY "Content of its elementary variables."
           DISPLAY "+ Fixed Pay       : [" ws-fixed-pay           "]."
           DISPLAY "-- Basic          : [" ws-basic               "]."
           DISPLAY "-- Da             : [" ws-da                  "]."
           DISPLAY "+ Add Pay         : [" ws-add-pay             "]."
           DISPLAY "-- Hra            : [" ws-hra                 "]."
           DISPLAY "-- Mi             : [" ws-mi                  "]."
           DISPLAY "+ Deduction       : [" ws-deduction           "]."
           DISPLAY "-- It Ded         : [" ws-it-ded              "]."
           DISPLAY "-- Oth Ded        : [" ws-oth-ded             "]."
           DISPLAY "-- Pf Ded         : [" ws-pf-ded              "]."

           DISPLAY SPACE
           DISPLAY "Content of variables regrouped in different ways."
           DISPLAY "Add pay to Deduct : [" ws-add-pay-to-ws-deduction
                   "]."
           DISPLAY "Basic  to Mi      : [" ws-basic-to-ws-mi      "]."
           DISPLAY "Da     to Hra     : [" ws-da-to-ws-hra        "]."
           DISPLAY "Da     to Mi      : [" ws-da-to-ws-mi         "]."
           DISPLAY "Da     to Pf  Ded : [" ws-da-to-ws-pf-ded     "]."
           DISPLAY "Fixed Pay to  Hra : [" ws-fixed-pay-to-ws-hra "]."
           DISPLAY "Hra    to It  Ded : [" ws-hra-to-ws-it-ded    "]."
           DISPLAY "Hra    to Oth Ded : [" ws-hra-to-ws-oth-ded   "]."
           DISPLAY "It Ded to Pf  Ded : [" ws-it-ded-to-ws-pf-ded "]."
           DISPLAY "Mi     to Pf  Ded : [" ws-mi-to-ws-pf-ded     "]."

           DISPLAY SPACE
           DISPLAY "Press the ENTER key to end the program..."
              WITH NO ADVANCING
           ACCEPT OMITTED

           STOP RUN.

       END PROGRAM Rnms.
