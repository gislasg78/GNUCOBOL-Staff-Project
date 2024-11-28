       IDENTIFICATION DIVISION.
       PROGRAM-ID. DaysElap.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-environmental-variables.
           03  ws-common-accumulators.
               05  ws-tot-leap-days        PIC 9(09)  VALUE ZEROES.
               05  ws-tot-stnd-days        PIC 9(09)  VALUE ZEROES.
               05  ws-total-years          PIC 9(04)  VALUE ZEROES.
               05  ws-total-days           PIC 9(09)  VALUE ZEROES.

           03  ws-tot-leap-stnd-edited     PIC +,+++9 VALUE ZEROES.
           03  ws-total-days-edited        PIC +,+++,+++,++9
                                           VALUE ZEROES.
       
           03  ws-leap-year-calculations.
               05  ws-quottient            PIC 9(04)  VALUE ZEROES.
               05  ws-residue-004          PIC 9(04)  VALUE ZEROES.
               05  ws-residue-100          PIC 9(04)  VALUE ZEROES.
               05  ws-residue-400          PIC 9(04)  VALUE ZEROES.
       
           03  ws-working-constants.
               05  ws-addition-constants.
                   07  ws-cte-01           PIC 9(01)  VALUE 1.
                   07  ws-cte-365          PIC 9(03)  VALUE 365.
                   07  ws-cte-366          PIC 9(03)  VALUE 366.
               05  ws-leap-year-constants.
                   07  ws-cte-04           PIC 9(01)  VALUE 4.
                   07  ws-cte-100          PIC 9(03)  VALUE 100.
                   07  ws-cte-400          PIC 9(03)  VALUE 400.
                   07  ws-cte-1582         PIC 9(04)  VALUE 1582.
       
           03  ws-years-calculations.
               05  ws-leap-years           PIC 9(04)  VALUE ZEROES.
               05  ws-stnd-years           PIC 9(04)  VALUE ZEROES.
               05  ws-year-goal            PIC 9(04)  VALUE ZEROES.
               05  ws-year-inc             PIC 9(04)  VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Program that determines days elapsed since 1582."
           DISPLAY "Target year: " WITH NO ADVANCING
           ACCEPT ws-year-goal
        
           PERFORM 100000-begin-count-elapsed-days
              THRU 100000-end-count-elapsed-days
           VARYING ws-year-inc
              FROM ws-cte-1582 BY ws-cte-01
             UNTIL ws-year-inc IS GREATER THAN ws-year-goal

           PERFORM 200000-begin-show-final-report
              THRU 200000-end-show-final-report

           STOP RUN.

       100000-begin-count-elapsed-days.
           PERFORM 110000-begin-determine-leap-year
              THRU 110000-end-determine-leap-year

           ADD ws-cte-01      TO ws-total-years

           PERFORM 120000-begin-validate-if-leap-year
              THRU 120000-end-validate-if-leap-year.
       100000-end-count-elapsed-days.
           EXIT.

        110000-begin-determine-leap-year.
           DIVIDE ws-cte-04         INTO ws-year-inc
           GIVING ws-quottient REMAINDER ws-residue-004

           DIVIDE ws-cte-100        INTO ws-year-inc
           GIVING ws-quottient REMAINDER ws-residue-100

           DIVIDE ws-cte-400        INTO ws-year-inc
           GIVING ws-quottient REMAINDER ws-residue-400.
        110000-end-determine-leap-year.
           EXIT.

        120000-begin-validate-if-leap-year.
           IF (ws-residue-004 IS EQUAL     TO ZEROES
           AND ws-residue-100 IS NOT EQUAL TO ZEROES)
            OR ws-residue-400 IS EQUAL     TO ZEROES

               ADD ws-cte-366         TO ws-tot-leap-days
                                         ws-total-days
               ADD ws-cte-01          TO ws-leap-years

           ELSE

               ADD ws-cte-365         TO ws-tot-stnd-days
                                         ws-total-days
               ADD ws-cte-01          TO ws-stnd-years

           END-IF.
        120000-end-validate-if-leap-year.
           EXIT.

       200000-begin-show-final-report.
           DISPLAY SPACE
           DISPLAY "Source year : [" ws-cte-1582 "]."
           DISPLAY "Target year : [" ws-year-goal "]."

           DISPLAY SPACE
           DISPLAY "Number of days elapsed."

           MOVE ws-stnd-years         TO ws-tot-leap-stnd-edited
           MOVE ws-tot-stnd-days      TO ws-total-days-edited 
           DISPLAY "Common years: [" ws-tot-leap-stnd-edited
                   "] x ["ws-cte-365 "] = [" ws-total-days-edited "]."

           MOVE ws-leap-years         TO ws-tot-leap-stnd-edited
           MOVE ws-tot-leap-days      TO ws-total-days-edited
           DISPLAY "Leap   years: [" ws-tot-leap-stnd-edited
                   "] x ["ws-cte-366 "] = [" ws-total-days-edited "]."

           DISPLAY SPACE
           DISPLAY "Totals calculated."

           MOVE ws-total-years        TO ws-tot-leap-stnd-edited
           DISPLAY "Total  years: [" ws-tot-leap-stnd-edited "]."

           MOVE ws-total-days         TO ws-total-days-edited
           DISPLAY "Total  days : [" ws-total-days-edited "].".
       200000-end-show-final-report.
           EXIT.

       END PROGRAM DaysElap.
