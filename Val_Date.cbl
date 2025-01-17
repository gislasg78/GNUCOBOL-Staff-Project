       IDENTIFICATION DIVISION.
       PROGRAM-ID. Val_Date.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  cte-01                                     VALUE 01.
       78  cte-02                                     VALUE 02.
       78  cte-12                                     VALUE 12.
       78  cte-1582                                   VALUE 1582.

       01  ws-date-input.
           03  ws-date-year                 PIC 9(04) VALUE ZEROES.
           03  ws-date-month                PIC 9(02) VALUE ZEROES.
           03  ws-date-day                  PIC 9(02) VALUE ZEROES.

       01  ws-residues-calculation-leap-year.
           03  ws-residues-calc-lp-constants.
               05  ws-cte-04                PIC 9(01) VALUE 4.
               05  ws-cte-100               PIC 9(03) VALUE 100.
               05  ws-cte-400               PIC 9(03) VALUE 400.
           03  ws-residues-calc-lp-quottients.
               05  ws-quottient-04          PIC 9(03) VALUE ZEROES.
               05  ws-quottient-100         PIC 9(03) VALUE ZEROES.
               05  ws-quottient-400         PIC 9(03) VALUE ZEROES.
           03  ws-residues-calc-lp-residues.
               05  ws-residue-04            PIC 9(03) VALUE ZEROES.
               05  ws-residue-100           PIC 9(03) VALUE ZEROES.
               05  ws-residue-400           PIC 9(03) VALUE ZEROES.

       01  ws-month-names-tables.
           03  ws-month-names-January.
               05  FILLER                   PIC 9(02) VALUE 01.
               05  FILLER                   PIC A(09) VALUE "January".
               05  FILLER                   PIC 9(02) VALUE 31.

           03  ws-month-names-February.
               05  FILLER                   PIC 9(02) VALUE 02.
               05  FILLER                   PIC A(09) VALUE "February".
               05  FILLER                   PIC 9(02) VALUE 28.

           03  ws-month-names-March.
               05  FILLER                   PIC 9(02) VALUE 03.
               05  FILLER                   PIC A(09) VALUE "March".
               05  FILLER                   PIC 9(02) VALUE 31.

           03  ws-month-names-April.
               05  FILLER                   PIC 9(02) VALUE 04.
               05  FILLER                   PIC A(09) VALUE "April".
               05  FILLER                   PIC 9(02) VALUE 30.

           03  ws-month-names-May.
               05  FILLER                   PIC 9(02) VALUE 05.
               05  FILLER                   PIC A(09) VALUE "May".
               05  FILLER                   PIC 9(02) VALUE 31.

           03  ws-month-names-June.
               05  FILLER                   PIC 9(02) VALUE 06.
               05  FILLER                   PIC A(09) VALUE "June".
               05  FILLER                   PIC 9(02) VALUE 30.

           03  ws-month-names-July.
               05  FILLER                   PIC 9(02) VALUE 07.
               05  FILLER                   PIC A(09) VALUE "July".
               05  FILLER                   PIC 9(02) VALUE 31.

           03  ws-month-names-August.
               05  FILLER                   PIC 9(02) VALUE 08.
               05  FILLER                   PIC A(09) VALUE "August".
               05  FILLER                   PIC 9(02) VALUE 31.

           03  ws-month-names-September.
               05  FILLER                   PIC 9(02) VALUE 09.
               05  FILLER                   PIC A(09) VALUE "September".
               05  FILLER                   PIC 9(02) VALUE 30.

           03  ws-month-names-October.
               05  FILLER                   PIC 9(02) VALUE 10.
               05  FILLER                   PIC A(09) VALUE "October".
               05  FILLER                   PIC 9(02) VALUE 31.

           03  ws-month-names-November.
               05  FILLER                   PIC 9(02) VALUE 11.
               05  FILLER                   PIC A(09) VALUE "November".
               05  FILLER                   PIC 9(02) VALUE 30.

           03  ws-month-names-December.
               05  FILLER                   PIC 9(02) VALUE 12.
               05  FILLER                   PIC A(09) VALUE "December".
               05  FILLER                   PIC 9(02) VALUE 31.

       01  ws-month-names-tables-redef REDEFINES ws-month-names-tables.
           03  ws-month-names-array         OCCURS cte-12 TIMES
                     ASCENDING KEY ws-month-names-array-numbermonth
                     INDEXED    BY idx-month-names-array.
               05  ws-month-names-array-numbermonth PIC 9(02).
               05  ws-month-names-array-nameofmonth PIC A(09).
               05  ws-month-names-array-totaldays   PIC 9(02).
                   88  sw-month-names-array-totaldays-Feb-Norm
                                                    VALUE 28.
                   88  sw-month-names-array-totaldays-Feb-Leap
                                                    VALUE 29.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "Date validity checker."
           DISPLAY "Please enter the following requested data."
           DISPLAY "Year : " WITH NO ADVANCING
            ACCEPT ws-date-year
           DISPLAY "Month: " WITH NO ADVANCING
            ACCEPT ws-date-month
           DISPLAY "Day  : " WITH NO ADVANCING
            ACCEPT ws-date-day

           DISPLAY SPACE
           PERFORM Check-Year-Number
           PERFORM Check-Month-Number
        
           STOP RUN.

       Check-Year-Number.
           IF ws-date-year IS GREATER THAN OR EQUAL TO cte-1582
              DISPLAY "Year : [" ws-date-year "]. OK!"
              PERFORM Check-Leap-Year
           ELSE
              DISPLAY SPACE
              DISPLAY "The year value: [" ws-date-year
                          "] is not valid."
              DISPLAY "The range must be between: [" cte-1582 "]"
                      " and [9999].".

       Check-Leap-Year.
           SET idx-month-names-array TO cte-02

           DIVIDE ws-cte-04  INTO ws-date-year GIVING ws-quottient-04
           REMAINDER ws-residue-04

           DIVIDE ws-cte-100 INTO ws-date-year GIVING ws-quottient-100
           REMAINDER ws-residue-100

           DIVIDE ws-cte-400 INTO ws-date-year GIVING ws-quottient-400
           REMAINDER ws-residue-400

           IF  (ws-residue-04  IS EQUAL     TO ZEROS
           AND  ws-residue-100 IS NOT EQUAL TO ZEROES)
            OR  ws-residue-400 IS EQUAL     TO ZEROES
                SET sw-month-names-array-totaldays-Feb-Leap
                   (idx-month-names-array)
                 TO TRUE
           ELSE
                SET sw-month-names-array-totaldays-Feb-Norm
                   (idx-month-names-array)
                 TO TRUE
           END-IF

           DISPLAY " + February days: [" ws-month-names-array-totaldays 
                                        (idx-month-names-array) "].".

       Check-Month-Number.
           SET idx-month-names-array TO cte-01
           SEARCH ALL ws-month-names-array
               AT END
                  DISPLAY SPACE
                  DISPLAY "The month value: [" ws-date-month "]"
                          " is not valid."
                  DISPLAY "The range must be between: [" cte-01 "] "
                          "and [" cte-12 "]."

             WHEN ws-month-names-array-numbermonth
                 (idx-month-names-array) IS EQUAL TO ws-date-month
                  DISPLAY "Month: [" ws-month-names-array-numbermonth
                                    (idx-month-names-array)
                          "] = ["    ws-month-names-array-nameofmonth
                                    (idx-month-names-array)
                          "] OK!"
                  PERFORM Check-Day-Number-In-Range-Month

           END-SEARCH.

       Check-Day-Number-In-Range-Month.
           EVALUATE ws-date-day
               WHEN IS GREATER THAN OR EQUAL TO cte-01
                AND IS LESS THAN OR EQUAL TO
                    ws-month-names-array-totaldays
                   (idx-month-names-array)
                    DISPLAY "Day  : [" ws-date-day "] OK!"

               WHEN OTHER
                    DISPLAY SPACE
                    DISPLAY "The day value: [" ws-date-day
                            "] is not valid."
                    DISPLAY "The range must be between: [" cte-01
                            "] and [" ws-month-names-array-totaldays
                                     (idx-month-names-array) "]."

           END-EVALUATE.

       END PROGRAM Val_Date.
