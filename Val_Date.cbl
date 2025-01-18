       IDENTIFICATION DIVISION.
       PROGRAM-ID. Val_Date.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ws-date-day-calc                 COMP-1    VALUE ZEROES.
       77  ws-date-quottient-aux            COMP-1    VALUE ZEROES.
       77  ws-date-dayofweek-aux            PIC 9(01) VALUE ZERO.

       78  cte-01                                     VALUE 01.
       78  cte-02                                     VALUE 02.
       78  cte-03                                     VALUE 03.
       78  cte-04                                     VALUE 04.
       78  cte-05                                     VALUE 05.
       78  cte-06                                     VALUE 06.
       78  cte-07                                     VALUE 07.
       78  cte-12                                     VALUE 12.
       78  cte-31                                     VALUE 31.
       78  cte-100                                    VALUE 100.
       78  cte-400                                    VALUE 400.
       78  cte-1582                                   VALUE 1582.

       01  ws-environmental-variables.
           03  ws-date-auxiliar.
               05  ws-date-year-aux         PIC 9(04) VALUE ZEROES.
               05  ws-date-month-aux        PIC 9(02) VALUE ZEROES.
               05  ws-date-day-aux          PIC 9(02) VALUE ZEROES.

           03  ws-date-input.
               05  ws-date-year             PIC 9(04) VALUE ZEROES.
                   88  sw-date-year-1582-to-9999
                                            VALUES 1582 THRU 9999.
               05  ws-date-month            PIC 9(02) VALUE ZEROES.
                   88  sw-date-month-01-to-12
                                            VALUES 01   THRU 12.
               05  ws-date-day              PIC 9(02) VALUE ZEROES.
                   88  sw-date-day-01-to-31 VALUES 01   THRU 31.

           03  ws-date-output.
               05  FILLER                   PIC X(01) VALUE X'5B'.
               05  ws-date-year             PIC 9(04) VALUE ZEROES.
               05  FILLER                   PIC X(01) VALUE X'2F'.
               05  ws-date-month            PIC 9(02) VALUE ZEROES.
               05  FILLER                   PIC X(01) VALUE X'2F'.
               05  ws-date-day              PIC 9(02) VALUE ZEROES.
               05  FILLER                   PIC X(01) VALUE X'5D'.

           03  ws-residues-calculation-leap-year.
               05  ws-residues-calc-lp-constants.
                   07  ws-cte-04            PIC 9(01) VALUE 4.
                   07  ws-cte-100           PIC 9(03) VALUE 100.
                   07  ws-cte-400           PIC 9(03) VALUE 400.
               05  ws-residues-calc-lp-quottients.
                   07  ws-quottient-04      PIC 9(03) VALUE ZEROES.
                   07  ws-quottient-100     PIC 9(03) VALUE ZEROES.
                   07  ws-quottient-400     PIC 9(03) VALUE ZEROES.
               05  ws-residues-calc-lp-residues.
                   07  ws-residue-04        PIC 9(03) VALUE ZEROES.
                   07  ws-residue-100       PIC 9(03) VALUE ZEROES.
                   07  ws-residue-400       PIC 9(03) VALUE ZEROES.

       01  ws-day-names-tables.
           03  ws-day-names-Saturday.
               05  FILLER                   PIC 9(01) VALUE ZERO.
               05  FILLER                   PIC A(09) VALUE "Saturday".
           03  ws-day-names-Sunday.
               05  FILLER                   PIC 9(01) VALUE 1.
               05  FILLER                   PIC A(09) VALUE "Sunday".
           03  ws-day-names-Monday.
               05  FILLER                   PIC 9(01) VALUE 2.
               05  FILLER                   PIC A(09) VALUE "Monday".
           03  ws-day-names-Tuesday.
               05  FILLER                   PIC 9(01) VALUE 3.
               05  FILLER                   PIC A(09) VALUE "Tuesday".
           03  ws-day-names-Wednesday.
               05  FILLER                   PIC 9(01) VALUE 4.
               05  FILLER                   PIC A(09) VALUE "Wednesday".
           03  ws-day-names-Thursday.
               05  FILLER                   PIC 9(01) VALUE 5.
               05  FILLER                   PIC A(09) VALUE "Thursday".
           03  ws-day-names-Friday.
               05  FILLER                   PIC 9(01) VALUE 6.
               05  FILLER                   PIC A(09) VALUE "Friday".
       01  ws-day-names-tables-redef REDEFINES ws-day-names-tables.
           03  ws-day-names-array           OCCURS cte-12 TIMES
                     ASCENDING KEY ws-day-names-array-numberday
                     INDEXED    BY idx-day-names-array.
               05  ws-day-names-array-numberday     PIC 9(01).
               05  ws-day-names-array-nameofday     PIC A(09).

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
       MAIN-PARAGRAPH.
           DISPLAY "Date validity checker."
           DISPLAY "Please enter the following requested data."
           DISPLAY "Year : " WITH NO ADVANCING
            ACCEPT ws-date-year  OF ws-date-input
           DISPLAY "Month: " WITH NO ADVANCING
            ACCEPT ws-date-month OF ws-date-input
           DISPLAY "Day  : " WITH NO ADVANCING
            ACCEPT ws-date-day   OF ws-date-input

           PERFORM Date-Validator

           STOP RUN.

       Date-Validator.
           DISPLAY SPACE

           IF  ws-date-year  OF ws-date-input IS EQUAL TO ZEROES
           AND ws-date-month OF ws-date-input IS EQUAL TO ZEROES
           AND ws-date-day   OF ws-date-input IS EQUAL TO ZEROES
              DISPLAY SPACE
              DISPLAY "Invalid date. The system date is taken."
              ACCEPT ws-date-input FROM DATE YYYYMMDD
           END-IF

           PERFORM Check-Year-Number

           IF sw-date-year-1582-to-9999        OF ws-date-input
              PERFORM Check-Leap-Year

              IF sw-date-month-01-to-12        OF ws-date-input
                 PERFORM Check-Month-Number

                 IF idx-month-names-array IS GREATER THAN OR EQUAL TO
                    cte-01 AND IS LESS THAN OR EQUAL TO cte-12
                    IF sw-date-day-01-to-31    OF ws-date-input
                       PERFORM Check-Day-Number-In-Range-Month
                    ELSE
                       DISPLAY "The day range must be between: ["
                                cte-01 "] and [" cte-31 "]."
                       DISPLAY "The day value: ["
                                ws-date-day    OF ws-date-input
                               "] is not valid."
                    END-IF
                 END-IF
              ELSE
                 DISPLAY "The month range must be between: ["
                         cte-01 "] and [" cte-12 "]."
                 DISPLAY "The month value: ["
                          ws-date-month        OF ws-date-input
                         "] is not valid."
              END-IF
           ELSE
              DISPLAY "The year range must be beyond: [" cte-1582 "]."
              DISPLAY "The year value: ["
                       ws-date-year            OF ws-date-input
                      "] is not valid."
           END-IF.

       Check-Year-Number.
           IF ws-date-year OF ws-date-input IS GREATER THAN
           OR EQUAL TO cte-1582
              DISPLAY "Year : [" ws-date-year  OF ws-date-input
                      "]. OK!".

       Check-Leap-Year.
           SET idx-month-names-array           TO cte-02

           DIVIDE ws-cte-04  INTO ws-date-year OF ws-date-input
           GIVING ws-quottient-04    REMAINDER ws-residue-04

           DIVIDE ws-cte-100 INTO ws-date-year OF ws-date-input
           GIVING ws-quottient-100   REMAINDER ws-residue-100

           DIVIDE ws-cte-400 INTO ws-date-year OF ws-date-input
           GIVING ws-quottient-400   REMAINDER ws-residue-400

           IF  (ws-residue-04  IS EQUAL     TO ZEROS
           AND  ws-residue-100 IS NOT EQUAL TO ZEROES)
            OR  ws-residue-400 IS EQUAL     TO ZEROES
                SET sw-month-names-array-totaldays-Feb-Leap
                   (idx-month-names-array)  TO TRUE
           ELSE
                SET sw-month-names-array-totaldays-Feb-Norm
                   (idx-month-names-array)  TO TRUE
           END-IF.

       Check-Month-Number.
           SET idx-month-names-array        TO cte-01
           SEARCH ALL ws-month-names-array
               AT END
                  DISPLAY "The month value: [" 
                           ws-date-month    OF ws-date-input
                          "] is not valid."

             WHEN ws-month-names-array-numbermonth
                 (idx-month-names-array) IS EQUAL TO 
                  ws-date-month OF ws-date-input
                  DISPLAY "Month: [" ws-month-names-array-numbermonth
                                    (idx-month-names-array)
                          "] = ["    ws-month-names-array-nameofmonth
                                    (idx-month-names-array)
                          "] OK!"

           END-SEARCH.

       Check-Day-Number-In-Range-Month.
           EVALUATE ws-date-day OF ws-date-input
               WHEN IS GREATER THAN OR EQUAL TO cte-01
                AND IS LESS THAN OR EQUAL TO
                    ws-month-names-array-totaldays
                   (idx-month-names-array)
                    DISPLAY "Day  : ["
                             ws-date-day OF ws-date-input
                            "] OK!"

                    PERFORM Perpetual-Calendar

               WHEN OTHER
                    DISPLAY "The day value: ["
                             ws-date-day OF ws-date-input
                            "] is not valid."
                    DISPLAY "The range must be between: [" cte-01
                            "] and [" ws-month-names-array-totaldays
                                     (idx-month-names-array) "]."

           END-EVALUATE.

       Perpetual-Calendar.
           MOVE ws-date-year  OF ws-date-input TO ws-date-year-aux
           MOVE ws-date-month OF ws-date-input TO ws-date-month-aux
           MOVE ws-date-day   OF ws-date-input TO ws-date-day-aux

           IF ws-date-month-aux IS LESS THAN   OR EQUAL TO cte-02
              ADD cte-12        TO ws-date-month-aux
              SUBTRACT cte-01 FROM ws-date-year-aux
           END-IF

           COMPUTE ws-date-day-calc ROUNDED = 
                  (ws-date-day-aux + cte-02 *
                   ws-date-month-aux + cte-03 *
                   (ws-date-month-aux + cte-01) / cte-05 +
                   ws-date-year-aux +
                   ws-date-year-aux / cte-04 -
                   ws-date-year-aux / cte-100 + 
                   ws-date-year-aux / cte-400 +
                   cte-02)

           DISPLAY SPACE
           DISPLAY "Day Calculation: [" ws-date-day-calc "]."

           DIVIDE ws-date-day-calc BY cte-07
           GIVING ws-date-quottient-aux REMAINDER ws-date-dayofweek-aux

           DISPLAY "Quottient   : [" ws-date-quottient-aux "]."
           DISPLAY "Day Of Week : [" ws-date-dayofweek-aux "]."

           IF  ws-date-dayofweek-aux  IS GREATER THAN OR EQUAL TO ZEROES
           AND ws-date-dayofweek-aux  IS LESS    THAN OR EQUAL TO cte-06
               MOVE CORR ws-date-input   TO ws-date-output

               DISPLAY SPACE
               DISPLAY "Day of Week : ["
                        ws-day-names-array-nameofday
                       (ws-date-dayofweek-aux + cte-01)
                       "]."
               DISPLAY "Correct Date: " ws-date-output "."
           ELSE
               DISPLAY "Incorrect day of week calculation."
           END-IF.

       END PROGRAM Val_Date.
