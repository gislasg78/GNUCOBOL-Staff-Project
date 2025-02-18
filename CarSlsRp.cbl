       IDENTIFICATION DIVISION.
       PROGRAM-ID. CarSlsRp.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL carsalesfile ASSIGN TO ws-name-carsalesfile
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS fs-carsalesfile.

       DATA DIVISION.
       FILE SECTION.
       FD  carsalesfile.
       01  salesdetails.
           88  endofsalesfile  VALUE HIGH-VALUES.
           03  salespersonname.
               05  lastname          PIC X(15).
               05  firstname         PIC X(10).
           03  quarterlysales.
               05  q1-sales	     PIC 9(05)V9(02).
               05  q2-sales	     PIC 9(05)V9(02).
               05  q3-sales	     PIC 9(05)V9(02).
               05  q4-sales	     PIC 9(05)V9(02).
           03  cartotal              PIC 9(07)V9(02).

       WORKING-STORAGE SECTION.
       77  fs-carsalesfile           PIC 9(02)         VALUE ZEROES.
       77  ws-name-carsalesfile      PIC X(12)         VALUE SPACES.

       78  cte-01                    VALUE 01.

       01  ws-car-sales.
           05  ws-average-estimated  PIC 9(12)V9(04)   VALUE ZEROES.
           05  ws-salesrecords-read  PIC 9(06)         VALUE ZEROES.
           05  ws-salesperson-yearly PIC 9(08)V9(04)   VALUE ZEROES.
           05  ws-yearly-sales       PIC 9(12)V9(04)   VALUE ZEROES.

       01  ws-today.
           05  ws-today-now          PIC 9(08)         VALUE ZEROES.
           05  ws-today-now-fmt      PIC 9999/99/99    VALUE ZEROES.

       01  ws-quarterlysales.
           05  ws-q1-sales           PIC 9(05)V9(02)   VALUE ZEROES.
           05  ws-q2-sales	     PIC 9(05)V9(02)   VALUE ZEROES.
           05  ws-q3-sales	     PIC 9(05)V9(02)   VALUE ZEROES.
           05  ws-q4-sales	     PIC 9(05)V9(02)   VALUE ZEROES.

       01  heading-line.
           05  FILLER	             PIC X(16) VALUE 'Salesperson name'.
           05  FILLER	             PIC X(20)         VALUE SPACES.
           05  FILLER	             PIC X(11) VALUE 'Qtr 1 Sales'.
           05  FILLER	             PIC X(02)         VALUE SPACES.
           05  FILLER	             PIC X(11) VALUE 'Qtr 2 Sales'.
           05  FILLER	             PIC X(02)         VALUE SPACES.
           05  FILLER	             PIC X(11) VALUE 'Qtr 3 Sales'.
           05  FILLER	             PIC X(02)         VALUE SPACES.
           05  FILLER	             PIC X(11) VALUE 'Qtr 4 Sales'.
           05  FILLER	             PIC X(03)         VALUE SPACES.
           05  FILLER 	             PIC X(12) VALUE 'Yearly Sales'.
           05  FILLER	             PIC X(02)         VALUE SPACES.

       01  detail-line.
           05  FILLER                PIC X(05)         VALUE SPACES.
           05  det-fname             PIC X(10)         VALUE SPACES.
           05  FILLER                PIC X(05)         VALUE SPACES.
           05  det-lname             PIC X(15)         VALUE SPACES.
           05  FILLER                PIC X(02)         VALUE SPACES.
           05  det-q1-sales          PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  det-q2-sales          PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  det-q3-sales          PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  det-q4-sales          PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  det-yearlysales       PIC $,$$$,$$9.99  VALUE ZEROES.
           05  FILLER                PIC X(19)         VALUE SPACES.

       01  detail-total-line.
           05  FILLER                PIC X(20)         VALUE SPACES.
           05  FILLER                PIC X(15)   VALUE "Totals: ".
           05  FILLER                PIC X(02)         VALUE SPACES.
           05  det-q1-tot-sales      PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  det-q2-tot-sales      PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  det-q3-tot-sales      PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  det-q4-tot-sales      PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(02)         VALUE SPACES.
           05  det-tot-yearlysales   PIC $$,$$$,$$9.99 VALUE ZEROES.
           05  FILLER                PIC X(19)         VALUE SPACES.						

       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON carsalesfile.

       status-check.
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "| File Status Information. |"
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "| + Name of File : [" ws-name-carsalesfile "]."
           DISPLAY "| + Status Code  : [" fs-carsalesfile "]."
           DISPLAY "+---+----+---+----+---+----+"
           STOP "An exception has occurred. Press ENTER to continue...".
       END DECLARATIVES.

       MAIN-PARAGRAPH.
           PERFORM 100-begin-start-program
              THRU 100-end-start-program

           PERFORM 200-begin-process-sales-records
              THRU 200-end-process-sales-records
             UNTIL endofsalesfile
                OR fs-carsalesfile IS NOT EQUAL TO ZEROES

           PERFORM 300-begin-finish-program
              THRU 300-end-finish-program

           STOP RUN.

       100-begin-start-program.
           DISPLAY "Sales Report Generator."
           DISPLAY "Enter the name of the input file: "
              WITH NO ADVANCING
            ACCEPT ws-name-carsalesfile

            ACCEPT  ws-today-now FROM DATE YYYYMMDD
              MOVE  ws-today-now   TO ws-today-now-fmt

           OPEN INPUT carsalesfile

           DISPLAY SPACE
           DISPLAY "Today: [" ws-today-now-fmt "]."
           DISPLAY "Opening. Status Code: [" fs-carsalesfile "]."

           DISPLAY SPACE
           DISPLAY heading-line

           DISPLAY SPACE.
       100-end-start-program.
           EXIT.

       200-begin-process-sales-records.
           READ carsalesfile RECORD
             AT END
                SET endofsalesfile    TO TRUE
                PERFORM 220-BEGIN-SHOW-CONSOLIDATED-TOTAL-LINES
                   THRU 220-END-SHOW-CONSOLIDATED-TOTAL-LINES

            NOT AT END
                PERFORM 210-BEGIN-GENERATE-SALES-detail-line
                   THRU 210-END-GENERATE-SALES-detail-line

           END-READ.
       200-end-process-sales-records.
           EXIT.

        210-begin-generate-sales-detail-line.
           ADD  cte-01                TO ws-salesrecords-read

           MOVE firstname             TO det-fname
           MOVE lastname              TO det-lname
           MOVE q1-sales              TO det-q1-sales
           MOVE q2-sales              TO det-q2-sales
           MOVE q3-sales              TO det-q3-sales
           MOVE q4-sales              TO det-q4-sales

           ADD  q1-sales              TO ws-q1-sales
           ADD  q2-sales              TO ws-q2-sales 
           ADD  q3-sales              TO ws-q3-sales 
           ADD  q4-sales              TO ws-q4-sales 

           ADD  q1-sales  q2-sales  q3-sales  q4-sales
           GIVING det-yearlysales
                  ws-salesperson-yearly

           ADD  ws-salesperson-yearly TO ws-yearly-sales

           DISPLAY detail-line.
        210-end-generate-sales-detail-line.
           EXIT.

        220-begin-show-consolidated-total-lines.
           MOVE ws-salesperson-yearly TO cartotal

           MOVE ws-q1-sales           TO det-q1-tot-sales
           MOVE ws-q2-sales           TO det-q2-tot-sales
           MOVE ws-q3-sales           TO det-q3-tot-sales
           MOVE ws-q4-sales           TO det-q4-tot-sales
           MOVE ws-yearly-sales       TO det-tot-yearlysales

           DISPLAY SPACE
           DISPLAY detail-total-line.
        220-end-show-consolidated-total-lines.
           EXIT.

       300-begin-finish-program.
           DIVIDE ws-salesrecords-read      INTO ws-yearly-sales
           GIVING ws-average-estimated   ROUNDED
               ON SIZE ERROR
                  DISPLAY SPACE
                  DISPLAY "The average sales cannot be determined."
                  DISPLAY "The calculation could not be made."
           END-DIVIDE

           DISPLAY SPACE
           DISPLAY "Summary"
           DISPLAY "Average estimated sales : [" ws-average-estimated"]"
           DISPLAY "Recovered sales records : [" ws-salesrecords-read"]"
           DISPLAY "Total sales calculated  : [" ws-yearly-sales "]"

           CLOSE carsalesfile

           DISPLAY SPACE
           DISPLAY "Closing. Status Code: [" fs-carsalesfile "].".
       300-end-finish-program.
           EXIT.

       END PROGRAM CarSlsRp.
