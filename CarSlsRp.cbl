       IDENTIFICATION DIVISION.
       PROGRAM-ID. CarSlsRp.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL CARSALESFILE ASSIGN TO WS-NAME-CARSALESFILE
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS FS-CARSALESFILE.

       DATA DIVISION.
       FILE SECTION.
       FD  CARSALESFILE.
       01  SALESDETAILS.
           88  ENDOFSALESFILE  VALUE HIGH-VALUES.
           03  SALESPERSONNAME.
               05  LASTNAME          PIC X(15).
               05  FIRSTNAME         PIC X(10).
           03  QUARTERLYSALES.
               05  Q1-SALES	     PIC 9(05)V9(02).
               05  Q2-SALES	     PIC 9(05)V9(02).
               05  Q3-SALES	     PIC 9(05)V9(02).
               05  Q4-SALES	     PIC 9(05)V9(02).
           03  CARTOTAL              PIC 9(07)V9(02).

       WORKING-STORAGE SECTION.
       77  FS-CARSALESFILE           PIC 9(02)         VALUE ZEROES.
       77  WS-NAME-CARSALESFILE      PIC X(12)         VALUE SPACES.

       78  CTE-01                    VALUE 01.

       01  WS-CAR-SALES.
           05  WS-AVERAGE-ESTIMATED  PIC 9(12)V9(04)   VALUE ZEROES.
           05  WS-SALESRECORDS-READ  PIC 9(06)         VALUE ZEROES.
           05  WS-SALESPERSON-YEARLY PIC 9(08)V9(04)   VALUE ZEROES.
           05  WS-YEARLY-SALES       PIC 9(12)V9(04)   VALUE ZEROES.

       01  WS-TODAY.
           05  WS-TODAY-NOW          PIC 9(08)         VALUE ZEROES.
           05  WS-TODAY-NOW-FMT      PIC 9999/99/99    VALUE ZEROES.

       01  WS-QUARTERLYSALES.
           05  WS-Q1-SALES           PIC 9(05)V9(02)   VALUE ZEROES.
           05  WS-Q2-SALES	     PIC 9(05)V9(02)   VALUE ZEROES.
           05  WS-Q3-SALES	     PIC 9(05)V9(02)   VALUE ZEROES.
           05  WS-Q4-SALES	     PIC 9(05)V9(02)   VALUE ZEROES.

       01  HEADING-LINE.
           05  FILLER	             PIC X(16) VALUE 'SALESPERSON NAME'.
           05  FILLER	             PIC X(20)         VALUE SPACES.
           05  FILLER	             PIC X(11) VALUE 'QTR 1 SALES'.
           05  FILLER	             PIC X(02)         VALUE SPACES.
           05  FILLER	             PIC X(11) VALUE 'QTR 2 SALES'.
           05  FILLER	             PIC X(02)         VALUE SPACES.
           05  FILLER	             PIC X(11) VALUE 'QTR 3 SALES'.
           05  FILLER	             PIC X(02)         VALUE SPACES.
           05  FILLER	             PIC X(11) VALUE 'QTR 4 SALES'.
           05  FILLER	             PIC X(03)         VALUE SPACES.
           05  FILLER 	             PIC X(12) VALUE 'YEARLY SALES'.
           05  FILLER	             PIC X(02)         VALUE SPACES.

       01  DETAIL-LINE.
           05  FILLER                PIC X(05)         VALUE SPACES.
           05  DET-FNAME             PIC X(10)         VALUE SPACES.
           05  FILLER                PIC X(05)         VALUE SPACES.
           05  DET-LNAME             PIC X(15)         VALUE SPACES.
           05  FILLER                PIC X(02)         VALUE SPACES.
           05  DET-Q1-SALES          PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  DET-Q2-SALES          PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  DET-Q3-SALES          PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  DET-Q4-SALES          PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  DET-YEARLYSALES       PIC $,$$$,$$9.99  VALUE ZEROES.
           05  FILLER                PIC X(19)         VALUE SPACES.

       01  DETAIL-TOTAL-LINE.
           05  FILLER                PIC X(20)         VALUE SPACES.
           05  FILLER                PIC X(15)   VALUE "TOTALS: ".
           05  FILLER                PIC X(02)         VALUE SPACES.
           05  DET-Q1-TOT-SALES      PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  DET-Q2-TOT-SALES      PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  DET-Q3-TOT-SALES      PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(03)         VALUE SPACES.
           05  DET-Q4-TOT-SALES      PIC $$$,$$9.99    VALUE ZEROES.
           05  FILLER                PIC X(02)         VALUE SPACES.
           05  DET-TOT-YEARLYSALES   PIC $$,$$$,$$9.99 VALUE ZEROES.
           05  FILLER                PIC X(19)         VALUE SPACES.						

       PROCEDURE DIVISION.
       DECLARATIVES.
       FILE-HANDLER SECTION.
           USE AFTER ERROR PROCEDURE ON CARSALESFILE.

       STATUS-CHECK.
           DISPLAY "FILE STATUS INFORMATION."
           DISPLAY "+ NAME OF FILE : [" WS-NAME-CARSALESFILE "]."
           DISPLAY "+ STATUS CODE  : [" FS-CARSALESFILE "]."
           STOP "AN EXCEPTION HAS OCCURRED. PRESS ENTER TO CONTINUE...".
       END DECLARATIVES.

       MAIN-PARAGRAPH.
           PERFORM 100-BEGIN-START-PROGRAM
              THRU 100-END-START-PROGRAM

           PERFORM 200-BEGIN-PROCESS-SALES-RECORDS
              THRU 200-END-PROCESS-SALES-RECORDS
             UNTIL ENDOFSALESFILE
                OR FS-CARSALESFILE IS NOT EQUAL TO ZEROES

           PERFORM 300-BEGIN-FINISH-PROGRAM
              THRU 300-END-FINISH-PROGRAM

           STOP RUN.

       100-BEGIN-START-PROGRAM.
           DISPLAY "SALES REPORT GENERATOR."
           DISPLAY "ENTER THE NAME OF THE INPUT FILE: "
              WITH NO ADVANCING
            ACCEPT WS-NAME-CARSALESFILE

            ACCEPT  WS-TODAY-NOW FROM DATE YYYYMMDD
              MOVE  WS-TODAY-NOW   TO WS-TODAY-NOW-FMT

           OPEN INPUT CARSALESFILE

           DISPLAY SPACE
           DISPLAY "TODAY: [" WS-TODAY-NOW-FMT "]."
           DISPLAY "OPENING. STATUS CODE: [" FS-CARSALESFILE "]."

           DISPLAY SPACE
           DISPLAY HEADING-LINE

           DISPLAY SPACE.
       100-END-START-PROGRAM.
           EXIT.

       200-BEGIN-PROCESS-SALES-RECORDS.
           READ CARSALESFILE RECORD
             AT END
                SET ENDOFSALESFILE    TO TRUE
                PERFORM 220-BEGIN-SHOW-CONSOLIDATED-TOTAL-LINES
                   THRU 220-END-SHOW-CONSOLIDATED-TOTAL-LINES

            NOT AT END
                PERFORM 210-BEGIN-GENERATE-SALES-DETAIL-LINE
                   THRU 210-END-GENERATE-SALES-DETAIL-LINE

           END-READ.
       200-END-PROCESS-SALES-RECORDS.
           EXIT.

        210-BEGIN-GENERATE-SALES-DETAIL-LINE.
           ADD  CTE-01                TO WS-SALESRECORDS-READ

           MOVE FIRSTNAME             TO DET-FNAME
           MOVE LASTNAME              TO DET-LNAME
           MOVE Q1-SALES              TO DET-Q1-SALES
           MOVE Q2-SALES              TO DET-Q2-SALES
           MOVE Q3-SALES              TO DET-Q3-SALES
           MOVE Q4-SALES              TO DET-Q4-SALES

           ADD  Q1-SALES              TO WS-Q1-SALES
           ADD  Q2-SALES              TO WS-Q2-SALES 
           ADD  Q3-SALES              TO WS-Q3-SALES 
           ADD  Q4-SALES              TO WS-Q4-SALES 

           ADD  Q1-SALES  Q2-SALES  Q3-SALES  Q4-SALES
           GIVING DET-YEARLYSALES
                  WS-SALESPERSON-YEARLY

           ADD  WS-SALESPERSON-YEARLY TO WS-YEARLY-SALES

           DISPLAY DETAIL-LINE.
        210-END-GENERATE-SALES-DETAIL-LINE.
           EXIT.

        220-BEGIN-SHOW-CONSOLIDATED-TOTAL-LINES.
           MOVE WS-SALESPERSON-YEARLY TO CARTOTAL

           MOVE WS-Q1-SALES           TO DET-Q1-TOT-SALES
           MOVE WS-Q2-SALES           TO DET-Q2-TOT-SALES
           MOVE WS-Q3-SALES           TO DET-Q3-TOT-SALES
           MOVE WS-Q4-SALES           TO DET-Q4-TOT-SALES
           MOVE WS-YEARLY-SALES       TO DET-TOT-YEARLYSALES

           DISPLAY SPACE
           DISPLAY DETAIL-TOTAL-LINE.
        220-END-SHOW-CONSOLIDATED-TOTAL-LINES.
           EXIT.

       300-BEGIN-FINISH-PROGRAM.
           DIVIDE WS-SALESRECORDS-READ      INTO WS-YEARLY-SALES
           GIVING WS-AVERAGE-ESTIMATED   ROUNDED
               ON SIZE ERROR
                  DISPLAY SPACE
                  DISPLAY "THE AVERAGE SALES CANNOT BE DETERMINED."
                  DISPLAY "THE CALCULATION COULD NOT BE MADE."
           END-DIVIDE

           DISPLAY SPACE
           DISPLAY "SUMMARY"
           DISPLAY "AVERAGE ESTIMATED SALES : [" WS-AVERAGE-ESTIMATED"]"
           DISPLAY "RECOVERED SALES RECORDS : [" WS-SALESRECORDS-READ"]"
           DISPLAY "TOTAL SALES CALCULATED  : [" WS-YEARLY-SALES "]"

           CLOSE CARSALESFILE
           DISPLAY SPACE
           DISPLAY "CLOSING. STATUS CODE: [" FS-CARSALESFILE "].".
       300-END-FINISH-PROGRAM.
           EXIT.

       END PROGRAM CarSlsRp.
