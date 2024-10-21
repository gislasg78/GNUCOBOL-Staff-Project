       IDENTIFICATION DIVISION.
       PROGRAM-ID. filex2.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS printable-ASCII-characters IS X'20' THRU X'7F'
           SYMBOLIC CHARACTERS     asterisk IS 43.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL f-filex2 ASSIGN TO ws-f-filex2
                              ORGANIZATION IS LINE SEQUENTIAL
                         PADDING CHARACTER IS asterisk
                               FILE STATUS IS fs-f-filex2.

       DATA DIVISION.
       FILE SECTION.
       FD  f-filex2
           BLOCK CONTAINS 05 TO 10 RECORDS
           DATA  RECORD   IS f-filex2-rec
           LABEL RECORD   IS STANDARD
           RECORD IS VARYING IN SIZE FROM 02   TO 80 CHARACTERS
                   DEPENDING ON ws-f-filex2-r-size.

       01  f-filex2-rec.
           03  f-filex2-r-size       PIC 9(02) VALUE ZEROES.
           03  f-filex2-r-content    PIC X(78) VALUE SPACES.
           03  f-filex2-r-cont-xcar  REDEFINES f-filex2-r-content
                                     OCCURS 78 TIMES
                                     INDEXED BY idx-f-filex2-r-cont-xcar
                                     PIC X(01).

       LOCAL-STORAGE SECTION.
       78  cte-one                             VALUE 01.
       78  cte-two-spaces                      VALUE "  ".

       77  fs-f-filex2               PIC 9(02) VALUE ZEROES.
       77  ws-f-filex2               PIC X(12) VALUE SPACES.
       77  ws-attempt-counter        PIC 9(04) VALUE ZEROES.
       77  ws-records-counter        PIC 9(04) VALUE ZEROES.

       01  ws-continue-capture       PIC A(01) VALUE SPACE.
           88  sw-no-continue-capt   VALUES ARE 'N' 'n'.

       01  ws-f-filex2-rec.
           03  ws-f-filex2-r-size    PIC 9(02) VALUE ZEROES.
           03  ws-f-filex2-r-content PIC X(78) VALUE SPACES.
           03  ws-f-filex2-r-cont-xcar
               REDEFINES ws-f-filex2-r-content OCCURS 78 TIMES
               INDEXED BY idx-ws-f-filex2-r-cont-xcar PIC X(01).

       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON f-filex2.
       Status-Check.
           DISPLAY SPACE
           DISPLAY "Error in file    : [" ws-f-filex2 "]."
           DISPLAY "File status code : [" fs-f-filex2 "]."
           STOP "Press Enter to continue...".
       END DECLARATIVES.

       MAIN-PARAGRAPH.
           DISPLAY "Processing sequential text files online."
           DISPLAY "Enter the file name: " WITH NO ADVANCING
           ACCEPT ws-f-filex2

           IF ws-f-filex2 IS printable-ASCII-characters
              OPEN EXTEND f-filex2

              PERFORM 100-process-capture
                 THRU 100-end-process-capture
              VARYING ws-attempt-counter
                 FROM cte-one BY cte-one
                UNTIL fs-f-filex2 IS NOT EQUAL TO ZEROES
                    OR sw-no-continue-capt

              CLOSE f-filex2

              DISPLAY SPACE
              DISPLAY "Final results."
              DISPLAY "Attempts to record the logs   : ["
                       ws-attempt-counter "]."
              DISPLAY "Records successfully recorded : ["
                       ws-records-counter "]."
          END-IF

          STOP RUN.

       100-process-capture.
           INITIALIZE f-filex2-rec
                      ws-f-filex2-rec

           DISPLAY SPACE
           DISPLAY "Enter a line of text to be recorded: " NO ADVANCING
           ACCEPT ws-f-filex2-r-content

           INSPECT ws-f-filex2-r-content TALLYING ws-f-filex2-r-size
               FOR CHARACTERS      BEFORE INITIAL cte-two-spaces

           DISPLAY SPACE
           DISPLAY "Recording information."
           DISPLAY "String length  : [" ws-f-filex2-r-size "]"

           IF ws-f-filex2-r-content IS printable-ASCII-characters
              PERFORM 110-save-record
                 THRU 110-end-save-record
           ELSE
              DISPLAY SPACE
              DISPLAY "Warning! The line to be written contains invalid"
                      " characters. It was not recorded!"
           END-IF

           DISPLAY SPACE
           DISPLAY "Do you want to continue entering more lines? (y/n) "
                   ": " WITH NO ADVANCING
           ACCEPT ws-continue-capture.
       100-end-process-capture.
           EXIT.

        110-save-record.
           ADD  cte-one                TO ws-records-counter

           DISPLAY SPACE
           DISPLAY "Character-by-character breakdown of the string."

           PERFORM VARYING idx-ws-f-filex2-r-cont-xcar
                      FROM cte-one     BY cte-one
                     UNTIL idx-ws-f-filex2-r-cont-xcar
                        IS GREATER THAN ws-f-filex2-r-size

                           DISPLAY "[" ws-f-filex2-r-cont-xcar
                                      (idx-ws-f-filex2-r-cont-xcar)
                                   "]" WITH NO ADVANCING
           END-PERFORM

           MOVE ws-f-filex2-r-size     TO f-filex2-r-size
           SET  ws-f-filex2-r-size     TO idx-ws-f-filex2-r-cont-xcar
           ADD  cte-one                TO ws-f-filex2-r-size
           MOVE ws-f-filex2-r-content  TO f-filex2-r-content
           WRITE f-filex2-rec        FROM ws-f-filex2-rec

           DISPLAY SPACE

           IF fs-f-filex2     IS EQUAL TO ZEROES
              DISPLAY "The record line has been recorded successfully!"
           ELSE
              DISPLAY "An error has occurred in log write operation!"
           END-IF.
        110-end-save-record.
           EXIT.

       END PROGRAM filex2.
