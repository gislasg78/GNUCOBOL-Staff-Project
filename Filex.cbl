       IDENTIFICATION DIVISION.
       PROGRAM-ID. Filex.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            SYMBOLIC CHARACTERS asterisk IS 43
            CLASS alphabetic-and-numeric IS X'20'
                                            X'30' THRU X'39'
                                            X'41' THRU X'5A'
                                            X'61' THRU X'7A'.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT OPTIONAL myFilex ASSIGN TO ws-f-name-myFilex
                   ORGANIZATION IS LINE SEQUENTIAL
                   PADDING CHARACTER IS asterisk
                   FILE  STATUS IS fs-myFilex.
            
       DATA DIVISION.
       FILE SECTION.
       FD  myFilex
           BLOCK CONTAINS  05 TO 10 RECORDS
           DATA RECORD     IS f-rec-myFilex
           LABEL RECORD    IS OMITTED
           RECORD CONTAINS 20 CHARACTERS
           RECORDING MODE  IS F

           LINAGE IS ws-linage-totlines LINES
             WITH FOOTING AT ws-linage-footing
            LINES AT TOP     ws-linage-top
            LINES AT BOTTOM  ws-linage-bottom.

       01  f-rec-myFilex                   PIC X(20)  VALUE SPACES.
           88  sw-f-rec-myFilex-empty                 VALUE SPACES.
       
       WORKING-STORAGE SECTION.
       77  fs-myFilex                      PIC 9(02)  VALUE ZEROES.

       01  ws-environmental-variables.
           03  ws-fixed-working-constants.
               05  ws-answer	           VALUE SPACE PIC A(01).
                   88  sw-exit-answer      VALUES ARE 'N', 'n'.
               05  ws-cnt-ins-rows         PIC S9(03) VALUE ZEROES
                                           SIGN  IS LEADING
                                           SEPARATE CHARACTER.
               05  ws-cte-01               PIC 9(01)  VALUE 1.

           03  ws-linage-work-variables.
               05  ws-linage-bottom        PIC 9(02)  VALUE 01.
               05  ws-linage-footing       PIC 9(02)  VALUE 09.
               05  ws-linage-top           PIC 9(02)  VALUE 01.
               05  ws-linage-totlines      PIC 9(02)  VALUE 10.

           03  ws-myFilex-vars.
               05  ws-f-name-myFilex       PIC X(12)  VALUE SPACES.
               05  ws-f-rec-myFilex        PIC X(20)  VALUE SPACES.
       
       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON myFilex.
       000000-status-check.
           DISPLAY "+---+----+---+----+---+----+---"
           DISPLAY "| Archive Status Information. |"
           DISPLAY "+---+----+---+----+---+----+---"
           DISPLAY "| File name  : [" ws-f-name-myFilex "]."
           DISPLAY "| Error code : [" fs-myFilex "]."
           DISPLAY "+---+----+---+----+---+----+---"
           STOP "Press the ENTER key to continue...".
       END DECLARATIVES.

       MAIN-PARAGRAPH.
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "|     Sequential Files.    |"
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY asterisk " File name to open: " WITH NO ADVANCING
           ACCEPT ws-f-name-myFilex

           DISPLAY "Sequential Organizing File Creator and Generator."
           OPEN EXTEND myFilex
           DISPLAY "Opening file. Status Code: [" fs-myFilex "].".

           PERFORM 100000-begin-save-master-record
              THRU 100000-end-save-master-record
             UNTIL sw-exit-answer
                OR fs-myFilex IS NOT EQUAL TO ZEROES

           CLOSE myFilex
           DISPLAY "Closing of archive. Status Code: [" fs-myFilex "].".

           DISPLAY "Rows inserted into output file: " ws-cnt-ins-rows
           STOP RUN.

       100000-begin-save-master-record.
           DISPLAY "Enter a text line to be recorded: "
             WITH NO ADVANCING
           ACCEPT ws-f-rec-myFilex

           IF ws-f-rec-myFilex IS alphabetic-and-numeric
              DISPLAY "Validated content!"

              PERFORM 110000-begin-keep-a-record
                 THRU 110000-end-keep-a-record
           ELSE
              DISPLAY "Operation not performed. File unchanged."
              DISPLAY "Input line contains other invalid characters."
           END-IF

           DISPLAY "[-> " ws-f-rec-myfilex " <-]"

           DISPLAY "Do you want to capture more lines of text "
                   "(y/n)? : " WITH NO ADVANCING
           ACCEPT ws-answer.
       100000-end-save-master-record.
           EXIT.


        110000-begin-keep-a-record.
           DISPLAY "Recording log in progress..."

           WRITE f-rec-myFilex            FROM ws-f-rec-myFilex
              AT END-OF-PAGE
                 PERFORM 111000-begin-add-page-break
                    THRU 111000-end-add-page-break

             NOT AT EOP
                 ADD ws-cte-01              TO ws-cnt-ins-rows
                 DISPLAY asterisk
                         " Inserted line: [" LINAGE-COUNTER "]. "
                         asterisk

           END-WRITE

           DISPLAY "Record Recording. Status Code: [" fs-myFilex "].".
        110000-end-keep-a-record.
           EXIT.

         111000-begin-add-page-break.
           DISPLAY asterisk
           DISPLAY asterisk
                   "Line break has occurred on line: ["
                   LINAGE-COUNTER "]."
                   asterisk
           DISPLAY asterisk

           MOVE SPACES                TO ws-f-rec-myFilex
           SET sw-f-rec-myFilex-empty TO TRUE

           WRITE f-rec-myFilex      FROM ws-f-rec-myFilex
                 AFTER ADVANCING PAGE
           END-WRITE.
         111000-end-add-page-break.
           EXIT.

       END PROGRAM Filex.
