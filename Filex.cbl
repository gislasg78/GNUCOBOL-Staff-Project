       IDENTIFICATION DIVISION.
       PROGRAM-ID. Filex.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            CLASS alphabetic-and-numeric IS X'20'
                                            X'2E'
                                            X'30' THRU X'39'
                                            X'41' THRU X'5A'
                                            X'61' THRU X'7A'.
            SYMBOLIC CHARACTERS asterisk IS 43.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT OPTIONAL myFilex ASSIGN TO ws-f-name-myFilex
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS  IS fs-myFilex.
            
       DATA DIVISION.
       FILE SECTION.
       FD  myFilex
           BLOCK CONTAINS  05 TO 10 RECORDS
           RECORD IS VARYING   IN SIZE FROM 02 TO 31 CHARACTERS
                     DEPENDING ON ws-f-rec-myFilex-size
           RECORDING MODE  IS V

           LINAGE IS ws-linage-totlines LINES
             WITH FOOTING AT ws-linage-footing
            LINES AT TOP     ws-linage-top
            LINES AT BOTTOM  ws-linage-bottom.

       01  f-rec-myFilex.
           88  sw-f-rec-myFilex-empty                 VALUE SPACES.
           03  f-rec-myFilex-size          PIC 9(02)  VALUE ZEROES.
           03  f-rec-myFilex-content       PIC X(29)  VALUE SPACES.
       
       WORKING-STORAGE SECTION.
       77  fs-myFilex                      PIC X(02)  VALUE SPACES.

       01  ws-environmental-variables.
           03  ws-fixed-working-constants.
               05  ws-answer	           VALUE SPACE PIC A(01).
                   88  sw-exit-answer      VALUES ARE 'N', 'n'.
               05  ws-cnt-ins-rows         PIC S9(03) VALUE ZEROES
                                           SIGN  IS LEADING
                                           SEPARATE CHARACTER.
               05  ws-cte-01               PIC 9(01)  VALUE 1.
               05  ws-cte-two-spaces       PIC X(02)  VALUE ALL X'20'.

           03  ws-linage-work-variables.
               05  ws-linage-bottom        PIC 9(02)  VALUE 01.
               05  ws-linage-footing       PIC 9(02)  VALUE 09.
               05  ws-linage-top           PIC 9(02)  VALUE 01.
               05  ws-linage-totlines      PIC 9(02)  VALUE 10.

           03  ws-myFilex-vars.
               05  ws-f-name-myFilex       PIC X(12)  VALUE SPACES.
               05  ws-f-rec-myFilex.
                   10  ws-f-rec-myFilex-size          PIC 9(02)
                                                      VALUE ZEROES.
                   10  ws-f-rec-myFilex-content       PIC X(29)
                                                      VALUE SPACES.
       
       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON myFilex.
       000000-status-check.
           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+---"
           DISPLAY "| Archive Status Information. |"
           DISPLAY "+---+----+---+----+---+----+---"
           DISPLAY "| + File name  : [" ws-f-name-myFilex "]."
           DISPLAY "| + Error code : [" fs-myFilex "]."
           DISPLAY "+---+----+---+----+---+----+---"
           DISPLAY "Press the ENTER key to continue..."
           ACCEPT OMITTED.
       END DECLARATIVES.

       MAIN-PARAGRAPH.
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "|     Sequential Files.    |"
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY asterisk " File name to open: " WITH NO ADVANCING
           ACCEPT ws-f-name-myFilex

           DISPLAY "Sequential Organizing File Creator and Generator."
           OPEN EXTEND myFilex
           DISPLAY "Opening. Status Code: [" fs-myFilex "].".

           PERFORM 100000-begin-save-master-record
              THRU 100000-end-save-master-record
             UNTIL fs-myFilex IS NOT EQUAL TO ZEROES
                OR sw-exit-answer

           CLOSE myFilex
           DISPLAY "Closing. Status Code: [" fs-myFilex "].".

           DISPLAY "Rows inserted into output file: " ws-cnt-ins-rows
           STOP RUN.

       100000-begin-save-master-record.
           INITIALIZE f-rec-myFilex
                      ws-f-rec-myFilex

           DISPLAY SPACE
           DISPLAY "Enter a text line to be recorded: "
              WITH NO ADVANCING
           ACCEPT ws-f-rec-myFilex-content

           IF ws-f-rec-myFilex-content IS alphabetic-and-numeric
              DISPLAY SPACE
              DISPLAY "Validated content!"

              INSPECT  ws-f-rec-myFilex-content
              TALLYING ws-f-rec-myFilex-size
                   FOR CHARACTERS BEFORE INITIAL ws-cte-two-spaces

              ADD LENGTH OF ws-f-rec-myFilex-size
               TO ws-f-rec-myFilex-size

              PERFORM 110000-begin-keep-a-record
                 THRU 110000-end-keep-a-record
           ELSE
              DISPLAY SPACE
              DISPLAY "Operation not performed. File unchanged!"
              DISPLAY "Input line contains other invalid characters!"
           END-IF

           DISPLAY SPACE
           DISPLAY "Do you want to capture more lines of text (y/n)? : "
              WITH NO ADVANCING
           ACCEPT ws-answer.
       100000-end-save-master-record.
           EXIT.

        110000-begin-keep-a-record.
           DISPLAY asterisk "Recording log in progress." asterisk
           DISPLAY X'5B'
                   ws-f-rec-myFilex-size
                   X'5D' X'20'
                   X'5B' X'2D' X'3E' X'20'
                   ws-f-rec-myfilex-content
                   X'20' X'3C' X'2D' X'5D'

           WRITE f-rec-myFilex            FROM ws-f-rec-myFilex
              AT END-OF-PAGE
                 PERFORM 111000-begin-add-page-break
                    THRU 111000-end-add-page-break

             NOT AT EOP
                 ADD ws-cte-01              TO ws-cnt-ins-rows
                 DISPLAY asterisk
                         "Inserted line: [" LINAGE-COUNTER "]."
                         asterisk

           END-WRITE

           DISPLAY "Writing. Status Code: [" fs-myFilex "].".
        110000-end-keep-a-record.
           EXIT.

         111000-begin-add-page-break.
           DISPLAY asterisk
                   "Line break has occurred on line: "
                   "[" LINAGE-COUNTER "]."
                   asterisk

           MOVE SPACES                TO ws-f-rec-myFilex
           SET sw-f-rec-myFilex-empty TO TRUE

           WRITE f-rec-myFilex      FROM ws-f-rec-myFilex
                 AFTER ADVANCING PAGE
           END-WRITE

           DISPLAY "Writing. Status Code: [" fs-myFilex "].".
         111000-end-add-page-break.
           EXIT.

       END PROGRAM Filex.
