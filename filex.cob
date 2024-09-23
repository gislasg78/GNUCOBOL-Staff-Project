       IDENTIFICATION DIVISION.
       PROGRAM-ID. filex.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            SYMBOLIC asterisk 43
            CLASS alphabetic-and-numeric IS SPACE,
					'0' THRU '9',
					'A' THRU 'Z',
					'a' THRU 'z'.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT OPTIONAL myfilex ASSIGN TO ws-f-name-myfilex
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE  STATUS IS fs-myfilex.
            
       DATA DIVISION.
       FILE SECTION.
       FD  myfilex
           DATA RECORD     IS f-rec-myfilex
           RECORD CONTAINS 20 CHARACTERS

           LINAGE IS 25 LINES
             WITH FOOTING AT 21
            LINES AT TOP 2
            LINES AT BOTTOM 3.

       01  f-rec-myfilex            PIC X(20)  VALUE SPACES.
           88  sw-f-rec-myfilex-empty          VALUE SPACES.
       
       WORKING-STORAGE SECTION.
       77  fs-myfilex               PIC 9(02)  VALUE ZEROES.
       77  ws-linage-cnt-ed         PIC +9(08) VALUE ZEROES.

       01  ws-cnt-ins-rows          PIC S9(03) VALUE ZEROES
                                    SIGN LEADING SEPARATE.
       01  ws-cte-01                PIC 9(01)  VALUE 01.

       01  ws-answer	            VALUE SPACE PIC A(01).
           88  sw-exit-answer       VALUES ARE 'N', 'n'.

       01  ws-f-name-myfilex        PIC X(12)  VALUE SPACES.
       01  ws-f-rec-myfilex         PIC X(80)  VALUE SPACES.
       
       PROCEDURE DIVISION.
       DECLARATIVES.
       FL-ERROR-HANDLER SECTION.
           USE AFTER ERROR PROCEDURE ON myfilex.
       STATUS-CHECK.
           DISPLAY "File name  : " ws-f-name-myfilex
           DISPLAY "Error code : " fs-myfilex.
       END DECLARATIVES.

       MAIN-PARAGRAPH.
           DISPLAY SPACE UPON CRT
           DISPLAY "Sequential Organizing File Creator and Generator."
                AT LINE 1 COLUMN 15 UPON CRT WITH BLANK SCREEN
           DISPLAY "File name to open: " AT 0203 UPON CRT
              WITH BLANK LINE
           ACCEPT ws-f-name-myfilex AT LINE 02 COL 22 FROM CRT
              WITH PROMPT

           OPEN EXTEND myfilex

           PERFORM WITH TEST AFTER VARYING ws-cnt-ins-rows
              FROM ws-cte-01 BY ws-cte-01 UNTIL sw-exit-answer
                   DISPLAY "Enter a text line to be recorded: "
                        AT LINE NUMBER 04 COLUMN NUMBER 03 UPON CRT
                      WITH BLANK LINE
                    ACCEPT ws-f-rec-myfilex
                        AT LINE NUMBER 04 COLUMN NUMBER 37 FROM CRT
                      WITH PROMPT UPDATE

                    IF ws-f-rec-myfilex IS alphabetic-and-numeric
                       CONTINUE
                    ELSE
                       DISPLAY "Input line contains other characters: "
                            AT LINE 06 COLUMN 03 UPON CRT
                               ws-f-rec-myfilex
                            AT LINE 06 COLUMN 41 UPON CRT
                    END-IF

                    WRITE f-rec-myfilex  FROM ws-f-rec-myfilex
                       AT END-OF-PAGE
                          MOVE LINAGE-COUNTER TO ws-linage-cnt-ed
                          DISPLAY "Line break has occurred on line: "
                               AT LINE 08 COLUMN 03 UPON CRT
                                  ws-linage-cnt-ed
                               AT LINE 08 COL 36 UPON CRT
                             WITH REVERSE-VIDEO

                          SET sw-f-rec-myfilex-empty TO TRUE
                          WRITE f-rec-myfilex AFTER ADVANCING PAGE
                          DISPLAY asterisk AT LINE 24 COL 40 UPON CRT

                     NOT AT EOP
                          MOVE LINAGE-COUNTER TO ws-linage-cnt-ed
                          DISPLAY "Inserted line: " AT 1003 UPON CRT
                          DISPLAY ws-linage-cnt-ed  AT 1018 UPON CRT
                             WITH REVERSE-VIDEO
                    END-WRITE

                    DISPLAY "Want to capture more lines of text (Y/N)? "
                         AT LINE 12 COL 03 UPON CRT
                     ACCEPT ws-answer AT LINE 12 COLUMN 45 FROM CRT
                       WITH PROMPT UPDATE
           END-PERFORM

           DISPLAY "Rows inserted into output file: " AT 1403 UPON CRT
                   ws-cnt-ins-rows AT 1435 UPON CRT

           CLOSE myfilex

           STOP RUN.

       END PROGRAM filex.
