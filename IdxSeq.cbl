       IDENTIFICATION DIVISION.
       PROGRAM-ID. IdxSeq.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           NUMERIC SIGN IS TRAILING SEPARATE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL IdxFile ASSIGN TO ws-name-IdxFile
                  ORGANIZATION IS INDEXED
                  RECORD   KEY IS f-IdxFile-rec-code-employee
                  ALTERNATE RECORD KEY IS f-IdxFile-rec-salary-employee
                            WITH DUPLICATES
                  FILE STATUS  IS fs-IdxFile.

       DATA DIVISION.
       FILE SECTION.
       FD  IdxFile.

       01  f-IdxFile-rec.
           03  f-IdxFile-rec-code-employee   PIC 9(06)     VALUE ZEROES.
           03  f-IdxFile-rec-salary-employee PIC S9(06)V9(02) 
                                             SIGN IS LEADING
                                             SEPARATE CHARACTER
                                                           VALUE ZEROES.

       WORKING-STORAGE SECTION.
       77  fs-IdxFile                        PIC X(02)     VALUE SPACES.

       77  ws-continue-response              PIC A(01)     VALUE SPACE.
           88  sw-continue-response-N        VALUES ARE 'N' 'n'.

       77  ws-IdxFile-EOF                    PIC A(01)     VALUE SPACE.
           88  sw-IdxFile-EOF-N                            VALUE 'N'.
           88  sw-IdxFile-EOF-Y                            VALUE 'Y'.

       77  ws-name-IdxFile                   PIC X(12)     VALUE SPACES.

       01  ws-f-IdxFile-rec.
           03  ws-f-IdxFile-rec-code-employee              PIC 9(06)
                                                           VALUE ZEROES.
           03  ws-f-IdxFile-rec-salary-employee        PIC S9(06)V9(02)
                                                       SIGN IS LEADING
                                                      SEPARATE CHARACTER 
                                                           VALUE ZEROES.

       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON IdxFile.
       Status-Check.
           DISPLAY SPACE
           DISPLAY "File status information."
           DISPLAY "+ File   Name: [" ws-name-IdxFile "]."
           DISPLAY "+ Status Code: [" fs-IdxFile "]."
           DISPLAY SPACE.
       END DECLARATIVES.

       MAIN-PARAGRAPH.
           DISPLAY "Basic maintenance to an indexed sequential file."
           DISPLAY "Enter the file name: " WITH NO ADVANCING
           ACCEPT ws-name-IdxFile

           OPEN EXTEND IdxFile
           DISPLAY "Opening. Status Code: [" fs-IdxFile "]."

           MOVE SPACE TO ws-continue-response
           PERFORM UNTIL fs-IdxFile IS NOT EQUAL TO ZEROES
                      OR sw-continue-response-N

                   INITIALIZE f-IdxFile-rec
                              ws-f-IdxFile-rec

                   DISPLAY SPACE
                   DISPLAY "Employee data capture."
                   DISPLAY "Employee code   : " WITH NO ADVANCING
                   ACCEPT ws-f-IdxFile-rec-code-employee
                   MOVE ws-f-IdxFile-rec-code-employee
                     TO f-IdxFile-rec-code-employee

                   DISPLAY "Salary Employee : " WITH NO ADVANCING
                   ACCEPT ws-f-IdxFile-rec-salary-employee
                   MOVE ws-f-IdxFile-rec-salary-employee
                     TO f-IdxFile-rec-salary-employee

                   WRITE f-IdxFile-rec        FROM ws-f-IdxFile-rec
                         INVALID KEY
                         DISPLAY "Invalid Key!"

                     NOT INVALID KEY
                         DISPLAY SPACE
                         DISPLAY "Record saved successfully!"
                         DISPLAY "+ Employee code   : [" 
                                 ws-f-IdxFile-rec-code-employee
                                 "] = ["
                                 f-IdxFile-rec-code-employee "]."
                         DISPLAY "+ Salary Employee : ["
                                 ws-f-IdxFile-rec-salary-employee
                                 "] = ["
                                 f-IdxFile-rec-salary-employee "]"

                   END-WRITE
                   DISPLAY "Writing. Status Code: [" fs-IdxFile "]."

                   DISPLAY SPACE
                   DISPLAY "Do you want to capture more records? (y/n) "
                           ": " WITH NO ADVANCING
                   ACCEPT ws-continue-response
           END-PERFORM

           CLOSE IdxFile
           DISPLAY "Closing. Status Code: [" fs-IdxFile "]."



           DISPLAY SPACE
           DISPLAY "Sequential read to an indexed sequential file."

           OPEN INPUT IdxFile
           DISPLAY "Opening. Status Code: [" fs-IdxFile "]."

           IF fs-IdxFile IS EQUAL TO ZEROES
              DISPLAY SPACE
              DISPLAY "Employee code to start: "
                      WITH NO ADVANCING
              ACCEPT ws-f-IdxFile-rec-code-employee
              MOVE ws-f-IdxFile-rec-code-employee
                TO f-IdxFile-rec-code-employee

              START IdxFile
                KEY IS GREATER THAN OR EQUAL TO 
                    f-IdxFile-rec-code-employee
                    INVALID KEY
                    DISPLAY "Employee code: ["
                            ws-f-IdxFile-rec-code-employee
                            "] not located."

                NOT INVALID KEY
                    DISPLAY SPACE
                    DISPLAY "Record positioned successfully!"
                    DISPLAY "+ Employee code: ["
                            ws-f-IdxFile-rec-code-employee
                            "] OK!"

              END-START
              DISPLAY "Starting. Status Code: [" fs-IdxFile "]."
           END-IF

           MOVE SPACE TO ws-continue-response
           PERFORM UNTIL fs-IdxFile IS NOT EQUAL TO ZEROES
                      OR sw-IdxFile-EOF-Y  OR sw-continue-response-N

                    READ IdxFile NEXT RECORD   INTO ws-f-IdxFile-rec
                        AT END
                           SET sw-IdxFile-EOF-Y  TO TRUE
                           DISPLAY "End Of File!"

                    NOT AT END
                           SET sw-IdxFile-EOF-N TO TRUE

                           DISPLAY SPACE
                           DISPLAY "Record retrieved successfully!"
                           DISPLAY "+ Employee code   : [" 
                                   ws-f-IdxFile-rec-code-employee
                                   "] = ["
                                   f-IdxFile-rec-code-employee
                                   "]."
                           DISPLAY "+ Salary Employee : ["
                                   ws-f-IdxFile-rec-salary-employee
                                   "] = ["
                                   f-IdxFile-rec-salary-employee
                                   "]."

                  END-READ
                  DISPLAY "Reading. Status Code: [" fs-IdxFile "]."
 
                  DISPLAY SPACE
                  DISPLAY "Do you want to continue viewing more "
                          "records? (y/n) : " WITH NO ADVANCING
                   ACCEPT ws-continue-response
           END-PERFORM

           CLOSE IdxFile
           DISPLAY "Closing. Status Code: [" fs-IdxFile "]."

           STOP RUN.

       END PROGRAM IdxSeq.
