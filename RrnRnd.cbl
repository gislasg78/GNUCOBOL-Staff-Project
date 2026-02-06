       IDENTIFICATION DIVISION.
       PROGRAM-ID. RrnRnd.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           NUMERIC SIGN IS TRAILING SEPARATE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL RrnFile ASSIGN TO ws-name-RrnFile
                  ORGANIZATION IS RELATIVE
                  ACCESS MODE  IS RANDOM
                  RELATIVE KEY IS ws-key-RrnFile
                  FILE STATUS  IS fs-RrnFile.

       DATA DIVISION.
       FILE SECTION.
       FD  RrnFile
           RECORD CONTAINS 15 CHARACTERS.

       01  RrnFile-rec.
           03  RrnFile-rec-code-employee   PIC 9(06)       VALUE ZEROES.
           03  RrnFile-rec-salary-employee PIC S9(06)V9(02) 
                                           SIGN IS LEADING
                                           SEPARATE CHARACTER
                                                           VALUE ZEROES.

       WORKING-STORAGE SECTION.
       77  fs-RrnFile                     PIC X(02)        VALUE SPACES.

       77  ws-continue-response           PIC A(01)        VALUE SPACE.
           88  sw-continue-response-N     VALUES ARE 'N' 'n'.

       77  ws-key-RrnFile                 PIC 9(06)        VALUE ZEROES.
       77  ws-name-RrnFile                PIC X(12)        VALUE SPACES.

       01  ws-RrnFile-rec.
           03  ws-RrnFile-rec-code-employee                PIC 9(06)
                                                           VALUE ZEROES.
           03  ws-RrnFile-rec-salary-employee          PIC S9(06)V9(02)
                                                      SIGN IS LEADING
                                                  SEPARATE CHARACTER
                                                     VALUE ZEROES.

       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON RrnFile.
       Status-Check.
           DISPLAY SPACE
           DISPLAY "File status information."
           DISPLAY "+ File   Name: [" ws-name-RrnFile "]."
           DISPLAY "+ Status Code: [" fs-RrnFile "].".
       END DECLARATIVES.

       MAIN-PARAGRAPH.
           DISPLAY "Basic maintenance of a relative file."
           DISPLAY "Enter the file name: " WITH NO ADVANCING
           ACCEPT ws-name-RrnFile

           OPEN I-O RrnFile
           DISPLAY "Opening. Status Code: [" fs-RrnFile "]."

           MOVE SPACE TO ws-continue-response
           PERFORM UNTIL sw-continue-response-N
                      OR fs-RrnFile IS NOT EQUAL TO ZEROES
                   INITIALIZE RrnFile-rec
                              ws-RrnFile-rec
                              ws-key-RrnFile

                   DISPLAY SPACE
                   DISPLAY "Employee data capture."
                   DISPLAY "Record Number # : "
                      WITH NO ADVANCING
                    ACCEPT ws-key-RrnFile

                   DISPLAY "Employee code   : " WITH NO ADVANCING
                    ACCEPT ws-RrnFile-rec-code-employee
                      MOVE ws-RrnFile-rec-code-employee 
                        TO RrnFile-rec-code-employee

                   DISPLAY "Salary Employee : " WITH NO ADVANCING
                    ACCEPT ws-RrnFile-rec-salary-employee
                      MOVE ws-RrnFile-rec-salary-employee
                        TO RrnFile-rec-salary-employee

                   DISPLAY SPACE
                   WRITE RrnFile-rec        FROM ws-RrnFile-rec
                         INVALID KEY
                         DISPLAY "Invalid Key!"

                     NOT INVALID KEY
                         DISPLAY "Record saved successfully."
                         DISPLAY "Record Number # : [" ws-key-RrnFile
                                 "]."
                         DISPLAY "Employee code   : [" 
                                 ws-RrnFile-rec-code-employee "] = ["
                                 RrnFile-rec-code-employee "]."
                         DISPLAY "Salary Employee : ["
                                 ws-RrnFile-rec-salary-employee "] = ["
                                 RrnFile-rec-salary-employee "]"

                   END-WRITE
                   DISPLAY "Writing. Status Code: [" fs-RrnFile "]."

                   DISPLAY SPACE
                   DISPLAY "Do you want to capture more records? (y/n) "
                           ": " WITH NO ADVANCING
                   ACCEPT ws-continue-response
           END-PERFORM

           CLOSE RrnFile
           DISPLAY "Closing. Status Code: [" fs-RrnFile "]."

           DISPLAY SPACE
           DISPLAY "Reading sequential file."

           OPEN INPUT RrnFile
           DISPLAY "Opening. Status Code: [" fs-RrnFile "]."

           MOVE SPACE TO ws-continue-response
           PERFORM UNTIL fs-RrnFile IS NOT EQUAL TO ZEROES
                      OR sw-continue-response-N
                   INITIALIZE RrnFile-rec
                              ws-RrnFile-rec
                              ws-key-RrnFile

                   DISPLAY SPACE
                   DISPLAY "Record number to locate: "
                      WITH NO ADVANCING
                     ACCEPT ws-key-RrnFile

                   DISPLAY SPACE
                   READ RrnFile RECORD         INTO ws-RrnFile-rec
                    KEY IS ws-key-RrnFile
                        INVALID KEY
                        DISPLAY "Record Number: [" ws-key-RrnFile "]"
                                " was not found."

                    NOT INVALID KEY
                           DISPLAY "Record Number # : [" ws-key-RrnFile
                                   "]."
                           DISPLAY "Employee code   : [" 
                                   ws-RrnFile-rec-code-employee
                                   "] = ["
                                   RrnFile-rec-code-employee
                                   "]."
                           DISPLAY "Salary Employee : ["
                                   ws-RrnFile-rec-salary-employee
                                   "] = ["
                                   RrnFile-rec-salary-employee
                                   "]."
                   END-READ
                   DISPLAY "Reading. Status Code: [" fs-RrnFile "]."

                   DISPLAY SPACE
                   DISPLAY "Do you want to retrieve more records? (y/n)"
                           " : " WITH NO ADVANCING
                   ACCEPT ws-continue-response
           END-PERFORM

           CLOSE RrnFile
           DISPLAY "Closing. Status Code: [" fs-RrnFile "]."

           STOP RUN.

       END PROGRAM RrnRnd.
