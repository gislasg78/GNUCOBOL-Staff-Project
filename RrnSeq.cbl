       IDENTIFICATION DIVISION.
       PROGRAM-ID. RrnFileSeq.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           NUMERIC SIGN IS TRAILING SEPARATE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL RrnFile ASSIGN TO ws-name-RrnFile
                  ORGANIZATION IS RELATIVE
                  ACCESS MODE  IS SEQUENTIAL
                  RELATIVE KEY IS ws-key-RrnFile
                  FILE STATUS  IS fs-RrnFile.

       DATA DIVISION.
       FILE SECTION.
       FD  RrnFile
           DATA RECORD IS RrnFile-rec
           RECORD CONTAINS 15 CHARACTERS.

       01  RrnFile-rec.
           03  RrnFile-rec-code-employee   PIC 9(06)       VALUE ZEROES.
           03  RrnFile-rec-salary-employee PIC S9(06)V9(02) 
                                           SIGN IS LEADING
                                           SEPARATE CHARACTER
                                                           VALUE ZEROES.

       WORKING-STORAGE SECTION.
       77  fs-RrnFile                     PIC 9(02)        VALUE ZEROES.

       77  ws-continue-response           PIC A(01)        VALUE SPACE.
           88  sw-continue-response-N     VALUES ARE 'N' 'n'.

       77  ws-key-RrnFile                 PIC 9(06)        VALUE ZEROES.
       77  ws-name-RrnFile                PIC X(12)        VALUE SPACES.

       77  ws-RrnFile-EOF                 PIC A(01)        VALUE SPACE.
           88  sw-RrnFile-EOF-N                            VALUE 'N'.
           88  sw-RrnFile-EOF-Y                            VALUE 'Y'.

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
           DISPLAY "File   Name: [" ws-name-RrnFile "]."
           DISPLAY "Status Code: [" fs-RrnFile "].".

       END DECLARATIVES.

       MAIN-PARAGRAPH.
           DISPLAY "Basic maintenance of a relative file."
           DISPLAY "Enter the file name: " WITH NO ADVANCING
           ACCEPT ws-name-RrnFile

           OPEN EXTEND RrnFile
           DISPLAY "Opening. Status Code: [" fs-RrnFile "]."

           PERFORM UNTIL sw-continue-response-N
                      OR fs-RrnFile IS NOT EQUAL TO ZEROES

                   INITIALIZE RrnFile-rec
                              ws-RrnFile-rec
                              ws-key-RrnFile

                   DISPLAY SPACE
                   DISPLAY "Employee data capture."
                   DISPLAY "Employee code   : " WITH NO ADVANCING
                   ACCEPT ws-RrnFile-rec-code-employee
                   MOVE ws-RrnFile-rec-code-employee 
                     TO RrnFile-rec-code-employee

                   DISPLAY "Salary Employee : " WITH NO ADVANCING
                   ACCEPT ws-RrnFile-rec-salary-employee
                   MOVE ws-RrnFile-rec-salary-employee
                     TO RrnFile-rec-salary-employee

                   WRITE RrnFile-rec        FROM ws-RrnFile-rec
                         INVALID KEY
                         DISPLAY "Invalid Key!"

                     NOT INVALID KEY
                         DISPLAY "Record saved successfully."
                         DISPLAY SPACE
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

           DISPLAY "Record number to start: "
              WITH NO ADVANCING
            ACCEPT ws-key-RrnFile

           START RrnFile
             KEY IS GREATER THAN OR EQUAL TO ws-key-RrnFile
                 INVALID KEY
                         DISPLAY "Record Number: ["
                                  ws-key-RrnFile
                                 "] was not found."
             NOT INVALID KEY
                         DISPLAY "Record Number: ["
                                  ws-key-RrnFile
                                 "] found successfully."

           END-START
           DISPLAY "Starting. Status Code: [" fs-RrnFile "]."

           PERFORM UNTIL sw-RrnFile-EOF-Y
                      OR fs-RrnFile IS NOT EQUAL TO ZEROES

                   INITIALIZE RrnFile-rec
                              ws-RrnFile-rec
                              ws-key-RrnFile

                   READ RrnFile NEXT RECORD    INTO ws-RrnFile-rec
                        AT END
                           SET sw-RrnFile-EOF-Y  TO TRUE
                           DISPLAY "End Of File!"

                    NOT AT END
                           SET sw-RrnFile-EOF-N TO TRUE

                           DISPLAY SPACE
                           DISPLAY "Record # [" ws-key-RrnFile "]."
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

                   DISPLAY "Press ENTER to continue..."
                   ACCEPT OMITTED
           END-PERFORM

           CLOSE RrnFile
           DISPLAY "Closing. Status Code: [" fs-RrnFile "]."

           STOP RUN.

       END PROGRAM RrnFileSeq.
