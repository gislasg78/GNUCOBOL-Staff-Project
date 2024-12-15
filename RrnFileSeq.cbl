       IDENTIFICATION DIVISION.
       PROGRAM-ID. RrnFileSeq.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           NUMERIC SIGN IS TRAILING SEPARATE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL rrnFile ASSIGN TO ws-name-rrnFile
                  ORGANIZATION IS RELATIVE
                  ACCESS MODE  IS SEQUENTIAL
                  RELATIVE KEY IS ws-key-rrnFile
                  FILE STATUS  IS fs-rrnFile.

       DATA DIVISION.
       FILE SECTION.
       FD  rrnFile
           DATA RECORD IS rrnFile-rec
           RECORD CONTAINS 15 CHARACTERS.

       01  rrnFile-rec.
           03  rrnFile-rec-code-employee   PIC 9(06)       VALUE ZEROES.
           03  rrnFile-rec-salary-employee PIC S9(06)V9(02) 
                                           SIGN IS LEADING
                                           SEPARATE CHARACTER
                                                           VALUE ZEROES.

       WORKING-STORAGE SECTION.
       77  fs-rrnFile                     PIC 9(02)        VALUE ZEROES.

       77  ws-continue-response           PIC A(01)        VALUE SPACE.
           88  sw-continue-response-N     VALUES ARE 'N' 'n'.

       77  ws-key-rrnFile                 PIC 9(06)        VALUE ZEROES.
       77  ws-name-rrnFile                PIC X(12)        VALUE SPACES.

       77  ws-rrnFile-EOF                 PIC A(01)        VALUE SPACE.
           88  sw-rrnFile-EOF-N                            VALUE 'N'.
           88  sw-rrnFile-EOF-Y                            VALUE 'Y'.

       01  ws-rrnFile-rec.
           03  ws-rrnFile-rec-code-employee                PIC 9(06)
                                                           VALUE ZEROES.
           03  ws-rrnFile-rec-salary-employee          PIC S9(06)V9(02)
                                                      SIGN IS LEADING
                                                  SEPARATE CHARACTER
                                                     VALUE ZEROES.

       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON rrnFile.

       Status-Check.
           DISPLAY SPACE
           DISPLAY "File status information."
           DISPLAY "File   Name: [" ws-name-rrnFile "]."
           DISPLAY "Status Code: [" fs-rrnFile "].".

       END DECLARATIVES.

       MAIN-PARAGRAPH.
           DISPLAY "Basic maintenance of a relative file."
           DISPLAY "Enter the file name: " WITH NO ADVANCING
           ACCEPT ws-name-rrnFile

           OPEN EXTEND rrnFile
           DISPLAY "Opening. Status Code: [" fs-rrnFile "]."

           PERFORM UNTIL sw-continue-response-N
                      OR fs-rrnFile IS NOT EQUAL TO ZEROES

                   INITIALIZE rrnFile-rec
                              ws-rrnFile-rec
                              ws-key-rrnfile

                   DISPLAY SPACE
                   DISPLAY "Employee data capture."
                   DISPLAY "Employee code   : " WITH NO ADVANCING
                   ACCEPT ws-rrnFile-rec-code-employee
                   MOVE ws-rrnFile-rec-code-employee 
                     TO rrnFile-rec-code-employee

                   DISPLAY "Salary Employee : " WITH NO ADVANCING
                   ACCEPT ws-rrnFile-rec-salary-employee
                   MOVE ws-rrnFile-rec-salary-employee
                     TO rrnFile-rec-salary-employee

                   WRITE rrnFile-rec        FROM ws-rrnFile-rec
                         INVALID KEY
                         DISPLAY "Invalid Key!"

                     NOT INVALID KEY
                         DISPLAY "Record saved successfully."
                         DISPLAY SPACE
                         DISPLAY "Record Number # : [" ws-key-rrnFile
                                 "]."
                         DISPLAY "Employee code   : [" 
                                 ws-rrnFile-rec-code-employee "] = ["
                                 rrnFile-rec-code-employee "]."
                         DISPLAY "Salary Employee : ["
                                 ws-rrnFile-rec-salary-employee "] = ["
                                 rrnFile-rec-salary-employee "]"

                   END-WRITE
                   DISPLAY "Writing. Status Code: [" fs-rrnFile "]."

                   DISPLAY "Do you want to capture more records? (y/n) "
                           ": " WITH NO ADVANCING
                   ACCEPT ws-continue-response
           END-PERFORM

           CLOSE rrnFile
           DISPLAY "Closing. Status Code: [" fs-rrnFile "]."

           DISPLAY SPACE
           DISPLAY "Reading sequential file."

           OPEN INPUT rrnFile
           DISPLAY "Opening. Status Code: [" fs-rrnFile "]."

           PERFORM UNTIL sw-rrnFile-EOF-Y
                      OR fs-rrnFile IS NOT EQUAL TO ZEROES

                   INITIALIZE rrnFile-rec
                              ws-rrnFile-rec
                              ws-key-rrnfile

                   READ rrnFile NEXT RECORD    INTO ws-rrnFile-rec
                        AT END
                           SET sw-rrnFile-EOF-Y  TO TRUE
                           DISPLAY "End Of File!"

                    NOT AT END
                           SET sw-rrnFile-EOF-N TO TRUE

                           DISPLAY SPACE
                           DISPLAY "Record # [" ws-key-rrnFile "]."
                           DISPLAY "Employee code   : [" 
                                   ws-rrnFile-rec-code-employee
                                   "] = ["
                                   rrnFile-rec-code-employee
                                   "]."
                           DISPLAY "Salary Employee : ["
                                   ws-rrnFile-rec-salary-employee
                                   "] = ["
                                   rrnFile-rec-salary-employee
                                   "]."

                   END-READ
                   DISPLAY "Reading. Status Code: [" fs-rrnFile "]."

                   DISPLAY "Press ENTER to continue..."
                   ACCEPT OMITTED
           END-PERFORM

           CLOSE rrnFile
           DISPLAY "Closing. Status Code: [" fs-rrnFile "]."

           STOP RUN.

       END PROGRAM RrnFileSeq.
