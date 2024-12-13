       IDENTIFICATION DIVISION.
       PROGRAM-ID. RrnFile.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           NUMERIC SIGN IS TRAILING SEPARATE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL relFile ASSIGN TO ws-name-relFile
                  ORGANIZATION IS RELATIVE
                  RELATIVE KEY IS ws-key-relFile
                  FILE STATUS  IS fs-relFile.

       DATA DIVISION.
       FILE SECTION.
       FD  relFile
           DATA RECORD IS relFile-rec
           RECORD CONTAINS 15 CHARACTERS.

       01  relFile-rec.
           03  relFile-rec-code       PIC 9(06)        VALUE ZEROES.
           03  relFile-rec-salary     PIC S9(06)V9(02) VALUE ZEROES.

       WORKING-STORAGE SECTION.
       77  fs-relfile                 PIC 9(02)        VALUE ZEROES.

       77  ws-continue-response       PIC A(01)        VALUE SPACE.
           88  sw-continue-response-N VALUES ARE 'N' 'n'.

       77  ws-key-relFile             PIC 9(06)        VALUE ZEROES.
       77  ws-name-relFile            PIC X(12)        VALUE SPACES.

       77  ws-relFile-EOF             PIC A(01)        VALUE SPACE.
           88  sw-relFile-EOF-N                        VALUE 'N'.
           88  sw-relFile-EOF-Y                        VALUE 'Y'.

       01  ws-relFile-rec.
           03  ws-relFile-rec-code    PIC 9(06)        VALUE ZEROES.
           03  ws-relFile-rec-salary  PIC S9(06)V9(02) VALUE ZEROES.

       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON relFile.

       Status-Check.
           DISPLAY SPACE
           DISPLAY "File status information."
           DISPLAY "File   Name: [" ws-name-relFile "]."
           DISPLAY "Status Code: [" fs-relFile "].".

       END DECLARATIVES.

       MAIN-PARAGRAPH.
           DISPLAY "Basic maintenance of a relative file."
           DISPLAY "Enter the file name: " WITH NO ADVANCING
           ACCEPT ws-name-relFile

           OPEN EXTEND relFile
           DISPLAY "Opening. Status Code: [" fs-relFile "]."

           PERFORM UNTIL sw-continue-response-N
                      OR fs-relFile IS NOT EQUAL TO ZEROES

                   INITIALIZE relFile-rec
                              ws-relFile-rec

                   DISPLAY SPACE
                   DISPLAY "Employee data capture."
                   DISPLAY "Employee code   : " WITH NO ADVANCING
                   ACCEPT ws-relFile-rec-code
                   MOVE ws-relFile-rec-code   TO relFile-rec-code

                   DISPLAY "Salary Employee : " WITH NO ADVANCING
                   ACCEPT ws-relFile-rec-salary
                   MOVE ws-relFile-rec-salary TO relFile-rec-salary

                   WRITE relFile-rec        FROM ws-relFile-rec
                         INVALID KEY
                         DISPLAY "Invalid Key!"

                     NOT INVALID KEY
                         DISPLAY "Record saved successfully."
                         DISPLAY SPACE
                         DISPLAY "Record Number # : [" ws-key-relFile
                                 "]."
                         DISPLAY "Employee code   : [" 
                                 ws-relFile-rec-code "] = ["
                                 relFile-rec-code "]."
                         DISPLAY "Salary Employee : ["
                                 ws-relFile-rec-salary "] = ["
                                 relFile-rec-salary "]"

                   END-WRITE

                   DISPLAY "Do you want to capture more records? (y/n) "
                           ": " WITH NO ADVANCING
                   ACCEPT ws-continue-response
           END-PERFORM

           CLOSE relFile
           DISPLAY "Closing. Status Code: [" fs-relFile "]."

           DISPLAY SPACE
           DISPLAY "Reading sequential file."

           OPEN INPUT relFile
           DISPLAY "Opening. Status Code: [" fs-relFile "]."

           PERFORM UNTIL sw-relFile-EOF-Y
                      OR fs-relFile IS NOT EQUAL TO ZEROES

                   READ relFile NEXT RECORD    INTO ws-relFile-rec
                        AT END
                           SET sw-relFile-EOF-Y  TO TRUE
                           DISPLAY "End Of File!"

                    NOT AT END
                           SET sw-relFile-EOF-N TO TRUE

                           DISPLAY SPACE
                           DISPLAY "Record # [" ws-key-relFile "]."
                           DISPLAY "Employee code   : [" 
                                   ws-relFile-rec-code
                                   "] = ["
                                   relFile-rec-code
                                   "]."
                           DISPLAY "Salary Employee : ["
                                   ws-relFile-rec-salary
                                   "] = ["
                                   relFile-rec-salary
                                   "]."
                           DISPLAY "Press ENTER to continue..."
                           ACCEPT OMITTED

           END-PERFORM

           CLOSE relFile
           DISPLAY "Closing. Status Code: [" fs-relFile "]."

           STOP RUN.

       END PROGRAM RrnFile.
