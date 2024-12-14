       IDENTIFICATION DIVISION.
       PROGRAM-ID. IdxFileSeq.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           NUMERIC SIGN IS TRAILING SEPARATE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL idxFile ASSIGN TO ws-name-idxFile
                  ORGANIZATION IS INDEXED
                  RECORD   KEY IS f-idxFile-rec-code
                  ALTERNATE RECORD KEY IS f-idxFile-rec-salary
                            WITH DUPLICATES
                  FILE STATUS  IS fs-idxFile.

       DATA DIVISION.
       FILE SECTION.
       FD  idxFile
           DATA RECORD IS f-idxFile-rec
           RECORD CONTAINS 15 CHARACTERS.

       01  f-idxFile-rec.
           03  f-idxFile-rec-code       PIC 9(06)        VALUE ZEROES.
           03  f-idxFile-rec-salary     PIC S9(06)V9(02) 
                                        SIGN IS LEADING
                                        SEPARATE CHARACTER
                                        VALUE ZEROES.

       WORKING-STORAGE SECTION.
       77  fs-idxFile                   PIC 9(02)        VALUE ZEROES.

       77  ws-continue-response         PIC A(01)        VALUE SPACE.
           88  sw-continue-response-N   VALUES ARE 'N' 'n'.

       77  ws-name-idxFile              PIC X(12)        VALUE SPACES.

       77  ws-idxFile-EOF               PIC A(01)        VALUE SPACE.
           88  sw-idxFile-EOF-N                          VALUE 'N'.
           88  sw-idxFile-EOF-Y                          VALUE 'Y'.

       01  ws-f-idxFile-rec.
           03  ws-f-idxFile-rec-code    PIC 9(06)        VALUE ZEROES.
           03  ws-f-idxFile-rec-salary  PIC S9(06)V9(02)
                                        SIGN IS LEADING
                                        SEPARATE CHARACTER 
                                        VALUE ZEROES.

       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON idxFile.

       Status-Check.
           DISPLAY SPACE
           DISPLAY "File status information."
           DISPLAY "File   Name: [" ws-name-idxFile "]."
           DISPLAY "Status Code: [" fs-idxFile "].".

       END DECLARATIVES.

       MAIN-PARAGRAPH.
           DISPLAY "Basic maintenance to an indexed sequential file."
           DISPLAY "Enter the file name: " WITH NO ADVANCING
           ACCEPT ws-name-idxFile

           OPEN EXTEND idxFile
           DISPLAY "Opening. Status Code: [" fs-idxFile "]."

           PERFORM UNTIL sw-continue-response-N
                      OR fs-idxFile IS NOT EQUAL TO ZEROES

                   INITIALIZE f-idxFile-rec
                              ws-f-idxFile-rec

                   DISPLAY SPACE
                   DISPLAY "Employee data capture."
                   DISPLAY "Employee code   : " WITH NO ADVANCING
                   ACCEPT ws-f-idxFile-rec-code
                   MOVE ws-f-idxFile-rec-code   TO f-idxFile-rec-code

                   DISPLAY "Salary Employee : " WITH NO ADVANCING
                   ACCEPT ws-f-idxFile-rec-salary
                   MOVE ws-f-idxFile-rec-salary TO f-idxFile-rec-salary

                   WRITE f-idxFile-rec        FROM ws-f-idxFile-rec
                         INVALID KEY
                         DISPLAY "Invalid Key!"

                     NOT INVALID KEY
                         DISPLAY SPACE
                         DISPLAY "Record saved successfully."
                         DISPLAY "Employee code   : [" 
                                 ws-f-idxFile-rec-code "] = ["
                                 f-idxFile-rec-code "]."
                         DISPLAY "Salary Employee : ["
                                 ws-f-idxFile-rec-salary "] = ["
                                 f-idxFile-rec-salary "]"

                   END-WRITE
                   DISPLAY "Writing. Status Code: [" fs-idxFile "]."

                   DISPLAY "Do you want to capture more records? (y/n) "
                           ": " WITH NO ADVANCING
                   ACCEPT ws-continue-response
           END-PERFORM

           CLOSE idxFile
           DISPLAY "Closing. Status Code: [" fs-idxFile "]."


           DISPLAY SPACE
           DISPLAY "Sequential read to an indexed sequential file."

           OPEN INPUT idxFile
           DISPLAY "Opening. Status Code: [" fs-idxFile "]."

           DISPLAY SPACE
           DISPLAY "Employee Key to start tracking: "
              WITH NO ADVANCING
           ACCEPT ws-f-idxFile-rec-code
           MOVE ws-f-idxFile-rec-code     TO f-idxFile-rec-code

           START idxFile
             KEY IS GREATER THAN OR EQUAL TO f-idxFile-rec-code
                 INVALID KEY
                         DISPLAY SPACE
                         DISPLAY "Key not located. "
                                 "Tracing from the beginning."
                         START idxFile FIRST END-START
                         DISPLAY "Starting First."
                         DISPLAY "Status Code: [" fs-idxFile "]."

             NOT INVALID KEY
                         DISPLAY SPACE
                         DISPLAY "Key found: ["
                         ws-f-idxFile-rec-code "] = ["
                         f-idxFile-rec-code "]."
           END-START
           DISPLAY "Starting. Status Code: [" fs-idxFile "]."

           PERFORM UNTIL sw-idxFile-EOF-Y
                      OR fs-idxFile IS NOT EQUAL TO ZEROES

                    READ idxFile NEXT RECORD    INTO ws-f-idxFile-rec
                        AT END
                           SET sw-idxFile-EOF-Y  TO TRUE
                           DISPLAY "End Of File!"

                    NOT AT END
                           SET sw-idxFile-EOF-N TO TRUE

                           DISPLAY SPACE
                           DISPLAY "Employee code   : [" 
                                   ws-f-idxFile-rec-code
                                   "] = ["
                                   f-idxFile-rec-code
                                   "]."
                           DISPLAY "Salary Employee : ["
                                   ws-f-idxFile-rec-salary
                                   "] = ["
                                   f-idxFile-rec-salary
                                   "]."
                           DISPLAY "Press ENTER to continue..."
                           ACCEPT OMITTED

                  END-READ
                  DISPLAY "Reading. Status Code: [" fs-idxFile "]."
 
           END-PERFORM

           CLOSE idxFile
           DISPLAY "Closing. Status Code: [" fs-idxFile "]."

           STOP RUN.

       END PROGRAM IdxFileSeq.
