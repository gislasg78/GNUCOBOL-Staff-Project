       IDENTIFICATION DIVISION.
       PROGRAM-ID. IdxFile.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                  SYMBOLIC CHARACTERS asterisk IS 43.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL idxfile ASSIGN TO ws-idxfile-name
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS f-idxfile-rec-cod-employee
                  ALTERNATE RECORD KEY IS f-idxfile-rec-salary-employee
                            WITH DUPLICATES
                  PADDING CHARACTER IS asterisk
                  FILE STATUS  IS fs-idxfile.

       DATA DIVISION.
       FILE SECTION.
       FD  idxfile
           BLOCK  CONTAINS 05 TO 15 RECORDS
           DATA   RECORD   IS f-idxfile-rec
           LABEL  RECORD   IS STANDARD
           RECORD CONTAINS 14 CHARACTERS
           RECORDING MODE  IS F.

       01  f-idxfile-rec.
           03  f-idxfile-rec-cod-employee       PIC 9(05)  VALUE ZEROES.
           03  f-idxfile-rec-salary-employee    PIC S9(06)V9(02)
                                                SIGN  IS LEADING
                                                SEPARATE CHARACTER
                                                           VALUE ZEROES.

       WORKING-STORAGE SECTION.
       77  fs-idxfile                           PIC 9(02)  VALUE ZEROES.
       77  ws-idxfile-name                      PIC X(12)  VALUE SPACES.

       78  cte-01                                          VALUE 01.

       01  ws-environmental-variables.
           03  ws-realization-questions.
               05  ws-carry-out-sure            PIC A(01)  VALUE SPACE.
                   88  sw-carry-out-sure-Y      VALUES ARE 'Y', 'y'.
               05  ws-continue-response         PIC A(01)  VALUE SPACE.
                   88  sw-continue-response-N   VALUES ARE 'N', 'n'.

           03  ws-f-idxfile-indicators.
               05  ws-idxfile-EOF               PIC A(01)  VALUE SPACE.
                   88  sw-idxfile-EOF-Y                    VALUE 'Y'.
               05  ws-idxfile-record-found      PIC A(01)  VALUE SPACE.
                   88  sw-idxfile-recrd-found-N            VALUE 'N'.
                   88  sw-idxfile-recrd-found-Y            VALUE 'Y'.
               05  ws-f-idxfile-rec-salary-employee-ed
                                                    PIC  $--,---,--9.99
                                                           VALUE ZEROES.

           03  ws-f-idxfile-rec.
               05  ws-f-idxfile-rec-cod-employee           PIC 9(05)
                                                           VALUE ZEROES.
               05  ws-f-idxfile-rec-salary-employee    PIC S9(06)V9(02)
                                                      SIGN  IS LEADING
                                                      SEPARATE CHARACTER
                                                           VALUE ZEROES.

           03  ws-menu-option                   PIC 9(01)  VALUE ZERO.
               88  sw-menu-option-add                      VALUE 1.
               88  sw-menu-option-delete                   VALUE 2.
               88  sw-menu-option-modify                   VALUE 3.
               88  sw-menu-option-look-for-one             VALUE 4.
               88  sw-menu-option-look-for-all             VALUE 5.
               88  sw-menu-option-exit                     VALUE 6.

           03  ws-menu-mode-read-option         PIC 9(02)  VALUE ZEROES.
               88  sw-menu-mode-read-option-start          VALUE 01.
               88  sw-menu-mode-read-option-givenkey-eq    VALUE 02.
               88  sw-menu-mode-read-option-givenkey-gteq  VALUE 03.
               88  sw-menu-mode-read-option-finish         VALUE 04.
               88  sw-menu-mode-read-option-r-first-rcrd   VALUE 05.
               88  sw-menu-mode-read-option-r-last-rcrd    VALUE 06.
               88  sw-menu-mode-read-option-r-backward     VALUE 07.
               88  sw-menu-mode-read-option-r-forward      VALUE 08.
               88  sw-menu-mode-read-option-prev-rcrd      VALUE 09.
               88  sw-menu-mode-read-option-next-rcrd      VALUE 10.
               88  sw-menu-mode-read-option-exit           VALUE 11.

           03  ws-operation-class               PIC A(13)  VALUE SPACES.
               88  sw-operation-class-CLOSE     VALUE "CLOSE".
               88  sw-operation-class-DELETE    VALUE "DELETE".
               88  sw-operation-class-OPEN      VALUE "OPEN".
               88  sw-operation-class-READ      VALUE "READ".
               88  sw-operation-class-READNEXT  VALUE "READ NEXT".
               88  sw-operation-class-READPREV  VALUE "READ PREVIOUS".
               88  sw-operation-class-REWRITE   VALUE "REWRITE".
               88  sw-operation-class-STARTEQ   VALUE "START EQUAL".
               88  sw-operation-class-STARTFRST VALUE "START FIRST".
               88  sw-operation-class-STARTGTEQ VALUE "START GTEQ".
               88  sw-operation-class-STARTLST  VALUE "START LAST".
               88  sw-operation-class-WRITE     VALUE "WRITE".

       01  ws-statistics-processed-records.
           03  ws-eliminated-records            PIC 9(04)  VALUE ZEROES.
           03  ws-reading-records               PIC 9(04)  VALUE ZEROES.
           03  ws-repositioning-records         PIC 9(04)  VALUE ZEROES.
           03  ws-rewritten-records             PIC 9(04)  VALUE ZEROES.
           03  ws-written-records               PIC 9(04)  VALUE ZEROES.

       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON idxfile.
       000000-status-check.
           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+---+"
           DISPLAY "|   File status information.   |"
           DISPLAY "+---+----+---+----+---+----+---+"
           DISPLAY "| " asterisk " File Name   : [" 
                                   ws-idxfile-name "]."
           DISPLAY "| " asterisk " Operation   : ["
                                   ws-operation-class "]."
           DISPLAY "| " asterisk " Status Code : ["
                                   fs-idxfile "]."
           DISPLAY "+---+----+---+----+---+----+---+"
           DISPLAY "Press the ENTER key to continue..."
              WITH NO ADVANCING
           ACCEPT OMITTED.
       END DECLARATIVES.

       MAIN-PARAGRAPH.
           PERFORM 100000-start-begin-program
              THRU 100000-finish-begin-program

           PERFORM 200000-start-process-menu
              THRU 200000-finish-process-menu
             UNTIL sw-menu-option-exit

           PERFORM 300000-start-end-program
              THRU 300000-finish-end-program

           STOP "This program has ended..."
           STOP RUN.

       100000-start-begin-program.
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "| Indexed Sequential Files.|"
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY asterisk " Enter the file name: " WITH NO ADVANCING
           ACCEPT ws-idxfile-name

           DISPLAY "Idx File to work on: ["
                   ws-idxfile-name "].".

           SET sw-operation-class-OPEN  TO TRUE
           OPEN I-O idxfile.
       100000-finish-begin-program.
           EXIT.

       200000-start-process-menu.
           INITIALIZE f-idxfile-rec
                      ws-environmental-variables

           PERFORM 210000-start-option-menu-display
              THRU 210000-finish-option-menu-display

           PERFORM 220000-start-validate-selected-menu-option
              THRU 220000-finish-validate-selected-menu-option.
       200000-finish-process-menu.
           EXIT.

        210000-start-option-menu-display.
           DISPLAY SPACE
           DISPLAY "+===+====+===+====+===+====+=="
           DISPLAY "|    Indexed maintenance.    |"
           DISPLAY "+===+====+===+====+===+====+=="
           DISPLAY "| [1]. Add a record.         |"
           DISPLAY "| [2]. Delete a record.      |"
           DISPLAY "| [3]. Modify a record.      |"
           DISPLAY "| [4]. Look for a record.    |"
           DISPLAY "| [5]. Look for all records. |"
           DISPLAY "| [6]. Exit this program.    |"
           DISPLAY "+===+====+===+====+===+====+=="
           DISPLAY "Enter your own choice: " WITH NO ADVANCING
           ACCEPT ws-menu-option

           DISPLAY "The chosen option was: " ws-menu-option.
        210000-finish-option-menu-display.
           EXIT.
 
        220000-start-validate-selected-menu-option.
           EVALUATE TRUE
               WHEN sw-menu-option-add
                    PERFORM 221000-start-add-a-record
                       THRU 221000-finish-add-a-record
                      UNTIL sw-continue-response-N

               WHEN sw-menu-option-delete
                    PERFORM 222000-start-delete-a-record
                       THRU 222000-finish-delete-a-record
                      UNTIL sw-continue-response-N

               WHEN sw-menu-option-modify
                    PERFORM 223000-start-modify-a-record
                       THRU 223000-finish-modify-a-record
                      UNTIL sw-continue-response-N

               WHEN sw-menu-option-look-for-one
                    PERFORM 224000-start-look-for-any-record
                       THRU 224000-finish-look-for-any-record
                      UNTIL sw-continue-response-N

               WHEN sw-menu-option-look-for-all
                    PERFORM 225000-start-look-for-all-records
                       THRU 225000-finish-look-for-all-records
                      UNTIL sw-menu-mode-read-option-exit
                        
               WHEN sw-menu-option-exit
                    DISPLAY "Leaving this program..."

               WHEN OTHER
                    DISPLAY "Unrecognized option. Please try again!"

           END-EVALUATE.
        220000-finish-validate-selected-menu-option.
           EXIT.

         221000-start-add-a-record.
           PERFORM 221100-start-capture-key-field
              THRU 221100-finish-capture-key-field

           PERFORM 221200-start-look-for-a-record
              THRU 221200-finish-look-for-a-record

           IF (sw-idxfile-recrd-found-N) THEN
               PERFORM 221300-start-continue-carry-out-oper
                  THRU 221300-finish-continue-carry-out-oper

               IF (sw-carry-out-sure-Y)  THEN
                   PERFORM 221400-start-capture-other-fields
                      THRU 221400-finish-capture-other-fields

                   PERFORM 221500-start-store-a-record
                      THRU 221500-finish-store-a-record
               ELSE
                   DISPLAY "Operation not performed. File unchanged."
               END-IF
           ELSE
               DISPLAY "Cannot add a record that already exists!"
           END-IF

           PERFORM 221600-start-continue-operation
              THRU 221600-finish-continue-operation.
         221000-finish-add-a-record.
           EXIT.

          221100-start-capture-key-field.
            DISPLAY asterisk " Employee Code   : " WITH NO ADVANCING
            ACCEPT ws-f-idxfile-rec-cod-employee

            MOVE ws-f-idxfile-rec-cod-employee
              TO f-idxfile-rec-cod-employee.
          221100-finish-capture-key-field.
            EXIT.

          221200-start-look-for-a-record.
            SET sw-operation-class-READ    TO TRUE

            READ idxfile                 INTO ws-f-idxfile-rec
             KEY IS f-idxfile-rec-cod-employee
                 INVALID KEY
                         SET sw-idxfile-recrd-found-N TO TRUE
                         DISPLAY "Record Not Found!"

             NOT INVALID KEY
                         ADD cte-01        TO ws-reading-records
                         SET sw-idxfile-recrd-found-Y TO TRUE
                         DISPLAY "Record found successfully!"

                         PERFORM 221210-start-show-file-info
                            THRU 221210-finish-show-file-info

            END-READ.
         221200-finish-look-for-a-record.
            EXIT.

         221210-start-show-file-info.
            MOVE ws-f-idxfile-rec-salary-employee
              TO ws-f-idxfile-rec-salary-employee-ed

            DISPLAY SPACE
            DISPLAY "+---+----+---+----+---+----+"
            DISPLAY "|   Employee Information.  |"
            DISPLAY "+---+----+---+----+---+----+"
            DISPLAY "| Code   : ["
                    ws-f-idxfile-rec-cod-employee "]."
            DISPLAY "| Salary : ["
                    ws-f-idxfile-rec-salary-employee-ed "]."
            DISPLAY "+---+----+---+----+---+----+"
            DISPLAY "Press the ENTER key to continue..."
               WITH NO ADVANCING
            ACCEPT OMITTED

            DISPLAY SPACE.
         221210-finish-show-file-info.
            EXIT.

         221300-start-continue-carry-out-oper.
            DISPLAY asterisk
                    "Are you really sure you want to carry out this "
                    "operation? (y/n) : "
               WITH NO ADVANCING
             ACCEPT ws-carry-out-sure.
         221300-finish-continue-carry-out-oper.
            EXIT.

         221400-start-capture-other-fields.
           DISPLAY asterisk " Employee Salary : " WITH NO ADVANCING
           ACCEPT ws-f-idxfile-rec-salary-employee

           MOVE ws-f-idxfile-rec-salary-employee
             TO f-idxfile-rec-salary-employee.
         221400-finish-capture-other-fields.
            EXIT.

         221500-start-store-a-record.
            SET sw-operation-class-WRITE   TO TRUE

            WRITE f-idxfile-rec          FROM ws-f-idxfile-rec
                  INVALID KEY
                          DISPLAY "Duplicate Key!"

              NOT INVALID KEY
                          ADD cte-01       TO ws-written-records
                          DISPLAY "Record saved successfully!"

            END-WRITE.
         221500-finish-store-a-record.
            EXIT.

         221600-start-continue-operation.
            DISPLAY asterisk
                    "Do you want to continue doing the same operation? "
                    "(y/n) : " WITH NO ADVANCING

            ACCEPT ws-continue-response.
         221600-finish-continue-operation.
            EXIT.

         222000-start-delete-a-record.
           PERFORM 221100-start-capture-key-field
              THRU 221100-finish-capture-key-field

           PERFORM 221200-start-look-for-a-record
              THRU 221200-finish-look-for-a-record

           IF (sw-idxfile-recrd-found-Y) THEN
               PERFORM 221300-start-continue-carry-out-oper
                  THRU 221300-finish-continue-carry-out-oper

               IF (sw-carry-out-sure-Y)  THEN
                   PERFORM 222100-start-eliminate-a-record
                      THRU 222100-finish-eliminate-a-record
               ELSE
                   DISPLAY "Operation not performed. File unchanged."
               END-IF
           END-IF

           PERFORM 221600-start-continue-operation
              THRU 221600-finish-continue-operation.
         222000-finish-delete-a-record.
            EXIT.

         222100-start-eliminate-a-record.
            SET sw-operation-class-DELETE  TO TRUE

            DELETE idxfile RECORD
                   INVALID KEY
                           DISPLAY "Invalid Key!"

               NOT INVALID KEY
                           ADD cte-01      TO ws-eliminated-records
                           DISPLAY "Record deleted successfully!"

            END-DELETE.
         222100-finish-eliminate-a-record.
            EXIT.

         223000-start-modify-a-record.
            PERFORM 221100-start-capture-key-field
               THRU 221100-finish-capture-key-field

            PERFORM 221200-start-look-for-a-record
               THRU 221200-finish-look-for-a-record

            IF (sw-idxfile-recrd-found-Y) THEN
               PERFORM 221300-start-continue-carry-out-oper
                  THRU 221300-finish-continue-carry-out-oper

               IF (sw-carry-out-sure-Y)   THEN
                   PERFORM 221400-start-capture-other-fields
                      THRU 221400-finish-capture-other-fields

                   PERFORM 223100-start-change-a-record
                      THRU 223100-finish-change-a-record
               ELSE
                   DISPLAY "Operation not performed. File unchanged."
               END-IF
            END-IF

            PERFORM 221600-start-continue-operation
               THRU 221600-finish-continue-operation.
         223000-finish-modify-a-record.
            EXIT.

         223100-start-change-a-record.
            SET sw-operation-class-REWRITE TO TRUE

            REWRITE f-idxfile-rec        FROM ws-f-idxfile-rec
                    INVALID KEY
                            DISPLAY "Invalid Key!"

                NOT INVALID KEY
                            ADD cte-01     TO ws-rewritten-records
                            DISPLAY "Record changed successfully!"

            END-REWRITE.
         223100-finish-change-a-record.
            EXIT.

         224000-start-look-for-any-record.
           PERFORM 221100-start-capture-key-field
              THRU 221100-finish-capture-key-field

           PERFORM 221200-start-look-for-a-record
              THRU 221200-finish-look-for-a-record

           PERFORM 221600-start-continue-operation
              THRU 221600-finish-continue-operation.
         224000-finish-look-for-any-record.
           EXIT.

         225000-start-look-for-all-records.
           INITIALIZE ws-realization-questions

           PERFORM 225100-start-menu-reading-offset
              THRU 225100-finish-option-menu-reading-offset

           PERFORM 225200-start-validate-option-menu-reading-offset
              THRU 225200-finish-validate-option-menu-reading-offset.
         225000-finish-look-for-all-records.
           EXIT.

          225100-start-menu-reading-offset.
            DISPLAY SPACE
            DISPLAY "+===+====+===+====+===+====+===+===+="
            DISPLAY "|        Record Reading Menu.       |"
            DISPLAY "+===+====+===+====+===+====+===+===+="
            DISPLAY "| Input/Output Pointer Positioning. |"
            DISPLAY "+---+----+---+----+---+----+---+---+-"
            DISPLAY "| [01]. At the start.               |"
            DISPLAY "| [02]. At on a exact & given key.  |"
            DISPLAY "| [03]. At on a aproximate key.     |"
            DISPLAY "| [04]. At the finish.              |"
            DISPLAY "+---+----+---+----+---+----+---+---+-"
            DISPLAY "|      Reading extreme records.     |"
            DISPLAY "+---+----+---+----+---+----+---+---+-"
            DISPLAY "| [05]. Read first record.          |"
            DISPLAY "| [06]. Read last record.           |"
            DISPLAY "+---+----+---+----+---+----+---+---+-"
            DISPLAY "|  Sequential traversal of records. |"
            DISPLAY "+---+----+---+----+---+----+---+---+-"
            DISPLAY "| [07]. Read the records backward.  |"
            DISPLAY "| [08]. Read the records forward.   |"
            DISPLAY "+---+----+---+----+---+----+---+---+-"
            DISPLAY "|      Record by record reading.    |"
            DISPLAY "+---+----+---+----+---+----+---+---+-"
            DISPLAY "| [09]. Read previous record.       |"
            DISPLAY "| [10]. Read next record.           |"
            DISPLAY "+---+----+---+----+---+----+---+---+-"
            DISPLAY "| [11]. Return to main menu.        |"
            DISPLAY "+===+====+===+====+===+====+===+===+="
            DISPLAY "Enter your choice: " WITH NO ADVANCING
            ACCEPT ws-menu-mode-read-option

            DISPLAY "The chosen option has been: "
                   ws-menu-mode-read-option.
          225100-finish-option-menu-reading-offset.
            EXIT.

          225200-start-validate-option-menu-reading-offset.
            INITIALIZE ws-idxfile-EOF

            EVALUATE TRUE
                WHEN sw-menu-mode-read-option-start
                     PERFORM 225210-start-menu-mode-start-position
                        THRU 225210-finish-menu-mode-start-position

                WHEN sw-menu-mode-read-option-givenkey-eq
                     PERFORM 221100-start-capture-key-field
                        THRU 221100-finish-capture-key-field
                     PERFORM 225220-start-menu-mode-read-position-eq
                        THRU 225220-finish-menu-mode-read-position-eq

                WHEN sw-menu-mode-read-option-givenkey-gteq
                     PERFORM 221100-start-capture-key-field
                        THRU 221100-finish-capture-key-field
                     PERFORM 225230-start-menu-mode-read-position-gteq
                        THRU 225230-finish-menu-mode-read-position-gteq

                WHEN sw-menu-mode-read-option-finish
                     PERFORM 225240-start-menu-mode-finish-position
                        THRU 225240-finish-menu-mode-finish-position

                WHEN sw-menu-mode-read-option-r-first-rcrd
                     PERFORM 225210-start-menu-mode-start-position
                        THRU 225210-finish-menu-mode-start-position
                     PERFORM 225260-start-menu-mode-read-forwarding
                        THRU 225260-finish-menu-mode-read-forwarding

                WHEN sw-menu-mode-read-option-r-last-rcrd
                     PERFORM 225240-start-menu-mode-finish-position
                        THRU 225240-finish-menu-mode-finish-position
                     PERFORM 225250-start-menu-mode-read-backwarding
                        THRU 225250-finish-menu-mode-read-backwarding

                WHEN sw-menu-mode-read-option-r-backward
                     PERFORM 225250-start-menu-mode-read-backwarding
                        THRU 225250-finish-menu-mode-read-backwarding
                       UNTIL sw-idxfile-EOF-Y

                WHEN sw-menu-mode-read-option-r-forward
                     PERFORM 225260-start-menu-mode-read-forwarding
                        THRU 225260-finish-menu-mode-read-forwarding
                       UNTIL sw-idxfile-EOF-Y

                WHEN sw-menu-mode-read-option-prev-rcrd
                     PERFORM 225250-start-menu-mode-read-backwarding
                        THRU 225250-finish-menu-mode-read-backwarding

                WHEN sw-menu-mode-read-option-next-rcrd
                     PERFORM 225260-start-menu-mode-read-forwarding
                        THRU 225260-finish-menu-mode-read-forwarding

                WHEN sw-menu-mode-read-option-exit 
                     DISPLAY "Returning to main menu..."

                WHEN OTHER
                     DISPLAY "Unrecognized option. Please try again!"

            END-EVALUATE.
          225200-finish-validate-option-menu-reading-offset.
            EXIT.

          225210-start-menu-mode-start-position.
            SET sw-operation-class-STARTFRST  TO TRUE

            START idxfile FIRST
                  INVALID KEY
                          DISPLAY "Error positioning at begin!"

              NOT INVALID KEY
                          ADD  cte-01      TO ws-repositioning-records
                          DISPLAY asterisk
                                  "Positioning at the begin."
                                  asterisk

            END-START.
          225210-finish-menu-mode-start-position.
            EXIT.

          225220-start-menu-mode-read-position-eq.
            SET sw-operation-class-STARTEQ    TO TRUE

            START idxfile
              KEY IS EQUAL TO f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records
                  DISPLAY asterisk
                          "Exact key to locate: ["
                           f-idxfile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning exact key done correctly!"
                          asterisk

            END-START.
          225220-finish-menu-mode-read-position-eq.
            EXIT.

          225230-start-menu-mode-read-position-gteq.
            SET sw-operation-class-STARTGTEQ  TO TRUE

            START idxfile
              KEY IS GREATER THAN OR EQUAL TO f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records
                  DISPLAY asterisk
                          "Approximate key to locate: ["
                           f-idxfile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning approximate key done correctly!"
                          asterisk

            END-START.
          225230-finish-menu-mode-read-position-gteq.
            EXIT.

          225240-start-menu-mode-finish-position.
            SET sw-operation-class-STARTLST   TO TRUE

            START idxfile LAST
                  INVALID KEY
                          DISPLAY "Error positioning at end!"

              NOT INVALID KEY
                          ADD  cte-01      TO ws-repositioning-records
                          DISPLAY asterisk
                                  "Positioning at the end."
                                  asterisk

            END-START.
          225240-finish-menu-mode-finish-position.
            EXIT.

          225250-start-menu-mode-read-backwarding.
            SET sw-operation-class-READPREV   TO TRUE

            READ idxfile PREVIOUS RECORD    INTO ws-f-idxfile-rec
              AT END
                 SET sw-idxfile-EOF-Y         TO TRUE
                 DISPLAY "End of file!"
                 PERFORM 225240-start-menu-mode-finish-position
                    THRU 225240-finish-menu-mode-finish-position

             NOT AT END
                 ADD cte-01                   TO ws-reading-records

                 PERFORM 221210-start-show-file-info
                    THRU 221210-finish-show-file-info

            END-READ.
          225250-finish-menu-mode-read-backwarding.
            EXIT.

          225260-start-menu-mode-read-forwarding.
            SET sw-operation-class-READNEXT   TO TRUE

            READ idxfile NEXT RECORD        INTO ws-f-idxfile-rec
              AT END
                 SET sw-idxfile-EOF-Y         TO TRUE
                 DISPLAY "End of file!"
                 PERFORM 225210-start-menu-mode-start-position
                    THRU 225210-finish-menu-mode-start-position

             NOT AT END
                 ADD cte-01                   TO ws-reading-records

                 PERFORM 221210-start-show-file-info
                    THRU 221210-finish-show-file-info

            END-READ.
          225260-finish-menu-mode-read-forwarding.
            EXIT.

       300000-start-end-program.
           SET sw-operation-class-CLOSE       TO TRUE
           CLOSE idxfile

           MOVE fs-idxfile                    TO RETURN-CODE

           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "|    Processed records.    |"
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "| Eliminated   : [" ws-eliminated-records "]."
           DISPLAY "| Read         : [" ws-reading-records "]."
           DISPLAY "| Repositioned : [" ws-repositioning-records "]."
           DISPLAY "| Rewritten    : [" ws-rewritten-records "]."
           DISPLAY "| Writings     : [" ws-written-records "]."
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY SPACE.
       300000-finish-end-program.
           EXIT.

       END PROGRAM IdxFile.
