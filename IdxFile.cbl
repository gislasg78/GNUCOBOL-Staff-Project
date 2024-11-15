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
           BLOCK  CONTAINS 10 RECORDS
           DATA   RECORD   IS f-idxfile-rec
           LABEL  RECORD   IS STANDARD
           RECORD CONTAINS 13 CHARACTERS
           RECORDING MODE  IS F.

       01  f-idxfile-rec.
           03  f-idxfile-rec-cod-employee       PIC 9(05)  VALUE ZEROES.
           03  f-idxfile-rec-salary-employee    PIC 9(06)V9(02)
                                                           VALUE ZEROES.

       WORKING-STORAGE SECTION.
       77  fs-idxfile                           PIC 9(02)  VALUE ZEROES.
       77  ws-idxfile-name                      PIC X(12)  VALUE SPACES.

       78  cte-01                                          VALUE 01.

       01  ws-environmental-variables.
           03  ws-continue-response             PIC A(01)  VALUE SPACE.
               88  sw-continue-response-N       VALUES ARE 'N', 'n'.

           03  ws-f-idxfile-indicators.
               05  ws-idxfile-EOF               PIC A(01)  VALUE SPACE.
                   88  sw-idxfile-EOF-Y                    VALUE 'Y'.
               05  ws-idxfile-record-found      PIC A(01)  VALUE SPACE.
                   88  sw-idxfile-recrd-found-N            VALUE 'N'.
                   88  sw-idxfile-recrd-found-Y            VALUE 'Y'.
               05  ws-f-idxfile-rec-salary-employee-ed PIC $Z,ZZZ,ZZ9.99
                                                           VALUE ZEROES.

           03  ws-f-idxfile-rec.
               05  ws-f-idxfile-rec-cod-employee           PIC 9(05)
                                                           VALUE ZEROES.
               05  ws-f-idxfile-rec-salary-employee    PIC 9(06)V9(02)
                                                           VALUE ZEROES.
           03  ws-menu-option                   PIC 9(01)  VALUE ZERO.
               88  sw-menu-option-add                      VALUE 1.
               88  sw-menu-option-delete                   VALUE 2.
               88  sw-menu-option-modify                   VALUE 3.
               88  sw-menu-option-look-for-one             VALUE 4.
               88  sw-menu-option-look-for-all             VALUE 5.
               88  sw-menu-option-exit                     VALUE 6.

           03  ws-menu-mode-read-option         PIC 9(01)  VALUE ZERO.
               88  sw-menu-mode-read-option-start          VALUE 1.
               88  sw-menu-mode-read-option-givenkey       VALUE 2.
               88  sw-menu-mode-read-option-finish         VALUE 3.
               88  sw-menu-mode-read-option-r-forward      VALUE 4.
               88  sw-menu-mode-read-option-r-backward     VALUE 5.
               88  sw-menu-mode-read-option-next-rcrd      VALUE 6.
               88  sw-menu-mode-read-option-prev-rcrd      VALUE 7.
               88  sw-menu-mode-read-option-exit           VALUE 8.

           03  ws-operation-class               PIC A(13)  VALUE SPACES.
               88  sw-operation-class-CLOSE     VALUE "CLOSE".
               88  sw-operation-class-DELETE    VALUE "DELETE".
               88  sw-operation-class-OPEN      VALUE "OPEN".
               88  sw-operation-class-READ      VALUE "READ".
               88  sw-operation-class-READNEXT  VALUE "READ NEXT".
               88  sw-operation-class-READPREV  VALUE "READ PREVIOUS".
               88  sw-operation-class-REWRITE   VALUE "REWRITE".
               88  sw-operation-class-START     VALUE "START".
               88  sw-operation-class-STARTFRST VALUE "START FIRST".
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
           DISPLAY "| File Name   : [" ws-idxfile-name "]."
           DISPLAY "| Operation   : [" ws-operation-class "]."
           DISPLAY "| Status Code : [" fs-idxfile "]."
           DISPLAY "+---+----+---+----+---+----+---+"
           DISPLAY "Press the ENTER key to continue..."
              WITH NO ADVANCING
           ACCEPT OMITTED

           GOBACK.
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
               PERFORM 221300-start-capture-other-fields
                  THRU 221300-finish-capture-other-fields

               PERFORM 221400-start-store-a-record
                  THRU 221400-finish-store-a-record
           ELSE
               DISPLAY "Cannot add a record that already exists!"
           END-IF

           PERFORM 221500-start-continue-operation
              THRU 221500-finish-continue-operation.
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
            DISPLAY "|  Employee Information.   |"
            DISPLAY "+---+----+---+----+---+----+"
            DISPLAY "| Code   : ["
                    ws-f-idxfile-rec-cod-employee "]."
            DISPLAY "| Salary : ["
                    ws-f-idxfile-rec-salary-employee-ed "]"
            DISPLAY "+---+----+---+----+---+----+"
            DISPLAY SPACE.
         221210-finish-show-file-info.
            EXIT.

         221300-start-capture-other-fields.
           DISPLAY asterisk " Employee Salary : " WITH NO ADVANCING
           ACCEPT ws-f-idxfile-rec-salary-employee

           MOVE ws-f-idxfile-rec-salary-employee
             TO f-idxfile-rec-salary-employee.
         221300-finish-capture-other-fields.
            EXIT.

         221400-start-store-a-record.
            SET sw-operation-class-WRITE   TO TRUE

            WRITE f-idxfile-rec          FROM ws-f-idxfile-rec
                  INVALID KEY
                          DISPLAY "Duplicate Key!"

              NOT INVALID KEY
                          ADD cte-01       TO ws-written-records
                          DISPLAY "Record saved successfully!"

            END-WRITE.
         221400-finish-store-a-record.
            EXIT.

         221500-start-continue-operation.
            DISPLAY "Do you want to continue doing the same "
                    "operation? (y/n) : " WITH NO ADVANCING

            ACCEPT ws-continue-response.
         221500-finish-continue-operation.
            EXIT.

         222000-start-delete-a-record.
           PERFORM 221100-start-capture-key-field
              THRU 221100-finish-capture-key-field

           PERFORM 221200-start-look-for-a-record
              THRU 221200-finish-look-for-a-record

           IF (sw-idxfile-recrd-found-Y) THEN
               PERFORM 222100-start-eliminate-a-record
                  THRU 222100-finish-eliminate-a-record
           END-IF

           PERFORM 221500-start-continue-operation
              THRU 221500-finish-continue-operation.
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
                PERFORM 221300-start-capture-other-fields
                   THRU 221300-finish-capture-other-fields

                PERFORM 223100-start-change-a-record
                   THRU 223100-finish-change-a-record
            END-IF

            PERFORM 221500-start-continue-operation
               THRU 221500-finish-continue-operation.
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

           PERFORM 221500-start-continue-operation
              THRU 221500-finish-continue-operation.
         224000-finish-look-for-any-record.
           EXIT.

         225000-start-look-for-all-records.
           INITIALIZE ws-continue-response

           PERFORM 225100-start-menu-reading-offset
              THRU 225100-finish-option-menu-reading-offset

           PERFORM 225200-start-validate-option-menu-reading-offset
              THRU 225200-finish-validate-option-menu-reading-offset.
         225000-finish-look-for-all-records.
           EXIT.

          225100-start-menu-reading-offset.
            DISPLAY SPACE
            DISPLAY "+===+====+===+====+===+====+===+===+"
            DISPLAY "|       Record Reading Menu.       |"
            DISPLAY "+===+====+===+====+===+====+===+===+"
            DISPLAY "| [1]. Positioning at the start.   |"
            DISPLAY "| [2]. Positioning on a given key. |"
            DISPLAY "| [3]. Positioning at the finish.  |"
            DISPLAY "| [4]. Read the records forward.   |"
            DISPLAY "| [5]. Read the records backward.  |"
            DISPLAY "| [6]. Read next record.           |"
            DISPLAY "| [7]. Read previous record.       |"
            DISPLAY "| [8]. Return to main menu.        |"
            DISPLAY "+===+====+===+====+===+====+===+===+"
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
                     PERFORM 225210-start-menu-mode-start-positioning
                        THRU 225210-finish-menu-mode-start-positioning

                WHEN sw-menu-mode-read-option-givenkey
                     PERFORM 221100-start-capture-key-field
                        THRU 221100-finish-capture-key-field

                     PERFORM 225220-start-menu-mode-read-positioning
                        THRU 225220-finish-menu-mode-read-positioning

                WHEN sw-menu-mode-read-option-finish
                     PERFORM 225230-start-menu-mode-finish-positioning
                        THRU 225230-finish-menu-mode-finish-positioning

                WHEN sw-menu-mode-read-option-r-forward
                     PERFORM 225240-start-menu-mode-read-forwarding
                        THRU 225240-finish-menu-mode-read-forwarding
                       UNTIL sw-idxfile-EOF-Y

                WHEN sw-menu-mode-read-option-r-backward
                     PERFORM 225250-start-menu-mode-read-backwarding
                        THRU 225250-finish-menu-mode-read-backwarding
                       UNTIL sw-idxfile-EOF-Y

                WHEN sw-menu-mode-read-option-next-rcrd
                     PERFORM 225240-start-menu-mode-read-forwarding
                        THRU 225240-finish-menu-mode-read-forwarding

                WHEN sw-menu-mode-read-option-prev-rcrd
                     PERFORM 225250-start-menu-mode-read-backwarding
                        THRU 225250-finish-menu-mode-read-backwarding

                WHEN sw-menu-mode-read-option-exit 
                     DISPLAY "Returning to main menu..."

                WHEN OTHER
                     DISPLAY "Unrecognized option. Please try again!"

            END-EVALUATE.
          225200-finish-validate-option-menu-reading-offset.
            EXIT.

          225210-start-menu-mode-start-positioning.
            SET sw-operation-class-STARTFRST  TO TRUE

            START idxfile FIRST
                  INVALID KEY
                          DISPLAY "Error positioning at begin."

              NOT INVALID KEY
                          DISPLAY "Positioning at the begin."
                          ADD  cte-01      TO ws-repositioning-records

            END-START.
          225210-finish-menu-mode-start-positioning.
            EXIT.

          225220-start-menu-mode-read-positioning.
            SET sw-operation-class-START   TO TRUE

            START idxfile
              KEY IS GREATER OR EQUAL      TO f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  PERFORM 225210-start-menu-mode-start-positioning
                     THRU 225210-finish-menu-mode-start-positioning

              NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records
                  DISPLAY "Positioning done correctly!"

            END-START.
          225220-finish-menu-mode-read-positioning.
            EXIT.

          225230-start-menu-mode-finish-positioning.
            SET sw-operation-class-STARTLST   TO TRUE

            START idxfile LAST
                  INVALID KEY
                          DISPLAY "Error positioning at end."

              NOT INVALID KEY
                          DISPLAY "Positioning at the end."
                          ADD  cte-01      TO ws-repositioning-records

            END-START.
          225230-finish-menu-mode-finish-positioning.
            EXIT.

          225240-start-menu-mode-read-forwarding.
            SET sw-operation-class-READNEXT   TO TRUE

            READ idxfile NEXT RECORD        INTO ws-f-idxfile-rec
              AT END
                 SET sw-idxfile-EOF-Y         TO TRUE
                 DISPLAY "End of file!"
                 PERFORM 225210-start-menu-mode-start-positioning
                    THRU 225210-finish-menu-mode-start-positioning

             NOT AT END
                 ADD cte-01                   TO ws-reading-records

                 PERFORM 221210-start-show-file-info
                    THRU 221210-finish-show-file-info

            END-READ.
          225240-finish-menu-mode-read-forwarding.
            EXIT.

          225250-start-menu-mode-read-backwarding.
            SET sw-operation-class-READPREV   TO TRUE

            READ idxfile PREVIOUS RECORD    INTO ws-f-idxfile-rec
              AT END
                 SET sw-idxfile-EOF-Y         TO TRUE
                 DISPLAY "End of file!"
                 PERFORM 225230-start-menu-mode-finish-positioning
                    THRU 225230-finish-menu-mode-finish-positioning

             NOT AT END
                 ADD cte-01                   TO ws-reading-records

                 PERFORM 221210-start-show-file-info
                    THRU 221210-finish-show-file-info

            END-READ.
          225250-finish-menu-mode-read-backwarding.
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
