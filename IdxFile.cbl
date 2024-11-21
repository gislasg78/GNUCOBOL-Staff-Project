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
       77  ws-error-status-code-table-desc-error-tag       PIC X(25)
                                         VALUE "Unknown File Status".
       77  ws-idxfile-name                      PIC X(12)  VALUE SPACES.

       78  cte-01                                          VALUE 01.
       78  cte-34                                          VALUE 34.

       01  ws-environmental-variables.
           03  ws-realization-questions.
               05  ws-carry-out-sure            PIC A(01)  VALUE SPACE.
                   88  sw-carry-out-sure-Y      VALUES ARE 'Y', 'y'.
               05  ws-continue-response         PIC A(01)  VALUE SPACE.
                   88  sw-continue-response-N   VALUES ARE 'N', 'n'.

           03  ws-f-idxfile-indicators.
               05  ws-error-status-code-table-aux.
                   07  ws-error-status-code-table-code-error-aux
                                                PIC 9(02)  VALUE ZEROES.
                   07  ws-error-status-code-table-desc-error-aux
                                                PIC X(25)  VALUE SPACES.

               05  ws-idxfile-EOF               PIC A(01)  VALUE SPACE.
                   88  sw-idxfile-EOF-Y                    VALUE 'Y'.
                   88  sw-idxfile-EOF-N                    VALUE 'N'.
               05  ws-idxfile-record-found      PIC A(01)  VALUE SPACE.
                   88  sw-idxfile-record-found-N           VALUE 'N'.
                   88  sw-idxfile-record-found-Y           VALUE 'Y'.
               05  ws-f-idxfile-rec-salary-employee-ed
                                                    PIC   $-,---,--9.99
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

           03  ws-menu-mode-modify-option       PIC 9(01)  VALUE ZERO.
               88  sw-menu-mode-modify-emp-salary          VALUE 1.
               88  sw-menu-mode-modify-emp-exitmenu        VALUE 2.

           03  ws-menu-mode-read-direct-option  PIC 9(01)  VALUE ZERO.
               88  sw-menu-mode-read-direct-read           VALUE 1.
               88  sw-menu-mode-read-dir-and-seq           VALUE 2.
               88  sw-menu-mode-read-dir-exitmenu          VALUE 3.

           03  ws-menu-mode-read-direct-keyaccess 
                                                 PIC 9(01) VALUE ZERO.
               88  sw-menu-mode-read-dir-keyacc-code       VALUE 1.
               88  sw-menu-mode-read-dir-keyacc-salary     VALUE 2.
               88  sw-menu-mode-read-dir-keyacc-exitmenu   VALUE 3.

           03  ws-menu-mode-read-option         PIC 9(02)  VALUE ZEROES.
               88  sw-menu-mode-read-option-start          VALUE 01.
               88  sw-menu-mode-read-option-givenkey-eq    VALUE 02.
               88  sw-menu-mode-read-option-givenkey-apprx VALUE 03.
               88  sw-menu-mode-read-option-finish         VALUE 04.
               88  sw-menu-mode-read-option-r-first-rcrd   VALUE 05.
               88  sw-menu-mode-read-option-r-last-rcrd    VALUE 06.
               88  sw-menu-mode-read-option-r-backward     VALUE 07.
               88  sw-menu-mode-read-option-r-forward      VALUE 08.
               88  sw-menu-mode-read-option-prev-rcrd      VALUE 09.
               88  sw-menu-mode-read-option-next-rcrd      VALUE 10.
               88  sw-menu-mode-read-option-exitmenu       VALUE 11.

           03  ws-menu-mode-read-option-givenkey PIC 9(01) VALUE ZEROES.
               88  sw-menu-mode-read-option-givenkey-gt    VALUE 1.
               88  sw-menu-mode-read-option-givenkey-gteq  VALUE 2.
               88  sw-menu-mode-read-option-givenkey-lt    VALUE 3.
               88  sw-menu-mode-read-option-givenkey-lteq  VALUE 4.
               88  sw-menu-mode-read-option-givenkey-exit  VALUE 5.

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
               88  sw-operation-class-STARTGT   VALUE "START GREATER".
               88  sw-operation-class-STARTGTEQ VALUE "START GTEQ".
               88  sw-operation-class-STARTLST  VALUE "START LAST".
               88  sw-operation-class-STARTLT   VALUE "START LESS".
               88  sw-operation-class-STARTLTEQ VALUE "START LTEQ".
               88  sw-operation-class-WRITE     VALUE "WRITE".

       01  ws-statistics-processed-records.
           03  ws-eliminated-records            PIC 9(04)  VALUE ZEROES.
           03  ws-reading-records               PIC 9(04)  VALUE ZEROES.
           03  ws-repositioning-records         PIC 9(04)  VALUE ZEROES.
           03  ws-rewritten-records             PIC 9(04)  VALUE ZEROES.
           03  ws-written-records               PIC 9(04)  VALUE ZEROES.

       01  ws-error-status-code-table.
           03  FILLER                       PIC 9(02)  VALUE ZEROES.
           03  FILLER                       PIC X(25)  VALUE
                                            "Success Completion".
           03  FILLER                       PIC 9(02)  VALUE 02.
           03  FILLER                       PIC X(25)  VALUE
                                            "Success Duplicate".
           03  FILLER                       PIC 9(02)  VALUE 04.
           03  FILLER                       PIC X(25)  VALUE
                                            "Success Incomplete".
           03  FILLER                       PIC 9(02)  VALUE 05.
           03  FILLER                       PIC X(25)  VALUE
                                            "Success Optional, Missing".
           03  FILLER                       PIC 9(02)  VALUE 06.
           03  FILLER                       PIC X(25)  VALUE 
                                            "Multiple Records LS".
           03  FILLER                       PIC 9(02)  VALUE 07.
           03  FILLER                       PIC X(25)  VALUE
                                            "Success No Unit".
           03  FILLER                       PIC 9(02)  VALUE 09.
           03  FILLER                       PIC X(25)  VALUE
                                            "Success LS Bad Data".
           03  FILLER                       PIC 9(02)  VALUE 10.
           03  FILLER                       PIC X(25)  VALUE
                                            "End Of File".
           03  FILLER                       PIC 9(02)  VALUE 14.
           03  FILLER                       PIC X(25)  VALUE 
                                            "Out Of Key Range".
           03  FILLER                       PIC 9(02)  VALUE 21.
           03  FILLER                       PIC X(25)  VALUE
                                            "Key Invalid".
           03  FILLER                       PIC 9(02)  VALUE 22.
           03  FILLER                       PIC X(25)  VALUE
                                            "Key Exists".
           03  FILLER                       PIC 9(02)  VALUE 23.
           03  FILLER                       PIC X(25)  VALUE
                                            "Key Not Exists".
           03  FILLER                       PIC 9(02)  VALUE 24.
           03  FILLER                       PIC X(25)  VALUE
                                            "Key Boundary violation".
           03  FILLER                       PIC 9(02)  VALUE 30.
           03  FILLER                       PIC X(25)  VALUE
                                            "Permanent Error".
           03  FILLER                       PIC 9(02)  VALUE 31.
           03  FILLER                       PIC X(25)  VALUE
                                            "Inconsistent Filename".
           03  FILLER                       PIC 9(02)  VALUE 34.
           03  FILLER                       PIC X(25)  VALUE
                                            "Boundary Violation".
           03  FILLER                       PIC 9(02)  VALUE 35.
           03  FILLER                       PIC X(25)  VALUE 
                                            "File Not Found".
           03  FILLER                       PIC 9(02)  VALUE 37.
           03  FILLER                       PIC X(25)  VALUE
                                            "Permission Denied".
           03  FILLER                       PIC 9(02)  VALUE 38.
           03  FILLER                       PIC X(25)  VALUE
                                            "Closed With Lock".
           03  FILLER                       PIC 9(02)  VALUE 39.
           03  FILLER                       PIC X(25)  VALUE
                                            "Conflict Attribute".
           03  FILLER                       PIC 9(02)  VALUE 41.
           03  FILLER                       PIC X(25)  VALUE
                                            "Already Open".
           03  FILLER                       PIC 9(02)  VALUE 42.
           03  FILLER                       PIC X(25)  VALUE 
                                            "Not Open".
           03  FILLER                       PIC 9(02)  VALUE 43.
           03  FILLER                       PIC X(25)  VALUE
                                            "Read Not Done".
           03  FILLER                       PIC 9(02)  VALUE 44.
           03  FILLER                       PIC X(25)  VALUE
                                            "Record Overflow".
           03  FILLER                       PIC 9(02)  VALUE 46.
           03  FILLER                       PIC X(25)  VALUE
                                            "Read Error".
           03  FILLER                       PIC 9(02)  VALUE 47.
           03  FILLER                       PIC X(25)  VALUE 
                                            "Input Denied".
           03  FILLER                       PIC 9(02)  VALUE 48.
           03  FILLER                       PIC X(25)  VALUE
                                            "Output Denied".
           03  FILLER                       PIC 9(02)  VALUE 49.
           03  FILLER                       PIC X(25)  VALUE
                                            "I/O Denied".
           03  FILLER                       PIC 9(02)  VALUE 51.
           03  FILLER                       PIC X(25)  VALUE
                                            "Record Locked".
           03  FILLER                       PIC 9(02)  VALUE 52.
           03  FILLER                       PIC X(25)  VALUE 
                                            "End-Of-Page".
           03  FILLER                       PIC 9(02)  VALUE 57.
           03  FILLER                       PIC X(25)  VALUE
                                            "I/O Linage".
           03  FILLER                       PIC 9(02)  VALUE 61.
           03  FILLER                       PIC X(25)  VALUE
                                            "File Sharing Failure".
           03  FILLER                       PIC 9(02)  VALUE 71.
           03  FILLER                       PIC X(25)  VALUE
                                            "Bad Character".
           03  FILLER                       PIC 9(02)  VALUE 91.
           03  FILLER                       PIC X(25)  VALUE 
                                            "File Not Available".

       01  ws-error-status-code-table-RED 
           REDEFINES ws-error-status-code-table.
           03  ws-error-status-code-table-OC   OCCURS cte-34 TIMES
               ASCENDING KEY ws-error-status-code-table-code-error
               INDEXED BY idx-error-status-code-table.
               05  ws-error-status-code-table-code-error PIC 9(02).
               05  ws-error-status-code-table-desc-error PIC X(25).
 
       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON idxfile.

       000100-search-for-error-and-description-codes.
            INITIALIZE ws-error-status-code-table-aux
            MOVE fs-idxfile TO ws-error-status-code-table-code-error-aux

            SET idx-error-status-code-table              TO cte-01
            SEARCH ALL ws-error-status-code-table-OC
                AT END
                   MOVE ws-error-status-code-table-desc-error-tag
                     TO ws-error-status-code-table-desc-error-aux
                   
              WHEN ws-error-status-code-table-code-error
                  (idx-error-status-code-table) IS EQUAL TO
                   ws-error-status-code-table-code-error-aux
                   MOVE ws-error-status-code-table-OC
                       (idx-error-status-code-table)
                     TO ws-error-status-code-table-aux

            END-SEARCH.

       000200-check-file-status-code.
           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+---+----+"
           DISPLAY "|      File status information.     |"
           DISPLAY "+---+----+---+----+---+----+---+----+"
           DISPLAY "| " asterisk " File Name      : [" 
                                   ws-idxfile-name "]."
           DISPLAY "| " asterisk " Operation      : ["
                                   ws-operation-class "]."
           DISPLAY "| " asterisk " Position Index : ["
                   idx-error-status-code-table "]."
           DISPLAY "| " asterisk " Status Code    : ["
                   ws-error-status-code-table-code-error-aux "]."
           DISPLAY "| " asterisk " Description    : "
           DISPLAY "| -> ["
                   ws-error-status-code-table-desc-error-aux
                   "] <-"
           DISPLAY "+---+----+---+----+---+----+---+----+".

       000300-preliminary-review-employee-code-contents.
            DISPLAY SPACE
            DISPLAY "|-> "
                    asterisk asterisk asterisk
                    " Information on the last record processed "
                    "and reached... "
                    asterisk asterisk asterisk
                    " <-|"         
            DISPLAY asterisk asterisk asterisk
                    " Code   Employee: ["
                    ws-f-idxfile-rec-cod-employee
                    "] = ["
                    f-idxfile-rec-cod-employee
                    "]. "
                    asterisk asterisk asterisk.

       000400-preliminary-review-employee-salary-contents.
            DISPLAY SPACE
            DISPLAY "|-> "
                    asterisk asterisk asterisk
                    " Information on the last record processed "
                    "and reached... "
                    asterisk asterisk asterisk
                    " <-|"         
            DISPLAY asterisk asterisk asterisk
                    " Salary Employee: ["
                    ws-f-idxfile-rec-salary-employee-ed
                    "] = ["
                    f-idxfile-rec-salary-employee
                    "]. "
                    asterisk asterisk asterisk.

       000500-press-enter-key-to-continue.
           DISPLAY "Press the ENTER key to continue..."
              WITH NO ADVANCING
            ACCEPT OMITTED.

       END DECLARATIVES.

       MAIN-PARAGRAPH.
           PERFORM 100000-start-begin-program
              THRU 100000-finish-begin-program

           IF fs-idxfile IS EQUAL TO ZEROES
              PERFORM 200000-start-process-menu
                 THRU 200000-finish-process-menu
                UNTIL sw-menu-option-exit
           END-IF

           PERFORM 300000-start-end-program
              THRU 300000-finish-end-program

           STOP "This program has ended..."
           STOP RUN.

       100000-start-begin-program.
           DISPLAY "+---+----+---+----+---+----+---+"
           DISPLAY "|   Indexed Sequential Files.  |"
           DISPLAY "+---+----+---+----+---+----+---+"
           DISPLAY asterisk " Enter the file name: " WITH NO ADVANCING
            ACCEPT ws-idxfile-name

           DISPLAY "Idx File to work on: [" ws-idxfile-name "]."

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
           DISPLAY "+===+====+===+====+===+====+===+"
           DISPLAY "|   Main Indexed Maintenance.  |"
           DISPLAY "+===+====+===+====+===+====+===+"
           DISPLAY "| [1]. Add a record.           |"
           DISPLAY "| [2]. Delete a record.        |"
           DISPLAY "| [3]. Modify a record...      |"
           DISPLAY "| [4]. Look for a record...    |"
           DISPLAY "| [5]. Look for all records... |"
           DISPLAY "| [6]. Exit this program.      |"
           DISPLAY "+===+====+===+====+===+====+===+"
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
                      UNTIL sw-menu-mode-modify-emp-exitmenu

               WHEN sw-menu-option-look-for-one
                    PERFORM 224000-start-look-for-any-record
                       THRU 224000-finish-look-for-any-record
                      UNTIL sw-menu-mode-read-dir-keyacc-exitmenu

               WHEN sw-menu-option-look-for-all
                    PERFORM 225000-start-look-for-all-records
                       THRU 225000-finish-look-for-all-records
                      UNTIL sw-menu-mode-read-option-exitmenu
                        
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

           IF (sw-idxfile-record-found-N) THEN
               PERFORM 221300-start-continue-carry-out-oper
                  THRU 221300-finish-continue-carry-out-oper

               IF (sw-carry-out-sure-Y)  THEN
                   PERFORM 221400-start-capture-salary-employee
                      THRU 221400-finish-capture-salary-employee

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

            PERFORM 000300-preliminary-review-employee-code-contents

            READ idxfile RECORD          INTO ws-f-idxfile-rec
             KEY IS f-idxfile-rec-cod-employee
                 INVALID KEY
                         SET sw-idxfile-record-found-N TO TRUE
                         DISPLAY "Record Not Found!"
                         PERFORM 000500-press-enter-key-to-continue

             NOT INVALID KEY
                         ADD cte-01        TO ws-reading-records
                         SET sw-idxfile-record-found-Y TO TRUE
                         DISPLAY "Record found successfully!"
                         PERFORM 000500-press-enter-key-to-continue
                         PERFORM 221210-start-show-file-info
                            THRU 221210-finish-show-file-info

            END-READ.
         221200-finish-look-for-a-record.
            EXIT.

         221210-start-show-file-info.
            MOVE ws-f-idxfile-rec-salary-employee
              TO ws-f-idxfile-rec-salary-employee-ed

            DISPLAY SPACE
            DISPLAY "+---+----+---+----+---+----+---+"
            DISPLAY "|     Employee Information.    |"
            DISPLAY "+---+----+---+----+---+----+---+"
            DISPLAY "| Code   : ["
                    ws-f-idxfile-rec-cod-employee "]."
            DISPLAY "| Salary : ["
                    ws-f-idxfile-rec-salary-employee-ed "]."
            DISPLAY "+---+----+---+----+---+----+---+"

            PERFORM 000500-press-enter-key-to-continue

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

         221400-start-capture-salary-employee.
           DISPLAY asterisk " Employee Salary : " WITH NO ADVANCING
           ACCEPT ws-f-idxfile-rec-salary-employee

           MOVE ws-f-idxfile-rec-salary-employee
             TO ws-f-idxfile-rec-salary-employee-ed
                f-idxfile-rec-salary-employee.
         221400-finish-capture-salary-employee.
            EXIT.

         221500-start-store-a-record.
            SET sw-operation-class-WRITE   TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents

            WRITE f-idxfile-rec          FROM ws-f-idxfile-rec
                  INVALID KEY
                          DISPLAY "Duplicate Key!"
                          PERFORM 000500-press-enter-key-to-continue

              NOT INVALID KEY
                          ADD cte-01       TO ws-written-records
                          DISPLAY "Record saved successfully!"
                          PERFORM 000500-press-enter-key-to-continue

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

           IF (sw-idxfile-record-found-Y) THEN
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

            PERFORM 000300-preliminary-review-employee-code-contents

            DELETE idxfile RECORD
                   INVALID KEY
                           DISPLAY "Invalid Key!"
                           PERFORM 000500-press-enter-key-to-continue

               NOT INVALID KEY
                           ADD cte-01      TO ws-eliminated-records
                           DISPLAY "Record deleted successfully!"
                           PERFORM 000500-press-enter-key-to-continue

            END-DELETE.
         222100-finish-eliminate-a-record.
            EXIT.

         223000-start-modify-a-record.
            PERFORM 223100-start-show-menu-modify-fields
               THRU 223100-finish-show-menu-modify-fields

            PERFORM 223200-start-validate-menu-modify-fields
               THRU 223200-finish-validate-menu-modify-fields.
         223000-finish-modify-a-record.
            EXIT.

         223100-start-show-menu-modify-fields.
            DISPLAY SPACE
            DISPLAY "+===+====+===+====+===+====+===+"
            DISPLAY "|   Modifying record fields.   |"
            DISPLAY "+===+====+===+====+===+====+===+"
            DISPLAY "| [1]. Salary Employee.        |"
            DISPLAY "| [2]. Return to main menu.    |"
            DISPLAY "+===+====+===+====+===+====+===+"
            DISPLAY "Enter your choice: " WITH NO ADVANCING
             ACCEPT ws-menu-mode-modify-option

            DISPLAY "The chosen option was: "
                    ws-menu-mode-modify-option.
         223100-finish-show-menu-modify-fields.
            EXIT.

         223200-start-validate-menu-modify-fields.
            IF NOT(sw-menu-mode-modify-emp-exitmenu)
               PERFORM 221100-start-capture-key-field
                  THRU 221100-finish-capture-key-field

               PERFORM 221200-start-look-for-a-record
                  THRU 221200-finish-look-for-a-record
            END-IF

            EVALUATE TRUE
                WHEN sw-menu-mode-modify-emp-salary
                     PERFORM 223210-start-modify-salary-employee
                        THRU 223210-finish-modify-salary-employee

                WHEN sw-menu-mode-modify-emp-exitmenu
                     DISPLAY "Exiting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. Please change your "
                             "option."

            END-EVALUATE.
         223200-finish-validate-menu-modify-fields.
            EXIT.

          223210-start-modify-salary-employee.
            IF (sw-idxfile-record-found-Y) THEN
                PERFORM 221300-start-continue-carry-out-oper
                   THRU 221300-finish-continue-carry-out-oper

                IF (sw-carry-out-sure-Y)   THEN
                    PERFORM 221400-start-capture-salary-employee
                       THRU 221400-finish-capture-salary-employee

                    PERFORM 223211-start-change-a-record
                       THRU 223211-finish-change-a-record
                ELSE
                    DISPLAY "Operation not performed. "
                            "File unchanged."
                END-IF
            END-IF.
          223210-finish-modify-salary-employee.
            EXIT.

          223211-start-change-a-record.
            SET sw-operation-class-REWRITE TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents

            REWRITE f-idxfile-rec        FROM ws-f-idxfile-rec
                    INVALID KEY
                            DISPLAY "Invalid Key!"
                            PERFORM 000500-press-enter-key-to-continue

                NOT INVALID KEY
                            ADD cte-01     TO ws-rewritten-records
                            DISPLAY "Record changed successfully!"
                            PERFORM 000500-press-enter-key-to-continue

            END-REWRITE.
          223211-finish-change-a-record.
            EXIT.

         224000-start-look-for-any-record.
           INITIALIZE ws-menu-mode-read-direct-option

           PERFORM 224100-start-show-menu-look-for-keyaccess
              THRU 224100-finish-show-menu-look-for-keyaccess

           PERFORM 224200-start-show-validate-menu-look-for-keyaccess
              THRU 224200-finish-show-validate-menu-look-for-keyaccess.
         224000-finish-look-for-any-record.
           EXIT.

          224100-start-show-menu-look-for-keyaccess.
           DISPLAY SPACE
           DISPLAY "+===+====+===+====+===+====+===+"
           DISPLAY "|     Look for any record.     |"
           DISPLAY "+===+====+===+====+===+====+===+"
           DISPLAY "| [1]. For Code Employee...    |"
           DISPLAY "| [2]. For Salary Employee...  |"
           DISPLAY "| [3]. Return to main menu.    |"
           DISPLAY "+===+====+===+====+===+====+===+"
           DISPLAY "Enter your choice: " WITH NO ADVANCING
            ACCEPT ws-menu-mode-read-direct-keyaccess

           DISPLAY "The chosen option was: "
                   ws-menu-mode-read-direct-keyaccess.
          224100-finish-show-menu-look-for-keyaccess.
            EXIT.

          224200-start-show-validate-menu-look-for-keyaccess.
            EVALUATE TRUE
                WHEN sw-menu-mode-read-dir-keyacc-code
                     PERFORM 224210-start-show-mode-look-for-code
                        THRU 224210-finish-show-mode-look-for-code
                       UNTIL sw-menu-mode-read-dir-exitmenu
 
                WHEN sw-menu-mode-read-dir-keyacc-salary
                     PERFORM 224220-start-show-mode-look-for-rc-sal
                        THRU 224220-finish-show-mode-look-for-rc-sal
                       UNTIL sw-menu-mode-read-dir-exitmenu

                WHEN sw-menu-mode-read-dir-keyacc-exitmenu
                     DISPLAY "Quitting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. "
                             "Please correct your choice."

           END-EVALUATE.
          224200-finish-show-validate-menu-look-for-keyaccess.
            EXIT.

          224210-start-show-mode-look-for-code.
            INITIALIZE ws-continue-response

            PERFORM 224211-start-show-reading-direct-menu
               THRU 224211-finish-show-reading-direct-menu

            PERFORM 224212-start-validate-reading-direct-menu
               THRU 224212-finish-validate-reading-direct-menu.
          224210-finish-show-mode-look-for-code.
            EXIT.

          224211-start-show-reading-direct-menu.
            DISPLAY SPACE
            DISPLAY "+===+====+===+====+===+====+===+"
            DISPLAY "| +Code Direct Reading Menu.+  |"
            DISPLAY "+===+====+===+====+===+====+===+"
            DISPLAY "| [1]. Locate directly.        |"
            DISPLAY "| [2]. Locate sequentially.    |"
            DISPLAY "| [3]. Exit this menu.         |"
            DISPLAY "+===+====+===+====+===+====+===+"
            DISPLAY "Enter your choice: " WITH NO ADVANCING
             ACCEPT ws-menu-mode-read-direct-option

            DISPLAY "The chosen option was: "
                   ws-menu-mode-read-direct-option.
          224211-finish-show-reading-direct-menu.
            EXIT.

          224212-start-validate-reading-direct-menu.
            EVALUATE TRUE
                WHEN sw-menu-mode-read-direct-read
                     PERFORM 2242121-start-routine-mode-read-direct-rd
                        THRU 2242121-finish-routine-mode-read-direct-rd
                       UNTIL sw-continue-response-N

                WHEN sw-menu-mode-read-dir-and-seq
                     PERFORM 2242122-start-routine-mode-read-dir-seq
                        THRU 2242122-finish-routine-mode-read-dir-seq
                       UNTIL sw-continue-response-N

                WHEN sw-menu-mode-read-dir-exitmenu
                     DISPLAY "Exiting this menu..." 

                WHEN OTHER
                     DISPLAY "Invalid option. "
                             "Please correct your choice."

            END-EVALUATE.
          224212-finish-validate-reading-direct-menu.
            EXIT.

          2242121-start-routine-mode-read-direct-rd.
            PERFORM 221100-start-capture-key-field
               THRU 221100-finish-capture-key-field

            PERFORM 221200-start-look-for-a-record
               THRU 221200-finish-look-for-a-record

            PERFORM 221600-start-continue-operation
               THRU 221600-finish-continue-operation.
          2242121-finish-routine-mode-read-direct-rd.
            EXIT.

          2242122-start-routine-mode-read-dir-seq.
            PERFORM 221100-start-capture-key-field
               THRU 221100-finish-capture-key-field

            PERFORM 225220-start-menu-mode-read-position-eq
               THRU 225220-finish-menu-mode-read-position-eq

            IF (sw-idxfile-record-found-Y)
                PERFORM 225260-start-menu-mode-read-forwarding
                   THRU 225260-finish-menu-mode-read-forwarding
            END-IF

            PERFORM 221600-start-continue-operation
               THRU 221600-finish-continue-operation.
          2242122-finish-routine-mode-read-dir-seq.
            EXIT.

          224220-start-show-mode-look-for-rc-sal.
            INITIALIZE ws-continue-response
                       ws-idxfile-record-found

            PERFORM 224221-start-show-mode-look-for-rcsal
               THRU 224221-finish-show-mode-look-for-rcsal

            PERFORM 224222-start-validate-mode-look-for-rcsal
               THRU 224222-finish-validate-mode-look-for-rcsal.
          224220-finish-show-mode-look-for-rc-sal.
            EXIT.

          224221-start-show-mode-look-for-rcsal.
            DISPLAY SPACE
            DISPLAY "+===+====+===+====+===+====+===+"
            DISPLAY "| +Salary Direct Reading Menu.+|"
            DISPLAY "+===+====+===+====+===+====+===+"
            DISPLAY "| [1]. Locate directly.        |"
            DISPLAY "| [2]. Locate sequentially.    |"
            DISPLAY "| [3]. Exit this menu.         |"
            DISPLAY "+===+====+===+====+===+====+===+"
            DISPLAY "Enter your choice: " WITH NO ADVANCING
             ACCEPT ws-menu-mode-read-direct-option

            DISPLAY "The chosen option was: "
                   ws-menu-mode-read-direct-option.
          224221-finish-show-mode-look-for-rcsal.
            EXIT.

          224222-start-validate-mode-look-for-rcsal.
            EVALUATE TRUE
                WHEN sw-menu-mode-read-direct-read
                     PERFORM 2242221-start-routine-mode-read-direct-sal
                        THRU 2242221-finish-routine-mode-read-direct-sal
                       UNTIL sw-continue-response-N

                WHEN sw-menu-mode-read-dir-and-seq
                     PERFORM 2242222-start-routine-mode-read-dirseq-sal
                        THRU 2242222-finish-routine-mode-read-dirseq-sal
                       UNTIL sw-continue-response-N

                WHEN sw-menu-mode-read-dir-exitmenu
                     DISPLAY "Exiting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. "
                             "Please correct your choice."

            END-EVALUATE.
          224222-finish-validate-mode-look-for-rcsal.
            EXIT.

          2242221-start-routine-mode-read-direct-sal.
            PERFORM 221400-start-capture-salary-employee
               THRU 221400-finish-capture-salary-employee

            PERFORM 22422211-start-read-record-salary-employee
               THRU 22422211-finish-read-record-salary-employee

            PERFORM 221600-start-continue-operation
               THRU 221600-finish-continue-operation.
          2242221-finish-routine-mode-read-direct-sal.
            EXIT.

          22422211-start-read-record-salary-employee.
            SET sw-operation-class-READ    TO TRUE

            PERFORM 000400-preliminary-review-employee-salary-contents

            READ idxfile RECORD          INTO ws-f-idxfile-rec
             KEY IS f-idxfile-rec-salary-employee
                 INVALID KEY
                         SET sw-idxfile-record-found-N TO TRUE
                         DISPLAY "Record Not Found!"
                         PERFORM 000500-press-enter-key-to-continue

             NOT INVALID KEY
                         ADD cte-01        TO ws-reading-records
                         SET sw-idxfile-record-found-Y TO TRUE
                         DISPLAY "Record found successfully!"
                         PERFORM 000500-press-enter-key-to-continue
                         PERFORM 221210-start-show-file-info
                            THRU 221210-finish-show-file-info

            END-READ.
          22422211-finish-read-record-salary-employee.
            EXIT.

          2242222-start-routine-mode-read-dirseq-sal.
            PERFORM 221400-start-capture-salary-employee
               THRU 221400-finish-capture-salary-employee

            PERFORM 22422221-start-routine-mode-locate-for-sal
               THRU 22422221-finish-routine-mode-locate-for-sal

            IF (sw-idxfile-record-found-Y)
                PERFORM 225260-start-menu-mode-read-forwarding
                   THRU 225260-finish-menu-mode-read-forwarding
            END-IF

            PERFORM 221600-start-continue-operation
               THRU 221600-finish-continue-operation.
          2242222-finish-routine-mode-read-dirseq-sal.
            EXIT.

          22422221-start-routine-mode-locate-for-sal.
            SET sw-operation-class-STARTEQ       TO TRUE

            PERFORM 000400-preliminary-review-employee-salary-contents

            START idxfile
              KEY IS EQUAL TO f-idxfile-rec-salary-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary could not be located for an "
                          "exactly equal or identical value from the "
                          "existing ones."
                  SET sw-idxfile-record-found-N  TO TRUE
                  PERFORM 000500-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD cte-01              TO ws-repositioning-records
                  SET sw-idxfile-record-found-Y  TO TRUE

                  DISPLAY asterisk
                          "The salary: ["
                           ws-f-idxfile-rec-salary-employee
                          "] was found for a value that was exactly the"
                          " same or identical to the existing ones: "
                          "[" f-idxfile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning exact salary done correctly!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          22422221-finish-routine-mode-locate-for-sal.
            EXIT.

         225000-start-look-for-all-records.
           INITIALIZE ws-realization-questions
                      ws-idxfile-record-found

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
            DISPLAY "| [03]. At on a aproximate key...   |"
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
            DISPLAY "|     Record by record reading.     |"
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
            INITIALIZE ws-menu-mode-read-option-givenkey
                       ws-idxfile-EOF

            EVALUATE TRUE
                WHEN sw-menu-mode-read-option-start
                     PERFORM 225210-start-menu-mode-start-position
                        THRU 225210-finish-menu-mode-start-position

                WHEN sw-menu-mode-read-option-givenkey-eq
                     PERFORM 221100-start-capture-key-field
                        THRU 221100-finish-capture-key-field
                     PERFORM 225220-start-menu-mode-read-position-eq
                        THRU 225220-finish-menu-mode-read-position-eq

                WHEN sw-menu-mode-read-option-givenkey-apprx
                     PERFORM 225230-start-menu-read-position-apprx
                        THRU 225230-start-menu-read-position-apprx
                       UNTIL sw-menu-mode-read-option-givenkey-exit

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

                WHEN sw-menu-mode-read-option-exitmenu 
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
                          PERFORM 000500-press-enter-key-to-continue

              NOT INVALID KEY
                          ADD  cte-01      TO ws-repositioning-records
                          DISPLAY asterisk
                                  "Positioning at the begin."
                                  asterisk
                          PERFORM 000500-press-enter-key-to-continue

            END-START.
          225210-finish-menu-mode-start-position.
            EXIT.

          225220-start-menu-mode-read-position-eq.
            SET sw-operation-class-STARTEQ       TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents

            START idxfile
              KEY IS EQUAL TO f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY "The value could not be located for an "
                          "exactly equal or identical key from the "
                          "existing ones."
                  SET sw-idxfile-record-found-N  TO TRUE
                  PERFORM 000500-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD cte-01              TO ws-repositioning-records
                  SET sw-idxfile-record-found-Y  TO TRUE

                  DISPLAY asterisk
                          "The value: [" ws-f-idxfile-rec-cod-employee
                          "] was found for a key that was exactly the "
                          "same or identical to the existing ones: "
                          "[" f-idxfile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning exact key done correctly!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          225220-finish-menu-mode-read-position-eq.
            EXIT.

          225230-start-menu-read-position-apprx.
            PERFORM 225231-start-show-approximate-offset-menu
               THRU 225231-finish-show-approximate-offset-menu

            PERFORM 225232-start-validate-approximate-offset-menu
               THRU 225232-finish-validate-approximate-offset-menu.
          225230-finish-menu-read-position-apprx.
            EXIT.

          225231-start-show-approximate-offset-menu.
            DISPLAY SPACE
            DISPLAY "+===+====+===+====+===+====+===+===+===+"
            DISPLAY "|       Approximate Key Locator.       |"
            DISPLAY "+===+====+===+====+===+====+===+===+===+"
            DISPLAY "| [1]. Key greater than value.         |"
            DISPLAY "| [2]. Key greater than or equal value.|"
            DISPLAY "| [3]. Key less than value.            |"
            DISPLAY "| [4]. Key less than or equal to value.|"
            DISPLAY "| [5]. Exit this menu.                 |"
            DISPLAY "+===+====+===+====+===+====+===+===+===+"
            DISPLAY "Enter your choice: " WITH NO ADVANCING
             ACCEPT ws-menu-mode-read-option-givenkey

            DISPLAY "The chosen option was: "
                    ws-menu-mode-read-option-givenkey.
          225231-finish-show-approximate-offset-menu.
            EXIT.

          225232-start-validate-approximate-offset-menu.
            EVALUATE TRUE
                WHEN sw-menu-mode-read-option-givenkey-gt
                     PERFORM 221100-start-capture-key-field
                        THRU 221100-finish-capture-key-field
                     PERFORM 2252321-start-menu-mode-read-position-gt
                        THRU 2252321-finish-menu-mode-read-position-gt

                WHEN sw-menu-mode-read-option-givenkey-gteq
                     PERFORM 221100-start-capture-key-field
                        THRU 221100-finish-capture-key-field
                     PERFORM 2252322-start-menu-mode-read-position-gteq
                        THRU 2252322-finish-menu-mode-read-position-gteq

                WHEN sw-menu-mode-read-option-givenkey-lt
                     PERFORM 221100-start-capture-key-field
                        THRU 221100-finish-capture-key-field
                     PERFORM 2252323-start-menu-mode-read-position-lt
                        THRU 2252323-finish-menu-mode-read-position-lt

                WHEN sw-menu-mode-read-option-givenkey-lteq
                     PERFORM 221100-start-capture-key-field
                        THRU 221100-finish-capture-key-field
                     PERFORM 2252324-start-menu-mode-read-position-lteq
                        THRU 2252324-finish-menu-mode-read-position-lteq

                WHEN sw-menu-mode-read-option-givenkey-exit
                     DISPLAY "Quitting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. "
                             "Please change your option."

            END-EVALUATE.
          225232-finish-validate-approximate-offset-menu.
            EXIT.

          2252321-start-menu-mode-read-position-gt.
            SET sw-operation-class-STARTGT TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents.

            START idxfile
              KEY IS GREATER THAN f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY "The value could not be located for a key "
                          "greater than one of those existing."
                  PERFORM 000500-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records

                  DISPLAY asterisk
                          "The value: [" ws-f-idxfile-rec-cod-employee
                          "] was found for a key that was "
                          "greater than one of the existing ones: "
                          "[" f-idxfile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Correct positioning on nearest upper key!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          2252321-finish-menu-mode-read-position-gt.
            EXIT.

          2252322-start-menu-mode-read-position-gteq.
            SET sw-operation-class-STARTGTEQ  TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents.

            START idxfile
              KEY IS GREATER THAN OR EQUAL TO f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY "The value could not be located for a key "
                          "greater than or equal to those existing."
                  PERFORM 000500-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records
                  DISPLAY asterisk
                          "The value: [" ws-f-idxfile-rec-cod-employee
                          "] was found for a key that was greater than "
                          "or equal to one of the existing ones: "
                          "[" f-idxfile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Correct positioning on nearest upper key!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          2252322-finish-menu-mode-read-position-gteq.
            EXIT.

          2252323-start-menu-mode-read-position-lt.
            SET sw-operation-class-STARTLT    TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents

            START idxfile
              KEY IS LESS THAN f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY "The value could not be located for a key "
                          "less than one of those existing."
                  PERFORM 000500-press-enter-key-to-continue
                  PERFORM 225240-start-menu-mode-finish-position
                     THRU 225240-finish-menu-mode-finish-position

             NOT INVALID KEY
                  DISPLAY asterisk
                          "The value: [" ws-f-idxfile-rec-cod-employee
                          "] was found for a key that was less than "
                          "one of the existing ones: "
                          "[" f-idxfile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Correct positioning on nearest lower key!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          2252323-finish-menu-mode-read-position-lt.
            EXIT.

          2252324-start-menu-mode-read-position-lteq.
            SET sw-operation-class-STARTLTEQ  TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents

            START idxfile
              KEY IS LESS THAN OR EQUAL TO f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY "The value could not be located for a key "
                          "less than or equal to those existing."
                  PERFORM 000500-press-enter-key-to-continue
                  PERFORM 225240-start-menu-mode-finish-position
                     THRU 225240-finish-menu-mode-finish-position

              NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records
                  DISPLAY asterisk
                          "The value: [" ws-f-idxfile-rec-cod-employee
                          "] was found for a key that was less than "
                          "or equal to one than of the existing ones: "
                          "[" f-idxfile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Correct positioning on nearest lower key!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          2252324-finish-menu-mode-read-position-lteq.
            EXIT.

          225240-start-menu-mode-finish-position.
            SET sw-operation-class-STARTLST   TO TRUE

            START idxfile LAST
                  INVALID KEY
                          DISPLAY "Error positioning at end!"
                          PERFORM 000500-press-enter-key-to-continue

              NOT INVALID KEY
                          ADD  cte-01      TO ws-repositioning-records
                          DISPLAY asterisk
                                  "Positioning at the end."
                                  asterisk
                          PERFORM 000500-press-enter-key-to-continue

            END-START.
          225240-finish-menu-mode-finish-position.
            EXIT.

          225250-start-menu-mode-read-backwarding.
            SET sw-operation-class-READPREV   TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents

            READ idxfile PREVIOUS RECORD    INTO ws-f-idxfile-rec
              AT END
                 SET sw-idxfile-EOF-Y         TO TRUE
                 DISPLAY "End of file!"
                 PERFORM 000500-press-enter-key-to-continue
                 PERFORM 225240-start-menu-mode-finish-position
                    THRU 225240-finish-menu-mode-finish-position

             NOT AT END
                 ADD cte-01                   TO ws-reading-records
                 SET sw-idxfile-EOF-N         TO TRUE
                 PERFORM 221210-start-show-file-info
                    THRU 221210-finish-show-file-info

            END-READ.
          225250-finish-menu-mode-read-backwarding.
            EXIT.

          225260-start-menu-mode-read-forwarding.
            SET sw-operation-class-READNEXT   TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents

            READ idxfile NEXT RECORD        INTO ws-f-idxfile-rec
              AT END
                 SET sw-idxfile-EOF-Y         TO TRUE
                 DISPLAY "End of file!"
                 PERFORM 000500-press-enter-key-to-continue
                 PERFORM 225210-start-menu-mode-start-position
                    THRU 225210-finish-menu-mode-start-position

             NOT AT END
                 ADD cte-01                   TO ws-reading-records
                 SET sw-idxfile-EOF-N         TO TRUE
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
