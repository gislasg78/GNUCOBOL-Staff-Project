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
       78  cte-34                                          VALUE 34.

       01  ws-environmental-variables.
           03  ws-current-date-and-time.
               05  ws-CDT-Year                  PIC 9(04)  VALUE ZEROES.
               05  ws-CDT-Month                 PIC 9(02)  VALUE ZEROES.
               05  ws-CDT-Day                   PIC 9(02)  VALUE ZEROES.
               05  ws-CDT-Hour                  PIC 9(02)  VALUE ZEROES.
               05  ws-CDT-Minutes               PIC 9(02)  VALUE ZEROES.
               05  ws-CDT-Seconds               PIC 9(02)  VALUE ZEROES.
               05  ws-CDT-Hundredths-Of-Secs    PIC 9(02)  VALUE ZEROES.
               05  ws-CDT-GMT-Diff-Hours        PIC S9(02) VALUE ZEROES
                                                SIGN  IS LEADING
                                                SEPARATE CHARACTER.
               05  ws-CDT-GMT-Diff-Minutes      PIC 9(02)  VALUE ZEROES.

           03  ws-f-idxfile-indicators.
               05  ws-f-error-status-code-table-aux.
                       07  ws-f-error-status-code-table-code-error-aux
                                                PIC 9(02)  VALUE ZEROES.
                       07  ws-f-error-status-code-table-desc-error-aux
                                                PIC X(25)  VALUE SPACES.
               05  ws-f-idxfile-rec-salary-employee-ed
                                                    PIC   $-,---,--9.99
                                                           VALUE ZEROES.
               05  ws-idxfile-EOF               PIC A(01)  VALUE SPACE.
                   88  sw-idxfile-EOF-Y                    VALUE 'Y'.
                   88  sw-idxfile-EOF-N                    VALUE 'N'.
               05  ws-idxfile-record-found      PIC A(01)  VALUE SPACE.
                   88  sw-idxfile-record-found-N           VALUE 'N'.
                   88  sw-idxfile-record-found-Y           VALUE 'Y'.

           03  ws-f-idxfile-rec.
               05  ws-f-idxfile-rec-cod-employee           PIC 9(05)
                                                           VALUE ZEROES.
               05  ws-f-idxfile-rec-salary-employee
                                                PIC S9(06)V9(02)
                                                SIGN  IS LEADING
                                                SEPARATE CHARACTER
                                                           VALUE ZEROES.

           03  ws-menu-standard-options-performance.
               05  ws-menu-option               PIC 9(01)
                                                           VALUE ZERO.
                   88  sw-menu-option-add                  VALUE 1.
                   88  sw-menu-option-delete               VALUE 2.
                   88  sw-menu-option-modify               VALUE 3.
                   88  sw-menu-option-look-for-one         VALUE 4.
                   88  sw-menu-option-look-for-all         VALUE 5.
                   88  sw-menu-option-exit                 VALUE 6.
               05  ws-menu-mode-modify-option   PIC 9(01)
                                                           VALUE ZERO.
                   88  sw-menu-mode-modify-emp-salary      VALUE 1.
                   88  sw-menu-mode-modify-emp-exitmenu    VALUE 2.
               05  ws-menu-mode-read-direct-option
                                                PIC 9(01)  VALUE ZERO.
                   88  sw-menu-mode-read-direct-read       VALUE 1.
                   88  sw-menu-mode-read-dir-and-seq       VALUE 2.
                   88  sw-menu-mode-read-dir-exitmenu      VALUE 3.

               05  ws-menu-mode-read-direct-keyaccess 
                                                PIC 9(01)  VALUE ZERO.
                   88  sw-menu-mode-read-dir-keyacc-code   VALUE 1.
                   88  sw-menu-mode-read-dir-keyacc-salary VALUE 2.
                   88  sw-menu-mode-read-dir-keyacc-exitmenu
                                                           VALUE 3.
               05  ws-menu-mode-read-option     PIC 9(02)  VALUE ZEROES.
                   88  sw-menu-mode-read-option-start      VALUE 01.
                   88  sw-menu-mode-read-option-givenkey-eq
                                                           VALUE 02.
                   88  sw-menu-mode-read-option-givenkey-apprx
                                                           VALUE 03.
                   88  sw-menu-mode-read-option-finish     VALUE 04.
                   88  sw-menu-mode-read-option-r-first-rcrd
                                                           VALUE 05.
                   88  sw-menu-mode-read-option-r-last-rcrd
                                                           VALUE 06.
                   88  sw-menu-mode-read-option-r-backward VALUE 07.
                   88  sw-menu-mode-read-option-r-forward  VALUE 08.
                   88  sw-menu-mode-read-option-prev-rcrd  VALUE 09.
                   88  sw-menu-mode-read-option-next-rcrd  VALUE 10.
                   88  sw-menu-mode-read-option-exitmenu   VALUE 11.
               05  ws-menu-mode-read-option-givenkey
                                                PIC 9(01)  VALUE ZEROES.
                   88  sw-menu-mode-read-option-givenkey-ngt
                                                           VALUE 1.
                   88  sw-menu-mode-read-option-givenkey-gt
                                                           VALUE 2.
                   88  sw-menu-mode-read-option-givenkey-gteq
                                                           VALUE 3.
                   88  sw-menu-mode-read-option-givenkey-nlt
                                                           VALUE 4.
                   88  sw-menu-mode-read-option-givenkey-lt
                                                           VALUE 5.
                   88  sw-menu-mode-read-option-givenkey-lteq
                                                           VALUE 6.
                   88  sw-menu-mode-read-option-givenkey-exit
                                                           VALUE 7.
               05  ws-operation-class           PIC A(13)  VALUE SPACES.
                   88  sw-operation-class-CLOSE
                                                VALUE "CLOSE".
                   88  sw-operation-class-DELETE
                                                VALUE "DELETE".
                   88  sw-operation-class-OPEN  VALUE "OPEN".
                   88  sw-operation-class-READ  VALUE "READ".
                   88  sw-operation-class-READNEXT
                                                VALUE "READ NEXT".
                   88  sw-operation-class-READPREV
                                                VALUE "READ PREVIOUS".
                   88  sw-operation-class-REWRITE
                                                VALUE "REWRITE".
                   88  sw-operation-class-STARTEQ
                                                VALUE "START EQUAL".
                   88  sw-operation-class-STARTFRST
                                                VALUE "START FIRST".
                   88  sw-operation-class-STARTGT
                                                VALUE "START GREATER".
                   88  sw-operation-class-STARTGTEQ
                                                VALUE "START GTEQ".
                   88  sw-operation-class-STARTLST
                                                VALUE "START LAST".
                   88  sw-operation-class-STARTLT
                                                VALUE "START LESS".
                   88  sw-operation-class-STARTLTEQ
                                                VALUE "START LTEQ".
                   88  sw-operation-class-STARTNGT
                                                VALUE "START NOT GT".
                   88  sw-operation-class-STARTNLT
                                                VALUE "START NOT LT".
                   88  sw-operation-class-WRITE
                                                VALUE "WRITE".
           03  ws-realization-questions.
               05  ws-carry-out-sure            PIC A(01)  VALUE SPACE.
                   88  sw-carry-out-sure-Y      VALUES ARE 'Y', 'y'.
               05  ws-continue-response         PIC A(01)  VALUE SPACE.
                   88  sw-continue-response-N   VALUES ARE 'N', 'n'.

       01  ws-statistics-processed-records.
           03  ws-eliminated-records            PIC 9(04)  VALUE ZEROES.
           03  ws-reading-records               PIC 9(04)  VALUE ZEROES.
           03  ws-repositioning-records         PIC 9(04)  VALUE ZEROES.
           03  ws-rewritten-records             PIC 9(04)  VALUE ZEROES.
           03  ws-written-records               PIC 9(04)  VALUE ZEROES.

       01  ws-f-error-status-code-table.
           03  ws-status-code-success-completion.
               05  FILLER                       PIC 9(02)  VALUE ZEROES.
               05  FILLER                       PIC X(25)  VALUE
                   "Success Completion".
           03  ws-status-code-success-duplicate.
               05  FILLER                       PIC 9(02)  VALUE 02.
               05  FILLER                       PIC X(25)  VALUE
                   "Success Duplicate".
           03  ws-status-code-success-incomplete.
               05  FILLER                       PIC 9(02)  VALUE 04.
               05  FILLER                       PIC X(25)  VALUE
                   "Success Incomplete".
           03  ws-status-code-optional-missing.
               05  FILLER                       PIC 9(02)  VALUE 05.
               05  FILLER                       PIC X(25)  VALUE
                   "Success Optional, Missing".
           03  ws-status-code-multiple-records-ls.
               05  FILLER                       PIC 9(02)  VALUE 06.
               05  FILLER                       PIC X(25)  VALUE 
                   "Multiple Records LS".
           03  ws-status-code-success-no-unit.
               05  FILLER                       PIC 9(02)  VALUE 07.
               05  FILLER                       PIC X(25)  VALUE
                   "Success No Unit".
           03  ws-status-code-success-ls-bad-data.
               05  FILLER                       PIC 9(02)  VALUE 09.
               05  FILLER                       PIC X(25)  VALUE
                   "Success LS Bad Data".
           03  ws-status-code-end-of-file.
               05  FILLER                       PIC 9(02)  VALUE 10.
               05  FILLER                       PIC X(25)  VALUE
                   "End Of File".
           03  ws-status-code-out-of-key-range.
               05  FILLER                       PIC 9(02)  VALUE 14.
               05  FILLER                       PIC X(25)  VALUE 
                   "Out Of Key Range".
           03  ws-status-code-key-invalid.
               05  FILLER                       PIC 9(02)  VALUE 21.
               05  FILLER                       PIC X(25)  VALUE
                   "Key Invalid".
           03  ws-status-code-key-exists.
               05  FILLER                       PIC 9(02)  VALUE 22.
               05  FILLER                       PIC X(25)  VALUE
                   "Key Exists".
           03  ws-status-code-key-not-exists.
               05  FILLER                       PIC 9(02)  VALUE 23.
               05  FILLER                       PIC X(25)  VALUE
                   "Key Not Exists".
           03  ws-status-code-key-boundary-violation.
               05  FILLER                       PIC 9(02)  VALUE 24.
               05  FILLER                       PIC X(25)  VALUE
                   "Key Boundary violation".
           03  ws-status-code-permanent-error.
               05  FILLER                       PIC 9(02)  VALUE 30.
               05  FILLER                       PIC X(25)  VALUE
                   "Permanent Error".
           03  ws-status-code-inconsistent-filename.
               05  FILLER                       PIC 9(02)  VALUE 31.
               05  FILLER                       PIC X(25)  VALUE
                   "Inconsistent Filename".
           03  ws-status-code-boundary-violation.
               05  FILLER                       PIC 9(02)  VALUE 34.
               05  FILLER                       PIC X(25)  VALUE
                   "Boundary Violation".
           03  ws-status-code-file-not-found.
               05  FILLER                       PIC 9(02)  VALUE 35.
               05  FILLER                       PIC X(25)  VALUE 
                   "File Not Found".
           03  ws-status-code-permission-denied.
               05  FILLER                       PIC 9(02)  VALUE 37.
               05  FILLER                       PIC X(25)  VALUE
                   "Permission Denied".
           03  ws-status-code-closed-with-lock.
               05  FILLER                       PIC 9(02)  VALUE 38.
               05  FILLER                       PIC X(25)  VALUE
                   "Closed With Lock".
           03  ws-status-code-conflict-attribute.
               05  FILLER                       PIC 9(02)  VALUE 39.
               05  FILLER                       PIC X(25)  VALUE
                   "Conflict Attribute".
           03  ws-status-code-already-open.
               05  FILLER                       PIC 9(02)  VALUE 41.
               05  FILLER                       PIC X(25)  VALUE
                   "Already Open".
           03  ws-status-code-not-open.
               05  FILLER                       PIC 9(02)  VALUE 42.
               05  FILLER                       PIC X(25)  VALUE 
                   "Not Open".
           03  ws-status-code-read-not-done.
               05  FILLER                       PIC 9(02)  VALUE 43.
               05  FILLER                       PIC X(25)  VALUE
                   "Read Not Done".
           03  ws-status-code-record-overflow.
               05  FILLER                       PIC 9(02)  VALUE 44.
               05  FILLER                       PIC X(25)  VALUE
                   "Record Overflow".
           03  ws-status-code-read-error.
               05  FILLER                       PIC 9(02)  VALUE 46.
               05  FILLER                       PIC X(25)  VALUE
                   "Read Error".
           03  ws-status-code-input-denied.
               05  FILLER                       PIC 9(02)  VALUE 47.
               05  FILLER                       PIC X(25)  VALUE 
                   "Input Denied".
           03  ws-status-code-output-denied.
               05  FILLER                       PIC 9(02)  VALUE 48.
               05  FILLER                       PIC X(25)  VALUE
                   "Output Denied".
           03  ws-status-code-i-o-denied.
               05  FILLER                       PIC 9(02)  VALUE 49.
               05  FILLER                       PIC X(25)  VALUE
                   "I/O Denied".
           03  ws-status-code-record-locked.
               05  FILLER                       PIC 9(02)  VALUE 51.
               05  FILLER                       PIC X(25)  VALUE
                   "Record Locked".
           03  ws-status-code-end-of-page.
               05  FILLER                       PIC 9(02)  VALUE 52.
               05  FILLER                       PIC X(25)  VALUE 
                   "End-Of-Page".
           03  ws-status-code-i-o-linage.
               05  FILLER                       PIC 9(02)  VALUE 57.
               05  FILLER                       PIC X(25)  VALUE
                   "I/O Linage".
           03  ws-status-code-file-sharing-failure.
               05  FILLER                       PIC 9(02)  VALUE 61.
               05  FILLER                       PIC X(25)  VALUE
                   "File Sharing Failure".
           03  ws-status-code-bad-character.
               05  FILLER                       PIC 9(02)  VALUE 71.
               05  FILLER                       PIC X(25)  VALUE
                   "Bad Character".
           03  ws-status-code-file-not-available.
               05  FILLER                       PIC 9(02)  VALUE 91.
               05  FILLER                       PIC X(25)  VALUE 
                   "File Not Available".
       01  ws-f-error-status-code-table-RED 
           REDEFINES ws-f-error-status-code-table.
           03  ws-f-error-status-code-table-OC  OCCURS cte-34 TIMES
               ASCENDING KEY ws-f-error-status-code-table-code-error
               INDEXED BY idx-error-status-code-table.
               05  ws-f-error-status-code-table-code-error PIC 9(02).
               05  ws-f-error-status-code-table-desc-error PIC X(25).

       01  ws-f-error-status-code-table-desc-error-tag     PIC X(25)
               VALUE "Unknown File Status".

       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON idxfile.

       000100-search-for-error-and-description-codes.
            INITIALIZE ws-f-error-status-code-table-aux
            MOVE fs-idxfile
              TO ws-f-error-status-code-table-code-error-aux

            SET idx-error-status-code-table              TO cte-01
            SEARCH ALL ws-f-error-status-code-table-OC
                AT END
                   MOVE ws-f-error-status-code-table-desc-error-tag
                     TO ws-f-error-status-code-table-desc-error-aux
                   
              WHEN ws-f-error-status-code-table-code-error
                  (idx-error-status-code-table) IS EQUAL TO
                   ws-f-error-status-code-table-code-error-aux
                   MOVE ws-f-error-status-code-table-OC
                       (idx-error-status-code-table)
                     TO ws-f-error-status-code-table-aux

            END-SEARCH.

       000200-check-file-status-code.
           MOVE FUNCTION CURRENT-DATE TO ws-current-date-and-time

           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+---+----+"
           DISPLAY "|      File status information.     |"
           DISPLAY "+---+----+---+----+---+----+---+----+"
           DISPLAY "|      [" ws-current-date-and-time
                   "]      |"
           DISPLAY "+-----------------------------------+"
           DISPLAY "| " asterisk " File Name      : [" 
                                   ws-idxfile-name "]."
           DISPLAY "| " asterisk " Operation      : ["
                                   ws-operation-class "]."
           DISPLAY "| " asterisk " Position Index : ["
                   idx-error-status-code-table "]."
           DISPLAY "| " asterisk " Status Code    : ["
                   ws-f-error-status-code-table-code-error-aux "]."
           DISPLAY "| " asterisk " Description    : "
           DISPLAY "| -> ["
                   ws-f-error-status-code-table-desc-error-aux
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
                    " Code Employee: ["
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
           OPEN I-O idxfile

           DISPLAY "Opening. Status Code: [" fs-idxfile "].".
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
           INITIALIZE ws-f-idxfile-indicators
                      ws-menu-standard-options-performance
                      ws-realization-questions

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
           INITIALIZE ws-f-idxfile-indicators
                      ws-menu-standard-options-performance
                      ws-realization-questions

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
            INITIALIZE ws-f-idxfile-indicators
                       ws-menu-standard-options-performance
                       ws-realization-questions

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
           INITIALIZE ws-f-idxfile-indicators
                      ws-menu-standard-options-performance
                      ws-realization-questions

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
            INITIALIZE ws-f-idxfile-indicators
                       ws-menu-standard-options-performance
                       ws-realization-questions

            PERFORM 224211-start-show-reading-direct-menu
               THRU 224211-finish-show-reading-direct-menu

            PERFORM 224212-start-validate-reading-direct-menu
               THRU 224212-finish-validate-reading-direct-menu.
          224210-finish-show-mode-look-for-code.
            EXIT.

          224211-start-show-reading-direct-menu.
            DISPLAY SPACE
            DISPLAY "+===+====+===+====+===+====+===+"
            DISPLAY "| Direct & Sequential Reading. |"
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

            PERFORM 2252211-start-menu-mode-code-pos-eq
               THRU 2252211-finish-menu-mode-code-pos-eq

            IF (sw-idxfile-record-found-Y)
                PERFORM 225260-start-menu-mode-read-forwarding
                   THRU 225260-finish-menu-mode-read-forwarding
            END-IF

            PERFORM 221600-start-continue-operation
               THRU 221600-finish-continue-operation.
          2242122-finish-routine-mode-read-dir-seq.
            EXIT.

          224220-start-show-mode-look-for-rc-sal.
            INITIALIZE ws-f-idxfile-indicators
                       ws-menu-standard-options-performance
                       ws-realization-questions

            PERFORM 224211-start-show-reading-direct-menu
               THRU 224211-finish-show-reading-direct-menu

            PERFORM 224221-start-validate-mode-look-for-rcsal
               THRU 224221-finish-validate-mode-look-for-rcsal.
          224220-finish-show-mode-look-for-rc-sal.
            EXIT.

          224221-start-validate-mode-look-for-rcsal.
            EVALUATE TRUE
                WHEN sw-menu-mode-read-direct-read
                     PERFORM 2242211-start-routine-mode-read-direct-sal
                        THRU 2242211-finish-routine-mode-read-direct-sal
                       UNTIL sw-continue-response-N

                WHEN sw-menu-mode-read-dir-and-seq
                     PERFORM 2242212-start-routine-mode-read-dirseq-sal
                        THRU 2242212-finish-routine-mode-read-dirseq-sal
                       UNTIL sw-continue-response-N

                WHEN sw-menu-mode-read-dir-exitmenu
                     DISPLAY "Exiting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. "
                             "Please correct your choice."

            END-EVALUATE.
          224221-finish-validate-mode-look-for-rcsal.
            EXIT.

          2242211-start-routine-mode-read-direct-sal.
            PERFORM 221400-start-capture-salary-employee
               THRU 221400-finish-capture-salary-employee

            PERFORM 22422111-start-read-record-salary-employee
               THRU 22422111-finish-read-record-salary-employee

            PERFORM 221600-start-continue-operation
               THRU 221600-finish-continue-operation.
          2242211-finish-routine-mode-read-direct-sal.
            EXIT.

          22422111-start-read-record-salary-employee.
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
          22422111-finish-read-record-salary-employee.
            EXIT.

          2242212-start-routine-mode-read-dirseq-sal.
            PERFORM 221400-start-capture-salary-employee
               THRU 221400-finish-capture-salary-employee

            PERFORM 22422121-start-routine-mode-locate-for-sal
               THRU 22422121-finish-routine-mode-locate-for-sal

            IF (sw-idxfile-record-found-Y)
                PERFORM 225260-start-menu-mode-read-forwarding
                   THRU 225260-finish-menu-mode-read-forwarding
            END-IF

            PERFORM 221600-start-continue-operation
               THRU 221600-finish-continue-operation.
          2242212-finish-routine-mode-read-dirseq-sal.
            EXIT.

          22422121-start-routine-mode-locate-for-sal.
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
          22422121-finish-routine-mode-locate-for-sal.
            EXIT.

         225000-start-look-for-all-records.
           INITIALIZE ws-f-idxfile-indicators
                      ws-menu-standard-options-performance
                      ws-realization-questions

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
            DISPLAY "| [02]. At on a exact & given key...|"
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
            EVALUATE TRUE
                WHEN sw-menu-mode-read-option-start
                     PERFORM 225210-start-menu-mode-start-position
                        THRU 225210-finish-menu-mode-start-position

                WHEN sw-menu-mode-read-option-givenkey-eq
                     PERFORM 225220-start-locate-givenkey-code-salary
                        THRU 225220-finish-locate-givenkey-code-salary
                       UNTIL sw-menu-mode-read-dir-keyacc-exitmenu

                WHEN sw-menu-mode-read-option-givenkey-apprx
                     PERFORM 225230-start-menu-read-approx-code-salary
                        THRU 225230-finish-menu-read-approx-code-salary
                       UNTIL sw-menu-mode-read-dir-keyacc-exitmenu

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
                          DISPLAY asterisk asterisk
                                  "Error positioning at begin!"
                                  asterisk asterisk
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

          225220-start-locate-givenkey-code-salary.
            INITIALIZE ws-f-idxfile-indicators
                       ws-menu-standard-options-performance
                       ws-realization-questions

            PERFORM 224100-start-show-menu-look-for-keyaccess
               THRU 224100-finish-show-menu-look-for-keyaccess

            PERFORM 225221-start-validate-menu-locate-givenkey-code-sl
               THRU 225221-finish-validate-menu-locate-givenkey-code-sl.
          225220-finish-locate-givenkey-code-salary.
            EXIT.

          225221-start-validate-menu-locate-givenkey-code-sl.
            EVALUATE TRUE
                WHEN sw-menu-mode-read-dir-keyacc-code
                     PERFORM 221100-start-capture-key-field
                        THRU 221100-finish-capture-key-field
                     PERFORM 2252211-start-menu-mode-code-pos-eq
                        THRU 2252211-finish-menu-mode-code-pos-eq

                WHEN sw-menu-mode-read-dir-keyacc-salary
                     PERFORM 221400-start-capture-salary-employee
                        THRU 221400-finish-capture-salary-employee
                     PERFORM 22422121-start-routine-mode-locate-for-sal
                        THRU 22422121-finish-routine-mode-locate-for-sal

                WHEN sw-menu-mode-read-dir-keyacc-exitmenu
                     DISPLAY "Quitting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. Please correct and change"
                             "your option..."

            END-EVALUATE.
          225221-finish-validate-menu-locate-givenkey-code-sl.
            EXIT.

          2252211-start-menu-mode-code-pos-eq.
            SET sw-operation-class-STARTEQ       TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents

            START idxfile
              KEY IS EQUAL TO f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for an "
                          "exactly equal or identical key from the "
                          "existing ones."
                          asterisk asterisk

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
          2252211-finish-menu-mode-code-pos-eq.
            EXIT.

          225230-start-menu-read-approx-code-salary.
            INITIALIZE ws-f-idxfile-indicators
                       ws-menu-standard-options-performance
                       ws-realization-questions

            PERFORM 224100-start-show-menu-look-for-keyaccess
               THRU 224100-finish-show-menu-look-for-keyaccess

            PERFORM 225231-start-validate-menu-read-apprx-cod-sal
               THRU 225231-finish-validate-menu-read-apprx-cod-sal.
          225230-finish-menu-read-approx-code-salary.
            EXIT.

          225231-start-validate-menu-read-apprx-cod-sal.
            EVALUATE TRUE
                WHEN sw-menu-mode-read-dir-keyacc-code
                     PERFORM 2252311-start-menu-read-code-apprx
                        THRU 2252311-finish-menu-read-code-apprx
                       UNTIL sw-menu-mode-read-option-givenkey-exit

                WHEN sw-menu-mode-read-dir-keyacc-salary
                     PERFORM 2252312-start-menu-read-salary-apprx
                        THRU 2252312-finish-menu-read-salary-apprx
                       UNTIL sw-menu-mode-read-option-givenkey-exit

                WHEN sw-menu-mode-read-dir-keyacc-exitmenu
                     DISPLAY "Quitting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. Please correct and "
                             "change your option..."

            END-EVALUATE.
          225231-finish-validate-menu-read-apprx-cod-sal.
            EXIT.

          2252311-start-menu-read-code-apprx.
            INITIALIZE ws-f-idxfile-indicators
                       ws-menu-standard-options-performance
                       ws-realization-questions

            PERFORM 22523111-start-show-approximate-offset-menu
               THRU 22523111-finish-show-approximate-offset-menu

            PERFORM 22523112-start-validate-approximate-offset-menu
               THRU 22523112-finish-validate-approximate-offset-menu.
          2252311-finish-menu-read-code-apprx.
            EXIT.

          22523111-start-show-approximate-offset-menu.
            DISPLAY SPACE
            DISPLAY "+===+====+===+====+===+====+===+===+===+"
            DISPLAY "|       Approximate Key Locator.       |"
            DISPLAY "+===+====+===+====+===+====+===+===+===+"
            DISPLAY "| [1]. Key is not greater than value.  |"
            DISPLAY "| [2]. Key greater than value.         |"
            DISPLAY "| [3]. Key greater than or equal value.|"
            DISPLAY "| [4]. Key is not less than value.     |"
            DISPLAY "| [5]. Key less than value.            |"
            DISPLAY "| [6]. Key less than or equal to value.|"
            DISPLAY "| [7]. Exit this menu.                 |"
            DISPLAY "+===+====+===+====+===+====+===+===+===+"
            DISPLAY "Enter your choice: " WITH NO ADVANCING
             ACCEPT ws-menu-mode-read-option-givenkey

            DISPLAY "The chosen option was: "
                    ws-menu-mode-read-option-givenkey.
          22523111-finish-show-approximate-offset-menu.
            EXIT.

          22523112-start-validate-approximate-offset-menu.
            IF NOT(sw-menu-mode-read-option-givenkey-exit)
               PERFORM 221100-start-capture-key-field
                  THRU 221100-finish-capture-key-field
            END-IF

            EVALUATE TRUE
                WHEN sw-menu-mode-read-option-givenkey-ngt
                     PERFORM 225231121-start-menu-mode-code-pos-ngt
                        THRU 225231121-finish-menu-mode-code-pos-ngt

                WHEN sw-menu-mode-read-option-givenkey-gt
                     PERFORM 225231122-start-menu-mode-code-pos-gt
                        THRU 225231122-finish-menu-mode-code-pos-gt

                WHEN sw-menu-mode-read-option-givenkey-gteq
                     PERFORM 225231123-start-menu-mode-code-pos-gteq
                        THRU 225231123-finish-menu-mode-code-pos-gteq

                WHEN sw-menu-mode-read-option-givenkey-nlt
                     PERFORM 225231124-start-menu-mode-code-pos-nlt
                        THRU 225231124-finish-menu-mode-code-pos-nlt

                WHEN sw-menu-mode-read-option-givenkey-lt
                     PERFORM 225231125-start-menu-mode-code-pos-lt
                        THRU 225231125-finish-menu-mode-code-pos-lt

                WHEN sw-menu-mode-read-option-givenkey-lteq
                     PERFORM 225231126-start-menu-mode-code-pos-lteq
                        THRU 225231126-finish-menu-mode-code-pos-lteq

                WHEN sw-menu-mode-read-option-givenkey-exit
                     DISPLAY "Quitting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. "
                             "Please change your option."

            END-EVALUATE.
          22523112-finish-validate-approximate-offset-menu.
            EXIT.

          225231121-start-menu-mode-code-pos-ngt.
            SET sw-operation-class-STARTNGT   TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents.

            START idxfile
              KEY IS NOT GREATER THAN f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for a key "
                          "is not greater than one of those existing."
                          asterisk asterisk

                  PERFORM 000500-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records

                  DISPLAY asterisk
                          "The value: [" ws-f-idxfile-rec-cod-employee
                          "] was found for a key that wasn't "
                          "greater than one of the existing ones: "
                          "[" f-idxfile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Correct positioning on nearest lower key!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          225231121-finish-menu-mode-code-pos-ngt.
            EXIT.

          225231122-start-menu-mode-code-pos-gt.
            SET sw-operation-class-STARTGT TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents.

            START idxfile
              KEY IS GREATER THAN f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for a key "
                          "greater than one of those existing."
                          asterisk asterisk

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
          225231122-finish-menu-mode-code-pos-gt.
            EXIT.

          225231123-start-menu-mode-code-pos-gteq.
            SET sw-operation-class-STARTGTEQ  TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents.

            START idxfile
              KEY IS GREATER THAN OR EQUAL TO f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for a key "
                          "greater than or equal to those existing."
                          asterisk asterisk

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
          225231123-finish-menu-mode-code-pos-gteq.
            EXIT.

          225231124-start-menu-mode-code-pos-nlt.
            SET sw-operation-class-STARTNLT   TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents

            START idxfile
              KEY IS NOT LESS THAN f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for a key "
                          "is not less than one of those existing."
                          asterisk asterisk

                  PERFORM 000500-press-enter-key-to-continue
                  PERFORM 225240-start-menu-mode-finish-position
                     THRU 225240-finish-menu-mode-finish-position

             NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records

                  DISPLAY asterisk
                          "The value: [" ws-f-idxfile-rec-cod-employee
                          "] was found for a key that wasn't less than "
                          "one of the existing ones: "
                          "[" f-idxfile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Correct positioning on nearest upper key!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          225231124-finish-menu-mode-code-pos-nlt.
            EXIT.

          225231125-start-menu-mode-code-pos-lt.
            SET sw-operation-class-STARTLT    TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents

            START idxfile
              KEY IS LESS THAN f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for a key "
                          "less than one of those existing."
                          asterisk asterisk

                  PERFORM 000500-press-enter-key-to-continue
                  PERFORM 225240-start-menu-mode-finish-position
                     THRU 225240-finish-menu-mode-finish-position

             NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records

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
          225231125-finish-menu-mode-code-pos-lt.
            EXIT.

          225231126-start-menu-mode-code-pos-lteq.
            SET sw-operation-class-STARTLTEQ  TO TRUE

            PERFORM 000300-preliminary-review-employee-code-contents

            START idxfile
              KEY IS LESS THAN OR EQUAL TO f-idxfile-rec-cod-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for a key "
                          "less than or equal to those existing."
                          asterisk asterisk

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
          225231126-finish-menu-mode-code-pos-lteq.
            EXIT.

          2252312-start-menu-read-salary-apprx.
            INITIALIZE ws-f-idxfile-indicators
                       ws-menu-standard-options-performance
                       ws-realization-questions

            PERFORM 22523111-start-show-approximate-offset-menu
               THRU 22523111-finish-show-approximate-offset-menu

            PERFORM 22523121-start-validate-loc-opt-salary-employee
               THRU 22523121-finish-validate-loc-opt-salary-employee.
          2252312-finish-menu-read-salary-apprx.
            EXIT.

          22523121-start-validate-loc-opt-salary-employee.
            IF NOT(sw-menu-mode-read-option-givenkey-exit)
               PERFORM 221400-start-capture-salary-employee
                  THRU 221400-finish-capture-salary-employee
            END-IF

            EVALUATE TRUE
                WHEN sw-menu-mode-read-option-givenkey-ngt
                     PERFORM 225231211-start-locate-salary-key-ngt
                        THRU 225231211-finish-locate-salary-key-ngt

                WHEN sw-menu-mode-read-option-givenkey-gt
                     PERFORM 225231212-start-locate-salary-key-gt
                        THRU 225231212-finish-locate-salary-key-gt

                WHEN sw-menu-mode-read-option-givenkey-gteq
                     PERFORM 225231213-start-locate-salary-key-gteq
                        THRU 225231213-finish-locate-salary-key-gteq

                WHEN sw-menu-mode-read-option-givenkey-nlt
                     PERFORM 225231214-start-locate-salary-key-nlt
                        THRU 225231214-finish-locate-salary-key-nlt

                WHEN sw-menu-mode-read-option-givenkey-lt
                     PERFORM 225231215-start-locate-salary-key-lt
                        THRU 225231215-finish-locate-salary-key-lt

                WHEN sw-menu-mode-read-option-givenkey-lteq
                     PERFORM 225231216-start-locate-salary-key-lteq
                        THRU 225231216-finish-locate-salary-key-lteq

                WHEN sw-menu-mode-read-option-givenkey-exit
                     DISPLAY "Exiting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. "
                             "Please change your option and correct it."

            END-EVALUATE.
          22523121-finish-validate-loc-opt-salary-employee.
            EXIT.

          225231211-start-locate-salary-key-ngt.
            SET sw-operation-class-STARTNGT      TO TRUE

            PERFORM 000400-preliminary-review-employee-salary-contents

            START idxfile
              KEY IS NOT GREATER THAN f-idxfile-rec-salary-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary cannot be allocated for a value "
                          "is not greater than that of the existing "
                          "ones."

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
                          "] was found for a value that wasn't greater "
                          "than that of the existing ones: "
                          "[" f-idxfile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning upper salary done correctly!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          225231211-finish-locate-salary-key-ngt.
            EXIT.

          225231212-start-locate-salary-key-gt.
            SET sw-operation-class-STARTGT       TO TRUE

            PERFORM 000400-preliminary-review-employee-salary-contents

            START idxfile
              KEY IS GREATER THAN f-idxfile-rec-salary-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary cannot be allocated for a value "
                          "is greater than that of the existing ones."

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
                          "] was found for a value that was greater "
                          "than that of the existing ones: "
                          "[" f-idxfile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning upper salary done correctly!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          225231212-finish-locate-salary-key-gt.
            EXIT.

          225231213-start-locate-salary-key-gteq.
            SET sw-operation-class-STARTGTEQ     TO TRUE

            PERFORM 000400-preliminary-review-employee-salary-contents

            START idxfile
              KEY IS GREATER OR EQUAL TO f-idxfile-rec-salary-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary cannot be allocated for a value "
                          "is greater than or equal to that of the "
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
                          "] was found for a value that was greater "
                          "than or equal to the existing ones: "
                          "[" f-idxfile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning upper or equal salary "
                          "done correctly!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          225231213-finish-locate-salary-key-gteq.
            EXIT.

          225231214-start-locate-salary-key-nlt.
            SET sw-operation-class-STARTNLT      TO TRUE

            PERFORM 000400-preliminary-review-employee-salary-contents

            START idxfile
              KEY IS NOT LESS THAN f-idxfile-rec-salary-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary cannot be allocated for a value "
                          "is not less than that of the existing ones."

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
                          "] was found for a value that wasn't less "
                          "than the existing ones: "
                          "[" f-idxfile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning upper salary done correctly!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          225231214-finish-locate-salary-key-nlt.
            EXIT.

          225231215-start-locate-salary-key-lt.
            SET sw-operation-class-STARTLT       TO TRUE

            PERFORM 000400-preliminary-review-employee-salary-contents

            START idxfile
              KEY IS LESS THAN f-idxfile-rec-salary-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary cannot be allocated for a value "
                          "is less than that of the existing ones."

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
                          "] was found for a value that was less "
                          "than that of the existing ones: "
                          "[" f-idxfile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning lower salary done correctly!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          225231215-finish-locate-salary-key-lt.
            EXIT.

          225231216-start-locate-salary-key-lteq.
            SET sw-operation-class-STARTLTEQ     TO TRUE

            PERFORM 000400-preliminary-review-employee-salary-contents

            START idxfile
              KEY IS LESS THAN OR EQUAL TO f-idxfile-rec-salary-employee
                  INVALID KEY
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary cannot be allocated for a value "
                          "is less than or equal to that of the "
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
                          "] was found for a value that was less "
                          "than or equal to the existing ones: "
                          "[" f-idxfile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning lower or equal salary "
                          "done correctly!"
                          asterisk
                  PERFORM 000500-press-enter-key-to-continue

            END-START.
          225231216-finish-locate-salary-key-lteq.
            EXIT.

          225240-start-menu-mode-finish-position.
            SET sw-operation-class-STARTLST   TO TRUE

            START idxfile LAST
                  INVALID KEY
                          DISPLAY asterisk asterisk
                                  "Error positioning at end!"
                                  asterisk asterisk
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

           DISPLAY "Closing. Status Code: ["  fs-idxfile "]."

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
