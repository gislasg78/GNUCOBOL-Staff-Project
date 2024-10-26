       IDENTIFICATION DIVISION.
       PROGRAM-ID. IdxFile.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL idxfile ASSIGN TO ws-idxfile-name
                             ORGANIZATION IS INDEXED
                             ACCESS MODE  IS DYNAMIC
                             RECORD KEY   IS
                             f-idxfile-rec-cod-employee
                             FILE STATUS  IS fs-idxfile.

       DATA DIVISION.
       FILE SECTION.
       FD  idxfile
           DATA RECORD IS f-idxfile-rec
           RECORD CONTAINS 13 CHARACTERS.
       01  f-idxfile-rec.
           03  f-idxfile-rec-cod-employee     PIC 9(05)  VALUE ZEROES.
           03  f-idxfile-rec-salary-employee  PIC 9(06)V9(02)
                                              VALUE ZEROES.

       WORKING-STORAGE SECTION.
       78  cte-01                                        VALUE 01.

       77  fs-idxfile                         PIC 9(02)  VALUE ZEROES.
           88  sw-fs-idxfile-success-completion          VALUE ZEROES.
           88  sw-fs-idxfile-missing-optional-file       VALUE 05.
           88  sw-fs-idxfile-end-of-file                 VALUE 10.
           88  sw-fs-idxfile-invalid-key                 VALUE 21.
           88  sw-fs-idxfile-duplicate-key               VALUE 22.
           88  sw-fs-idxfile-record-not-found            VALUE 23.
           88  sw-fs-idxfile-file-not-found              VALUE 35.

       77  ws-idxfile-name                    PIC X(12)  VALUE SPACES.
       77  ws-idxfile-EOF                     PIC A(01)  VALUE SPACE.
           88  sw-idxfile-EOF-Y                          VALUE 'Y'.

       01  ws-statistics-processed-records.
           03  ws-eliminated-records          PIC 9(04)  VALUE ZEROES.
           03  ws-reading-records             PIC 9(04)  VALUE ZEROES.
           03  ws-repositioning-records       PIC 9(04)  VALUE ZEROES.
           03  ws-rewritten-records           PIC 9(04)  VALUE ZEROES.
           03  ws-written-records             PIC 9(04)  VALUE ZEROES.

       01  ws-continue-response               PIC A(01)  VALUE SPACE.
           88  sw-continue-response-N         VALUES ARE 'N', 'n'.

       01  ws-menu-option                     PIC 9(01)  VALUE ZERO.
           88  sw-menu-option-add             VALUE 1.
           88  sw-menu-option-delete          VALUE 2.
           88  sw-menu-option-modify          VALUE 3.
           88  sw-menu-option-view-one        VALUE 4.
           88  sw-menu-option-view-all        VALUE 5.
           88  sw-menu-option-exit            VALUE 6.

       01  ws-menu-mode-read-option           PIC 9(01)  VALUE ZERO.
           88  sw-menu-mode-read-option-forward          VALUE 01.
           88  sw-menu-mode-read-option-backward         VALUE 02.
           88  sw-menu-mode-read-option-exit             VALUE 03.

       01  ws-f-idxfile-rec.
           03  ws-f-idxfile-rec-cod-employee  PIC 9(05)  VALUE ZEROES.
           03  ws-f-idxfile-rec-salary-employee       PIC 9(06)V9(02)
                                                         VALUE ZEROES.

       PROCEDURE DIVISION.
       DECLARATIVES.
       File-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON idxfile.
       Status-Check.
           DISPLAY SPACE
           DISPLAY "Information about errors in files."
           DISPLAY "File Name   : [" ws-idxfile-name "]."
           DISPLAY "Status Code : [" fs-idxfile "]."
           STOP "Press ENTER to continue...".
       END DECLARATIVES.

       MAIN-PARAGRAPH.
           PERFORM 100000-capture-file-name
              THRU 100000-end-capture-file-name

           OPEN I-O idxfile
           DISPLAY "Opening. Status Code: [" fs-idxfile "]"
           PERFORM 100000-tolerating-error-codes
              THRU 100000-end-tolerating-error-codes

           PERFORM 100000-main-menu
              THRU 100000-end-main-menu
             UNTIL fs-idxfile IS NOT EQUAL TO ZEROES
                OR sw-menu-option-exit

           CLOSE idxfile
           DISPLAY "Closing. Status Code: [" fs-idxfile "]."

           PERFORM 300000-results-obtained
              THRU 300000-end-results-obtained

           STOP RUN.

       100000-capture-file-name.
           DISPLAY "Program for processing indexed sequential files."
           DISPLAY "Enter the file name: " WITH NO ADVANCING
           ACCEPT ws-idxfile-name

           DISPLAY "Name of the indexed file to work on: ["
                   ws-idxfile-name "].".
       100000-end-capture-file-name.
           EXIT.

       100000-tolerating-error-codes.
           EVALUATE TRUE
               WHEN sw-fs-idxfile-success-completion
                    DISPLAY "Successfull completion!"

               WHEN sw-fs-idxfile-missing-optional-file
                    DISPLAY "Missing Optional file!"
                    MOVE ZEROES TO fs-idxfile

               WHEN sw-fs-idxfile-end-of-file
                    DISPLAY "End of file!"
                    MOVE ZEROES TO fs-idxfile

               WHEN sw-fs-idxfile-invalid-key
                    DISPLAY "Invalid Key!"
                    MOVE ZEROES TO fs-idxfile

               WHEN sw-fs-idxfile-duplicate-key
                    DISPLAY "Duplicate Key!"
                    MOVE ZEROES TO fs-idxfile

               WHEN sw-fs-idxfile-record-not-found
                    DISPLAY "Record Not Found!"
                    MOVE ZEROES TO fs-idxfile

               WHEN sw-fs-idxfile-file-not-found
                    DISPLAY "File Not Found!"

               WHEN OTHER
                    DISPLAY "A code error [" fs-idxfile
                            "] has occurred in the file: ["
                            ws-idxfile-name "]."

           END-EVALUATE.
       100000-end-tolerating-error-codes.
           EXIT.

       100000-main-menu.
           INITIALIZE ws-continue-response
                      ws-menu-mode-read-option
                      ws-f-idxfile-rec
                      f-idxfile-rec

           PERFORM 100000-option-menu-display
              THRU 100000-end-option-menu-display

           PERFORM 200000-validate-selected-menu-option
              THRU 200000-end-validate-selected-menu-option.
       100000-end-main-menu.
           EXIT.
 
       100000-option-menu-display.
           DISPLAY SPACE
           DISPLAY "Options menu."
           DISPLAY "[1] Add a record."
           DISPLAY "[2] Delete a record."
           DISPLAY "[3] Modify a record."
           DISPLAY "[4] View a record."
           DISPLAY "[5] View all records."
           DISPLAY "[6] Exit this program."
           DISPLAY "Enter your choice: " WITH NO ADVANCING
           ACCEPT ws-menu-option.
       100000-end-option-menu-display.
           EXIT.

       200000-validate-selected-menu-option.
           EVALUATE TRUE
               WHEN sw-menu-option-add
                    PERFORM 100000-add-records
                       THRU 100000-end-add-records
                      UNTIL fs-idxfile IS NOT EQUAL TO ZEROES
                         OR sw-continue-response-N

               WHEN sw-menu-option-delete
                    PERFORM 100500-delete-a-record
                       THRU 100500-end-delete-a-record
                      UNTIL fs-idxfile IS NOT EQUAL TO ZEROES
                         OR sw-continue-response-N

               WHEN sw-menu-option-modify
                    PERFORM 110000-modify-record
                       THRU 110000-end-modify-record
                      UNTIL fs-idxfile IS NOT EQUAL TO ZEROES
                         OR sw-continue-response-N

               WHEN sw-menu-option-view-one
                    PERFORM 200000-view-any-record
                       THRU 200000-end-view-any-record
                      UNTIL fs-idxfile IS NOT EQUAL TO ZEROES
                         OR sw-continue-response-N

               WHEN sw-menu-option-view-all
                    PERFORM 200100-main-reading-menu
                       THRU 200100-end-main-reading-menu
                      UNTIL fs-idxfile IS NOT EQUAL TO ZEROES
                         OR sw-menu-mode-read-option-exit
                        
               WHEN sw-menu-option-exit
                    DISPLAY SPACE
                    DISPLAY "Leaving this program..."

               WHEN OTHER
                    DISPLAY SPACE
                    DISPLAY "Unrecognized option. Please try again!"

           END-EVALUATE.
       200000-end-validate-selected-menu-option.
           EXIT.

       200100-main-reading-menu.
           INITIALIZE ws-continue-response

           PERFORM 200200-option-menu-reading
              THRU 200200-end-option-menu-reading

           PERFORM 200300-validate-option-menu-reading
              THRU 200300-end-validate-option-menu-reading.
       200100-end-main-reading-menu.
           EXIT.

       200200-option-menu-reading.
           DISPLAY SPACE
           DISPLAY "Record display options menu."
           DISPLAY "[1] Reading forward."
           DISPLAY "[2] Reading backward."
           DISPLAY "[3] Return to main menu."
           DISPLAY "Enter your choice: " WITH NO ADVANCING
           ACCEPT ws-menu-mode-read-option.
       200200-end-option-menu-reading.
           EXIT.

       200300-validate-option-menu-reading.
           EVALUATE TRUE
               WHEN sw-menu-mode-read-option-forward
                    PERFORM 300000-reading-forward
                       THRU 300000-end-reading-forward
                      UNTIL fs-idxfile IS NOT EQUAL TO ZEROES
                         OR sw-continue-response-N

               WHEN sw-menu-mode-read-option-backward
                    PERFORM 400000-reading-backward
                       THRU 400000-end-reading-backward
                      UNTIL fs-idxfile IS NOT EQUAL TO ZEROES
                         OR sw-continue-response-N

               WHEN sw-menu-mode-read-option-exit
                    DISPLAY "Returning to main menu..."

               WHEN OTHER
                    DISPLAY SPACE
                    DISPLAY "Unrecognized option. Please try again!"

           END-EVALUATE.
       200300-end-validate-option-menu-reading.
           EXIT.

       100000-add-records.
           PERFORM 100111-capture-employee-code
              THRU 100111-end-capture-employee-code

           PERFORM 100112-capture-employee-salary
              THRU 100112-end-capture-employee-salary

           DISPLAY SPACE
           WRITE f-idxfile-rec     FROM ws-f-idxfile-rec
                 INVALID KEY
                     DISPLAY "Error code: [" fs-idxfile "] when"
                             " recording employee code: ["
                             ws-f-idxfile-rec-cod-employee "]."

             NOT INVALID KEY
                     ADD cte-01      TO ws-written-records

                     PERFORM 100100-display-employee-info
                        THRU 100100-end-display-employee-info

           END-WRITE
           DISPLAY "Writing. Status Code: [" fs-idxfile "]."

           PERFORM 100000-tolerating-error-codes
              THRU 100000-end-tolerating-error-codes

           PERFORM 101010-question-continue-operation
              THRU 101010-end-question-continue-operation.
       100000-end-add-records.
           EXIT.

       100111-capture-employee-code.
           DISPLAY SPACE
           DISPLAY "Capture employee code."
           DISPLAY "Employee Code   : " WITH NO ADVANCING
           ACCEPT ws-f-idxfile-rec-cod-employee

           MOVE ws-f-idxfile-rec-cod-employee
             TO f-idxfile-rec-cod-employee.
       100111-end-capture-employee-code.
           EXIT.

       100112-capture-employee-salary.
           DISPLAY SPACE
           DISPLAY "Capturing employee salary."
           DISPLAY "Employee Salary : " WITH NO ADVANCING
           ACCEPT ws-f-idxfile-rec-salary-employee

           MOVE ws-f-idxfile-rec-salary-employee
             TO f-idxfile-rec-salary-employee.
       100112-end-capture-employee-salary.
           EXIT.

       100100-display-employee-info.
           DISPLAY SPACE
           DISPLAY "Displaying employee information."
           DISPLAY "Employee Code   : ["
                   ws-f-idxfile-rec-cod-employee "]."
           DISPLAY "Employee Salary : ["
                   ws-f-idxfile-rec-salary-employee "]".
       100100-end-display-employee-info.
           EXIT.

       101010-question-continue-operation.
           DISPLAY SPACE
           DISPLAY "You want to continue performing this same operation"
                   " for this same file? (y/n) : " WITH NO ADVANCING
           ACCEPT ws-continue-response.
       101010-end-question-continue-operation.
           EXIT.

       100500-delete-a-record.
           PERFORM 100111-capture-employee-code
              THRU 100111-end-capture-employee-code

           PERFORM 200200-see-a-record
              THRU 200220-end-see-a-record

           PERFORM 100000-tolerating-error-codes
              THRU 100000-end-tolerating-error-codes

           PERFORM 100550-erase-a-record
              THRU 100550-end-erase-a-record

           PERFORM 100000-tolerating-error-codes
              THRU 100000-end-tolerating-error-codes

           PERFORM 101010-question-continue-operation
              THRU 101010-end-question-continue-operation.
       100500-end-delete-a-record.
           EXIT.

       100550-erase-a-record.
           DISPLAY SPACE

           DELETE idxfile RECORD
                  INVALID KEY
                          DISPLAY "Operation     : [Rewriting]."
                          DISPLAY "Employee Code : ["
                                   ws-f-idxfile-rec-cod-employee
                                  "]."
                          DISPLAY "Status Code   : [" fs-idxfile "]."

              NOT INVALID KEY
                          ADD cte-01          TO ws-eliminated-records

                          PERFORM 100100-display-employee-info
                             THRU 100100-end-display-employee-info

           END-DELETE

           DISPLAY "Deletion. Status Code: [" fs-idxfile "].".
       100550-end-erase-a-record.
           EXIT.

       110000-modify-record.
           PERFORM 100111-capture-employee-code
              THRU 100111-end-capture-employee-code

           PERFORM 200200-see-a-record
              THRU 200220-end-see-a-record

           PERFORM 100000-tolerating-error-codes
              THRU 100000-end-tolerating-error-codes

           PERFORM 100112-capture-employee-salary
              THRU 100112-end-capture-employee-salary

           PERFORM 110113-change-a-record
              THRU 110113-end-change-a-record

           PERFORM 100000-tolerating-error-codes
              THRU 100000-end-tolerating-error-codes

           PERFORM 101010-question-continue-operation
              THRU 101010-end-question-continue-operation.
       110000-end-modify-record.
           EXIT.

       110113-change-a-record.
           DISPLAY SPACE

           REWRITE f-idxfile-rec     FROM ws-f-idxfile-rec
                   INVALID KEY
                           DISPLAY "Operation     : [Rewriting]."
                           DISPLAY "Employee Code : ["
                                   ws-f-idxfile-rec-cod-employee
                                   "]."
                           DISPLAY "Status Code   : [" fs-idxfile "]."

               NOT INVALID KEY
                   ADD cte-01          TO ws-rewritten-records

                   PERFORM 100100-display-employee-info
                      THRU 100100-end-display-employee-info

           END-REWRITE

           DISPLAY "Rewriting. Status Code: [" fs-idxfile "].".
       110113-end-change-a-record.
           EXIT.

       200000-view-any-record.
           DISPLAY SPACE
           DISPLAY "View a specific record."
           PERFORM 100111-capture-employee-code
              THRU 100111-end-capture-employee-code

           PERFORM 200200-see-a-record
              THRU 200220-end-see-a-record

           PERFORM 100000-tolerating-error-codes
              THRU 100000-end-tolerating-error-codes
 
           PERFORM 101010-question-continue-operation
              THRU 101010-end-question-continue-operation.
       200000-end-view-any-record.
           EXIT.

       200200-see-a-record.
           DISPLAY SPACE

           READ idxfile INTO ws-f-idxfile-rec
                        KEY IS f-idxfile-rec-cod-employee
                INVALID KEY
                           DISPLAY "Operation     : [Direct Reading]."
                           DISPLAY "Employee Code : ["
                                   ws-f-idxfile-rec-cod-employee
                                   "]."
                           DISPLAY "Status Code   : [" fs-idxfile "]."

            NOT INVALID KEY
                        ADD cte-01  TO ws-reading-records

                        PERFORM 100100-display-employee-info
                           THRU 100100-end-display-employee-info

           END-READ

           DISPLAY "Direct Reading. Status Code: [" fs-idxfile "].".
       200220-end-see-a-record.
           EXIT.

       300000-reading-forward.
           DISPLAY "From which employee code are you going to read the "
                   "records in a forward direction?"
           PERFORM 300300-reposition-idxfile-pointer
              THRU 300300-end-reposition-idxfile-pointer

           PERFORM UNTIL fs-idxfile IS NOT EQUAL TO ZEROES
                      OR sw-idxfile-EOF-Y

               DISPLAY SPACE
               READ idxfile NEXT RECORD INTO ws-f-idxfile-rec
                    AT END
                       DISPLAY "End of archive reached..."
                       SET sw-idxfile-EOF-Y TO TRUE

                NOT AT END
                       ADD cte-01  TO ws-reading-records

                       PERFORM 100100-display-employee-info
                          THRU 100100-end-display-employee-info

               END-READ
               DISPLAY "Reading Forward. Status Code: [" fs-idxfile "]."

           END-PERFORM

           PERFORM 100000-tolerating-error-codes
              THRU 100000-end-tolerating-error-codes

           PERFORM 101010-question-continue-operation
              THRU 101010-end-question-continue-operation.
       300000-end-reading-forward.
           EXIT.

       300300-reposition-idxfile-pointer.
           INITIALIZE ws-idxfile-EOF

           DISPLAY SPACE
           DISPLAY "View all records."
           DISPLAY "Press Enter to start from the first possible "
                   "logical record."

           PERFORM 100111-capture-employee-code
              THRU 100111-end-capture-employee-code

           DISPLAY SPACE
           START idxfile KEY IS GREATER THAN OR EQUAL TO
                 f-idxfile-rec-cod-employee
                 INVALID KEY
                         DISPLAY "Operation     : [Repositioning]."
                         DISPLAY "Employee Code : ["
                                  ws-f-idxfile-rec-cod-employee
                                 "]."
                         DISPLAY "Status Code   : [" fs-idxfile "]."

             NOT INVALID KEY
                         ADD  cte-01  TO ws-repositioning-records
                         DISPLAY "Successful repositioning!"

           END-START
           DISPLAY "Starting. Status Code: [" fs-idxfile "].".
       300300-end-reposition-idxfile-pointer.
           EXIT.

       400000-reading-backward.
           DISPLAY "From which employee code are you going to read the "
                   " records in a backward direction?"
           PERFORM 300300-reposition-idxfile-pointer
              THRU 300300-end-reposition-idxfile-pointer

           PERFORM UNTIL fs-idxfile IS NOT EQUAL TO ZEROES
                      OR sw-idxfile-EOF-Y

               DISPLAY SPACE
               READ idxfile PREVIOUS RECORD INTO ws-f-idxfile-rec
                    AT END
                       DISPLAY "Start of archive reached..."
                       SET sw-idxfile-EOF-Y TO TRUE

                NOT AT END
                       ADD cte-01  TO ws-reading-records

                       PERFORM 100100-display-employee-info
                          THRU 100100-end-display-employee-info

               END-READ
               DISPLAY "Reading Backward. Status Code: [" fs-idxfile
                       "]."

           END-PERFORM

           PERFORM 100000-tolerating-error-codes
              THRU 100000-end-tolerating-error-codes

           PERFORM 101010-question-continue-operation
              THRU 101010-end-question-continue-operation.
       400000-end-reading-backward.
           EXIT.

       300000-results-obtained.
           DISPLAY SPACE
           DISPLAY "Statistics of successfully processed records."
           DISPLAY "Eliminated   : [" ws-eliminated-records "]."
           DISPLAY "Read         : [" ws-reading-records "]."
           DISPLAY "Repositioned : [" ws-repositioning-records "]."
           DISPLAY "Rewritten    : [" ws-rewritten-records "]."
           DISPLAY "Writings     : [" ws-written-records "].".
       300000-end-results-obtained.
           EXIT.

       END PROGRAM IdxFile.
