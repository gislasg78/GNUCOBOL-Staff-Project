       IDENTIFICATION DIVISION.
       PROGRAM-ID. IdxDyn.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                  ALPHABET ascii-code IS STANDARD-1.
                  CLASS alphabetic-and-numeric IS X'20'
                                                  X'2E'
                                                  X'30' THRU X'39'
                                                  X'41' THRU X'5A'
                                                  X'61' THRU X'7A'.
                  NUMERIC SIGN IS TRAILING SEPARATE.
                  SYMBOLIC CHARACTERS asterisk IS 43 IN ascii-code.

       REPOSITORY. FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL IdxFile ASSIGN TO DISK ws-IdxFile-name
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS DYNAMIC
                  RECORD KEY   IS f-IdxFile-rec-cod-employee
                  ALTERNATE RECORD KEY IS f-IdxFile-rec-salary-employee
                            WITH DUPLICATES
                  FILE STATUS  IS fs-IdxFile.

           SELECT OPTIONAL OutFile ASSIGN TO ws-OutFile-name
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS fs-OutFile.

       DATA DIVISION.
       FILE SECTION.
       FD  IdxFile
           BLOCK  CONTAINS 05 TO 10 RECORDS
           RECORD CONTAINS 15 CHARACTERS
           RECORDING  MODE IS FIXED.

       01  f-IdxFile-rec.
           88  sw-f-IdxFile-rec-empty                      VALUE ZEROES.
           03  f-IdxFile-rec-cod-employee       PIC 9(06)  VALUE ZEROES.
           03  f-IdxFile-rec-salary-employee    PIC S9(06)V9(02)
                                                    SIGN  IS LEADING
                                                    SEPARATE CHARACTER
                                                           VALUE ZEROES.

       FD  OutFile
           BLOCK CONTAINS  05 TO 10 RECORDS
           RECORDING MODE  IS FIXED

           LINAGE IS ws-linage-totlines LINES
             WITH FOOTING AT ws-linage-footing
            LINES AT TOP     ws-linage-top
            LINES AT BOTTOM  ws-linage-bottom.

       01  f-OutFile-rec                        PIC X(31)  VALUE SPACES.
           88  sw-f-OutFile-rec-empty                      VALUE SPACES.

       WORKING-STORAGE SECTION.
       01  ws-work-section-begins               PIC X(42)  VALUE
           "The working storage section begins here...".

       77  fs-IdxFile                           PIC X(02)  VALUE SPACES.
       77  fs-OutFile                           PIC X(02)  VALUE SPACES.

       78  cte-01                                          VALUE 01.
       78  cte-34                                          VALUE 34.

       01  ws-environmental-variables.
           03  ws-current-date-and-time.
               05  ws-CDT-Date.
                   07  ws-CDT-Year.
                       09  ws-CDT-Year-Century  PIC 9(02)  VALUE ZEROES.
                       09  ws-CDT-Year-Year     PIC 9(02)  VALUE ZEROES.
                   07  ws-CDT-Month             PIC 9(02)  VALUE ZEROES.
                   07  ws-CDT-Day               PIC 9(02)  VALUE ZEROES.
               05  ws-CDT-Time.
                   07  ws-CDT-Hour              PIC 9(02)  VALUE ZEROES.
                   07  ws-CDT-Minutes           PIC 9(02)  VALUE ZEROES.
                   07  ws-CDT-Seconds           PIC 9(02)  VALUE ZEROES.
                   07  ws-CDT-Hundredths-Of-Secs
                                                PIC 9(02)  VALUE ZEROES.
               05  ws-CDT-Time-Zone.
                   07  ws-CDT-GMT-Diff-Hours    PIC S9(02) VALUE ZEROES
                                                SIGN  IS LEADING
                                                SEPARATE CHARACTER.
                   07  ws-CDT-GMT-Diff-Minutes  PIC 9(02)  VALUE ZEROES.

           03  ws-date-and-time-formatted.
               05  ws-FT-Date.
                   07  ws-FT-Year.
                       09  ws-FT-Year-Century   PIC 9(02)  VALUE ZEROES.
                       09  ws-FT-Year-Year      PIC 9(02)  VALUE ZEROES.
                   07  FILLER                   PIC X(01)  VALUE X'2F'.
                   07  ws-FT-Month              PIC 9(02)  VALUE ZEROES.
                   07  FILLER                   PIC X(01)  VALUE X'2F'.
                   07  ws-FT-Day                PIC 9(02)  VALUE ZEROES.
               05  FILLER                       PIC X(01)  VALUE X'20'.
               05  FILLER                       PIC X(01)  VALUE X'2D'.
               05  FILLER                       PIC X(01)  VALUE X'20'.
               05  ws-FT-Time.
                   07  ws-FT-Hour               PIC 9(02)  VALUE ZEROES.
                   07  FILLER                   PIC X(01)  VALUE X'3A'.
                   07  ws-FT-Minutes            PIC 9(02)  VALUE ZEROES.
                   07  FILLER                   PIC X(01)  VALUE X'3A'.
                   07  ws-FT-Seconds            PIC 9(02)  VALUE ZEROES.
                   07  FILLER                   PIC X(01)  VALUE X'2E'.
                   07  ws-FT-Hundredths-Of-Secs PIC 9(02)  VALUE ZEROES.
               05  FILLER                       PIC X(01)  VALUE X'20'.
               05  ws-FT-Time-Zone.
                   07  ws-FT-GMT-Diff-Hours     PIC +9(2)  VALUE ZEROES.
                   07  FILLER                   PIC X(01)  VALUE X'3A'.
                   07  ws-FT-GMT-Diff-Minutes   PIC 9(02)  VALUE ZEROES.

           03  ws-f-IdxFile-error-status-code-indicators.
               05  ws-f-error-status-code-table-aux.
                       07  ws-f-error-status-code-table-code-error-aux
                                                PIC X(02)  VALUE SPACES.
                       07  ws-f-error-status-code-table-desc-error-aux
                                                PIC X(25)  VALUE SPACES.
               05  ws-f-IdxFile-rec-salary-employee-ed
                                                PIC $-,---,--9.99
                                                           VALUE ZEROES.
               05  ws-IdxFile-EOF               PIC A(01)  VALUE SPACE.
                   88  sw-IdxFile-EOF-Y                    VALUE 'Y'.
                   88  sw-IdxFile-EOF-N                    VALUE 'N'.
               05  ws-IdxFile-record-found      PIC A(01)  VALUE SPACE.
                   88  sw-IdxFile-record-found-N           VALUE 'N'.
                   88  sw-IdxFile-record-found-Y           VALUE 'Y'.

           03  ws-Files-records.
               05  ws-f-IdxFile-rec.
                   07  ws-f-IdxFile-rec-cod-employee       PIC 9(06)
                                                           VALUE ZEROES.
                   07  ws-f-IdxFile-rec-salary-employee
                                                PIC S9(06)V9(02)
                                                SIGN  IS LEADING
                                                SEPARATE CHARACTER
                                                           VALUE ZEROES.
               05  ws-f-OutFile-rec.
                   07  ws-f-OutFile-rec-record-counter     PIC Z(11)
                                                           VALUE ZEROES.
                   07  FILLER                              PIC X(01)
                                                           VALUE SPACE.
                   07  ws-f-OutFile-rec-cod-employee       PIC 9(06)
                                                           VALUE ZEROES.
                   07  FILLER                              PIC X(01)
                                                           VALUE SPACE.
                   07  ws-f-OutFile-rec-salary-employee PIC -,---,--9.99
                                                           VALUE ZEROES.

           03  ws-Files-names.
               05  ws-IdxFile-name              PIC X(12)  VALUE SPACES.
               05  ws-OutFile-name              PIC X(12)  VALUE SPACES.
               05  ws-TempFile-name             PIC X(12)  VALUE SPACES.

           03  ws-menu-standard-options-performance.
               05  ws-menu-option               PIC 9(01)  VALUE ZERO.
                   88  sw-menu-option-add                  VALUE 1.
                   88  sw-menu-option-delete               VALUE 2.
                   88  sw-menu-option-modify               VALUE 3.
                   88  sw-menu-option-look-for-one         VALUE 4.
                   88  sw-menu-option-look-for-all         VALUE 5.
                   88  sw-menu-option-exit                 VALUE 6.

               05  ws-menu-mode-modify-option   PIC 9(01)  VALUE ZERO.
                   88  sw-menu-mode-modify-emp-salary      VALUE 1.
                   88  sw-menu-mode-modify-emp-exitmenu    VALUE 2.

               05  ws-menu-mode-read-dir-opt    PIC 9(01)  VALUE ZERO.
                   88  sw-menu-mode-r-d-read               VALUE 1.
                   88  sw-menu-mode-r-d-and-seq            VALUE 2.
                   88  sw-menu-mode-r-d-exitmenu           VALUE 3.

               05  ws-menu-mode-read-direct-key PIC 9(01)  VALUE ZERO.
                   88  sw-menu-mode-r-d-keyacc-code        VALUE 1.
                   88  sw-menu-mode-r-d-keyacc-salary      VALUE 2.
                   88  sw-menu-mode-r-d-keyacc-exitmenu    VALUE 3.

               05  ws-menu-mode-read-option     PIC 9(02)  VALUE ZEROES.
                   88  sw-menu-mode-r-o-start              VALUE 01.
                   88  sw-menu-mode-r-o-givenkey-eq        VALUE 02.
                   88  sw-menu-mode-r-o-givenkey-apprx     VALUE 03.
                   88  sw-menu-mode-r-o-finish             VALUE 04.
                   88  sw-menu-mode-r-o-r-first-rcrd       VALUE 05.
                   88  sw-menu-mode-r-o-r-last-rcrd        VALUE 06.
                   88  sw-menu-mode-r-o-r-backward         VALUE 07.
                   88  sw-menu-mode-r-o-r-forward          VALUE 08.
                   88  sw-menu-mode-r-o-prev-rcrd          VALUE 09.
                   88  sw-menu-mode-r-o-next-rcrd          VALUE 10.
                   88  sw-menu-mode-r-o-exitmenu           VALUE 11.

               05  ws-menu-mode-read-opt-givkey PIC 9(01)  VALUE ZEROES.
                   88  sw-menu-mode-r-o-givenkey-ngt       VALUE 1.
                   88  sw-menu-mode-r-o-givenkey-gt        VALUE 2.
                   88  sw-menu-mode-r-o-givenkey-gteq      VALUE 3.
                   88  sw-menu-mode-r-o-givenkey-nlt       VALUE 4.
                   88  sw-menu-mode-r-o-givenkey-lt        VALUE 5.
                   88  sw-menu-mode-r-o-givenkey-lteq      VALUE 6.
                   88  sw-menu-mode-r-o-givenkey-exit      VALUE 7.

               05  ws-operation-class           PIC A(13)  VALUE SPACES.
                   88  sw-op-class-CLOSE        VALUE "CLOSE".
                   88  sw-op-class-DELETE       VALUE "DELETE".
                   88  sw-op-class-OPEN         VALUE "OPEN".
                   88  sw-op-class-READ         VALUE "READ".
                   88  sw-op-class-READNEXT     VALUE "READ NEXT".
                   88  sw-op-class-READPREV     VALUE "READ PREVIOUS".
                   88  sw-op-class-REWRITE      VALUE "REWRITE".
                   88  sw-op-class-STARTEQ      VALUE "START EQUAL".
                   88  sw-op-class-STARTFRST    VALUE "START FIRST".
                   88  sw-op-class-STARTGT      VALUE "START GREATER".
                   88  sw-op-class-STARTGTEQ    VALUE "START GTEQ".
                   88  sw-op-class-STARTLST     VALUE "START LAST".
                   88  sw-op-class-STARTLT      VALUE "START LESS".
                   88  sw-op-class-STARTLTEQ    VALUE "START LTEQ".
                   88  sw-op-class-STARTNGT     VALUE "START NOT GRT".
                   88  sw-op-class-STARTNLT     VALUE "START NOT LST".
                   88  sw-op-class-WRITE        VALUE "WRITE".

           03  ws-realization-question-message  PIC X(58)  VALUE SPACES.
               88  sw-carry-out-sure-msg        VALUE
           "Are you sure you want to carry out this operation? (y/n): ".
               88  sw-continue-response-msg     VALUE
           "Do you want to continue doing this same operation? (y/n): ".
               88  sw-question-print-rec-msg    VALUE
           "Do you want to save this previously retrieved log? (y/n): ".

           03  ws-realization-questions.
               05  ws-captured-answer           PIC A(01)  VALUE SPACE.
                   88  sw-captured-answer-N     VALUES ARE 'N', 'n'.
                   88  sw-captured-answer-Y     VALUES ARE 'Y', 'y'.
               05  ws-carry-out-sure            PIC A(01)  VALUE SPACE.
                   88  sw-carry-out-sure-N      VALUES ARE 'N', 'n'.
                   88  sw-carry-out-sure-Y      VALUES ARE 'Y', 'y'.
               05  ws-continue-response         PIC A(01)  VALUE SPACE.
                   88  sw-continue-response-N   VALUES ARE 'N', 'n'.
                   88  sw-continue-response-Y   VALUES ARE 'Y', 'y'.
               05  ws-question-print-record     PIC A(01)  VALUE SPACE.
                   88  sw-question-print-rec-N  VALUES ARE 'N', 'n'.
                   88  sw-question-print-rec-Y  VALUES ARE 'Y', 'y'.

       01  ws-statistics-processed-records.
           03  ws-eliminated-records            PIC S9(06) VALUE ZEROES.
           03  ws-last-printed-report-line      PIC S9(06) VALUE ZEROES.
           03  ws-printed-pages                 PIC S9(06) VALUE ZEROES.
           03  ws-reading-records               PIC S9(06) VALUE ZEROES.
           03  ws-reporting-read-records-page   PIC S9(06) VALUE ZEROES.
           03  ws-reporting-read-records-sum    PIC S9(06) VALUE ZEROES.
           03  ws-reporting-written-records-pag PIC S9(06) VALUE ZEROES.
           03  ws-reporting-written-records-sum PIC S9(06) VALUE ZEROES.
           03  ws-repositioning-records         PIC S9(06) VALUE ZEROES.
           03  ws-rewritten-records             PIC S9(06) VALUE ZEROES.
           03  ws-written-records               PIC S9(06) VALUE ZEROES.

       01  ws-f-error-status-code-table-record.
           03  ws-f-error-status-code-table.
               05  ws-status-code-success-completion.
                    07  FILLER                  PIC 9(02)  VALUE ZEROES.
                    07  FILLER                  PIC X(25)  VALUE
                        "Success Completion".
               05  ws-status-code-success-duplicate.
                    07  FILLER                  PIC 9(02)  VALUE 02.
                    07  FILLER                  PIC X(25)  VALUE
                        "Success Duplicate".
               05  ws-status-code-success-incomplete.
                    07  FILLER                  PIC 9(02)  VALUE 04.
                    07  FILLER                  PIC X(25)  VALUE
                       "Success Incomplete".
               05  ws-status-code-optional-missing.
                    07  FILLER                  PIC 9(02)  VALUE 05.
                    07  FILLER                  PIC X(25)  VALUE
                        "Success Optional, Missing".
               05  ws-status-code-multiple-records-ls.
                    07  FILLER                  PIC 9(02)  VALUE 06.
                    07  FILLER                  PIC X(25)  VALUE 
                        "Multiple Records LS".
               05  ws-status-code-success-no-unit.
                    07  FILLER                  PIC 9(02)  VALUE 07.
                    07  FILLER                  PIC X(25)  VALUE
                        "Success No Unit".
               05  ws-status-code-success-ls-bad-data.
                   07  FILLER                   PIC 9(02)  VALUE 09.
                   07  FILLER                   PIC X(25)  VALUE
                       "Success LS Bad Data".
               05  ws-status-code-end-of-file.
                   07  FILLER                   PIC 9(02)  VALUE 10.
                   07  FILLER                   PIC X(25)  VALUE
                       "End Of File".
               05  ws-status-code-out-of-key-range.
                   07  FILLER                   PIC 9(02)  VALUE 14.
                   07  FILLER                   PIC X(25)  VALUE 
                       "Out Of Key Range".
               05  ws-status-code-key-invalid.
                   07  FILLER                   PIC 9(02)  VALUE 21.
                   07  FILLER                   PIC X(25)  VALUE
                       "Key Invalid".
               05  ws-status-code-key-exists.
                   07  FILLER                   PIC 9(02)  VALUE 22.
                   07  FILLER                   PIC X(25)  VALUE
                       "Key Exists".
               05  ws-status-code-key-not-exists.
                   07  FILLER                   PIC 9(02)  VALUE 23.
                   07  FILLER                   PIC X(25)  VALUE
                       "Key Not Exists".
               05  ws-status-code-key-boundary-violation.
                   07  FILLER                   PIC 9(02)  VALUE 24.
                   07  FILLER                   PIC X(25)  VALUE
                       "Key Boundary violation".
               05  ws-status-code-permanent-error.
                   07  FILLER                   PIC 9(02)  VALUE 30.
                   07  FILLER                   PIC X(25)  VALUE
                       "Permanent Error".
               05  ws-status-code-inconsistent-filename.
                   07  FILLER                   PIC 9(02)  VALUE 31.
                   07  FILLER                   PIC X(25)  VALUE
                       "Inconsistent Filename".
               05  ws-status-code-boundary-violation.
                   07  FILLER                   PIC 9(02)  VALUE 34.
                   07  FILLER                   PIC X(25)  VALUE
                       "Boundary Violation".
               05  ws-status-code-file-not-found.
                   07  FILLER                   PIC 9(02)  VALUE 35.
                   07  FILLER                   PIC X(25)  VALUE 
                       "File Not Found".
               05  ws-status-code-permission-denied.
                   07  FILLER                   PIC 9(02)  VALUE 37.
                   07  FILLER                   PIC X(25)  VALUE
                       "Permission Denied".
               05  ws-status-code-closed-with-lock.
                   07  FILLER                   PIC 9(02)  VALUE 38.
                   07  FILLER                   PIC X(25)  VALUE
                       "Closed With Lock".
               05  ws-status-code-conflict-attribute.
                   07  FILLER                   PIC 9(02)  VALUE 39.
                   07  FILLER                   PIC X(25)  VALUE
                       "Conflict Attribute".
               05  ws-status-code-already-open.
                   07  FILLER                   PIC 9(02)  VALUE 41.
                   07  FILLER                   PIC X(25)  VALUE
                       "Already Open".
               05  ws-status-code-not-open.
                   07  FILLER                   PIC 9(02)  VALUE 42.
                   07  FILLER                   PIC X(25)  VALUE 
                       "Not Open".
               05  ws-status-code-read-not-done.
                   07  FILLER                   PIC 9(02)  VALUE 43.
                   07  FILLER                   PIC X(25)  VALUE
                       "Read Not Done".
               05  ws-status-code-record-overflow.
                   07  FILLER                   PIC 9(02)  VALUE 44.
                   07  FILLER                   PIC X(25)  VALUE
                       "Record Overflow".
               05  ws-status-code-read-error.
                   07  FILLER                   PIC 9(02)  VALUE 46.
                   07  FILLER                   PIC X(25)  VALUE
                       "Read Error".
               05  ws-status-code-input-denied.
                   07  FILLER                   PIC 9(02)  VALUE 47.
                   07  FILLER                   PIC X(25)  VALUE 
                       "Input Denied".
               05  ws-status-code-output-denied.
                   07  FILLER                   PIC 9(02)  VALUE 48.
                   07  FILLER                   PIC X(25)  VALUE
                       "Output Denied".
               05  ws-status-code-i-o-denied.
                   07  FILLER                   PIC 9(02)  VALUE 49.
                   07  FILLER                   PIC X(25)  VALUE
                       "I/O Denied".
               05  ws-status-code-record-locked.
                   07  FILLER                   PIC 9(02)  VALUE 51.
                   07  FILLER                   PIC X(25)  VALUE
                       "Record Locked".
               05  ws-status-code-end-of-page.
                   07  FILLER                   PIC 9(02)  VALUE 52.
                   07  FILLER                   PIC X(25)  VALUE 
                       "End-Of-Page".
               05  ws-status-code-i-o-linage.
                   07  FILLER                   PIC 9(02)  VALUE 57.
                   07  FILLER                   PIC X(25)  VALUE
                       "I/O Linage".
               05  ws-status-code-file-sharing-failure.
                   07  FILLER                   PIC 9(02)  VALUE 61.
                   07  FILLER                   PIC X(25)  VALUE
                       "File Sharing Failure".
               05  ws-status-code-bad-character.
                   07  FILLER                   PIC 9(02)  VALUE 71.
                   07  FILLER                   PIC X(25)  VALUE
                       "Bad Character".
               05  ws-status-code-file-not-available.
                   07  FILLER                   PIC 9(02)  VALUE 91.
                   07  FILLER                   PIC X(25)  VALUE 
                       "File Not Available".
           03  ws-f-error-status-code-table-RED REDEFINES
               ws-f-error-status-code-table.
               05  ws-f-error-status-code-table-array
                   OCCURS cte-34 TIMES
                   ASCENDING KEY ws-f-error-status-code-table-code-error
                     INDEXED  BY idx-error-status-code-table.
                   07  ws-f-error-status-code-table-registry.
                       09  ws-f-error-status-code-table-code-error
                                                           PIC X(02).
                       09  ws-f-error-status-code-table-desc-error
                                                           PIC X(25).

       01  ws-f-error-status-code-table-desc-error-tag     PIC X(25)
               VALUE "Unknown File Status".

       01  ws-linage-work-variables.
           03  ws-linage-bottom                 PIC 9(01)  VALUE 01.
           03  ws-linage-footing                PIC 9(02)  VALUE 23.
           03  ws-linage-top                    PIC 9(01)  VALUE 01.
           03  ws-linage-totlines               PIC 9(02)  VALUE 26.

       01  ws-reporting-lines.
           03  ws-rep-page-heading-first-line.
               05  FILLER                       PIC X(08)  VALUE SPACES.
               05  FILLER                       PIC A(15)
                                                VALUE "Employee Report".
               05  FILLER                       PIC X(08)  VALUE SPACES.
           03  ws-rep-page-heading-second-line.
               05  FILLER                       PIC X(01)  VALUE SPACE.
               05  FILLER                       PIC X(15)
                                                VALUE ALL X'2D'.
               05  FILLER                       PIC X(01)  VALUE SPACE.
               05  FILLER                       PIC X(05)
                                                VALUE "Page:".
               05  FILLER                       PIC X(01)  VALUE SPACE.
               05  FILLER                       PIC X(01)  VALUE X'5B'.
               05  ws-rep-p-sec-l-pages-rep     PIC S9(04)
                                                SIGN IS LEADING
                                                SEPARATE CHARACTER
                                                VALUE ZEROES.
               05  FILLER                       PIC X(01)  VALUE X'5D'.
               05  FILLER                       PIC X(01)  VALUE X'2E'.
               05  FILLER                       PIC X(01)  VALUE SPACE.
           03  ws-rep-page-heading-third-line.
               05  FILLER                       PIC X(03)  VALUE SPACES.
               05  FILLER                       PIC A(06)
                                                VALUE "Record".
               05  FILLER                       PIC X(04)  VALUE SPACES.
               05  FILLER                       PIC A(04)  VALUE "Code".
               05  FILLER                       PIC X(05)  VALUE SPACES.
               05  FILLER                       PIC A(06)
                                                VALUE "Salary".
               05  FILLER                       PIC X(52)  VALUE SPACES.
           03  ws-rep-page-heading-fourth-underlines.
               05  FILLER                       PIC X(01)  VALUE SPACE.
               05  FILLER                       PIC X(10)
                                                VALUE ALL X'3D'.
               05  FILLER                       PIC X(01)  VALUE SPACE.
               05  FILLER                       PIC X(06)
                                                VALUE ALL X'3D'.
               05  FILLER                       PIC X(01)  VALUE SPACE.
               05  FILLER                       PIC X(12)
                                                VALUE ALL X'3D'.
               05  FILLER                       PIC X(48)  VALUE SPACES.
           03  ws-rep-page-footing.
               05  FILLER                       PIC X(01)  VALUE SPACE.
               05  FILLER                       PIC X(01)  VALUE X'5B'.
               05  ws-rep-page-foot-recs-rep    PIC +9(04) VALUE ZEROES.
               05  FILLER                       PIC X(01)  VALUE X'5D'.
               05  FILLER                       PIC X(01)  VALUE SPACE.
               05  FILLER                       PIC A(17)  VALUE
                   "records processed".
               05  FILLER                       PIC X(06)  VALUE SPACES.

       01  ws-work-section-ends                 PIC X(42)  VALUE
           "The working storage section ends here...".

       PROCEDURE DIVISION.
       DECLARATIVES.
       IdxFile-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON IdxFile.
       000000-start-status-IdxFile-check.
           INITIALIZE ws-f-error-status-code-table-aux

           DISPLAY SPACE
           DISPLAY "+===+====+===+====+===+====+===+"
           DISPLAY "|   Indexed Sequential File.   |"
           DISPLAY "+===+====+===+====+===+====+===+"

            MOVE fs-IdxFile
              TO ws-f-error-status-code-table-code-error-aux
           PERFORM 000100-search-for-error-and-description-codes

           PERFORM 000200-get-current-date-and-time-record

           MOVE ws-IdxFile-name TO ws-TempFile-name
           PERFORM 000300-check-file-status-code

           PERFORM 000400-preliminary-review-employee-code-contents
           PERFORM 000500-preliminary-review-employee-salary-contents.
       000000-finish-status-IdxFile-check.
           EXIT SECTION.

       OutFile-Handler SECTION.
           USE AFTER ERROR PROCEDURE ON OutFile.
       000000-start-status-OutFile-check.
           INITIALIZE ws-f-error-status-code-table-aux

           DISPLAY SPACE
           DISPLAY "+===+====+===+====+===+====+===+"
           DISPLAY "|      Output Report File.     |"
           DISPLAY "+===+====+===+====+===+====+===+"

            MOVE fs-OutFile
              TO ws-f-error-status-code-table-code-error-aux
           PERFORM 000100-search-for-error-and-description-codes

           PERFORM 000200-get-current-date-and-time-record

           MOVE ws-OutFile-name TO ws-TempFile-name
           PERFORM 000300-check-file-status-code

           PERFORM 000400-preliminary-review-employee-code-contents
           PERFORM 000500-preliminary-review-employee-salary-contents.
       000000-finish-status-OutFile-check.
           EXIT SECTION.

       000100-search-for-error-and-description-codes.
            SET idx-error-status-code-table              TO cte-01

            SEARCH ALL ws-f-error-status-code-table-array
                AT END
                   MOVE ws-f-error-status-code-table-desc-error-tag
                     TO ws-f-error-status-code-table-desc-error-aux
                   
              WHEN ws-f-error-status-code-table-code-error
                  (idx-error-status-code-table) IS EQUAL TO
                   ws-f-error-status-code-table-code-error-aux
                   MOVE ws-f-error-status-code-table-array
                       (idx-error-status-code-table)
                     TO ws-f-error-status-code-table-aux

            END-SEARCH.

       000200-get-current-date-and-time-record.
           DISPLAY SPACE
           DISPLAY "Running Information."

           MOVE FUNCTION CURRENT-DATE     TO ws-current-date-and-time
           PERFORM 000210-get-date-and-time-formatted
           DISPLAY "Current Date : [" ws-date-and-time-formatted "]."

           MOVE FUNCTION WHEN-COMPILED    TO ws-current-date-and-time
           PERFORM 000210-get-date-and-time-formatted
           DISPLAY "Latest Build : [" ws-date-and-time-formatted "].".

       000210-get-date-and-time-formatted.
           DISPLAY SPACE
           DISPLAY "Providing editing format to current date and time."

           MOVE ws-CDT-Year-Century       TO ws-FT-Year-Century
           MOVE ws-CDT-Year-Year          TO ws-FT-Year-Year

           MOVE ws-CDT-Month              TO ws-FT-Month
           MOVE ws-CDT-Day                TO ws-FT-Day
           
           MOVE ws-CDT-Hour               TO ws-FT-Hour
           MOVE ws-CDT-Minutes            TO ws-FT-Minutes
           MOVE ws-CDT-Seconds            TO ws-FT-Seconds
           MOVE ws-CDT-Hundredths-Of-Secs TO ws-FT-Hundredths-Of-Secs

           MOVE ws-CDT-GMT-Diff-Hours     TO ws-FT-GMT-Diff-Hours
           MOVE ws-CDT-GMT-Diff-Minutes   TO ws-FT-GMT-Diff-Minutes.

       000300-check-file-status-code.
           DISPLAY SPACE
           DISPLAY "+===+====+===+====+===+====+===+====+"
           DISPLAY "|      File status information.     |"
           DISPLAY "+===+====+===+====+===+====+===+====+"
           DISPLAY "| " asterisk " File Name   : [" 
                   ws-TempFile-name
                   "].   |"
           DISPLAY "| " asterisk " Index       : ["
                   idx-error-status-code-table
                   "].     |"
           DISPLAY "| " asterisk " Operation   : ["
                   ws-operation-class
                   "].  |"
           DISPLAY "+---+----+---+----+---+----+---+----+"
           DISPLAY "| " asterisk " Status Code : ["
                   ws-f-error-status-code-table-code-error-aux
                   "].             |"
           DISPLAY "| " asterisk " Description : "
                   "                  |"
           DISPLAY "| " asterisk " ["
                   ws-f-error-status-code-table-desc-error-aux
                   "].    |"
           DISPLAY "+===+====+===+====+===+====+===+====+"
           PERFORM 000600-press-enter-key-to-continue.

       000400-preliminary-review-employee-code-contents.
            DISPLAY SPACE
            DISPLAY asterisk
                    " Information on the last record processed and"
                    " reached. "
                    asterisk
            DISPLAY asterisk
                    " Code Employee: ["
                    ws-f-IdxFile-rec-cod-employee
                    "] = ["
                    f-IdxFile-rec-cod-employee
                    "]. "
                    asterisk
            PERFORM 000600-press-enter-key-to-continue.

       000500-preliminary-review-employee-salary-contents.
            DISPLAY SPACE
            DISPLAY asterisk
                    " Information on the last record processed and"
                    " reached. "
                    asterisk
            DISPLAY asterisk
                    " Salary Employee: ["
                    ws-f-IdxFile-rec-salary-employee-ed
                    "] = ["
                    f-IdxFile-rec-salary-employee
                    "]. "
                    asterisk
            PERFORM 000600-press-enter-key-to-continue.

       000600-press-enter-key-to-continue.
           DISPLAY "Press the ENTER key to continue..."
              WITH NO ADVANCING
            ACCEPT OMITTED.

       END DECLARATIVES.

       MAIN-PARAGRAPH.
           DISPLAY "Basic maintenance to an indexed sequential file."

           PERFORM 100000-start-begin-program
              THRU 100000-finish-begin-program

           IF  fs-IdxFile IS EQUAL TO ZEROES
           AND fs-OutFile IS EQUAL TO ZEROES
              PERFORM 200000-start-process-menu
                 THRU 200000-finish-process-menu
                UNTIL sw-menu-option-exit
           END-IF

           PERFORM 300000-start-end-program
              THRU 300000-finish-end-program

           DISPLAY "This program has ended."
           PERFORM 000600-press-enter-key-to-continue

           STOP RUN.

       100000-start-begin-program.
           PERFORM 110000-start-open-IdxFile
              THRU 110000-finish-open-IdxFile

           PERFORM 120000-start-open-OutFile
              THRU 120000-finish-open-OutFile.
       100000-finish-begin-program.
           EXIT.

        110000-start-open-IdxFile.
           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+---+"
           DISPLAY "|   Indexed Sequential File.   |"
           DISPLAY "+---+----+---+----+---+----+---+"

           MOVE SPACES                  TO ws-TempFile-name
           PERFORM 111000-start-capture-name-file
              THRU 111000-finish-capture-name-file
           MOVE ws-TempFile-name        TO ws-IdxFile-name

           DISPLAY "Idx File to work on: [" ws-IdxFile-name "]."

           SET sw-op-class-OPEN  TO TRUE
           OPEN I-O IdxFile

           DISPLAY "Opening. Status Code: [" fs-IdxFile "].".
        110000-finish-open-IdxFile.
           EXIT.

         111000-start-capture-name-file.
           DISPLAY asterisk " Enter the file name: " WITH NO ADVANCING
            ACCEPT ws-TempFile-name

           DISPLAY SPACE
           DISPLAY "Working File Name: [" ws-TempFile-name "]".

           IF ws-TempFile-name IS alphabetic-and-numeric  THEN
              IF ws-TempFile-name IS EQUAL TO SPACES THEN
                 DISPLAY asterisk
                         asterisk
                         "Blank filename!"
                         asterisk
                         asterisk
              ELSE
                 DISPLAY asterisk "Valid File Name!" asterisk
              END-IF
           ELSE
              DISPLAY asterisk asterisk
                      "It is not valid as a file name!"
                      asterisk asterisk
           END-IF

           DISPLAY SPACE.
         111000-finish-capture-name-file.
           EXIT.

        120000-start-open-OutFile.
           INITIALIZE f-OutFile-rec
                      ws-f-OutFile-rec

           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+---+"
           DISPLAY "|      Output Report File.     |"
           DISPLAY "+---+----+---+----+---+----+---+"

           MOVE SPACES                     TO ws-TempFile-name
           PERFORM 111000-start-capture-name-file
              THRU 111000-finish-capture-name-file
           MOVE ws-TempFile-name           TO ws-OutFile-name

           DISPLAY "Report File to work on: [" ws-OutFile-name "]."

           SET sw-op-class-OPEN            TO TRUE
           OPEN EXTEND OutFile

           DISPLAY "Opening. Status Code: [" fs-OutFile "].".

           IF (fs-OutFile         IS EQUAL TO ZEROES)
               MOVE cte-01                 TO ws-printed-pages

               PERFORM 121000-start-printout-headlines
                  THRU 121000-finish-printout-headlines
           END-IF.
        120000-finish-open-OutFile.
           EXIT.

         121000-start-printout-headlines.
           MOVE SPACES                     TO f-OutFile-rec
                                              ws-f-OutFile-rec
           MOVE ws-rep-page-heading-first-line
             TO f-OutFile-rec
                ws-f-OutFile-rec
           PERFORM 121100-start-write-output-report-record
              THRU 121100-finish-write-output-report-record

           MOVE ws-printed-pages           TO ws-rep-p-sec-l-pages-rep
           MOVE ws-rep-page-heading-second-line
             TO f-OutFile-rec
                ws-f-OutFile-rec
           PERFORM 121100-start-write-output-report-record
              THRU 121100-finish-write-output-report-record

           MOVE SPACES                     TO f-OutFile-rec
                                              ws-f-OutFile-rec
           MOVE FUNCTION CURRENT-DATE      TO ws-current-date-and-time
           PERFORM 000210-get-date-and-time-formatted
           MOVE ws-date-and-time-formatted TO f-OutFile-rec
                                              ws-f-OutFile-rec
           PERFORM 121100-start-write-output-report-record
              THRU 121100-finish-write-output-report-record

           MOVE SPACES                     TO f-OutFile-rec
                                              ws-f-OutFile-rec
           PERFORM 121100-start-write-output-report-record
              THRU 121100-finish-write-output-report-record

           MOVE ws-rep-page-heading-third-line
             TO f-OutFile-rec
                ws-f-OutFile-rec
           PERFORM 121100-start-write-output-report-record
              THRU 121100-finish-write-output-report-record

           MOVE ws-rep-page-heading-fourth-underlines
             TO f-OutFile-rec
                ws-f-OutFile-rec
           PERFORM 121100-start-write-output-report-record
              THRU 121100-finish-write-output-report-record

           MOVE SPACES                     TO f-OutFile-rec
                                              ws-f-OutFile-rec.
         121000-finish-printout-headlines.
           EXIT.

          121100-start-write-output-report-record.
            WRITE f-OutFile-rec          FROM ws-f-OutFile-rec
               AT END-OF-PAGE
                  PERFORM 121110-start-write-output-advance-page
                     THRU 121110-finish-write-output-advance-page
                  PERFORM 121000-start-printout-headlines
                     THRU 121000-finish-printout-headlines

              NOT AT EOP
                  ADD cte-01        TO ws-reporting-written-records-pag
                                       ws-reporting-written-records-sum
                  DISPLAY asterisk
                          "Inserted line: [" LINAGE-COUNTER "]."
                          asterisk

            END-WRITE

            DISPLAY "Writing. Status Code: ["  fs-OutFile "].".
          121100-finish-write-output-report-record.
            EXIT.

          121110-start-write-output-advance-page.
            ADD cte-01                     TO ws-printed-pages

            MOVE SPACES                    TO f-OutFile-rec
                                              ws-f-OutFile-rec
            WRITE f-OutFile-rec          FROM ws-f-OutFile-rec
            DISPLAY "Writing. Status Code: [" fs-OutFile "]."

            MOVE ws-reporting-read-records-page
              TO ws-rep-page-foot-recs-rep
            MOVE ws-rep-page-footing       TO f-OutFile-rec
                                              ws-f-OutFile-rec 
            WRITE f-OutFile-rec          FROM ws-f-OutFile-rec
            DISPLAY "Writing. Status Code: [" fs-OutFile "]."

            MOVE SPACES                    TO f-OutFile-rec
                                              ws-f-OutFile-rec
            WRITE f-OutFile-rec          FROM ws-f-OutFile-rec
            DISPLAY "Writing. Status Code: [" fs-OutFile "]."

            WRITE f-OutFile-rec          FROM ws-f-OutFile-rec
                  AFTER ADVANCING PAGE
            END-WRITE
            DISPLAY asterisk "Turn the page!" asterisk
            DISPLAY "Writing. Status Code: [" fs-OutFile "]."

            DISPLAY asterisk
                    "Records read per page: ["
                    ws-reporting-read-records-page "]."
                    asterisk
            DISPLAY asterisk
                    "Records written per page: ["
                    ws-reporting-written-records-pag "]."
                    asterisk

            INITIALIZE f-OutFile-rec
                       ws-f-OutFile-rec
            MOVE ZEROES             TO ws-reporting-read-records-page
                                       ws-reporting-written-records-pag

            DISPLAY asterisk
                    "Inserted line: [" LINAGE-COUNTER "]."
                    asterisk
            DISPLAY asterisk "Page feed inserted!" asterisk.
          121110-finish-write-output-advance-page.
            EXIT.

       200000-start-process-menu.
           INITIALIZE f-IdxFile-rec
                      f-OutFile-rec
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
                      UNTIL sw-menu-mode-r-d-keyacc-exitmenu

               WHEN sw-menu-option-look-for-all
                    PERFORM 225000-start-look-for-all-records
                       THRU 225000-finish-look-for-all-records
                      UNTIL sw-menu-mode-r-o-exitmenu

               WHEN sw-menu-option-exit
                    DISPLAY "Leaving this program..."

               WHEN OTHER
                    DISPLAY "Unrecognized option. Please try again!"

           END-EVALUATE.
        220000-finish-validate-selected-menu-option.
           EXIT.

         221000-start-add-a-record.
           INITIALIZE ws-f-IdxFile-error-status-code-indicators
                      ws-menu-standard-options-performance
                      ws-realization-questions

           PERFORM 221100-start-capture-key-field
              THRU 221100-finish-capture-key-field

           PERFORM 221200-start-look-for-a-record
              THRU 221200-finish-look-for-a-record

           IF (sw-IdxFile-record-found-N) THEN
               PERFORM 221300-start-continue-carry-out-oper
                  THRU 221300-finish-continue-carry-out-oper
                  WITH TEST AFTER
                 UNTIL sw-carry-out-sure-Y OR sw-carry-out-sure-N

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
              THRU 221600-finish-continue-operation
              WITH TEST AFTER
             UNTIL sw-continue-response-Y OR sw-continue-response-N.
         221000-finish-add-a-record.
           EXIT.

          221100-start-capture-key-field.
            DISPLAY asterisk " Employee Code   : " WITH NO ADVANCING
            ACCEPT ws-f-IdxFile-rec-cod-employee

            MOVE ws-f-IdxFile-rec-cod-employee
              TO f-IdxFile-rec-cod-employee

            IF ws-f-IdxFile-rec-cod-employee IS GREATER THAN ZEROES
               DISPLAY asterisk asterisk
                       "The employee code is greater than zeroes. OK!"
                       asterisk asterisk
            ELSE
               DISPLAY asterisk asterisk
                       "The employee code is not greater than zeroes!"
                       asterisk asterisk
            END-IF.
          221100-finish-capture-key-field.
            EXIT.

          221200-start-look-for-a-record.
            SET sw-op-class-READ    TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents

            READ IdxFile RECORD                   INTO ws-f-IdxFile-rec
             KEY IS f-IdxFile-rec-cod-employee
                 INVALID KEY
                         SET sw-IdxFile-record-found-N TO TRUE
                         DISPLAY "Record Not Found!"
                         PERFORM 000600-press-enter-key-to-continue

             NOT INVALID KEY
                         ADD cte-01        TO ws-reading-records
                         SET sw-IdxFile-record-found-Y TO TRUE
                         DISPLAY "Record found successfully!"

                         PERFORM 000600-press-enter-key-to-continue
                         PERFORM 221210-start-show-file-info
                            THRU 221210-finish-show-file-info
                         PERFORM 221220-start-write-report-outp-record
                            THRU 221220-finish-write-report-outp-record

            END-READ

            DISPLAY "Reading. Status Code: [" fs-IdxFile "].".
         221200-finish-look-for-a-record.
            EXIT.

         221210-start-show-file-info.
            MOVE ws-f-IdxFile-rec-salary-employee
              TO ws-f-IdxFile-rec-salary-employee-ed

            DISPLAY SPACE
            DISPLAY "+---+----+---+----+---+----+---+"
            DISPLAY "|     Employee Information.    |"
            DISPLAY "+---+----+---+----+---+----+---+"
            DISPLAY "| Code   : ["
                    ws-f-IdxFile-rec-cod-employee
                    "].           |"
            DISPLAY "| Salary : ["
                    ws-f-IdxFile-rec-salary-employee-ed
                    "].    |"
            DISPLAY "+---+----+---+----+---+----+---+"

            PERFORM 000600-press-enter-key-to-continue

            DISPLAY SPACE.
         221210-finish-show-file-info.
            EXIT.

         221220-start-write-report-outp-record.
            INITIALIZE f-OutFile-rec
                       ws-f-OutFile-rec

            SET sw-question-print-rec-msg TO TRUE
            MOVE SPACE                    TO ws-captured-answer
            PERFORM 221221-start-display-captured-selected-option
               THRU 221221-finish-display-captured-selected-option
               WITH TEST AFTER
              UNTIL sw-captured-answer-Y  OR sw-captured-answer-N
            MOVE ws-captured-answer       TO ws-question-print-record

            IF sw-question-print-rec-Y
               ADD cte-01          TO ws-reporting-read-records-page
                                      ws-reporting-read-records-sum

               MOVE ws-reporting-read-records-page
                 TO ws-f-OutFile-rec-record-counter

               MOVE LINAGE-COUNTER TO ws-last-printed-report-line

               MOVE ws-f-IdxFile-rec-cod-employee
                 TO ws-f-OutFile-rec-cod-employee

               MOVE ws-f-IdxFile-rec-salary-employee
                 TO ws-f-OutFile-rec-salary-employee

               PERFORM 121100-start-write-output-report-record
                  THRU 121100-finish-write-output-report-record
            END-IF.
         221220-finish-write-report-outp-record.
            EXIT.

          221221-start-display-captured-selected-option.
            MOVE SPACE                    TO ws-captured-answer

            DISPLAY asterisk
                    ws-realization-question-message
                    WITH NO ADVANCING
            ACCEPT ws-captured-answer

            DISPLAY asterisk
                    "The selected option was: ["
                    ws-captured-answer
                    "]."
                    asterisk

            IF (sw-captured-answer-Y) OR (sw-captured-answer-N)
               DISPLAY asterisk
                       "The captured option was: ["
                       ws-captured-answer
                       "]. OK!"
                       asterisk
            ELSE
               DISPLAY asterisk
                       "Invalid answer: [" ws-captured-answer "]. "
                       "Please correct it now!"
                       asterisk
            END-IF

            DISPLAY SPACE.
          221221-finish-display-captured-selected-option.
            EXIT.

         221300-start-continue-carry-out-oper.
            SET sw-carry-out-sure-msg     TO TRUE
            MOVE SPACE                    TO ws-captured-answer

            PERFORM 221221-start-display-captured-selected-option
               THRU 221221-finish-display-captured-selected-option
               WITH TEST AFTER
              UNTIL sw-captured-answer-Y OR sw-captured-answer-N

            MOVE ws-captured-answer       TO ws-carry-out-sure.
         221300-finish-continue-carry-out-oper.
            EXIT.

         221400-start-capture-salary-employee.
            DISPLAY asterisk " Employee Salary : " WITH NO ADVANCING
            ACCEPT ws-f-IdxFile-rec-salary-employee

            MOVE ws-f-IdxFile-rec-salary-employee
              TO ws-f-IdxFile-rec-salary-employee-ed
                 f-IdxFile-rec-salary-employee

            IF ws-f-IdxFile-rec-salary-employee IS POSITIVE
               DISPLAY asterisk asterisk
                       "The salary of employee is positive. OK!"
                       asterisk asterisk
            ELSE
               DISPLAY asterisk asterisk
                       "The salary of employee is not positive!"
                       asterisk asterisk
            END-IF.
         221400-finish-capture-salary-employee.
            EXIT.

         221500-start-store-a-record.
            SET sw-op-class-WRITE   TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents

            WRITE f-IdxFile-rec          FROM ws-f-IdxFile-rec
                  INVALID KEY
                          DISPLAY asterisk "Duplicate Key!" asterisk

              NOT INVALID KEY
                          ADD cte-01       TO ws-written-records

                          DISPLAY asterisk
                                  "Record saved successfully!"
                                  asterisk

            END-WRITE

            DISPLAY "Writing. Status Code: [" fs-IdxFile "]."

            PERFORM 000600-press-enter-key-to-continue.
         221500-finish-store-a-record.
            EXIT.

         221600-start-continue-operation.
            SET sw-continue-response-msg  TO TRUE
            MOVE SPACE                    TO ws-captured-answer

            PERFORM 221221-start-display-captured-selected-option
               THRU 221221-finish-display-captured-selected-option
               WITH TEST AFTER
              UNTIL sw-captured-answer-Y OR sw-captured-answer-N

            MOVE ws-captured-answer       TO ws-continue-response.
         221600-finish-continue-operation.
            EXIT.

         222000-start-delete-a-record.
           INITIALIZE ws-f-IdxFile-error-status-code-indicators
                      ws-menu-standard-options-performance
                      ws-realization-questions

           PERFORM 221100-start-capture-key-field
              THRU 221100-finish-capture-key-field

           PERFORM 221200-start-look-for-a-record
              THRU 221200-finish-look-for-a-record

           IF (sw-IdxFile-record-found-Y) THEN
               PERFORM 221300-start-continue-carry-out-oper
                  THRU 221300-finish-continue-carry-out-oper
                  WITH TEST AFTER
                 UNTIL sw-carry-out-sure-Y OR sw-carry-out-sure-N

               IF (sw-carry-out-sure-Y)   THEN
                   PERFORM 222100-start-eliminate-a-record
                      THRU 222100-finish-eliminate-a-record
               ELSE
                   DISPLAY "Operation not performed. File unchanged."
               END-IF
           END-IF

           PERFORM 221600-start-continue-operation
              THRU 221600-finish-continue-operation
              WITH TEST AFTER
             UNTIL sw-continue-response-Y OR sw-continue-response-N.
         222000-finish-delete-a-record.
            EXIT.

         222100-start-eliminate-a-record.
            SET sw-op-class-DELETE  TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents

            DELETE IdxFile RECORD
                   INVALID KEY
                           DISPLAY asterisk "Invalid Key!" asterisk

               NOT INVALID KEY
                           ADD cte-01      TO ws-eliminated-records

                           DISPLAY asterisk
                                   "Record deleted successfully!"
                                   asterisk

            END-DELETE

            DISPLAY "Deleting. Status Code: [" fs-IdxFile "]."

            PERFORM 000600-press-enter-key-to-continue.
         222100-finish-eliminate-a-record.
            EXIT.

         223000-start-modify-a-record.
            INITIALIZE ws-f-IdxFile-error-status-code-indicators
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
            IF (sw-IdxFile-record-found-Y) THEN
                PERFORM 221300-start-continue-carry-out-oper
                   THRU 221300-finish-continue-carry-out-oper
                   WITH TEST AFTER
                  UNTIL sw-carry-out-sure-Y OR sw-carry-out-sure-N

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
            SET sw-op-class-REWRITE TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents

            REWRITE f-IdxFile-rec        FROM ws-f-IdxFile-rec
                    INVALID KEY
                            DISPLAY asterisk "Invalid Key!" asterisk

                NOT INVALID KEY
                            ADD cte-01     TO ws-rewritten-records

                            DISPLAY asterisk
                                    "Record changed successfully!"
                                    asterisk

            END-REWRITE

            DISPLAY "Rewriting. Status Code: [" fs-IdxFile "]."

            PERFORM 000600-press-enter-key-to-continue.
          223211-finish-change-a-record.
            EXIT.

         224000-start-look-for-any-record.
           INITIALIZE ws-f-IdxFile-error-status-code-indicators
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
            ACCEPT ws-menu-mode-read-direct-key

           DISPLAY "The chosen option was: "
                   ws-menu-mode-read-direct-key.
          224100-finish-show-menu-look-for-keyaccess.
            EXIT.

          224200-start-show-validate-menu-look-for-keyaccess.
            EVALUATE TRUE
                WHEN sw-menu-mode-r-d-keyacc-code
                     PERFORM 224210-start-show-mode-look-for-code
                        THRU 224210-finish-show-mode-look-for-code
                       UNTIL sw-menu-mode-r-d-exitmenu
 
                WHEN sw-menu-mode-r-d-keyacc-salary
                     PERFORM 224220-start-show-mode-look-for-rc-sal
                        THRU 224220-finish-show-mode-look-for-rc-sal
                       UNTIL sw-menu-mode-r-d-exitmenu

                WHEN sw-menu-mode-r-d-keyacc-exitmenu
                     DISPLAY "Quitting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. "
                             "Please correct your choice."

           END-EVALUATE.
          224200-finish-show-validate-menu-look-for-keyaccess.
            EXIT.

          224210-start-show-mode-look-for-code.
            INITIALIZE ws-f-IdxFile-error-status-code-indicators
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
             ACCEPT ws-menu-mode-read-dir-opt

            DISPLAY "The chosen option was: "
                   ws-menu-mode-read-dir-opt.
          224211-finish-show-reading-direct-menu.
            EXIT.

          224212-start-validate-reading-direct-menu.
            EVALUATE TRUE
                WHEN sw-menu-mode-r-d-read
                     PERFORM 2242121-start-routine-mode-read-direct-rd
                        THRU 2242121-finish-routine-mode-read-direct-rd
                       UNTIL sw-continue-response-N

                WHEN sw-menu-mode-r-d-and-seq
                     PERFORM 2242122-start-routine-mode-read-dir-seq
                        THRU 2242122-finish-routine-mode-read-dir-seq
                       UNTIL sw-continue-response-N

                WHEN sw-menu-mode-r-d-exitmenu
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
               THRU 221600-finish-continue-operation
               WITH TEST AFTER
              UNTIL sw-continue-response-Y OR sw-continue-response-N.
          2242121-finish-routine-mode-read-direct-rd.
            EXIT.

          2242122-start-routine-mode-read-dir-seq.
            PERFORM 221100-start-capture-key-field
               THRU 221100-finish-capture-key-field

            PERFORM 2252211-start-menu-mode-code-pos-eq
               THRU 2252211-finish-menu-mode-code-pos-eq

            IF (sw-IdxFile-record-found-Y)
                PERFORM 225260-start-menu-mode-read-forwarding
                   THRU 225260-finish-menu-mode-read-forwarding
            END-IF

            PERFORM 221600-start-continue-operation
               THRU 221600-finish-continue-operation
               WITH TEST AFTER
              UNTIL sw-continue-response-Y OR sw-continue-response-N.
          2242122-finish-routine-mode-read-dir-seq.
            EXIT.

          224220-start-show-mode-look-for-rc-sal.
            INITIALIZE ws-f-IdxFile-error-status-code-indicators
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
                WHEN sw-menu-mode-r-d-read
                     PERFORM 2242211-start-routine-mode-read-direct-sal
                        THRU 2242211-finish-routine-mode-read-direct-sal
                       UNTIL sw-continue-response-N

                WHEN sw-menu-mode-r-d-and-seq
                     PERFORM 2242212-start-routine-mode-read-dirseq-sal
                        THRU 2242212-finish-routine-mode-read-dirseq-sal
                       UNTIL sw-continue-response-N

                WHEN sw-menu-mode-r-d-exitmenu
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
               THRU 221600-finish-continue-operation
               WITH TEST AFTER
              UNTIL sw-continue-response-Y OR sw-continue-response-N.
          2242211-finish-routine-mode-read-direct-sal.
            EXIT.

          22422111-start-read-record-salary-employee.
            SET sw-op-class-READ           TO TRUE

            PERFORM 000500-preliminary-review-employee-salary-contents

            READ IdxFile RECORD                   INTO ws-f-IdxFile-rec
             KEY IS f-IdxFile-rec-salary-employee
                 INVALID KEY
                         SET sw-IdxFile-record-found-N TO TRUE
                         DISPLAY "Record Not Found!"
                         PERFORM 000600-press-enter-key-to-continue

             NOT INVALID KEY
                         ADD cte-01        TO ws-reading-records
                         SET sw-IdxFile-record-found-Y TO TRUE
                         DISPLAY "Record found successfully!"

                         PERFORM 000600-press-enter-key-to-continue
                         PERFORM 221210-start-show-file-info
                            THRU 221210-finish-show-file-info
                         PERFORM 221220-start-write-report-outp-record
                            THRU 221220-finish-write-report-outp-record

            END-READ

            DISPLAY "Reading. Status Code: [" fs-IdxFile "].".
          22422111-finish-read-record-salary-employee.
            EXIT.

          2242212-start-routine-mode-read-dirseq-sal.
            PERFORM 221400-start-capture-salary-employee
               THRU 221400-finish-capture-salary-employee

            PERFORM 22422121-start-routine-mode-locate-for-sal
               THRU 22422121-finish-routine-mode-locate-for-sal

            IF (sw-IdxFile-record-found-Y)
                PERFORM 225260-start-menu-mode-read-forwarding
                   THRU 225260-finish-menu-mode-read-forwarding
            END-IF

            PERFORM 221600-start-continue-operation
               THRU 221600-finish-continue-operation
               WITH TEST AFTER
              UNTIL sw-continue-response-Y OR sw-continue-response-N.
          2242212-finish-routine-mode-read-dirseq-sal.
            EXIT.

          22422121-start-routine-mode-locate-for-sal.
            SET sw-op-class-STARTEQ       TO TRUE

            PERFORM 000500-preliminary-review-employee-salary-contents

            START IdxFile
              KEY IS EQUAL TO f-IdxFile-rec-salary-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary could not be located for an "
                          "exactly equal or identical value from the "
                          "existing ones."

                  SET sw-IdxFile-record-found-N  TO TRUE
                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD cte-01              TO ws-repositioning-records
                  SET sw-IdxFile-record-found-Y  TO TRUE

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The salary: ["
                           ws-f-IdxFile-rec-salary-employee
                          "] was found for a value that was exactly the"
                          " same or identical to the existing ones: "
                          "[" f-IdxFile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning exact salary done correctly!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          22422121-finish-routine-mode-locate-for-sal.
            EXIT.

         225000-start-look-for-all-records.
           INITIALIZE ws-f-IdxFile-error-status-code-indicators
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
                WHEN sw-menu-mode-r-o-start
                     PERFORM 225210-start-menu-mode-start-position
                        THRU 225210-finish-menu-mode-start-position

                WHEN sw-menu-mode-r-o-givenkey-eq
                     PERFORM 225220-start-locate-givenkey-code-salary
                        THRU 225220-finish-locate-givenkey-code-salary
                       UNTIL sw-menu-mode-r-d-keyacc-exitmenu

                WHEN sw-menu-mode-r-o-givenkey-apprx
                     PERFORM 225230-start-menu-read-approx-code-salary
                        THRU 225230-finish-menu-read-approx-code-salary
                       UNTIL sw-menu-mode-r-d-keyacc-exitmenu

                WHEN sw-menu-mode-r-o-finish
                     PERFORM 225240-start-menu-mode-finish-position
                        THRU 225240-finish-menu-mode-finish-position

                WHEN sw-menu-mode-r-o-r-first-rcrd
                     PERFORM 225210-start-menu-mode-start-position
                        THRU 225210-finish-menu-mode-start-position
                     PERFORM 225260-start-menu-mode-read-forwarding
                        THRU 225260-finish-menu-mode-read-forwarding

                WHEN sw-menu-mode-r-o-r-last-rcrd
                     PERFORM 225240-start-menu-mode-finish-position
                        THRU 225240-finish-menu-mode-finish-position
                     PERFORM 225250-start-menu-mode-read-backwarding
                        THRU 225250-finish-menu-mode-read-backwarding

                WHEN sw-menu-mode-r-o-r-backward
                     PERFORM 225270-start-menu-mode-trace-backwarding
                        THRU 225270-finish-menu-mode-trace-backwarding
                       UNTIL fs-IdxFile IS NOT EQUAL TO ZEROES
                          OR sw-IdxFile-EOF-Y OR sw-carry-out-sure-N

                WHEN sw-menu-mode-r-o-r-forward
                     PERFORM 225280-start-menu-mode-trace-forwarding
                        THRU 225280-finish-menu-mode-trace-forwarding
                       UNTIL fs-IdxFile IS NOT EQUAL TO ZEROES
                          OR sw-IdxFile-EOF-Y OR sw-carry-out-sure-N

                WHEN sw-menu-mode-r-o-prev-rcrd
                     PERFORM 225250-start-menu-mode-read-backwarding
                        THRU 225250-finish-menu-mode-read-backwarding

                WHEN sw-menu-mode-r-o-next-rcrd
                     PERFORM 225260-start-menu-mode-read-forwarding
                        THRU 225260-finish-menu-mode-read-forwarding

                WHEN sw-menu-mode-r-o-exitmenu 
                     DISPLAY "Returning to main menu..."

                WHEN OTHER
                     DISPLAY "Unrecognized option. Please try again!"

            END-EVALUATE.
          225200-finish-validate-option-menu-reading-offset.
            EXIT.

          225210-start-menu-mode-start-position.
            SET sw-op-class-STARTFRST  TO TRUE

            START IdxFile FIRST
                  INVALID KEY
                          DISPLAY asterisk asterisk
                                  "Error positioning at begin!"
                                  asterisk asterisk

              NOT INVALID KEY
                          ADD  cte-01      TO ws-repositioning-records

                          DISPLAY asterisk
                                  "Positioning at the begin."
                                  asterisk

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "]."
            PERFORM 000600-press-enter-key-to-continue.
          225210-finish-menu-mode-start-position.
            EXIT.

          225220-start-locate-givenkey-code-salary.
            INITIALIZE ws-f-IdxFile-error-status-code-indicators
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
                WHEN sw-menu-mode-r-d-keyacc-code
                     PERFORM 221100-start-capture-key-field
                        THRU 221100-finish-capture-key-field
                     PERFORM 2252211-start-menu-mode-code-pos-eq
                        THRU 2252211-finish-menu-mode-code-pos-eq

                WHEN sw-menu-mode-r-d-keyacc-salary
                     PERFORM 221400-start-capture-salary-employee
                        THRU 221400-finish-capture-salary-employee
                     PERFORM 22422121-start-routine-mode-locate-for-sal
                        THRU 22422121-finish-routine-mode-locate-for-sal

                WHEN sw-menu-mode-r-d-keyacc-exitmenu
                     DISPLAY "Quitting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. Please correct and change"
                             "your option..."

            END-EVALUATE.
          225221-finish-validate-menu-locate-givenkey-code-sl.
            EXIT.

          2252211-start-menu-mode-code-pos-eq.
            SET sw-op-class-STARTEQ       TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents

            START IdxFile
              KEY IS EQUAL TO f-IdxFile-rec-cod-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for an "
                          "exactly equal or identical key from the "
                          "existing ones."
                          asterisk asterisk

                  SET sw-IdxFile-record-found-N  TO TRUE
                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD cte-01              TO ws-repositioning-records
                  SET sw-IdxFile-record-found-Y  TO TRUE

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The value: [" ws-f-IdxFile-rec-cod-employee
                          "] was found for a key that was exactly the "
                          "same or identical to the existing ones: "
                          "[" f-IdxFile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning exact key done correctly!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          2252211-finish-menu-mode-code-pos-eq.
            EXIT.

          225230-start-menu-read-approx-code-salary.
            INITIALIZE ws-f-IdxFile-error-status-code-indicators
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
                WHEN sw-menu-mode-r-d-keyacc-code
                     PERFORM 2252311-start-menu-read-code-apprx
                        THRU 2252311-finish-menu-read-code-apprx
                       UNTIL sw-menu-mode-r-o-givenkey-exit

                WHEN sw-menu-mode-r-d-keyacc-salary
                     PERFORM 2252312-start-menu-read-salary-apprx
                        THRU 2252312-finish-menu-read-salary-apprx
                       UNTIL sw-menu-mode-r-o-givenkey-exit

                WHEN sw-menu-mode-r-d-keyacc-exitmenu
                     DISPLAY "Quitting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. Please correct and "
                             "change your option..."

            END-EVALUATE.
          225231-finish-validate-menu-read-apprx-cod-sal.
            EXIT.

          2252311-start-menu-read-code-apprx.
            INITIALIZE ws-f-IdxFile-error-status-code-indicators
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
             ACCEPT ws-menu-mode-read-opt-givkey

            DISPLAY "The chosen option was: "
                    ws-menu-mode-read-opt-givkey.
          22523111-finish-show-approximate-offset-menu.
            EXIT.

          22523112-start-validate-approximate-offset-menu.
            IF NOT(sw-menu-mode-r-o-givenkey-exit)
               PERFORM 221100-start-capture-key-field
                  THRU 221100-finish-capture-key-field
            END-IF

            EVALUATE TRUE
                WHEN sw-menu-mode-r-o-givenkey-ngt
                     PERFORM 225231121-start-menu-mode-code-pos-ngt
                        THRU 225231121-finish-menu-mode-code-pos-ngt

                WHEN sw-menu-mode-r-o-givenkey-gt
                     PERFORM 225231122-start-menu-mode-code-pos-gt
                        THRU 225231122-finish-menu-mode-code-pos-gt

                WHEN sw-menu-mode-r-o-givenkey-gteq
                     PERFORM 225231123-start-menu-mode-code-pos-gteq
                        THRU 225231123-finish-menu-mode-code-pos-gteq

                WHEN sw-menu-mode-r-o-givenkey-nlt
                     PERFORM 225231124-start-menu-mode-code-pos-nlt
                        THRU 225231124-finish-menu-mode-code-pos-nlt

                WHEN sw-menu-mode-r-o-givenkey-lt
                     PERFORM 225231125-start-menu-mode-code-pos-lt
                        THRU 225231125-finish-menu-mode-code-pos-lt

                WHEN sw-menu-mode-r-o-givenkey-lteq
                     PERFORM 225231126-start-menu-mode-code-pos-lteq
                        THRU 225231126-finish-menu-mode-code-pos-lteq

                WHEN sw-menu-mode-r-o-givenkey-exit
                     DISPLAY "Quitting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. "
                             "Please change your option."

            END-EVALUATE.
          22523112-finish-validate-approximate-offset-menu.
            EXIT.

          225231121-start-menu-mode-code-pos-ngt.
            SET sw-op-class-STARTNGT   TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents.

            START IdxFile
              KEY IS NOT GREATER THAN f-IdxFile-rec-cod-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for a key "
                          "is not greater than one of those existing."
                          asterisk asterisk

                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The value: [" ws-f-IdxFile-rec-cod-employee
                          "] was found for a key that wasn't "
                          "greater than one of the existing ones: "
                          "[" f-IdxFile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Correct positioning on nearest lower key!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          225231121-finish-menu-mode-code-pos-ngt.
            EXIT.

          225231122-start-menu-mode-code-pos-gt.
            SET sw-op-class-STARTGT TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents.

            START IdxFile
              KEY IS GREATER THAN f-IdxFile-rec-cod-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for a key "
                          "greater than one of those existing."
                          asterisk asterisk

                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The value: [" ws-f-IdxFile-rec-cod-employee
                          "] was found for a key that was "
                          "greater than one of the existing ones: "
                          "[" f-IdxFile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Correct positioning on nearest upper key!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          225231122-finish-menu-mode-code-pos-gt.
            EXIT.

          225231123-start-menu-mode-code-pos-gteq.
            SET sw-op-class-STARTGTEQ  TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents.

            START IdxFile
              KEY IS GREATER THAN OR EQUAL TO f-IdxFile-rec-cod-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for a key "
                          "greater than or equal to those existing."
                          asterisk asterisk

                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The value: [" ws-f-IdxFile-rec-cod-employee
                          "] was found for a key that was greater than "
                          "or equal to one of the existing ones: "
                          "[" f-IdxFile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Correct positioning on nearest upper key!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          225231123-finish-menu-mode-code-pos-gteq.
            EXIT.

          225231124-start-menu-mode-code-pos-nlt.
            SET sw-op-class-STARTNLT   TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents

            START IdxFile
              KEY IS NOT LESS THAN f-IdxFile-rec-cod-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for a key "
                          "is not less than one of those existing."
                          asterisk asterisk

                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225240-start-menu-mode-finish-position
                     THRU 225240-finish-menu-mode-finish-position

             NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The value: [" ws-f-IdxFile-rec-cod-employee
                          "] was found for a key that wasn't less than "
                          "one of the existing ones: "
                          "[" f-IdxFile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Correct positioning on nearest upper key!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          225231124-finish-menu-mode-code-pos-nlt.
            EXIT.

          225231125-start-menu-mode-code-pos-lt.
            SET sw-op-class-STARTLT    TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents

            START IdxFile
              KEY IS LESS THAN f-IdxFile-rec-cod-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for a key "
                          "less than one of those existing."
                          asterisk asterisk

                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225240-start-menu-mode-finish-position
                     THRU 225240-finish-menu-mode-finish-position

             NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The value: [" ws-f-IdxFile-rec-cod-employee
                          "] was found for a key that was less than "
                          "one of the existing ones: "
                          "[" f-IdxFile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Correct positioning on nearest lower key!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          225231125-finish-menu-mode-code-pos-lt.
            EXIT.

          225231126-start-menu-mode-code-pos-lteq.
            SET sw-op-class-STARTLTEQ  TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents

            START IdxFile
              KEY IS LESS THAN OR EQUAL TO f-IdxFile-rec-cod-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY asterisk asterisk
                          "The value could not be located for a key "
                          "less than or equal to those existing."
                          asterisk asterisk

                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225240-start-menu-mode-finish-position
                     THRU 225240-finish-menu-mode-finish-position

              NOT INVALID KEY
                  ADD  cte-01              TO ws-repositioning-records

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The value: [" ws-f-IdxFile-rec-cod-employee
                          "] was found for a key that was less than "
                          "or equal to one than of the existing ones: "
                          "[" f-IdxFile-rec-cod-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Correct positioning on nearest lower key!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          225231126-finish-menu-mode-code-pos-lteq.
            EXIT.

          2252312-start-menu-read-salary-apprx.
            INITIALIZE ws-f-IdxFile-error-status-code-indicators
                       ws-menu-standard-options-performance
                       ws-realization-questions

            PERFORM 22523111-start-show-approximate-offset-menu
               THRU 22523111-finish-show-approximate-offset-menu

            PERFORM 22523121-start-validate-loc-opt-salary-employee
               THRU 22523121-finish-validate-loc-opt-salary-employee.
          2252312-finish-menu-read-salary-apprx.
            EXIT.

          22523121-start-validate-loc-opt-salary-employee.
            IF NOT(sw-menu-mode-r-o-givenkey-exit)
               PERFORM 221400-start-capture-salary-employee
                  THRU 221400-finish-capture-salary-employee
            END-IF

            EVALUATE TRUE
                WHEN sw-menu-mode-r-o-givenkey-ngt
                     PERFORM 225231211-start-locate-salary-key-ngt
                        THRU 225231211-finish-locate-salary-key-ngt

                WHEN sw-menu-mode-r-o-givenkey-gt
                     PERFORM 225231212-start-locate-salary-key-gt
                        THRU 225231212-finish-locate-salary-key-gt

                WHEN sw-menu-mode-r-o-givenkey-gteq
                     PERFORM 225231213-start-locate-salary-key-gteq
                        THRU 225231213-finish-locate-salary-key-gteq

                WHEN sw-menu-mode-r-o-givenkey-nlt
                     PERFORM 225231214-start-locate-salary-key-nlt
                        THRU 225231214-finish-locate-salary-key-nlt

                WHEN sw-menu-mode-r-o-givenkey-lt
                     PERFORM 225231215-start-locate-salary-key-lt
                        THRU 225231215-finish-locate-salary-key-lt

                WHEN sw-menu-mode-r-o-givenkey-lteq
                     PERFORM 225231216-start-locate-salary-key-lteq
                        THRU 225231216-finish-locate-salary-key-lteq

                WHEN sw-menu-mode-r-o-givenkey-exit
                     DISPLAY "Exiting this menu..."

                WHEN OTHER
                     DISPLAY "Invalid option. "
                             "Please change your option and correct it."

            END-EVALUATE.
          22523121-finish-validate-loc-opt-salary-employee.
            EXIT.

          225231211-start-locate-salary-key-ngt.
            SET sw-op-class-STARTNGT      TO TRUE

            PERFORM 000500-preliminary-review-employee-salary-contents

            START IdxFile
              KEY IS NOT GREATER THAN f-IdxFile-rec-salary-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary cannot be allocated for a value "
                          "is not greater than that of the existing "
                          "ones."

                  SET sw-IdxFile-record-found-N  TO TRUE
                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD cte-01              TO ws-repositioning-records
                  SET sw-IdxFile-record-found-Y  TO TRUE

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The salary: ["
                           ws-f-IdxFile-rec-salary-employee
                          "] was found for a value that wasn't greater "
                          "than that of the existing ones: "
                          "[" f-IdxFile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning upper salary done correctly!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          225231211-finish-locate-salary-key-ngt.
            EXIT.

          225231212-start-locate-salary-key-gt.
            SET sw-op-class-STARTGT       TO TRUE

            PERFORM 000500-preliminary-review-employee-salary-contents

            START IdxFile
              KEY IS GREATER THAN f-IdxFile-rec-salary-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary cannot be allocated for a value "
                          "is greater than that of the existing ones."

                  SET sw-IdxFile-record-found-N  TO TRUE
                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD cte-01              TO ws-repositioning-records
                  SET sw-IdxFile-record-found-Y  TO TRUE

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The salary: ["
                           ws-f-IdxFile-rec-salary-employee
                          "] was found for a value that was greater "
                          "than that of the existing ones: "
                          "[" f-IdxFile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning upper salary done correctly!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          225231212-finish-locate-salary-key-gt.
            EXIT.

          225231213-start-locate-salary-key-gteq.
            SET sw-op-class-STARTGTEQ     TO TRUE

            PERFORM 000500-preliminary-review-employee-salary-contents

            START IdxFile
              KEY IS GREATER OR EQUAL TO f-IdxFile-rec-salary-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary cannot be allocated for a value "
                          "is greater than or equal to that of the "
                          "existing ones."

                  SET sw-IdxFile-record-found-N  TO TRUE
                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD cte-01              TO ws-repositioning-records
                  SET sw-IdxFile-record-found-Y  TO TRUE

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The salary: ["
                           ws-f-IdxFile-rec-salary-employee
                          "] was found for a value that was greater "
                          "than or equal to the existing ones: "
                          "[" f-IdxFile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning upper or equal salary "
                          "done correctly!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          225231213-finish-locate-salary-key-gteq.
            EXIT.

          225231214-start-locate-salary-key-nlt.
            SET sw-op-class-STARTNLT      TO TRUE

            PERFORM 000500-preliminary-review-employee-salary-contents

            START IdxFile
              KEY IS NOT LESS THAN f-IdxFile-rec-salary-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary cannot be allocated for a value "
                          "is not less than that of the existing ones."

                  SET sw-IdxFile-record-found-N  TO TRUE
                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD cte-01              TO ws-repositioning-records
                  SET sw-IdxFile-record-found-Y  TO TRUE

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The salary: ["
                           ws-f-IdxFile-rec-salary-employee
                          "] was found for a value that wasn't less "
                          "than the existing ones: "
                          "[" f-IdxFile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning upper salary done correctly!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          225231214-finish-locate-salary-key-nlt.
            EXIT.

          225231215-start-locate-salary-key-lt.
            SET sw-op-class-STARTLT       TO TRUE

            PERFORM 000500-preliminary-review-employee-salary-contents

            START IdxFile
              KEY IS LESS THAN f-IdxFile-rec-salary-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary cannot be allocated for a value "
                          "is less than that of the existing ones."

                  SET sw-IdxFile-record-found-N  TO TRUE
                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD cte-01              TO ws-repositioning-records
                  SET sw-IdxFile-record-found-Y  TO TRUE

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The salary: ["
                           ws-f-IdxFile-rec-salary-employee
                          "] was found for a value that was less "
                          "than that of the existing ones: "
                          "[" f-IdxFile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning lower salary done correctly!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          225231215-finish-locate-salary-key-lt.
            EXIT.

          225231216-start-locate-salary-key-lteq.
            SET sw-op-class-STARTLTEQ     TO TRUE

            PERFORM 000500-preliminary-review-employee-salary-contents

            START IdxFile
              KEY IS LESS THAN OR EQUAL TO f-IdxFile-rec-salary-employee
                  INVALID KEY
                  DISPLAY SPACE
                  DISPLAY "Invalid Key!"
                  DISPLAY "The salary cannot be allocated for a value "
                          "is less than or equal to that of the "
                          "existing ones."

                  SET sw-IdxFile-record-found-N  TO TRUE
                  PERFORM 000600-press-enter-key-to-continue
                  PERFORM 225210-start-menu-mode-start-position
                     THRU 225210-finish-menu-mode-start-position

              NOT INVALID KEY
                  ADD cte-01              TO ws-repositioning-records
                  SET sw-IdxFile-record-found-Y  TO TRUE

                  DISPLAY SPACE
                  DISPLAY asterisk
                          "The salary: ["
                           ws-f-IdxFile-rec-salary-employee
                          "] was found for a value that was less "
                          "than or equal to the existing ones: "
                          "[" f-IdxFile-rec-salary-employee "]."
                          asterisk
                  DISPLAY asterisk
                          "Positioning lower or equal salary "
                          "done correctly!"
                          asterisk
                  PERFORM 000600-press-enter-key-to-continue

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "].".
          225231216-finish-locate-salary-key-lteq.
            EXIT.

          225240-start-menu-mode-finish-position.
            SET sw-op-class-STARTLST   TO TRUE

            START IdxFile LAST
                  INVALID KEY
                          DISPLAY asterisk asterisk
                                  "Error positioning at end!"
                                  asterisk asterisk

              NOT INVALID KEY
                          ADD  cte-01      TO ws-repositioning-records

                          DISPLAY asterisk
                                  "Positioning at the end."
                                  asterisk

            END-START

            DISPLAY "Starting. Status Code: [" fs-IdxFile "]."
            PERFORM 000600-press-enter-key-to-continue.
          225240-finish-menu-mode-finish-position.
            EXIT.

          225250-start-menu-mode-read-backwarding.
            SET sw-op-class-READPREV   TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents

            READ IdxFile PREVIOUS RECORD    INTO ws-f-IdxFile-rec
              AT END
                 SET sw-IdxFile-EOF-Y         TO TRUE
                 DISPLAY asterisk "Begin of file!" asterisk

                 PERFORM 000600-press-enter-key-to-continue
                 PERFORM 225240-start-menu-mode-finish-position
                    THRU 225240-finish-menu-mode-finish-position

             NOT AT END
                 ADD cte-01                   TO ws-reading-records
                 SET sw-IdxFile-EOF-N         TO TRUE

                 PERFORM 221210-start-show-file-info
                    THRU 221210-finish-show-file-info
                 PERFORM 221220-start-write-report-outp-record
                    THRU 221220-finish-write-report-outp-record

            END-READ

            DISPLAY "Reading. Status Code: [" fs-IdxFile "].".
          225250-finish-menu-mode-read-backwarding.
            EXIT.

          225260-start-menu-mode-read-forwarding.
            SET sw-op-class-READNEXT   TO TRUE

            PERFORM 000400-preliminary-review-employee-code-contents

            READ IdxFile NEXT RECORD        INTO ws-f-IdxFile-rec
              AT END
                 SET sw-IdxFile-EOF-Y         TO TRUE
                 DISPLAY asterisk "End of file!" asterisk

                 PERFORM 000600-press-enter-key-to-continue
                 PERFORM 225210-start-menu-mode-start-position
                    THRU 225210-finish-menu-mode-start-position

             NOT AT END
                 ADD cte-01                   TO ws-reading-records
                 SET sw-IdxFile-EOF-N         TO TRUE

                 PERFORM 221210-start-show-file-info
                    THRU 221210-finish-show-file-info
                 PERFORM 221220-start-write-report-outp-record
                    THRU 221220-finish-write-report-outp-record

            END-READ

            DISPLAY "Reading. Status Code: [" fs-IdxFile "].".
          225260-finish-menu-mode-read-forwarding.
            EXIT.

          225270-start-menu-mode-trace-backwarding.
            PERFORM 225250-start-menu-mode-read-backwarding
               THRU 225250-finish-menu-mode-read-backwarding

            PERFORM 221300-start-continue-carry-out-oper
               THRU 221300-finish-continue-carry-out-oper
               WITH TEST AFTER
              UNTIL sw-carry-out-sure-Y OR sw-carry-out-sure-N.
          225270-finish-menu-mode-trace-backwarding.
            EXIT.

          225280-start-menu-mode-trace-forwarding.
            PERFORM 225260-start-menu-mode-read-forwarding
               THRU 225260-finish-menu-mode-read-forwarding

            PERFORM 221300-start-continue-carry-out-oper
               THRU 221300-finish-continue-carry-out-oper
               WITH TEST AFTER
              UNTIL sw-carry-out-sure-Y OR sw-carry-out-sure-N.
          225280-finish-menu-mode-trace-forwarding.

       300000-start-end-program.
           PERFORM 310000-start-close-IdxFile
              THRU 310000-finish-close-IdxFile

           PERFORM 320000-start-close-OutFile
              THRU 320000-finish-close-OutFile

           PERFORM 330000-start-view-statistics
              THRU 330000-finish-view-statistics.
       300000-finish-end-program.
           EXIT.

        310000-start-close-IdxFile.
           INITIALIZE f-IdxFile-rec
                      ws-f-IdxFile-rec

           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+---+"
           DISPLAY "|   Indexed Sequential File.   |"
           DISPLAY "+---+----+---+----+---+----+---+"

           SET sw-op-class-CLOSE       TO TRUE
           CLOSE IdxFile

           MOVE fs-IdxFile                    TO RETURN-CODE

           DISPLAY "Closing. Status Code: ["  fs-IdxFile "].".
        310000-finish-close-IdxFile.
           EXIT.

        320000-start-close-OutFile.
           INITIALIZE f-OutFile-rec
                      ws-f-OutFile-rec

           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+---+"
           DISPLAY "|      Output Report File.     |"
           DISPLAY "+---+----+---+----+---+----+---+"

           IF fs-OutFile IS EQUAL TO ZEROES
              PERFORM 321000-start-print-OutFile-Report-Footing
                 THRU 321000-finish-print-OutFile-Report-Footing
           END-IF

           SET sw-op-class-CLOSE       TO TRUE
           CLOSE OutFile

           MOVE fs-OutFile                    TO RETURN-CODE

           DISPLAY "Closing. Status Code: ["  fs-OutFile "].".
        320000-finish-close-OutFile.
           EXIT.

         321000-start-print-OutFile-Report-Footing.
           MOVE SPACES                        TO f-OutFile-rec
                                                 ws-f-OutFile-rec
           PERFORM 121100-start-write-output-report-record
              THRU 121100-finish-write-output-report-record

           MOVE ws-reporting-read-records-page
             TO ws-rep-page-foot-recs-rep
           MOVE ws-rep-page-footing     TO f-OutFile-rec
                                                 ws-f-OutFile-rec
           PERFORM 121100-start-write-output-report-record
              THRU 121100-finish-write-output-report-record

           MOVE ws-reporting-read-records-sum
             TO ws-rep-page-foot-recs-rep
           MOVE ws-rep-page-footing     TO f-OutFile-rec
                                                 ws-f-OutFile-rec
           PERFORM 121100-start-write-output-report-record
              THRU 121100-finish-write-output-report-record

           MOVE SPACES                        TO f-OutFile-rec
                                                 ws-f-OutFile-rec
           PERFORM 121100-start-write-output-report-record
              THRU 121100-finish-write-output-report-record.
         321000-finish-print-OutFile-Report-Footing.
           EXIT.

        330000-start-view-statistics.
           DISPLAY SPACE
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "|    Processed records.    |"
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY "| Eliminated  : [" ws-eliminated-records "]."
           DISPLAY "| Last line   : [" ws-last-printed-report-line "]."
           DISPLAY "| Pages       : [" ws-printed-pages "]."
           DISPLAY "| Read        : [" ws-reading-records "]."
           DISPLAY "| Records log : [" ws-reporting-read-records-sum
                   "]."
           DISPLAY "| Report rows : [" ws-reporting-written-records-sum
                   "]."
           DISPLAY "| Repositions : [" ws-repositioning-records "]."
           DISPLAY "| Rewritten   : [" ws-rewritten-records "]."
           DISPLAY "| Writings    : [" ws-written-records "]."
           DISPLAY "+---+----+---+----+---+----+"
           DISPLAY SPACE.
        330000-finish-view-statistics.
           EXIT.

       END PROGRAM IdxDyn.
