       IDENTIFICATION DIVISION.
       PROGRAM-ID. Screx.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ws-key-pause                         PIC X(01)  VALUE SPACE.

       78  cte-13                                          VALUE 13.

       01  ws-constants-symbolics.
           03  ws-cte-01                        PIC 9(01)  VALUE 01.
           03  ws-cte-03                        PIC 9(01)  VALUE 03.
           03  ws-cte-14                        PIC 9(02)  VALUE 14.
           03  ws-cte-15                        PIC 9(02)  VALUE 15.
           03  ws-cte-16                        PIC 9(02)  VALUE 16.
           03  ws-cte-25                        PIC 9(02)  VALUE 25.
           03  ws-cte-35                        PIC 9(02)  VALUE 35.

       01  ws-screen-coords.
           03  ws-screen-bounds.
               05  ws-horizontal-bounds.
                   07  ws-bottom-row            PIC 9(02)  VALUE ZEROES.
                       88  sw-bottom-row-01                VALUE 01.
                   07  ws-top-row               PIC 9(02)  VALUE ZEROES.
                       88  sw-top-row-24                   VALUE 24.
               05  ws-vertical-bounds.
                   07  ws-left-col              PIC 9(02)  VALUE ZEROES.
                       88  sw-left-col-01                  VALUE 01.
                   07  ws-right-col             PIC 9(02)  VALUE ZEROES.
                       88  sw-right-col-80                 VALUE 80.
           03  ws-screen-initiators.
               05  ws-screen-displacement-vars.
                   07  ws-char-aux              PIC X(01)  VALUE SPACE.
                       88  sw-char-normal-space            VALUE X'20'.
                       88  sw-char-closing-excl-mark       VALUE X'21'.
                       88  sw-char-asterisk                VALUE X'2A'.
                       88  sw-char-plus-sign               VALUE X'2B'.
                       88  sw-char-dash-hyphen             VALUE X'2D'.
                       88  sw-char-dot-point               VALUE X'2E'.
                       88  sw-char-equal-sign              VALUE X'3D'.
                       88  sw-char-underscore              VALUE X'5F'.
                       88  sw-char-pipe                    VALUE X'7C'.
                   07  ws-screen-displacement-col-row.
                       09  ws-char              PIC X(01)  VALUE SPACE.
                       09  ws-col               PIC 9(02)  VALUE ZEROES.
                       09  ws-row               PIC 9(02)  VALUE ZEROES.
                   07  ws-screen-styles-edges.
                       09  ws-screen-styles-corners.
                           11  ws-char-left-upper-corner   PIC X(01)
                                                           VALUE SPACE.
                           11  ws-char-left-lower-corner   PIC X(01)
                                                           VALUE SPACE.
                           11  ws-char-right-upper-corner  PIC X(01)
                                                           VALUE SPACE.
                           11  ws-char-right-lower-corner  PIC X(01)
                                                           VALUE SPACE.
                       09  ws-screen-styles-horizontal-bounds.
                           11  ws-char-bottom-row          PIC X(01)
                                                           VALUE SPACE.
                           11  ws-char-top-row             PIC X(01)
                                                           VALUE SPACE.
                       09  ws-screen-styles-vertical-bounds.
                           11  ws-char-left-col            PIC X(01)
                                                           VALUE SPACE.
                           11  ws-char-right-col           PIC X(01)
                                                           VALUE SPACE.
               05  ws-position-initiatiors.
                   07  ws-finish-pos            PIC 9(02)  VALUE ZEROES.
                   07  ws-pos                   PIC 9(02)  VALUE ZEROES.
                   07  ws-interval-pos          PIC 9(01)  VALUE ZERO.
                   07  ws-start-pos             PIC 9(02)  VALUE ZEROES.
               05  ws-screen-fixed-initiators.
                   07  ws-fixed-col             PIC 9(02)  VALUE ZEROES.
                   07  ws-fixed-row             PIC 9(02)  VALUE ZEROES.
               05  ws-switch-row-column         PIC 9(01)  VALUE ZERO.
                   88  sw-switch-row-column-row            VALUE 1.
                   88  sw-switch-row-column-column         VALUE 2.

       01  ws-screen-contents-inside.
           03  ws-current-date-formats.
               05  ws-current-date.
                   07  ws-current-date-year     PIC 9(04)  VALUE ZEROES.
                   07  ws-current-date-month    PIC 9(02)  VALUE ZEROES.
                   07  ws-current-date-day      PIC 9(02)  VALUE ZEROES.
               05  ws-current-date-num
                   REDEFINES ws-current-date    PIC 9(08).
               05  ws-current-time.
                   07  ws-current-time-hour     PIC 9(02)  VALUE ZEROES.
                   07  ws-current-time-minute   PIC 9(02)  VALUE ZEROES.
                   07  ws-current-time-second   PIC 9(02)  VALUE ZEROES.
               05  ws-current-time-num
                   REDEFINES ws-current-time    PIC 9(06).
               05  ws-current-date-time-formats.
                   07  ws-current-date-fmt      PIC 9999/99/99
                                                VALUE ZEROES.
                   07  ws-current-time-fmt      PIC 99B99B99
                                                VALUE ZEROES.

           03  ws-employee-data.
               05  ws-address-employee          PIC X(30)  VALUE SPACES.
               05  ws-admission-date-employee   PIC 9(08)  VALUE ZEROES.
               05  ws-code-employee             PIC 9(06)  VALUE ZEROES.
               05  ws-full-name-employee        PIC A(20)  VALUE SPACES.
               05  ws-phone-employee            PIC 9(10)  VALUE ZEROES.
               05  ws-salary-employee           PIC S9(06)V9(02)
                                                VALUE ZEROES.
               05  ws-status-employee           PIC A(01)  VALUE SPACE.
               05  ws-type-of-employee          PIC 9(01)  VALUE ZERO.
           03  ws-final-questions.
               05  ws-line-employee-list        PIC 9(02)  VALUE ZEROES.
               05  ws-line-question             PIC 9(02)  VALUE 22.
               05  ws-question-response         PIC A(01)  VALUE SPACE.
                   88  sw-question-response-N   VALUES ARE 'N', 'n'.
                   88  sw-question-response-Y   VALUES ARE 'Y' 'y'.

       01  ws-names-table-example.
           03  FILLER                 PIC 9(02) VALUE 01.
           03  FILLER                 PIC A(15) VALUE "Andrea Skeldon".
           03  FILLER                 PIC 9(02) VALUE 02.
           03  FILLER                 PIC A(15) VALUE "Betty Lewis".
           03  FILLER                 PIC 9(02) VALUE 03.
           03  FILLER                 PIC A(15) VALUE "Cynthia Turner".
           03  FILLER                 PIC 9(02) VALUE 04.
           03  FILLER                 PIC A(15) VALUE "Debra Watson".
           03  FILLER                 PIC 9(02) VALUE 05.
           03  FILLER                 PIC A(15) VALUE "Dorothy Collins".
           03  FILLER                 PIC 9(02) VALUE 06.
           03  FILLER                 PIC A(15) VALUE "Heather Wood".
           03  FILLER                 PIC 9(02) VALUE 07.
           03  FILLER                 PIC A(15) VALUE "Julia Fletcher".
           03  FILLER                 PIC 9(02) VALUE 08.
           03  FILLER                 PIC A(15) VALUE "Kimberly James".
           03  FILLER                 PIC 9(02) VALUE 09.
           03  FILLER                 PIC A(15) VALUE "Lauren Marshall".
           03  FILLER                 PIC 9(02) VALUE 10.
           03  FILLER                 PIC A(15) VALUE "Patricia Davis".
           03  FILLER                 PIC 9(02) VALUE 11.
           03  FILLER                 PIC A(15) VALUE "Rachael Glass".
           03  FILLER                 PIC 9(02) VALUE 12.
           03  FILLER                 PIC A(15) VALUE "Sophia Bennett".
           03  FILLER                 PIC 9(02) VALUE 13.
           03  FILLER                 PIC A(15) VALUE "Victoria Parker".
       01  ws-names-table-example-occ REDEFINES ws-names-table-example
           OCCURS cte-13 TIMES ASCENDING KEY ws-names-table-example-code 
           INDEXED BY idx-names-table-example-occ.
           03  ws-names-table-example-code      PIC 9(02).
           03  ws-names-table-example-name      PIC A(15).

       SCREEN SECTION.
       01  scr-main-content-screen.
           03  LINE 02 COLUMN 28 VALUE "Capture of employee data.".
           03  LINE 03 COLUMN 28 
                       PIC X(25) VALUE "=========================".

           03  LINE 05.
               05  COLUMN 03     VALUE "Code:".
               05  COLUMN 21     PIC 9(06) TO ws-code-employee.
           03  LINE 06.
               05  COLUMN 03     VALUE "Admission date:".
               05  COLUMN 21     PIC 9(08)
                                 TO ws-admission-date-employee.
               05  COLUMN 35     PIC 9999/99/99
                                 FROM ws-admission-date-employee.
           03  LINE 07.
               05  COLUMN 03     VALUE "Full name:".
               05  COLUMN 21     PIC A(20) TO ws-full-name-employee.
           03  LINE 08.
               05  COLUMN 03     VALUE "Address:".
               05  COLUMN 21     PIC X(30) TO ws-address-employee.
           03  LINE 09.
               05  COLUMN 03     VALUE "Phone number:".
               05  COLUMN 21     PIC 9(10) TO ws-phone-employee.
               05  COLUMN 35     PIC 9(02)B9(02)B9(03)B9(03)
                                 FROM ws-phone-employee.
           03  LINE 10.
               05  COLUMN 03     VALUE "Type:".
               05  COLUMN 21     PIC 9(01) TO ws-type-of-employee.
           03  LINE 11.
               05  COLUMN 03     VALUE "State:".
               05  COLUMN 21     PIC A(01) TO ws-status-employee.
           03  LINE 12 COLUMN 03 VALUE "Salary received:".
           03  LINE 12 COLUMN 21 PIC S9(06)V9(02)
                                 TO ws-salary-employee.
           03  LINE 12 COLUMN 35 PIC +$(04),$(02)9.9(02)
                                 FROM ws-salary-employee.

       01  scr-display-employee-list.
           03  scr-display-employee-list-header.
               05  LINE 04 COLUMN 53  PIC A(22)
                                      VALUE "List of employee names".
               05  LINE 05 COLUMN 53  PIC X(22)
                                      VALUE "======================".
           03  scr-display-employee-list-detail.
               05  LINE 05.
                    07  LINE PLUS ws-line-employee-list.
                        09  COLUMN 53 PIC 9(02)
                            FROM ws-names-table-example-code
                                (idx-names-table-example-occ).
                        09  COLUMN 56 PIC A(15)
                            FROM ws-names-table-example-name
                                (idx-names-table-example-occ).

       01  scr-main-content-screen-question.
           03  LINE ws-line-question.
               05  COLUMN 02     PIC X(43)
                   VALUE "-------------------------------------------".
               05  COLUMN 45     PIC X(35)
                   VALUE "-----------------------------------".
               05  LINE PLUS 01.
                   05  COLUMN 18
                   VALUE "Do you want to correct this record? (y/n) :".
                   05  COLUMN 62     PIC X(01) TO ws-question-response.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY SPACE
                AT LINE ws-cte-01 COLUMN ws-cte-01
              WITH BLANK SCREEN

           ACCEPT ws-current-date FROM DATE YYYYMMDD
           ACCEPT ws-current-time FROM TIME

           PERFORM 100000-start-construct-main-text-window
              THRU 100000-finish-construct-main-text-window

           MOVE ws-current-date-num TO ws-admission-date-employee

           PERFORM UNTIL sw-question-response-N
                   DISPLAY scr-main-content-screen
                           scr-display-employee-list-header

                   PERFORM VARYING idx-names-table-example-occ
                              FROM ws-cte-01 BY ws-cte-01
                             UNTIL idx-names-table-example-occ
                                IS GREATER THAN cte-13

                           SET ws-line-employee-list
                            TO idx-names-table-example-occ
                           DISPLAY scr-display-employee-list-detail
                   END-PERFORM

                   ACCEPT scr-main-content-screen
                   DISPLAY scr-main-content-screen

                   DISPLAY scr-main-content-screen-question
                   ACCEPT scr-main-content-screen-question
           END-PERFORM


           DISPLAY "Record saved successfully!"
                AT LINE ws-cte-14 COLUMN ws-cte-03

           MOVE ws-current-date-num   TO ws-current-date-fmt
           DISPLAY ws-current-date-fmt
                AT LINE ws-cte-15 COLUMN ws-cte-03

           MOVE ws-current-time-num   TO ws-current-time-fmt
           DISPLAY ws-current-time-fmt
                AT LINE ws-cte-16 COLUMN ws-cte-03


           DISPLAY "Press the ENTER key to continue..."
                AT LINE ws-cte-25 COLUMN ws-cte-01
           ACCEPT ws-key-pause
               AT LINE ws-cte-25 COLUMN ws-cte-35 WITH PROMPT

           STOP RUN.

       100000-start-construct-main-text-window.
           INITIALIZE ws-screen-coords

           SET sw-bottom-row-01             TO TRUE
           SET sw-top-row-24                TO TRUE
           SET sw-left-col-01               TO TRUE
           SET sw-right-col-80              TO TRUE

           PERFORM 110000-start-cleaning-window-frame-area
              THRU 110000-finish-cleaning-window-frame-area

           PERFORM 120000-start-construct-vertical-edges-frame
              THRU 120000-finish-construct-vertical-edges-frame

           PERFORM 130000-start-construct-horizontal-edges-frame
              THRU 130000-finish-construct-horizontal-edges-frame

           PERFORM 140000-start-construct-window-frame-corners
              THRU 140000-finish-construct-window-frame-corners.
       100000-finish-construct-main-text-window.
           EXIT.

        110000-start-cleaning-window-frame-area.
           SET  sw-char-normal-space        TO TRUE
           MOVE ws-char-aux                 TO ws-char

           PERFORM VARYING ws-row FROM ws-bottom-row BY ws-cte-01
             UNTIL ws-row  IS GREATER THAN ws-top-row
             AFTER ws-col  FROM ws-left-col BY ws-cte-01
             UNTIL ws-col  IS GREATER THAN ws-right-col
                   DISPLAY ws-char
                        AT LINE ws-row COLUMN ws-col
                   END-DISPLAY
           END-PERFORM.
        110000-finish-cleaning-window-frame-area.
           EXIT.

        120000-start-construct-vertical-edges-frame.
           SET  sw-char-closing-excl-mark   TO TRUE
           MOVE ws-char-aux                 TO ws-char-left-col
           SET  sw-char-pipe                TO TRUE
           MOVE ws-char-aux                 TO ws-char-right-col

           PERFORM 121000-start-build-vertical-edges-frame
              THRU 121000-finish-build-vertical-edges-frame.
        120000-finish-construct-vertical-edges-frame.
           EXIT.

         121000-start-build-vertical-edges-frame.
           SET  sw-switch-row-column-row    TO TRUE
           MOVE ws-char-left-col            TO ws-char
           MOVE ws-left-col                 TO ws-fixed-col
           MOVE ws-bottom-row               TO ws-start-pos
           MOVE ws-top-row                  TO ws-finish-pos
           MOVE ws-cte-01                   TO ws-interval-pos

           PERFORM 121100-start-build-text-window-bricks
              THRU 121100-finish-build-text-window-bricks
           VARYING ws-pos FROM ws-start-pos BY ws-interval-pos
             UNTIL ws-pos IS GREATER THAN ws-finish-pos

           MOVE ws-char-right-col           TO ws-char
           MOVE ws-right-col                TO ws-fixed-col

           PERFORM 121100-start-build-text-window-bricks
              THRU 121100-finish-build-text-window-bricks
           VARYING ws-pos FROM ws-start-pos BY ws-interval-pos
             UNTIL ws-pos IS GREATER THAN ws-finish-pos.
         121000-finish-build-vertical-edges-frame.
           EXIT.

          121100-start-build-text-window-bricks.
            IF (sw-switch-row-column-row)
                DISPLAY ws-char AT LINE ws-pos COLUMN ws-fixed-col
            ELSE
                IF (sw-switch-row-column-column)
                    DISPLAY ws-char AT LINE ws-fixed-row COLUMN ws-pos
                END-IF
            END-IF.
          121100-finish-build-text-window-bricks.
            EXIT.

        130000-start-construct-horizontal-edges-frame.
           SET  sw-char-equal-sign          TO TRUE
           MOVE ws-char-aux                 TO ws-char-bottom-row
           SET  sw-char-dash-hyphen         TO TRUE
           MOVE ws-char-aux                 TO ws-char-top-row

           PERFORM 131000-start-build-horizontal-edges-frame
              THRU 131000-finish-build-horizontal-edges-frame.
        130000-finish-construct-horizontal-edges-frame.
            EXIT.

         131000-start-build-horizontal-edges-frame.
           SET  sw-switch-row-column-column TO TRUE
           MOVE ws-char-top-row             TO ws-char
           MOVE ws-top-row                  TO ws-fixed-row
           MOVE ws-left-col                 TO ws-start-pos
           MOVE ws-right-col                TO ws-finish-pos
           MOVE ws-cte-01                   TO ws-interval-pos

           PERFORM 121100-start-build-text-window-bricks
              THRU 121100-finish-build-text-window-bricks
           VARYING ws-pos FROM ws-start-pos BY ws-interval-pos
             UNTIL ws-pos IS GREATER THAN ws-finish-pos

           MOVE ws-char-bottom-row          TO ws-char
           MOVE ws-bottom-row               TO ws-fixed-row

           PERFORM 121100-start-build-text-window-bricks
              THRU 121100-finish-build-text-window-bricks
           VARYING ws-pos FROM ws-start-pos BY ws-interval-pos
             UNTIL ws-pos IS GREATER THAN ws-finish-pos.
         131000-finish-build-horizontal-edges-frame.           
           EXIT.

        140000-start-construct-window-frame-corners.
           SET  sw-char-plus-sign           TO TRUE
           MOVE ws-char-aux                 TO ws-char-left-lower-corner
                                               ws-char-left-upper-corner
           SET  sw-char-asterisk            TO TRUE
           MOVE ws-char-aux              TO ws-char-right-lower-corner
                                            ws-char-right-upper-corner

           PERFORM 141000-start-build-set-window-frame-corners
              THRU 141000-finish-build-set-window-frame-corners.
        140000-finish-construct-window-frame-corners.
           EXIT.

         141000-start-build-set-window-frame-corners.
           DISPLAY ws-char-left-lower-corner
                AT LINE ws-bottom-row COLUMN ws-left-col

           DISPLAY ws-char-right-lower-corner
                AT LINE ws-bottom-row COLUMN ws-right-col
 
           DISPLAY ws-char-left-upper-corner
                AT LINE ws-top-row    COLUMN ws-left-col

           DISPLAY ws-char-right-upper-corner
                AT LINE ws-top-row    COLUMN ws-right-col.
         141000-finish-build-set-window-frame-corners.
           EXIT.

       END PROGRAM Screx.
