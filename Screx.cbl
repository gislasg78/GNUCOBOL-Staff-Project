       IDENTIFICATION DIVISION.
       PROGRAM-ID. Screx.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-cte-01                        PIC 9(01)  VALUE 01.

       01  ws-screen-coords.
           03  ws-screen-bounds.
               05  ws-horizontal-bounds.
                   07  ws-bottom-row        PIC 9(02)  VALUE ZEROES.
                       88  sw-bottom-row-01            VALUE 01.
                   07  ws-top-row           PIC 9(02)  VALUE ZEROES.
                       88  sw-top-row-24               VALUE 24.
               05  ws-vertical-bounds.
                   07  ws-left-col          PIC 9(02)  VALUE ZEROES.
                       88  sw-left-col-01              VALUE 01.
                   07  ws-right-col         PIC 9(02)  VALUE ZEROES.
                       88  sw-right-col-80             VALUE 80.

           03  ws-screen-initiators.
               05  ws-screen-displacement-vars.
                   07  ws-char              PIC X(01)  VALUE SPACE.
                       88  sw-char-normal-space        VALUE X'20'.
                       88  sw-char-closing-excl-mark   VALUE X'21'.
                       88  sw-char-asterisk            VALUE X'2A'.
                       88  sw-char-plus-sign           VALUE X'2B'.
                       88  sw-char-dash-hyphen         VALUE X'2D'.
                       88  sw-char-dot-point           VALUE X'2E'.
                       88  sw-char-equal-sign          VALUE X'3D'.
                       88  sw-char-underscore          VALUE X'5F'.
                       88  sw-char-pipe                VALUE X'7C'. 
                   07  ws-col               PIC 9(02)  VALUE ZEROES.
                   07  ws-row               PIC 9(02)  VALUE ZEROES.
               05  ws-position-initiatiors.
                   07  ws-finish-pos        PIC 9(02)  VALUE ZEROES.
                   07  ws-pos               PIC 9(02)  VALUE ZEROES.
                   07  ws-interval-pos      PIC 9(01)  VALUE ZERO.
                   07  ws-start-pos         PIC 9(02)  VALUE ZEROES.
               05  ws-screen-fixed-initiators.
                   07  ws-fixed-col         PIC 9(02)  VALUE ZEROES.
                   07  ws-fixed-row         PIC 9(02)  VALUE ZEROES.
               05  ws-switch-row-column     PIC 9(01)  VALUE ZERO.
                   88  sw-switch-row-column-row        VALUE 1.
                   88  sw-switch-row-column-column     VALUE 2.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY SPACE WITH BLANK SCREEN

           PERFORM 000100-start-build-main-screen
              THRU 000100-finish-build-main-screen

           PERFORM 000200-start-put-field-labels
              THRU 000200-finish-put-field-labels

           STOP RUN.

       000100-start-build-main-screen.
           SET sw-bottom-row-01             TO TRUE
           SET sw-top-row-24                TO TRUE
           SET sw-left-col-01               TO TRUE
           SET sw-right-col-80              TO TRUE
           PERFORM 100000-start-construct-text-window
              THRU 100000-finish-construct-text-windows

           MOVE 01                          TO ws-bottom-row
           MOVE 01                          TO ws-left-col
           MOVE 03                          TO ws-top-row
           MOVE 80                          TO ws-right-col
           PERFORM 100000-start-construct-text-window
              THRU 100000-finish-construct-text-windows

           MOVE 05                          TO ws-bottom-row
           MOVE 04                          TO ws-left-col
           MOVE 08                          TO ws-top-row
           MOVE 41                          TO ws-right-col
           PERFORM 100000-start-construct-text-window
              THRU 100000-finish-construct-text-windows

           MOVE 10                          TO ws-bottom-row
           MOVE 04                          TO ws-left-col
           MOVE 15                          TO ws-top-row
           MOVE 41                          TO ws-right-col
           PERFORM 100000-start-construct-text-window
              THRU 100000-finish-construct-text-windows

           MOVE 15                          TO ws-bottom-row
           MOVE 04                          TO ws-left-col
           MOVE 20                          TO ws-top-row
           MOVE 41                          TO ws-right-col
           PERFORM 100000-start-construct-text-window
              THRU 100000-finish-construct-text-windows

           MOVE 05                          TO ws-bottom-row
           MOVE 44                          TO ws-left-col
           MOVE 08                          TO ws-top-row
           MOVE 76                          TO ws-right-col
           PERFORM 100000-start-construct-text-window
              THRU 100000-finish-construct-text-windows

           MOVE 10                          TO ws-bottom-row
           MOVE 44                          TO ws-left-col
           MOVE 15                          TO ws-top-row
           MOVE 76                          TO ws-right-col
           PERFORM 100000-start-construct-text-window
              THRU 100000-finish-construct-text-windows

           MOVE 15                          TO ws-bottom-row
           MOVE 44                          TO ws-left-col
           MOVE 20                          TO ws-top-row
           MOVE 76                          TO ws-right-col
           PERFORM 100000-start-construct-text-window
              THRU 100000-finish-construct-text-windows

           MOVE 22                          TO ws-bottom-row
           MOVE 01                          TO ws-left-col
           MOVE 24                          TO ws-top-row
           MOVE 80                          TO ws-right-col
           PERFORM 100000-start-construct-text-window
              THRU 100000-finish-construct-text-windows.
       000100-finish-build-main-screen.
           EXIT.

       100000-start-construct-text-window.
           INITIALIZE ws-screen-initiators

           PERFORM 110000-start-cleaning-window-frame-area
              THRU 110000-finish-cleaning-window-frame-area

           PERFORM 120000-start-build-vertical-edges-frame
              THRU 120000-finish-build-vertical-edges-frame

           PERFORM 130000-start-build-horizontal-edges-frame
              THRU 130000-finish-build-horizontal-edges-frame

           PERFORM 140000-start-set-window-frame-corners
              THRU 140000-finish-set-window-frame-corners.           
       100000-finish-construct-text-windows.
           EXIT.

        110000-start-cleaning-window-frame-area.
           SET  sw-char-normal-space        TO TRUE

           PERFORM VARYING ws-row
              FROM ws-bottom-row            BY ws-cte-01
             UNTIL ws-row           IS GREATER THAN ws-top-row
                   PERFORM VARYING ws-col
                      FROM ws-left-col      BY ws-cte-01
                     UNTIL ws-col   IS GREATER THAN ws-right-col
                           DISPLAY ws-char
                                AT LINE ws-row COLUMN ws-col
                           END-DISPLAY
                   END-PERFORM
           END-PERFORM.
        110000-finish-cleaning-window-frame-area.
           EXIT.

        120000-start-build-vertical-edges-frame.
           SET  sw-switch-row-column-row    TO TRUE
           SET  sw-char-closing-excl-mark   TO TRUE
           MOVE ws-left-col                 TO ws-fixed-col
           MOVE ws-bottom-row               TO ws-start-pos
           MOVE ws-top-row                  TO ws-finish-pos
           MOVE ws-cte-01                   TO ws-interval-pos

           PERFORM 121000-start-build-text-window-bricks
              THRU 121000-finish-build-text-windows-bricks
           VARYING ws-pos FROM ws-start-pos BY ws-interval-pos
             UNTIL ws-pos IS GREATER THAN ws-finish-pos

           MOVE ws-right-col                TO ws-fixed-col

           PERFORM 121000-start-build-text-window-bricks
              THRU 121000-finish-build-text-windows-bricks
           VARYING ws-pos FROM ws-start-pos BY ws-interval-pos
             UNTIL ws-pos IS GREATER THAN ws-finish-pos.
        120000-finish-build-vertical-edges-frame.
           EXIT.

         121000-start-build-text-window-bricks.
            IF (sw-switch-row-column-row)
                DISPLAY ws-char AT LINE ws-pos COLUMN ws-fixed-col
            ELSE
                IF (sw-switch-row-column-column)
                    DISPLAY ws-char AT LINE ws-fixed-row COLUMN ws-pos.
         121000-finish-build-text-windows-bricks.
            EXIT.

        130000-start-build-horizontal-edges-frame.
           SET  sw-switch-row-column-column TO TRUE
           SET  sw-char-dash-hyphen         TO TRUE
           MOVE ws-top-row                  TO ws-fixed-row
           MOVE ws-left-col                 TO ws-start-pos
           MOVE ws-right-col                TO ws-finish-pos
           MOVE ws-cte-01                   TO ws-interval-pos

           PERFORM 121000-start-build-text-window-bricks
              THRU 121000-finish-build-text-windows-bricks
           VARYING ws-pos FROM ws-start-pos BY ws-interval-pos
             UNTIL ws-pos IS GREATER THAN ws-finish-pos

           MOVE ws-bottom-row               TO ws-fixed-row

           PERFORM 121000-start-build-text-window-bricks
              THRU 121000-finish-build-text-windows-bricks
           VARYING ws-pos FROM ws-start-pos BY ws-interval-pos
             UNTIL ws-pos IS GREATER THAN ws-finish-pos.
        130000-finish-build-horizontal-edges-frame.           
           EXIT.

        140000-start-set-window-frame-corners.
           SET  sw-char-plus-sign           TO TRUE

           DISPLAY ws-char AT LINE ws-bottom-row COLUMN ws-left-col
           DISPLAY ws-char AT LINE ws-bottom-row COLUMN ws-right-col
 
           DISPLAY ws-char AT LINE ws-top-row    COLUMN ws-left-col
           DISPLAY ws-char AT LINE ws-top-row    COLUMN ws-right-col.
        140000-finish-set-window-frame-corners.           
           EXIT.

       000200-start-put-field-labels.
           DISPLAY "Product registrations."
                AT LINE 02 COLUMN 30

           DISPLAY "Code Key:" AT LINE 06 COLUMN 06
           DISPLAY "Status:"   AT LINE 06 COLUMN 25
           DISPLAY "Store:"    AT LINE 07 COLUMN 09
           DISPLAY "Sequence:" AT LINE 07 COLUMN 23

           DISPLAY "Manufacturing information."
                AT LINE 09 COLUMN 10

           DISPLAY "Class:"    AT LINE 11 COLUMN 09
           DISPLAY "Name:"     AT LINE 12 COLUMN 10
           DISPLAY "Type:"     AT LINE 13 COLUMN 10
           DISPLAY "Unit:"     AT LINE 14 COLUMN 10

           DISPLAY "Changed:"  AT LINE 16 COLUMN 07
           DISPLAY "Creation:" AT LINE 17 COLUMN 06
           DISPLAY "Cost:"     AT LINE 18 COLUMN 10
           DISPLAY "Stock:"    AT LINE 19 COLUMN 09

           DISPLAY "Inventory Cost:"
                AT LINE 06 COLUMN 46
           DISPLAY "Total:"    AT LINE 07 COLUMN 53

           DISPLAY "Latest Outcomes."
                AT LINE 09 COLUMN 53

           DISPLAY "Posts:"    AT LINE 11 COLUMN 46
           DISPLAY "Amount:"   AT LINE 12 COLUMN 52
           DISPLAY "Cost:"     AT LINE 13 COLUMN 54
           DISPLAY "Date:"     AT LINE 14 COLUMN 54

           DISPLAY "Departures:"
                AT LINE 16 COLUMN 46
           DISPLAY "Amount:"   AT LINE 17 COLUMN 52
           DISPLAY "Cost:"     AT LINE 18 COLUMN 54
           DISPLAY "Date:"     AT LINE 19 COLUMN 54

           DISPLAY SPACE       AT LINE 25 COLUMN 01.
       000200-finish-put-field-labels.
           EXIT.

       END PROGRAM Screx.
