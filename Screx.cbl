       IDENTIFICATION DIVISION.
       PROGRAM-ID. Screx.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ws-key-pause                     PIC X(01)  VALUE SPACE.

       01  ws-constants-symbolics.
           03  ws-cte-01                    PIC 9(01)  VALUE 01.
           03  ws-cte-25                    PIC 9(02)  VALUE 25.
           03  ws-cte-35                    PIC 9(02)  VALUE 35.

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
                   07  ws-displacement-col-row.
                       09  ws-col           PIC 9(02)  VALUE ZEROES.
                       09  ws-row           PIC 9(02)  VALUE ZEROES.
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
           DISPLAY SPACE
                AT LINE ws-cte-01 COLUMN ws-cte-01
              WITH BLANK SCREEN

           SET sw-bottom-row-01             TO TRUE
           SET sw-top-row-24                TO TRUE
           SET sw-left-col-01               TO TRUE
           SET sw-right-col-80              TO TRUE

           PERFORM 100000-start-construct-text-window
              THRU 100000-finish-construct-text-window

           DISPLAY "Press the ENTER key to continue..."
                AT LINE ws-cte-25 COLUMN ws-cte-01
           ACCEPT ws-key-pause
               AT LINE ws-cte-25 COLUMN ws-cte-35 WITH PROMPT

           STOP RUN.

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
       100000-finish-construct-text-window.
           EXIT.

        110000-start-cleaning-window-frame-area.
           SET  sw-char-normal-space        TO TRUE

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

        120000-start-build-vertical-edges-frame.
           SET  sw-switch-row-column-row    TO TRUE
           SET  sw-char-closing-excl-mark   TO TRUE
           MOVE ws-left-col                 TO ws-fixed-col
           MOVE ws-bottom-row               TO ws-start-pos
           MOVE ws-top-row                  TO ws-finish-pos
           MOVE ws-cte-01                   TO ws-interval-pos

           PERFORM 121000-start-build-text-window-bricks
              THRU 121000-finish-build-text-window-bricks
           VARYING ws-pos FROM ws-start-pos BY ws-interval-pos
             UNTIL ws-pos IS GREATER THAN ws-finish-pos

           MOVE ws-right-col                TO ws-fixed-col

           PERFORM 121000-start-build-text-window-bricks
              THRU 121000-finish-build-text-window-bricks
           VARYING ws-pos FROM ws-start-pos BY ws-interval-pos
             UNTIL ws-pos IS GREATER THAN ws-finish-pos.
        120000-finish-build-vertical-edges-frame.
           EXIT.

         121000-start-build-text-window-bricks.
            IF (sw-switch-row-column-row)
                DISPLAY ws-char AT LINE ws-pos COLUMN ws-fixed-col
            ELSE
                IF (sw-switch-row-column-column)
                    DISPLAY ws-char AT LINE ws-fixed-row COLUMN ws-pos
                END-IF
            END-IF.
         121000-finish-build-text-window-bricks.
            EXIT.

        130000-start-build-horizontal-edges-frame.
           SET  sw-switch-row-column-column TO TRUE
           SET  sw-char-dash-hyphen         TO TRUE
           MOVE ws-top-row                  TO ws-fixed-row
           MOVE ws-left-col                 TO ws-start-pos
           MOVE ws-right-col                TO ws-finish-pos
           MOVE ws-cte-01                   TO ws-interval-pos

           PERFORM 121000-start-build-text-window-bricks
              THRU 121000-finish-build-text-window-bricks
           VARYING ws-pos FROM ws-start-pos BY ws-interval-pos
             UNTIL ws-pos IS GREATER THAN ws-finish-pos

           MOVE ws-bottom-row               TO ws-fixed-row

           PERFORM 121000-start-build-text-window-bricks
              THRU 121000-finish-build-text-window-bricks
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

       END PROGRAM Screx.
