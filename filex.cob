       IDENTIFICATION DIVISION.
       PROGRAM-ID. screx.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-cte-01                    PIC 9(01)  VALUE 01.

       01  ws-screen-coords.
           03  ws-screen-bounds.
               05  ws-horizontal-bounds.
                   07  ws-bottom-row    PIC 9(02)  VALUE 01.
                   07  ws-top-row       PIC 9(02)  VALUE 24.
               05  ws-vertical-bounds.
                   07  ws-left-col      PIC 9(02)  VALUE 01.
                   07  ws-right-col     PIC 9(02)  VALUE 80.

           03  ws-screen-initiators.
               05  ws-horizontal-initiators.
                   07  ws-start-row     PIC 9(02)  VALUE ZEROES.
                   07  ws-finish-row    PIC 9(02)  VALUE ZEROES.
               05  ws-vertical-initiators.
                   07  ws-start-col     PIC 9(02)  VALUE ZEROES.
                   07  ws-finish-col    PIC 9(02)  VALUE ZEROES.

           03  ws-screen-printing-chars.
               05  ws-horizontal-chars.
                   07  ws-bottom-char   PIC X(01)  VALUE X'2D'.
                   07  ws-top-char      PIC X(01)  VALUE X'3D'.
               05  ws-vertical-chars.
                   07  ws-left-char     PIC X(01)  VALUE X'21'.
                   07  ws-right-char    PIC X(01)  VALUE X'7C'.
               05  ws-corner-char       PIC X(01)  VALUE X'2B'.

           03  ws-screen-displacement-vars.
               05  ws-col               PIC 9(02)  VALUE ZEROES.
               05  ws-row               PIC 9(02)  VALUE ZEROES.
               05  ws-char              PIC X(01)  VALUE SPACE.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY SPACE WITH BLANK SCREEN


           MOVE ws-left-char   TO ws-char
           MOVE ws-left-col    TO ws-start-col
           MOVE ws-bottom-row  TO ws-start-row
           MOVE ws-top-row     TO ws-finish-row

           PERFORM 000000-begin-build-vertical-lines
              THRU 000000-end-build-vertical-lines
           VARYING ws-row FROM ws-start-row BY ws-cte-01
             UNTIL ws-row IS GREATER THAN ws-finish-row

           MOVE ws-right-char  TO ws-char
           MOVE ws-right-col   TO ws-start-col
           MOVE ws-bottom-row  TO ws-start-row
           MOVE ws-top-row     TO ws-finish-row

           PERFORM 000000-begin-build-vertical-lines
              THRU 000000-end-build-vertical-lines
           VARYING ws-row FROM ws-start-row BY ws-cte-01
             UNTIL ws-row IS GREATER THAN ws-finish-row


           MOVE ws-top-char    TO ws-char
           MOVE ws-top-row     TO ws-start-row
           MOVE ws-left-col    TO ws-start-col
           MOVE ws-right-col   TO ws-finish-col

           PERFORM 000000-begin-build-horizontal-lines
              THRU 000000-end-build-horizontal-line
           VARYING ws-col FROM ws-start-col BY ws-cte-01
             UNTIL ws-col IS GREATER THAN ws-finish-col

           MOVE ws-bottom-char TO ws-char
           MOVE ws-bottom-row  TO ws-start-row
           MOVE ws-left-col    TO ws-start-col
           MOVE ws-right-col   TO ws-finish-col

           PERFORM 000000-begin-build-horizontal-lines
              THRU 000000-end-build-horizontal-line
           VARYING ws-col FROM ws-start-col BY ws-cte-01
             UNTIL ws-col IS GREATER THAN ws-finish-col


           MOVE ws-corner-char TO ws-char
           MOVE ws-top-row     TO ws-start-row
           MOVE ws-bottom-row  TO ws-finish-row
           MOVE ws-left-col    TO ws-start-col
           MOVE ws-right-col   TO ws-finish-col

           PERFORM 000000-begin-build-corners
              THRU 000000-end-build-corners

           STOP RUN.

       000000-begin-build-vertical-lines.
           DISPLAY ws-char AT LINE ws-row        COLUMN ws-start-col.
       000000-end-build-vertical-lines.
           EXIT.

       000000-begin-build-horizontal-lines.
           DISPLAY ws-char AT LINE ws-start-row  COLUMN ws-col.
       000000-end-build-horizontal-line.
           EXIT.

       000000-begin-build-corners.
           DISPLAY ws-char AT LINE ws-start-row  COLUMN ws-start-col
           DISPLAY ws-char AT LINE ws-start-row  COLUMN ws-finish-col

           DISPLAY ws-char AT LINE ws-finish-row COLUMN ws-start-col
           DISPLAY ws-char AT LINE ws-finish-row COLUMN ws-finish-col.
       000000-end-build-corners.
           EXIT.

       END PROGRAM screx.
