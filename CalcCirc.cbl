       IDENTIFICATION DIVISION.
       PROGRAM-ID. CalcCirc.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01   ws-constants.
            03  ws-cte-two                PIC 9(01)        VALUE 2.
            03  ws-PI                     PIC 9(01)V9(11)
                                          VALUE 3.14159265359.
       01   ws-dates.
            03  ws-date-today             PIC 9(08)        VALUE ZEROES.
            03  ws-date-formatted         PIC 9999/99/99   VALUE ZEROES.

       01   ws-circle.
            03  ws-area                   PIC 9(07)V9(11)  VALUE ZEROES.
            03  ws-circumference          PIC 9(07)V9(11)  VALUE ZEROES.
            03  ws-radius                 PIC 9(07)V9(11)  VALUE ZEROES.

       01   ws-show-values.
            03  ws-show-area              PIC Z(09)9.9(11) VALUE ZEROES.
            03  ws-show-circumference     PIC Z(09)9.9(11) VALUE ZEROES.
            03  ws-show-PI                PIC *(09)9.9(11) VALUE ZEROES.
            03  ws-show-radius            PIC Z(09)9.9(11) VALUE ZEROES.			 

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Calculation of the area and perimeter of a circle."	
           DISPLAY "Enter radius: " WITH NO ADVANCING
            ACCEPT  ws-radius

           ACCEPT ws-date-today FROM DATE YYYYMMDD
             MOVE ws-date-today   TO ws-date-formatted

           DISPLAY SPACE
           DISPLAY "[" ws-date-formatted "]."
           DISPLAY "Circle information details."

              MOVE ws-PI          TO ws-show-PI
           DISPLAY "- PI        : [" ws-show-PI "]."

              MOVE ws-radius      TO ws-show-radius
           DISPLAY "- Radius    : [" ws-show-radius "]"

           DISPLAY SPACE
           COMPUTE ws-circumference, ws-show-circumference =
                   ws-cte-two * WS-PI * ws-radius
           DISPLAY "- Perimeter : [" ws-show-circumference "]."

           COMPUTE ws-area, ws-show-area =
                   ws-PI * (ws-radius ** ws-cte-two)
           DISPLAY "- Area      : [" ws-show-area "]."

           STOP RUN.
	        
       END PROGRAM CalcCirc.
