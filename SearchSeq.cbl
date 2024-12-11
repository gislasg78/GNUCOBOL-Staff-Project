       IDENTIFICATION DIVISION.
       PROGRAM-ID. SearchSeq.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  ws-begin-pos                PIC 99 VALUE ZEROES.
       77  ws-value                    PIC 99 VALUE ZEROES.

       01  ws-my-row
           VALUE '02030507111317192329313741434753596167717379838997'.
           03  ws-my-line      OCCURS  25 TIMES
                            ASCENDING KEY ws-my-line-data
                              INDEXED  BY idx-my-line.
               05  ws-my-line-data     PIC 99.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Search for 100 first prime numbers."
           DISPLAY "Enter the start position (01 - 25) : "
              WITH NO ADVANCING
           ACCEPT ws-begin-pos

           DISPLAY "Enter an integer value   (00 - 99) : "
              WITH NO ADVANCING
           ACCEPT ws-value

           PERFORM FindGivenValue

           STOP "End of the program..."
           STOP RUN.

       FindGivenValue.
           DISPLAY SPACE
           DISPLAY "The value to search for is: [" ws-value "]."
           DISPLAY SPACE

           SET idx-my-line             TO ws-begin-pos
           SEARCH ws-my-line
               AT END
                  DISPLAY "Record Not Found!"

             WHEN ws-my-line-data(idx-my-line) = ws-value
                  DISPLAY "Exact localized data."
                  PERFORM DisplayDataFound

             WHEN ws-my-line-data(idx-my-line) > ws-value
                  DISPLAY "Data closest to the one sought."
                  PERFORM DisplayDataFound

           END-SEARCH.

       DisplayDataFound.
           DISPLAY "Index: {" idx-my-line "}."
           DISPLAY "Value: [" ws-my-line-data(idx-my-line) "].".

       END PROGRAM SearchSeq.
