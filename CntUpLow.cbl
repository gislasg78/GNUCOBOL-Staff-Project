        IDENTIFICATION DIVISION.
        PROGRAM-ID. CntUpLow.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  ws-environmental-variables.
            03  ws-string-char         PIC A(260) VALUE SPACES.

            03  ws-constants-group.
                05  ws-cte-01          PIC 9(01)  VALUE 1.
                05  ws-cte-52          PIC 9(02)  VALUE 52.

            03  ws-abecedarian-group.
                05  ws-abecedarian.
                    07  FILLER         PIC A(26)  VALUE
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
                    07  FILLER         PIC A(26)  VALUE
                        "abcdefghijklmnopqrstuwvxyz".
                05  ws-abecedarian-red REDEFINES ws-abecedarian.
                    07  ws-abc-item    OCCURS  52 TIMES
                                       INDEXED BY idx-abc-item
                                       PIC A(01).

                05  ws-abecedarian-counting.
                    07  ws-abc-counter OCCURS  52 TIMES
                                       INDEXED BY idx-abc-counter
                                       PIC 9(02)  VALUE ZEROES.

        PROCEDURE DIVISION.
        MAIN-PARAGRAPH.
            DISPLAY "Program to count upper and lower case letters."
            DISPLAY "Enter a string of characters: " WITH NO ADVANCING
             ACCEPT ws-string-char

            INITIALIZE ws-abecedarian-counting

            SET idx-abc-counter TO ws-cte-01
            PERFORM VARYING idx-abc-item    FROM ws-cte-01 BY ws-cte-01
              UNTIL idx-abc-item IS GREATER THAN ws-cte-52

                    INSPECT ws-string-char
                            TALLYING ws-abc-counter  (idx-abc-counter)
                                 FOR ALL ws-abc-item (idx-abc-item)

                    SET idx-abc-counter UP BY ws-cte-01

            END-PERFORM
            
            DISPLAY SPACE
            DISPLAY "Accounting for upper and lower case letters."

            SET idx-abc-item    TO ws-cte-01
            PERFORM VARYING idx-abc-counter FROM ws-cte-01 BY ws-cte-01
              UNTIL idx-abc-counter IS GREATER THAN ws-cte-52

                    IF ws-abc-counter (idx-abc-counter) NOT EQUAL ZEROES               
                       DISPLAY "["    ws-abc-item    (idx-abc-item)
                               "] = " ws-abc-counter (idx-abc-counter)
                               "."
                    END-IF
                            
                    SET idx-abc-item UP BY ws-cte-01
                    
            END-PERFORM

            STOP RUN.

        END PROGRAM CntUpLow.
