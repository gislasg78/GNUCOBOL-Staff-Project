        IDENTIFICATION DIVISION.
        PROGRAM-ID. CntUpLow.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
        78  cte-80                                    VALUE 80.
        78  cte-95                                    VALUE 95.

        01  ws-environmental-variables.
            03  ws-character-set-group.
                05  ws-character-set.
                    07  ws-character-set-printables-X20-X2F.
                        09  FILLER         PIC X(01)  VALUE X'20'.
                        09  FILLER         PIC X(01)  VALUE X'21'.
                        09  FILLER         PIC X(01)  VALUE X'22'.
                        09  FILLER         PIC X(01)  VALUE X'23'.
                        09  FILLER         PIC X(01)  VALUE X'24'.
                        09  FILLER         PIC X(01)  VALUE X'25'.
                        09  FILLER         PIC X(01)  VALUE X'26'.
                        09  FILLER         PIC X(01)  VALUE X'27'.
                        09  FILLER         PIC X(01)  VALUE X'28'.
                        09  FILLER         PIC X(01)  VALUE X'29'.
                        09  FILLER         PIC X(01)  VALUE X'2A'.
                        09  FILLER         PIC X(01)  VALUE X'2B'.
                        09  FILLER         PIC X(01)  VALUE X'2C'.
                        09  FILLER         PIC X(01)  VALUE X'2D'.
                        09  FILLER         PIC X(01)  VALUE X'2E'.
                        09  FILLER         PIC X(01)  VALUE X'2F'.
                    07  ws-character-set-printables-digits.
                        09  FILLER         PIC X(01)  VALUE X'30'.
                        09  FILLER         PIC X(01)  VALUE X'31'.
                        09  FILLER         PIC X(01)  VALUE X'32'.
                        09  FILLER         PIC X(01)  VALUE X'33'.
                        09  FILLER         PIC X(01)  VALUE X'34'.
                        09  FILLER         PIC X(01)  VALUE X'35'.
                        09  FILLER         PIC X(01)  VALUE X'36'.
                        09  FILLER         PIC X(01)  VALUE X'37'.
                        09  FILLER         PIC X(01)  VALUE X'38'.
                        09  FILLER         PIC X(01)  VALUE X'39'.
                    07  ws-character-set-printables-X3A-X40.
                        09  FILLER         PIC X(01)  VALUE X'3A'.
                        09  FILLER         PIC X(01)  VALUE X'3B'.
                        09  FILLER         PIC X(01)  VALUE X'3C'.
                        09  FILLER         PIC X(01)  VALUE X'3D'.
                        09  FILLER         PIC X(01)  VALUE X'3E'.
                        09  FILLER         PIC X(01)  VALUE X'3F'.
                        09  FILLER         PIC X(01)  VALUE X'40'.
                    07  ws-character-set-printables-uppercases.
                        09  FILLER         PIC X(01)  VALUE X'41'.
                        09  FILLER         PIC X(01)  VALUE X'42'.
                        09  FILLER         PIC X(01)  VALUE X'43'.
                        09  FILLER         PIC X(01)  VALUE X'44'.
                        09  FILLER         PIC X(01)  VALUE X'45'.
                        09  FILLER         PIC X(01)  VALUE X'46'.
                        09  FILLER         PIC X(01)  VALUE X'47'.
                        09  FILLER         PIC X(01)  VALUE X'48'.
                        09  FILLER         PIC X(01)  VALUE X'49'.
                        09  FILLER         PIC X(01)  VALUE X'4A'.
                        09  FILLER         PIC X(01)  VALUE X'4B'.
                        09  FILLER         PIC X(01)  VALUE X'4C'.
                        09  FILLER         PIC X(01)  VALUE X'4D'.
                        09  FILLER         PIC X(01)  VALUE X'4E'.
                        09  FILLER         PIC X(01)  VALUE X'4F'.
                        09  FILLER         PIC X(01)  VALUE X'50'.
                        09  FILLER         PIC X(01)  VALUE X'51'.
                        09  FILLER         PIC X(01)  VALUE X'52'.
                        09  FILLER         PIC X(01)  VALUE X'53'.
                        09  FILLER         PIC X(01)  VALUE X'54'.
                        09  FILLER         PIC X(01)  VALUE X'55'.
                        09  FILLER         PIC X(01)  VALUE X'56'.
                        09  FILLER         PIC X(01)  VALUE X'57'.
                        09  FILLER         PIC X(01)  VALUE X'58'.
                        09  FILLER         PIC X(01)  VALUE X'59'.
                        09  FILLER         PIC X(01)  VALUE X'5A'.
                    07  ws-character-set-printables-X5B-X60.
                        09  FILLER         PIC X(01)  VALUE X'5B'.
                        09  FILLER         PIC X(01)  VALUE X'5C'.
                        09  FILLER         PIC X(01)  VALUE X'5D'.
                        09  FILLER         PIC X(01)  VALUE X'5E'.
                        09  FILLER         PIC X(01)  VALUE X'5F'.
                        09  FILLER         PIC X(01)  VALUE X'60'.
                    07  ws-character-set-printables-lowercases.
                        09  FILLER         PIC X(01)  VALUE X'61'.
                        09  FILLER         PIC X(01)  VALUE X'62'.
                        09  FILLER         PIC X(01)  VALUE X'63'.
                        09  FILLER         PIC X(01)  VALUE X'64'.
                        09  FILLER         PIC X(01)  VALUE X'65'.
                        09  FILLER         PIC X(01)  VALUE X'66'.
                        09  FILLER         PIC X(01)  VALUE X'67'.
                        09  FILLER         PIC X(01)  VALUE X'68'.
                        09  FILLER         PIC X(01)  VALUE X'69'.
                        09  FILLER         PIC X(01)  VALUE X'6A'.
                        09  FILLER         PIC X(01)  VALUE X'6B'.
                        09  FILLER         PIC X(01)  VALUE X'6C'.
                        09  FILLER         PIC X(01)  VALUE X'6D'.
                        09  FILLER         PIC X(01)  VALUE X'6E'.
                        09  FILLER         PIC X(01)  VALUE X'6F'.
                        09  FILLER         PIC X(01)  VALUE X'70'.
                        09  FILLER         PIC X(01)  VALUE X'71'.
                        09  FILLER         PIC X(01)  VALUE X'72'.
                        09  FILLER         PIC X(01)  VALUE X'73'.
                        09  FILLER         PIC X(01)  VALUE X'74'.
                        09  FILLER         PIC X(01)  VALUE X'75'.
                        09  FILLER         PIC X(01)  VALUE X'76'.
                        09  FILLER         PIC X(01)  VALUE X'77'.
                        09  FILLER         PIC X(01)  VALUE X'78'.
                        09  FILLER         PIC X(01)  VALUE X'79'.
                        09  FILLER         PIC X(01)  VALUE X'7A'.
                    07  ws-character-set-printables-X7B-X7F.
                        09  FILLER         PIC X(01)  VALUE X'7B'.
                        09  FILLER         PIC X(01)  VALUE X'7C'.
                        09  FILLER         PIC X(01)  VALUE X'7E'.
                        09  FILLER         PIC X(01)  VALUE X'7F'.

                05  ws-character-set-red   REDEFINES  ws-character-set.
                    07  ws-charset-item    OCCURS  cte-95 TIMES
                                           INDEXED BY idx-charset-item
                                           PIC X(01).

                05  ws-character-set-counting.
                    07  ws-charset-count   OCCURS  cte-95 TIMES
                                           INDEXED BY idx-charset-count
                                           PIC 9(02)  VALUE ZEROES.

            03  ws-accounting-positions.
                05  ws-starting-position   PIC 9(02)  VALUE ZEROES.
                05  ws-finishing-position  PIC 9(02)  VALUE ZEROES.

            03  ws-constants-group.
                05  ws-ctes-pos-X20-X2F.
                    07  ws-cte-01          PIC 9(01)  VALUE 1.
                    07  ws-cte-16          PIC 9(02)  VALUE 16.
                05  ws-ctes-pos-digits.
                    07  ws-cte-17          PIC 9(02)  VALUE 17.
                    07  ws-cte-26          PIC 9(02)  VALUE 26.
                05  ws-ctes-pos-X3A-X40.
                    07  ws-cte-27          PIC 9(02)  VALUE 27.
                    07  ws-cte-33          PIC 9(02)  VALUE 33.
                05  ws-ctes-pos-uppercases.
                    07  ws-cte-34          PIC 9(02)  VALUE 34.
                    07  ws-cte-59          PIC 9(02)  VALUE 59.
                05  ws-ctes-pos-X5B-X60.
                    07  ws-cte-60          PIC 9(02)  VALUE 60.
                    07  ws-cte-65          PIC 9(02)  VALUE 65.
                05  ws-ctes-pos-lowercases.
                    07  ws-cte-66          PIC 9(02)  VALUE 66.
                    07  ws-cte-91          PIC 9(02)  VALUE 91.
                05  ws-ctes-pos-X7B-X7F.
                    07  ws-cte-92          PIC 9(02)  VALUE 92.
                    07  ws-cte-95          PIC 9(02)  VALUE 95.

            03  ws-options-menu-choice     PIC 9(01)  VALUE ZERO.
                88  sw-options-menu-choice-allvalids  VALUES 1 THRU 8.
                88  sw-options-menu-choice-uppercase  VALUE 1.
                88  sw-options-menu-choice-lowercase  VALUE 2.
                88  sw-options-menu-choice-bothcase   VALUE 3.
                88  sw-options-menu-choice-numdigits  VALUE 4.
                88  sw-options-menu-choice-letanddig  VALUE 5.
                88  sw-options-menu-choice-printsymb  VALUE 6.
                88  sw-options-menu-choice-numbsymb   VALUE 7.
                88  sw-options-menu-choice-allchars   VALUE 8.
                88  sw-options-menu-choice-exitprog   VALUE 9.

            03  ws-string.         
                05  ws-string-char         OCCURS cte-80 TIMES
                                           INDEXED BY idx-string-char
                                           PIC X(01)  VALUE SPACE.

        PROCEDURE DIVISION.
        MAIN-PARAGRAPH.
           DISPLAY "Program to count characters in strings."

           PERFORM 100000-start-options-menu
              THRU 100000-finish-options-menu
             UNTIL sw-options-menu-choice-exitprog

           STOP "This program has ended..."
           STOP RUN.

       100000-start-options-menu.
           INITIALIZE ws-accounting-positions
                      ws-character-set-counting
                      ws-options-menu-choice
                      ws-string

           PERFORM 110000-start-display-options-menu
              THRU 110000-finish-display-options-menu

           PERFORM 120000-start-validate-menu-option
              THRU 120000-finish-validate-menu-option.
       100000-finish-options-menu.
           EXIT.

        110000-start-display-options-menu.
           DISPLAY "+===+===+===+===+===+===+===+===+"
           DISPLAY "|    Counting Options Menu.     |"
           DISPLAY "+===+===+===+===+===+===+===+===+"
           DISPLAY "| [1]. Uppercase letters.       |"
           DISPLAY "| [2]. Lowercase letters.       |"
           DISPLAY "| [3]. Upper & lower cases.     |"
           DISPLAY "| [4]. Numeric Digits.          |"
           DISPLAY "| [5]. Upper & lower & digits.  |"
           DISPLAY "| [6]. Printable symbols.       |"
           DISPLAY "| [7]. Digits & symbols.        |"
           DISPLAY "| [8]. All printable chars.     |"
           DISPLAY "| [9]. Exit this program.       |"
           DISPLAY "+===+===+===+===+===+===+===+===+"
           DISPLAY "Enter your choice: " WITH NO ADVANCING
           ACCEPT ws-options-menu-choice

           DISPLAY "The chosen option was: " ws-options-menu-choice.
        110000-finish-display-options-menu.
           EXIT.

        120000-start-validate-menu-option.
           IF sw-options-menu-choice-allvalids THEN
              PERFORM 121000-start-get-string-chars
                 THRU 121000-finish-get-string-chars
           END-IF

           EVALUATE TRUE
               WHEN sw-options-menu-choice-uppercase
                    PERFORM 122000-start-tally-uppercase
                       THRU 122000-finish-tally-uppercase

               WHEN sw-options-menu-choice-lowercase
                    PERFORM 123000-start-tally-lowercase
                       THRU 123000-finish-tally-lowercase

               WHEN sw-options-menu-choice-bothcase
                    PERFORM 124000-start-tally-bothcase
                       THRU 124000-finish-tally-bothcase

               WHEN sw-options-menu-choice-numdigits
                    PERFORM 125000-start-tally-numdigits
                       THRU 125000-finish-tally-numdigits

               WHEN sw-options-menu-choice-letanddig
                    PERFORM 126000-start-tally-letanddig
                       THRU 126000-finish-tally-letanddig

               WHEN sw-options-menu-choice-printsymb
                    PERFORM 127000-start-tally-printsymb
                       THRU 127000-finish-tally-printsymb

               WHEN sw-options-menu-choice-numbsymb
                    PERFORM 128000-start-tally-numbsymb
                    PERFORM 128000-finish-tally-numbsymb

               WHEN sw-options-menu-choice-allchars
                    PERFORM 129000-start-tally-allchars
                       THRU 129000-finish-tally-allchars

               WHEN sw-options-menu-choice-exitprog
                    DISPLAY "Leaving this program..."

               WHEN OTHER
                    DISPLAY "Invalid option selected. "
                            "Please correct your capture."

           END-EVALUATE.
        120000-finish-validate-menu-option.
           EXIT.

         121000-start-get-string-chars.
           DISPLAY "Enter any character string: " WITH NO ADVANCING
           ACCEPT ws-string.
         121000-finish-get-string-chars.
           EXIT.

         122000-start-tally-uppercase.
           DISPLAY "Capitalization Count."

           MOVE ws-cte-34  TO ws-starting-position
           MOVE ws-cte-59  TO ws-finishing-position

           PERFORM 122100-start-counting-cases
              THRU 122100-finish-counting-cases.
         122000-finish-tally-uppercase.
           EXIT.

          122100-start-counting-cases.
            DISPLAY "Counting statistics."

            SET idx-charset-count        TO ws-starting-position
            PERFORM 122110-start-counting-appearances
               THRU 122110-finish-counting-appearances
            VARYING idx-string-char
               FROM ws-cte-01            BY ws-cte-01
              UNTIL idx-string-char      IS GREATER THAN cte-80
              AFTER idx-charset-item
               FROM ws-starting-position BY ws-cte-01
              UNTIL idx-charset-item
                 IS GREATER THAN ws-finishing-position

            SET idx-charset-item         TO ws-starting-position
            PERFORM 122120-start-view-appearance-counts
               THRU 122120-finish-view-appearance-counts
            VARYING idx-charset-count
               FROM ws-starting-position BY ws-cte-01
              UNTIL idx-charset-count
                 IS GREATER THAN ws-finishing-position.
          122100-finish-counting-cases.
            EXIT.

          122110-start-counting-appearances.
            IF ws-string-char  (idx-string-char) IS EQUAL TO 
               ws-charset-item (idx-charset-item)
                 SET idx-charset-count TO idx-charset-item

                 ADD ws-cte-01
                  TO ws-charset-count (idx-charset-count)
            END-IF.
          122110-finish-counting-appearances.
            EXIT.
 
          122120-start-view-appearance-counts.
            IF ws-charset-count (idx-charset-count) IS NOT EQUAL TO ZEROES
               DISPLAY "["       ws-charset-item   (idx-charset-item)
                       "] = {"   ws-charset-count  (idx-charset-count)
                       "}."
            END-IF
                            
            SET idx-charset-item UP BY ws-cte-01.
          122120-finish-view-appearance-counts.
            EXIT.

         123000-start-tally-lowercase.
           DISPLAY "Lowercase Counting."

           MOVE ws-cte-66  TO ws-starting-position
           MOVE ws-cte-91  TO ws-finishing-position

           PERFORM 122100-start-counting-cases
              THRU 122100-finish-counting-cases.
         123000-finish-tally-lowercase.
           EXIT.

         124000-start-tally-bothcase.
           DISPLAY "Uppercase and Lowercase Counting."
   
           PERFORM 122000-start-tally-uppercase
              THRU 122000-finish-tally-uppercase

           PERFORM 123000-start-tally-lowercase
              THRU 123000-finish-tally-lowercase.
         124000-finish-tally-bothcase.
            EXIT.

         125000-start-tally-numdigits.
           DISPLAY "Counting Numerical Digits."

           MOVE ws-cte-17  TO ws-starting-position
           MOVE ws-cte-26  TO ws-finishing-position

           PERFORM 122100-start-counting-cases
              THRU 122100-finish-counting-cases.
         125000-finish-tally-numdigits.
           EXIT.

         126000-start-tally-letanddig.
           DISPLAY "Counting Uppercase, Lowercase and Numeric Digits."

           PERFORM 124000-start-tally-bothcase
              THRU 124000-finish-tally-bothcase

           PERFORM 125000-start-tally-numdigits
              THRU 125000-finish-tally-numdigits.
         126000-finish-tally-letanddig.
           EXIT.

         127000-start-tally-printsymb.
           DISPLAY "Printable Special Symbol Counting."

           MOVE ws-cte-01  TO ws-starting-position
           MOVE ws-cte-16  TO ws-finishing-position
           PERFORM 122100-start-counting-cases
              THRU 122100-finish-counting-cases

           MOVE ws-cte-27  TO ws-starting-position
           MOVE ws-cte-33  TO ws-finishing-position
           PERFORM 122100-start-counting-cases
              THRU 122100-finish-counting-cases

           MOVE ws-cte-60  TO ws-starting-position
           MOVE ws-cte-65  TO ws-finishing-position
           PERFORM 122100-start-counting-cases
              THRU 122100-finish-counting-cases

           MOVE ws-cte-92  TO ws-starting-position
           MOVE ws-cte-95  TO ws-finishing-position
           PERFORM 122100-start-counting-cases
              THRU 122100-finish-counting-cases.
         127000-finish-tally-printsymb.
           EXIT.

         128000-start-tally-numbsymb.
           DISPLAY "Number Digit and Special Symbols Counting."

           PERFORM 125000-start-tally-numdigits
              THRU 125000-finish-tally-numdigits

           PERFORM 127000-start-tally-printsymb
              THRU 127000-finish-tally-printsymb.
         128000-finish-tally-numbsymb.
           EXIT.

         129000-start-tally-allchars.
           DISPLAY "Count of all printable characters."

           MOVE ws-cte-01  TO ws-starting-position
           MOVE ws-cte-95  TO ws-finishing-position
           PERFORM 122100-start-counting-cases
              THRU 122100-finish-counting-cases.
         129000-finish-tally-allchars.
           EXIT.

       END PROGRAM CntUpLow.
