       IDENTIFICATION DIVISION.
       PROGRAM-ID. bmi.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ws-working-variables.
           03  ws-BMI                 USAGE FLOAT     VALUE ZERO.
           03  ws-height-group.
               05  ws-height          PIC 9(03)V9(04) VALUE ZEROES.
               05  ws-height-squared  USAGE FLOAT     VALUE ZERO.
               03  ws-weight          PIC 9(03)V9(04) VALUE ZEROES.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "(BMI) Body Mass Index."
           DISPLAY "Enter your height (m): "  WITH NO ADVANCING
            ACCEPT ws-height
           DISPLAY "Enter your weight (kg): " WITH NO ADVANCING
            ACCEPT ws-weight

          MULTIPLY ws-height         BY ws-height
            GIVING ws-height-squared
          END-MULTIPLY

          DIVIDE ws-height-squared INTO ws-weight
          GIVING ws-BMI
              ON SIZE ERROR
                 DISPLAY "Fatal error! Division by zero."
          END-DIVIDE

          DISPLAY SPACE
          DISPLAY "(BMI) BMI Information."
          DISPLAY X"2B" X'20' "BMI:"      X'09' X"09" X"7B" ws-BMI
                  X"7D" X"2E"
          DISPLAY X"2D" X'20' "Height:"   X'09' X"5B" ws-height
                  X"5D" X'20' "meters"    X"2E"
          DISPLAY X"2D" X'20' "Weight:"   X'09' X"5B" ws-weight
                  X"5D" X"20" "kilograms" X"2E"
          DISPLAY "Press the ENTER key to continue..."
             WITH NO ADVANCING
           ACCEPT OMITTED

          DISPLAY SPACE
          DISPLAY "Done!"
          DISPLAY "This program has ended."
          DISPLAY "Press the ENTER key to continue..."
             WITH NO ADVANCING
           ACCEPT OMITTED

          STOP RUN.

       END-PROGRAM. bmi.
