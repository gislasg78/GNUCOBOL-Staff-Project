       IDENTIFICATION DIVISION.
       PROGRAM-ID. SameAs.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Main-Message-Text.
           03  Main-File-Name            PIC X(12) VALUE "IdxFile.txt".
           03  Main-Info-File.
               05  FILLER                PIC X(01) VALUE X'3A'.
               05  Main-Info-Detail-No   PIC 9999  VALUE 3060.

       01  Z-Message-Main-File-Name      SAME AS Main-File-Name.
       01  Z-Message-Main-Info-File      SAME AS Main-Info-File.
       01  Z-Message-Main-Info-Detail-No SAME AS Main-Info-Detail-No
                                         OF Z-Message-Main-Info-File.

       01  Z-Message-Main-Message-Text.
           03  Main-Message-Text-Copy    SAME AS Main-Message-Text.
           03  Main-Message-Text-Copy-REN 
                                    REDEFINES Main-Message-Text-Copy
                                         SAME AS Main-Message-Text.

       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "Program for transferring properties between "
                   "COBOL variables."

           DISPLAY SPACE
           DISPLAY "Source variables."
           DISPLAY "(" Main-Message-Text ")."
           DISPLAY "(" Main-File-Name        OF Main-Message-Text ")."
           DISPLAY "(" Main-Info-File        OF Main-Message-Text ")."
           DISPLAY "(" Main-Info-Detail-No   OF Main-Message-Text ")."

           DISPLAY SPACE
           DISPLAY "Inherited variables."
           DISPLAY "[" Z-Message-Main-File-Name "]."
           DISPLAY "[" Z-Message-Main-Info-File "]."
           DISPLAY "[" Main-Info-Detail-No   OF Z-Message-Main-Info-File
                   "]."
           DISPLAY "[" Z-Message-Main-Info-Detail-No "]."
           DISPLAY "[" Z-Message-Main-Message-Text "]."

           DISPLAY SPACE
           DISPLAY "{" Main-Message-Text-Copy "}."
           DISPLAY "{" Main-Message-Text-Copy-REN "}."
           DISPLAY "{" Main-File-Name        OF Main-Message-Text-Copy
                   "}."
           DISPLAY "{" Main-Info-File        OF Main-Message-Text-Copy
                   "}."
           DISPLAY "{" Main-Info-Detail-No   OF Main-Message-Text-Copy
                   "}."

           GOBACK.

       END PROGRAM SameAs.
