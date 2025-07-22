*>****************************************************************
*> Author:
*> Date:
*> Purpose:
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. YOUR-PROGRAM-NAME.
DATA DIVISION.
FILE SECTION.
WORKING-STORAGE SECTION.
       *> colorCodes.cpy - ANSI color definitions
       01  COLOR-CODES.
           05  ESC             PIC X    VALUE X'1B'. *> Escape character
           05  COLOR-RESET     PIC X(3) VALUE '[0m'.
           05  COLOR-RED       PIC X(4) VALUE '[31m'.
           05  COLOR-GREEN     PIC X(4) VALUE '[32m'.
           05  COLOR-YELLOW    PIC X(4) VALUE '[33m'.
           05  COLOR-BLUE      PIC X(4) VALUE '[34m'.
           05  COLOR-MAGENTA   PIC X(4) VALUE '[35m'.
           05  COLOR-CYAN      PIC X(4) VALUE '[36m'.
           05  COLOR-WHITE     PIC X(4) VALUE '[37m'.
           05  COLOR-BOLD      PIC X(3) VALUE '[1m'.
PROCEDURE DIVISION.
MAIN-PROCEDURE.
    DISPLAY "Hello world"
    STOP RUN.
END PROGRAM YOUR-PROGRAM-NAME.
