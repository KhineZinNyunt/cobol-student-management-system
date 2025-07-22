IDENTIFICATION DIVISION.
PROGRAM-ID. NCURSES-EXAMPLE.

DATA DIVISION.
WORKING-STORAGE SECTION.
       *> colorCodes.cpy - ANSI color definitions
       01  COLOR-CODES.
           05  ESC             PIC X    VALUE X'1B'.
           05  COLOR-RESET     PIC X(3) VALUE '[0m'.
           05  COLOR-RED       PIC X(4) VALUE '[31m'.
           05  COLOR-GREEN     PIC X(4) VALUE '[32m'.
           05  COLOR-YELLOW    PIC X(4) VALUE '[33m'.
           05  COLOR-BLUE      PIC X(4) VALUE '[34m'.
           05  COLOR-MAGENTA   PIC X(4) VALUE '[35m'.
           05  COLOR-CYAN      PIC X(4) VALUE '[36m'.
           05  COLOR-WHITE     PIC X(4) VALUE '[37m'.
           05  COLOR-BOLD      PIC X(3) VALUE '[1m'.
01  MENU-COLORS.
    05  MENU-TITLE    PIC X(10) VALUE X'1B' & "[1;36m".  *> Bold Cyan
    05  MENU-OPTION   PIC X(10) VALUE X'1B' & "[1;33m".  *> Bold Yellow
    05  MENU-PROMPT   PIC X(10) VALUE X'1B' & "[1;32m".  *> Bold Green
    05  RESET-COLOR   PIC X(10) VALUE X'1B' & "[0m".     *> Reset
PROCEDURE DIVISION.
MAIN-MENU.
    DISPLAY MENU-TITLE
            "********************************"
            RESET-COLOR.
    DISPLAY MENU-TITLE
            "* STUDENT MANAGEMENT SYSTEM *"
            RESET-COLOR.
    DISPLAY MENU-TITLE
            "********************************"
            RESET-COLOR.

    DISPLAY MENU-OPTION "1. " RESET-COLOR "Manage Records".
    DISPLAY MENU-OPTION "2. " RESET-COLOR "View Reports".
    DISPLAY MENU-OPTION "3. " RESET-COLOR "Search Record".
    DISPLAY MENU-OPTION "4. " RESET-COLOR "Exit".

    DISPLAY MENU-PROMPT "Enter your choice (1-4): "
            WITH NO ADVANCING.
