IDENTIFICATION DIVISION.
PROGRAM-ID. STUDENT-MGMT.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT STUDENT-FILE-SEM1 ASSIGN TO "student_sem1.dat"
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT STUDENT-FILE-SEM2 ASSIGN TO "student_sem2.dat"
        ORGANIZATION IS LINE SEQUENTIAL.


DATA DIVISION.
FILE SECTION.
FD  STUDENT-FILE-SEM1.
01  STUDENT-RECORD-SEM1.
    05  STUDENT-ID1        PIC X(10).
    05  STUDENT-NAME1      PIC X(30).
    05  STUDENT-SUBJECTS1.
        10  MARK1-CST11101 PIC 9(3).
        10  MARK1-CST11201 PIC 9(3).
        10  MARK1-CST11401 PIC 9(3).
        10  MARK1-CST11501 PIC 9(3).
        10  MARK1-CST11601 PIC 9(3).
        10  MARK1-CST11701 PIC 9(3).
    05  TOTAL-MARKS1       PIC 9(3).
    05  GRADE1             PIC X(2).

FD  STUDENT-FILE-SEM2.
01  STUDENT-RECORD-SEM2.
    05  STUDENT-ID2        PIC X(10).
    05  STUDENT-NAME2      PIC X(30).
    05  STUDENT-SUBJECTS2.
        10  MARK2-CST12101 PIC 9(3).
        10  MARK2-CST12201 PIC 9(3).
        10  MARK2-CST12401 PIC 9(3).
        10  MARK2-CST12501 PIC 9(3).
        10  MARK2-CST12601 PIC 9(3).
        10  MARK2-CST12701 PIC 9(3).
    05  TOTAL-MARKS2       PIC 9(3).
    05  GRADE2             PIC X(2).

WORKING-STORAGE SECTION.
01  WS-INPUT-VALIDATION.
    05  WS-CHOICE          PIC X(3).
    05  WS-NUMERIC-CHOICE  PIC 9.
    05  WS-VALID-CHOICE    PIC X VALUE 'N'.
        88  IS-VALID-CHOICE VALUE 'Y'.
    05  WS-MANAGE-CHOICE   PIC X(3).
    05  WS-NUMERIC-MANAGE  PIC 9.
    05  WS-VALID-MANAGE    PIC X VALUE 'N'.
        88  IS-VALID-MANAGE VALUE 'Y'.
    05  WS-SEMESTER        PIC 9.
    05  WS-VALID-SEMESTER  PIC X VALUE 'N'.
        88  IS-VALID-SEMESTER VALUE 'Y'.

01  WS-FILE-STATUS.
    05  WS-EOF             PIC X VALUE 'N'.
        88  END-OF-FILE     VALUE 'Y'.
        88  NOT-END-OF-FILE VALUE 'N'.
    05  WS-FOUND           PIC X VALUE 'N'.
        88  RECORD-FOUND    VALUE 'Y'.
        88  RECORD-NOT-FOUND VALUE 'N'.
    05  WS-DELETED         PIC X VALUE 'N'.

01  WS-STUDENT-DATA.
    05  STUDENT-ID         PIC X(10).
    05  STUDENT-NAME       PIC X(30).
    05  MARKS              OCCURS 6 TIMES PIC 9(3).
    05  IDX                PIC 9 VALUE 1.
    05  WS-ID-SEARCH       PIC X(10).
    05  WS-ID-DELETE       PIC X(10).
    05  WS-ID-EDIT         PIC X(10).
    05  WS-EDIT-FOUND      PIC X.
01  WS-EDIT-RETURN-CODE    PIC X VALUE 'N'.
    88  EDIT-SUCCESS       VALUE 'Y'.
    88  EDIT-FAILED        VALUE 'N'.
01 DISP-TOTAL-MARK       PIC Z(4).
01 DISP-RANK             PIC Z(4)..
01 IDY                   PIC 9(4) VALUE 1.
01 IDZ                   PIC 9 VALUE 1.
01 TOTAL-MARKS           PIC 9999.
01 GRADE                 PIC X.

01 DISP-MARK1            PIC Z(3).
01 DISP-MARK2            PIC Z(3).
01 DISP-MARK3            PIC Z(3).
01 DISP-MARK4            PIC Z(3).
01 DISP-MARK5            PIC Z(3).
01 DISP-MARK6            PIC Z(3).

01 WS-STUDENT-COUNT      PIC 9(4) VALUE 0.
01 WS-STUDENT-TABLE OCCURS 1 TO 1000 TIMES
                        DEPENDING ON WS-STUDENT-COUNT.
    05 WS-SORT-ID        PIC X(10).
    05 WS-SORT-NAME      PIC X(30).
    05 WS-SORT-MARKS     OCCURS 6 TIMES PIC 999.
    05 WS-SORT-TOTAL     PIC 9999.
    05 WS-SORT-GRADE     PIC X.

01 WS-TEMP-STUDENT.
    05 WS-TEMP-ID        PIC X(10).
    05 WS-TEMP-NAME      PIC X(30).
    05 WS-TEMP-MARKS     OCCURS 6 TIMES PIC 999.
    05 WS-TEMP-TOTAL     PIC 9(4).
    05 WS-TEMP-GRADE     PIC X.
01  ESCAPE-CHAR          PIC X VALUE X'1B'.
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
PROCEDURE DIVISION.
BEGIN.
    PERFORM UNTIL WS-NUMERIC-CHOICE = 4
        DISPLAY ESC COLOR-CYAN "**********************************"ESC COLOR-RESET
        DISPLAY ESC COLOR-BOLD"*Student Record Management System*"ESC COLOR-RESET
        DISPLAY ESC COLOR-CYAN "**********************************"ESC COLOR-RESET
        DISPLAY ESC COLOR-RED"1. "ESC COLOR-RESET "Manage Records"
        DISPLAY ESC COLOR-RED"2. "ESC COLOR-RESET "View Reports"
        DISPLAY ESC COLOR-RED"3. "ESC COLOR-RESET "Search Record"
        DISPLAY ESC COLOR-RED"4. "ESC COLOR-RESET "Exit"
        DISPLAY ESC COLOR-GREEN"Enter your choice (1-4): "ESC COLOR-RESET
        ACCEPT WS-CHOICE

        *> Validate main menu choice
        PERFORM VALIDATE-MENU-CHOICE

        IF IS-VALID-CHOICE
            EVALUATE WS-NUMERIC-CHOICE
                WHEN 1
                    PERFORM MANAGE-RECORDS
                WHEN 2
                    PERFORM VIEW-REPORTS
                WHEN 3
                    PERFORM SEARCH-RECORD-PROCESS
                WHEN 4
                    DISPLAY "Exiting program..."
            END-EVALUATE
        ELSE
            DISPLAY "Invalid Choice. Please enter a single digit 1-4."
        END-IF
    END-PERFORM
    STOP RUN.

VALIDATE-MENU-CHOICE.
    MOVE 'N' TO WS-VALID-CHOICE
    IF WS-CHOICE(1:1) IS NUMERIC AND
       WS-CHOICE(2:1) = SPACE AND
       WS-CHOICE(3:1) = SPACE
        MOVE WS-CHOICE(1:1) TO WS-NUMERIC-CHOICE
        IF WS-NUMERIC-CHOICE >= 1 AND WS-NUMERIC-CHOICE <= 4
            MOVE 'Y' TO WS-VALID-CHOICE
        END-IF
    END-IF.

MANAGE-RECORDS.
    *> Reset flags before starting the loop
    MOVE 'N' TO WS-VALID-MANAGE
    MOVE 'N' TO WS-VALID-SEMESTER

    PERFORM UNTIL IS-VALID-MANAGE
        DISPLAY ESC COLOR-CYAN "---------------------------------------------------------" ESC COLOR-RESET
        DISPLAY ESC COLOR-BOLD "Manage Records Menu" ESC COLOR-RESET
        DISPLAY ESC COLOR-YELLOW "1. " ESC COLOR-RESET "Add Record"
        DISPLAY ESC COLOR-YELLOW "2. " ESC COLOR-RESET "Edit Record"
        DISPLAY ESC COLOR-YELLOW "3. " ESC COLOR-RESET "Delete Record"
        DISPLAY ESC COLOR-YELLOW "4. " ESC COLOR-RESET "Back to Main Menu"
        DISPLAY ESC COLOR-GREEN "Enter Manage Option (1-4): " ESC COLOR-RESET
        ACCEPT WS-MANAGE-CHOICE

        *> Validate manage menu choice
        PERFORM VALIDATE-MANAGE-CHOICE

        IF NOT IS-VALID-MANAGE
            DISPLAY "Invalid option. Please enter 1-4."
            DISPLAY "Press Enter to continue..."
            ACCEPT WS-CHOICE *> Pause
        ELSE
            *> Check if user selected "Back to Main Menu"
            IF WS-NUMERIC-MANAGE = 4
                MOVE 'Y' TO WS-VALID-MANAGE  *> Exit manage records menu
                CONTINUE
            ELSE
                *> Reset semester flag before validation
                MOVE 'N' TO WS-VALID-SEMESTER
                PERFORM VALIDATE-SEMESTER-INPUT

            IF IS-VALID-SEMESTER
                    EVALUATE WS-NUMERIC-MANAGE
                        WHEN 1
                            PERFORM ADD-RECORD
                        WHEN 2
                            PERFORM EDIT-RECORD-PROCESS
                        WHEN 3
                            PERFORM DELETE-RECORD-PROCESS
                    END-EVALUATE
                ELSE
                    DISPLAY "Invalid semester. Please enter 1 or 2."
                    DISPLAY "Press Enter to continue..."
                    ACCEPT WS-CHOICE *> Pause
                END-IF
            END-IF
        END-IF
    END-PERFORM.

VALIDATE-MANAGE-CHOICE.
    MOVE 'N' TO WS-VALID-MANAGE
    IF WS-MANAGE-CHOICE(1:1) IS NUMERIC AND
       WS-MANAGE-CHOICE(2:1) = SPACE AND
       WS-MANAGE-CHOICE(3:1) = SPACE
        MOVE WS-MANAGE-CHOICE(1:1) TO WS-NUMERIC-MANAGE
        IF WS-NUMERIC-MANAGE >= 1 AND WS-NUMERIC-MANAGE <= 4  *> Changed to 4
            MOVE 'Y' TO WS-VALID-MANAGE
        END-IF
    END-IF.

*> ADD-RECORD.
        *> CALL "INSERT" USING WS-SEMESTER
    *> IF RETURN-CODE = 0
        *> DISPLAY "Record added successfully."
        *> DISPLAY "Displaying updated records..."
        *> CALL "VIEW" USING WS-SEMESTER
    *> ELSE
        *> DISPLAY "Error occurred during record insertion."
    *> END-IF
    *> PERFORM ASK-TO-CONTINUE.

    *> ADD-RECORD.
    *> *> Reset semester validation flag
    *> MOVE 'N' TO WS-VALID-SEMESTER
    *> PERFORM VALIDATE-SEMESTER-INPUT

    *> IF IS-VALID-SEMESTER
        *> CALL "INSERT" USING WS-SEMESTER
        *> IF RETURN-CODE = 0
            *> DISPLAY "Record added successfully."
            *> DISPLAY "Displaying updated records..."
            *> CALL "VIEW" USING WS-SEMESTER
        *> ELSE
            *> DISPLAY "Error occurred during record insertion."
        *> END-IF
    *> ELSE
        *> DISPLAY "Invalid semester selection."
    *> END-IF
    *> PERFORM ASK-TO-CONTINUE.
 ADD-RECORD.
    *> Remove the semester validation here since INSERT.cbl will handle it
    CALL "INSERT" USING WS-SEMESTER
    IF RETURN-CODE = 0
        DISPLAY "Record added successfully."
        DISPLAY "Displaying updated records..."
        CALL "VIEW" USING WS-SEMESTER
    ELSE
        DISPLAY "Error occurred during record insertion."
    END-IF
    PERFORM ASK-TO-CONTINUE.

EDIT-RECORD-PROCESS.
    *> Get student ID to edit
    DISPLAY "Enter Student ID to Edit: "
    ACCEPT WS-ID-EDIT

        CALL 'EDIT' USING
            BY CONTENT WS-ID-EDIT,
            BY CONTENT WS-SEMESTER,
            BY REFERENCE WS-EDIT-RETURN-CODE

        IF EDIT-SUCCESS
            *> DISPLAY "Record edited successfully."
            *> After successful edit, regenerate ranked files
            DISPLAY "Updating ranked results..."
            CALL "VIEW" USING WS-SEMESTER
        ELSE
            DISPLAY "Student not found in Semester " WS-SEMESTER
        END-IF


    PERFORM ASK-TO-CONTINUE.

SEARCH-RECORD-PROCESS.
    DISPLAY "Enter Student ID to Search: "
    ACCEPT WS-ID-SEARCH
    CALL "SEARCH-RECORD" USING WS-ID-SEARCH
    PERFORM ASK-TO-CONTINUE.
DELETE-RECORD-PROCESS.
           DISPLAY "Enter Student ID to Delete: "
           ACCEPT WS-ID-DELETE
           MOVE 'N' TO WS-DELETED
       CALL 'DELETE-RECORD' USING WS-ID-DELETE, WS-SEMESTER, WS-DELETED
           IF WS-DELETED = 'Y'
               DISPLAY "Record deleted successfully."
           ELSE
               DISPLAY "Record not found in Semester " WS-SEMESTER
           END-IF.
          CALL "VIEW" USING WS-SEMESTER
    PERFORM ASK-TO-CONTINUE.
VIEW-REPORTS.
    DISPLAY "Select Semester (1 or 2): "
    ACCEPT WS-SEMESTER
    CALL "VIEW" USING WS-SEMESTER.
    PERFORM ASK-TO-CONTINUE.

VALIDATE-SEMESTER-INPUT.
    DISPLAY "Select Semester (1 or 2): "
    ACCEPT WS-SEMESTER
    MOVE 'N' TO WS-VALID-SEMESTER
    IF WS-SEMESTER = 1 OR WS-SEMESTER = 2
        MOVE 'Y' TO WS-VALID-SEMESTER
    END-IF.
*> ASK-TO-CONTINUE.
    *> DISPLAY "Do you want to continue? (Y/N): "
    *> ACCEPT WS-CHOICE
    *> IF WS-CHOICE = 'N' OR WS-CHOICE = 'n'
        *> MOVE 4 TO WS-NUMERIC-CHOICE
    *> END-IF.
ASK-TO-CONTINUE.
    DISPLAY "Do you want to continue? (Y/N): "
    ACCEPT WS-CHOICE
    IF WS-CHOICE = 'N' OR WS-CHOICE = 'n'
        MOVE 4 TO WS-NUMERIC-CHOICE
    ELSE
        *> Reset relevant flags for next operation
        MOVE 'N' TO WS-VALID-MANAGE
        MOVE 'N' TO WS-VALID-SEMESTER
        MOVE 'N' TO WS-EDIT-RETURN-CODE
        MOVE 'N' TO WS-DELETED
        MOVE 'N' TO WS-FOUND
    END-IF.
