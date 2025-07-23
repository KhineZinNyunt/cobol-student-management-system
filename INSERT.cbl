IDENTIFICATION DIVISION.
PROGRAM-ID. INSERT.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT STUDENT-FILE-SEM1 ASSIGN TO "student_sem1.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS FILE-STATUS-SEM1.
    SELECT STUDENT-FILE-SEM2 ASSIGN TO "student_sem2.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS FILE-STATUS-SEM2.
    SELECT TEMP-STUDENT-FILE ASSIGN TO "temp_student.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS FILE-STATUS-TEMP.

DATA DIVISION.
FILE SECTION.
FD STUDENT-FILE-SEM1.
01 STUDENT-RECORD-SEM1.
    05 STUDENT-ID1        PIC X(10).
    05 STUDENT-NAME1      PIC X(30).
    05 STUDENT-SUBJECTS1.
        10 MARK1-CST11101  PIC 9(3).
        10 MARK1-CST11201  PIC 9(3).
        10 MARK1-CST11401  PIC 9(3).
        10 MARK1-CST11501  PIC 9(3).
        10 MARK1-CST11601  PIC 9(3).
        10 MARK1-CST11701  PIC 9(3).
    05 TOTAL-MARKS1       PIC 9(3).
    05 GRADE1             PIC X(2).

FD STUDENT-FILE-SEM2.
01 STUDENT-RECORD-SEM2.
    05 STUDENT-ID2        PIC X(10).
    05 STUDENT-NAME2      PIC X(30).
    05 STUDENT-SUBJECTS2.
        10 MARK2-CST12101  PIC 9(3).
        10 MARK2-CST12201  PIC 9(3).
        10 MARK2-CST12401  PIC 9(3).
        10 MARK2-CST12501  PIC 9(3).
        10 MARK2-CST12601  PIC 9(3).
        10 MARK2-CST12701  PIC 9(3).
    05 TOTAL-MARKS2       PIC 9(3).
    05 GRADE2             PIC X(2).

FD TEMP-STUDENT-FILE.
01 TEMP-STUDENT-RECORD    PIC X(80).

WORKING-STORAGE SECTION.
01 WS-EOF                  PIC X VALUE 'N'.
   88 FILE-END             VALUE 'Y'.
   88 FILE-NOT-END         VALUE 'N'.

01  WS-ID-VALID-FLAG       PIC X VALUE "N".
            88  ID-VALID            VALUE "Y".
            88  ID-NOT-VALID        VALUE "N".

01 WS-SEMESTER             PIC 9 VALUE 0.
01 WS-STUDENT-FOUND        PIC X VALUE 'N'.
01 WS-SUBJECT-INDEX        PIC 9 VALUE 1.
01 WS-MARK-INPUT           PIC x(10).
01 WS-MARK                 PIC x(10).
01 WS-TRIMMED-MARK         PIC X(3).
01 WS-MARK-NUMERIC         PIC 9(3).
01 WS-NUMERIC-ONLY         PIC X VALUE 'Y'.
01 WS-CHAR                 PIC X VALUE SPACES.
01 WS-MARK-VALID           PIC X VALUE 'N'.
01 WS-STUDENT-ID           PIC X(10).
01 WS-STUDENT-NAME         PIC X(30).
01 WS-INDEX                PIC 9(2).
01 WS-TOTAL-MARKS          PIC 9(4) VALUE 0.
01 WS-GRADE                PIC X(2).
01 WS-FINAL-RECORD-LINE    PIC X(80).
01 WS-FORMATTED-ID         PIC X(10).
01 WS-FORMATTED-NAME       PIC X(30).
01 WS-FORMATTED-MARKS.
    05 WS-MARK-TEXT OCCURS 6 TIMES PIC X(3).
01 WS-FORMATTED-TOTAL      PIC 9(3).
01 WS-FORMATTED-GRADE      PIC X(2).

01 FILE-STATUS-SEM1        PIC XX.
01 FILE-STATUS-SEM2        PIC XX.
01 FILE-STATUS-TEMP        PIC XX.

01 DISPLAY-LINE.
   05 FILLER               PIC X(10) VALUE "ID: ".
   05 DL-ID                PIC X(10).
   05 FILLER               PIC X(5) VALUE SPACES.
   05 FILLER               PIC X(6) VALUE "Name: ".
   05 DL-NAME              PIC X(30).
01 DISPLAY-MARKS.

   05 DL-SUBJECT           PIC X(10).
   05 FILLER               PIC X(5) VALUE SPACES.

   05 DL-MARK              PIC Z99.
01 DISPLAY-TOTAL.
   05 FILLER               PIC X(10) VALUE "TOTAL: ".
   05 DL-TOTAL             PIC ZZZ.
   05 FILLER               PIC X(5) VALUE SPACES.
   05 FILLER               PIC X(7) VALUE "GRADE: ".
   05 DL-GRADE             PIC X(2).
01 SUBJECT-NAMES-SEM1.
   05 FILLER PIC X(10) VALUE "CST11101".
   05 FILLER PIC X(10) VALUE "CST11201".
   05 FILLER PIC X(10) VALUE "CST11401".
   05 FILLER PIC X(10) VALUE "CST11501".
   05 FILLER PIC X(10) VALUE "CST11601".
   05 FILLER PIC X(10) VALUE "CST11701".
01 SUBJECT-NAME-TABLE-SEM1 REDEFINES SUBJECT-NAMES-SEM1.
   05 SUBJECT-NAME-SEM1 OCCURS 6 TIMES PIC X(10).

01 SUBJECT-NAMES-SEM2.
   05 FILLER PIC X(10) VALUE "CST12101".
   05 FILLER PIC X(10) VALUE "CST12201".
   05 FILLER PIC X(10) VALUE "CST12401".
   05 FILLER PIC X(10) VALUE "CST12501".
   05 FILLER PIC X(10) VALUE "CST12601".
   05 FILLER PIC X(10) VALUE "CST12701".
01 SUBJECT-NAME-TABLE-SEM2 REDEFINES SUBJECT-NAMES-SEM2.
   05 SUBJECT-NAME-SEM2 OCCURS 6 TIMES PIC X(10).


01 WS-LEN              PIC 9(2).
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
LINKAGE SECTION.
01 LS-SEMESTER PIC 9.

PROCEDURE DIVISION  USING LS-SEMESTER.
MAIN-LOGIC.
    MOVE LS-SEMESTER TO WS-SEMESTER.
    IF WS-SEMESTER NOT = 1 AND WS-SEMESTER NOT = 2
        DISPLAY "Invalid semester. Only 1 or 2 allowed."
        PERFORM GET-SEMESTER
    END-IF.
    PERFORM SEARCH-STUDENT
    IF WS-STUDENT-FOUND = 'Y'
        PERFORM ACCEPT-MARKS
        PERFORM CALCULATE-TOTAL-MARKS-AND-GRADE
        PERFORM UPDATE-STUDENT-RECORD
        DISPLAY "Marks successfully updated for student " WS-STUDENT-ID
    ELSE
        DISPLAY "Student ID " WS-STUDENT-ID " not found in semester " WS-SEMESTER
    END-IF
    GOBACK.

GET-SEMESTER.
    DISPLAY "Enter semester to insert marks (1 or 2): "
    ACCEPT WS-SEMESTER
    IF WS-SEMESTER NOT = 1 AND WS-SEMESTER NOT = 2
        DISPLAY ESC COLOR-RED "Invalid semester. Only 1 or 2 allowed." ESC COLOR-RESET
        PERFORM GET-SEMESTER
    END-IF.




SEARCH-STUDENT.
    PERFORM UNTIL WS-STUDENT-FOUND = 'Y'
        *> Request student ID input
        DISPLAY "Enter Student ID to search: "
        ACCEPT WS-STUDENT-ID

        MOVE 'N' TO WS-STUDENT-FOUND
        SET FILE-NOT-END TO TRUE

        IF WS-SEMESTER = 1
            *> Search in Semester 1 file
            OPEN INPUT STUDENT-FILE-SEM1
            IF FILE-STATUS-SEM1 NOT = "00"
                DISPLAY "Error opening semester 1 file. Status: " FILE-STATUS-SEM1
                STOP RUN
            END-IF

            PERFORM UNTIL FILE-END
                READ STUDENT-FILE-SEM1
                    AT END
                        SET FILE-END TO TRUE
                    NOT AT END
                        IF STUDENT-ID1 = WS-STUDENT-ID  *> Fixed variable name from STUDENT-ID2
                            MOVE 'Y' TO WS-STUDENT-FOUND
                            MOVE STUDENT-NAME1 TO WS-STUDENT-NAME
                            DISPLAY "Student found: " STUDENT-NAME1
                            SET FILE-END TO TRUE
                        END-IF
                END-READ
            END-PERFORM

            IF WS-STUDENT-FOUND = 'N'
                DISPLAY "Student ID " WS-STUDENT-ID " not found in Semester 1."
                DISPLAY "Please try again."
            END-IF

            CLOSE STUDENT-FILE-SEM1
        ELSE
            *> Search in Semester 2 file
            OPEN INPUT STUDENT-FILE-SEM2
            IF FILE-STATUS-SEM2 NOT = "00"
                DISPLAY "Error opening semester 2 file. Status: " FILE-STATUS-SEM2
                STOP RUN
            END-IF

            PERFORM UNTIL FILE-END
                READ STUDENT-FILE-SEM2
                    AT END
                        SET FILE-END TO TRUE
                    NOT AT END
                        IF STUDENT-ID2 = WS-STUDENT-ID
                            MOVE 'Y' TO WS-STUDENT-FOUND
                            MOVE STUDENT-NAME2 TO WS-STUDENT-NAME
                            DISPLAY "Student found: " STUDENT-NAME2
                            SET FILE-END TO TRUE
                        END-IF
                END-READ
            END-PERFORM

            IF WS-STUDENT-FOUND = 'N'
                DISPLAY "Student ID " WS-STUDENT-ID " not found in Semester 2."
                DISPLAY "Please try again."
            END-IF

            CLOSE STUDENT-FILE-SEM2
        END-IF
    END-PERFORM.


ACCEPT-MARKS.

   MOVE 1 TO WS-SUBJECT-INDEX
    PERFORM UNTIL WS-SUBJECT-INDEX > 6
        MOVE 'N' TO WS-MARK-VALID
        PERFORM UNTIL WS-MARK-VALID = 'Y'
            IF WS-SEMESTER = 1
                DISPLAY "Enter mark for " SUBJECT-NAME-SEM1(WS-SUBJECT-INDEX) " (0 to 100): "
            ELSE
                DISPLAY "Enter mark for " SUBJECT-NAME-SEM2(WS-SUBJECT-INDEX) " (0 to 100): "
            END-IF

            ACCEPT WS-MARK-INPUT

            *> Initialize validation flags
            MOVE ZERO TO WS-MARK-NUMERIC
            MOVE "Y" TO WS-NUMERIC-ONLY
            MOVE FUNCTION TRIM(WS-MARK-INPUT) TO WS-TRIMMED-MARK

            *> First check for empty input
            IF FUNCTION LENGTH(FUNCTION TRIM(WS-TRIMMED-MARK)) = 0
                DISPLAY "Mark cannot be empty."
            ELSE
                *> Check for negative sign (only allowed as first character)
                IF WS-TRIMMED-MARK(1:1) = '-'
                    DISPLAY "Mark cannot be negative."
                    MOVE "N" TO WS-NUMERIC-ONLY
                ELSE
                    *> Check length doesn't exceed 3 digits
                    IF FUNCTION LENGTH(FUNCTION TRIM(WS-MARK-INPUT)) > 3
                        DISPLAY "Mark must be between 0 and 100."
                        MOVE "N" TO WS-NUMERIC-ONLY
                    ELSE
                        *> Verify all characters are digits
                        PERFORM VARYING WS-INDEX FROM 1 BY 1
                        UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM(WS-TRIMMED-MARK))
                        MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                         IF WS-CHAR NOT NUMERIC
                           MOVE "N" TO WS-NUMERIC-ONLY
                         END-IF
                        END-PERFORM
                    END-IF
                END-IF

                IF WS-NUMERIC-ONLY = "Y"
                MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK) TO WS-MARK-NUMERIC
                IF WS-MARK-NUMERIC >= 0 AND WS-MARK-NUMERIC <= 100
                    MOVE "Y" TO WS-MARK-VALID
                ELSE
                    DISPLAY "Mark must be between 0 and 100."
                END-IF
            ELSE
                DISPLAY "Invalid input. Only numbers 0-100 allowed."
            END-IF
        END-PERFORM


        *> Store the formatted mark
        MOVE WS-MARK-NUMERIC TO WS-MARK-TEXT(WS-SUBJECT-INDEX)
        ADD 1 TO WS-SUBJECT-INDEX
    END-PERFORM.


CALCULATE-TOTAL-MARKS-AND-GRADE.
    MOVE 0 TO WS-TOTAL-MARKS
    PERFORM VARYING WS-SUBJECT-INDEX FROM 1 BY 1 UNTIL WS-SUBJECT-INDEX > 6
        ADD FUNCTION NUMVAL(WS-MARK-TEXT(WS-SUBJECT-INDEX)) TO WS-TOTAL-MARKS
    END-PERFORM

    EVALUATE TRUE
        WHEN WS-TOTAL-MARKS >= 450
            MOVE "A " TO WS-GRADE
        WHEN WS-TOTAL-MARKS >= 400
            MOVE "B " TO WS-GRADE
        WHEN WS-TOTAL-MARKS >= 300
            MOVE "C " TO WS-GRADE
        WHEN OTHER
            MOVE "F " TO WS-GRADE
    END-EVALUATE.

DISPLAY " "
    DISPLAY "=============STUDENT RECORD ================"
    MOVE WS-STUDENT-ID TO DL-ID
    MOVE WS-STUDENT-NAME TO DL-NAME
    DISPLAY DISPLAY-LINE
    DISPLAY " "
    DISPLAY "---------- MARKS ----------"

    PERFORM VARYING WS-SUBJECT-INDEX FROM 1 BY 1 UNTIL WS-SUBJECT-INDEX > 6
        IF WS-SEMESTER = 1
            MOVE SUBJECT-NAME-SEM1(WS-SUBJECT-INDEX) TO DL-SUBJECT
        ELSE
            MOVE SUBJECT-NAME-SEM2(WS-SUBJECT-INDEX) TO DL-SUBJECT
        END-IF
        MOVE WS-MARK-TEXT(WS-SUBJECT-INDEX) TO DL-MARK
        DISPLAY DISPLAY-MARKS
    END-PERFORM

    DISPLAY " "
    MOVE WS-TOTAL-MARKS TO DL-TOTAL
    MOVE WS-GRADE TO DL-GRADE
    DISPLAY DISPLAY-TOTAL
    DISPLAY "=========================================="
    DISPLAY " ".
    IF WS-SEMESTER=1
       DISPLAY "  CST11101-Basic Data Structures "
       DISPLAY "  CST11201-Calculus I "
       DISPLAY "  CST11401-Digital Fundamentals of Computer System "
       DISPLAY "  CST11501-English Language Proficiency I "
       DISPLAY "  CST11601-Myanmar Literature "
       DISPLAY "  CST11701-Physics(Mechanics)"
    ELSE
        DISPLAY "  CST12101-Programming in C++ "
       DISPLAY "  CST12201-CalculusII "
       DISPLAY "  CST12401-Web Technology"
       DISPLAY "  CST12501-English Language Proficiency II"
       DISPLAY "  CST12601-Myanmar Literature "
       DISPLAY "  CST12701-Physics (Electromagnetism)"
    END-IF.

FORMAT-RECORD-LINE.
    MOVE WS-STUDENT-ID TO WS-FORMATTED-ID
    MOVE WS-STUDENT-NAME TO WS-FORMATTED-NAME

    PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 6
        INSPECT WS-MARK-TEXT(WS-INDEX) REPLACING LEADING SPACES BY '0'
    END-PERFORM

    MOVE WS-TOTAL-MARKS TO WS-FORMATTED-TOTAL
    INSPECT WS-FORMATTED-TOTAL REPLACING LEADING SPACES BY '0'
    MOVE WS-GRADE TO WS-FORMATTED-GRADE

    STRING
        WS-FORMATTED-ID DELIMITED BY SIZE
        WS-FORMATTED-NAME DELIMITED BY SIZE
        WS-MARK-TEXT(1) DELIMITED BY SIZE
        WS-MARK-TEXT(2) DELIMITED BY SIZE
        WS-MARK-TEXT(3) DELIMITED BY SIZE
        WS-MARK-TEXT(4) DELIMITED BY SIZE
        WS-MARK-TEXT(5) DELIMITED BY SIZE
        WS-MARK-TEXT(6) DELIMITED BY SIZE
        WS-FORMATTED-TOTAL DELIMITED BY SIZE
        WS-FORMATTED-GRADE DELIMITED BY SIZE
        INTO WS-FINAL-RECORD-LINE.

UPDATE-STUDENT-RECORD.
    *> Open temp file for writing
    OPEN OUTPUT TEMP-STUDENT-FILE
    IF FILE-STATUS-TEMP NOT = "00"
        DISPLAY "Error creating temp file. Status: " FILE-STATUS-TEMP
        STOP RUN
    END-IF

    *> Write the updated record to temp file
    PERFORM FORMAT-RECORD-LINE
    MOVE WS-FINAL-RECORD-LINE TO TEMP-STUDENT-RECORD
    WRITE TEMP-STUDENT-RECORD
    IF FILE-STATUS-TEMP NOT = "00"
        DISPLAY "Error writing to temp file. Status: " FILE-STATUS-TEMP
        STOP RUN
    END-IF
    CLOSE TEMP-STUDENT-FILE

    *> Now copy all records from original file to temp file, replacing the updated record
    IF WS-SEMESTER = 1
        OPEN INPUT STUDENT-FILE-SEM1
        OPEN EXTEND TEMP-STUDENT-FILE
    ELSE
        OPEN INPUT STUDENT-FILE-SEM2
        OPEN EXTEND TEMP-STUDENT-FILE
    END-IF

    *> Reset EOF flag
    MOVE 'N' TO WS-EOF

    *> Copy records, replacing the updated one
    PERFORM UNTIL FILE-END
        IF WS-SEMESTER = 1
            READ STUDENT-FILE-SEM1
                AT END
                    SET FILE-END TO TRUE
                NOT AT END
                    IF STUDENT-ID1 NOT = WS-STUDENT-ID
                        MOVE STUDENT-RECORD-SEM1 TO TEMP-STUDENT-RECORD
                        WRITE TEMP-STUDENT-RECORD
                    END-IF
            END-READ
        ELSE
            READ STUDENT-FILE-SEM2
                AT END
                    SET FILE-END TO TRUE
                NOT AT END
                    IF STUDENT-ID2 NOT = WS-STUDENT-ID
                        MOVE STUDENT-RECORD-SEM2 TO TEMP-STUDENT-RECORD
                        WRITE TEMP-STUDENT-RECORD
                    END-IF
            END-READ
        END-IF
    END-PERFORM

    CLOSE STUDENT-FILE-SEM1
    CLOSE STUDENT-FILE-SEM2
    CLOSE TEMP-STUDENT-FILE

    *> Now replace original file with temp file
    IF WS-SEMESTER = 1
        OPEN OUTPUT STUDENT-FILE-SEM1
        OPEN INPUT TEMP-STUDENT-FILE
    ELSE
        OPEN OUTPUT STUDENT-FILE-SEM2
        OPEN INPUT TEMP-STUDENT-FILE
    END-IF

    *> Reset EOF flag
    MOVE 'N' TO WS-EOF

    *> Copy all records from temp to original file
    PERFORM UNTIL FILE-END
        READ TEMP-STUDENT-FILE
            AT END
                SET FILE-END TO TRUE
            NOT AT END
                IF WS-SEMESTER = 1
                    MOVE TEMP-STUDENT-RECORD TO STUDENT-RECORD-SEM1
                    WRITE STUDENT-RECORD-SEM1
                ELSE
                    MOVE TEMP-STUDENT-RECORD TO STUDENT-RECORD-SEM2
                    WRITE STUDENT-RECORD-SEM2
                END-IF
        END-READ
    END-PERFORM

    CLOSE STUDENT-FILE-SEM1
    CLOSE STUDENT-FILE-SEM2
    CLOSE TEMP-STUDENT-FILE.
