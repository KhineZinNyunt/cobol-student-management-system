IDENTIFICATION DIVISION.
PROGRAM-ID. REGISTER-SEMS.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT STUDENT-FILE-SEM1 ASSIGN TO 'student_sem1.dat'
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT STUDENT-FILE-SEM2 ASSIGN TO 'student_sem2.dat'
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT RANKED-RESULTS-FILE ASSIGN TO 'ranked_results_sem2.dat'
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT SORT-WORK-FILE ASSIGN TO 'SORTWORK'.
    SELECT TEMP-SEM2-FILE ASSIGN TO 'temp_sem2.dat'
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
    05  RANK               PIC 9(3).

FD  RANKED-RESULTS-FILE.
01  RANKED-RECORD         PIC X(80).

SD  SORT-WORK-FILE.
01  SORT-RECORD.
    05  SORT-STUDENT-ID    PIC X(10).
    05  SORT-STUDENT-NAME  PIC X(30).
    05  SORT-SUBJECTS.
        10  SORT-MARK2-CST12101 PIC 9(3).
        10  SORT-MARK2-CST12201 PIC 9(3).
        10  SORT-MARK2-CST12401 PIC 9(3).
        10  SORT-MARK2-CST12501 PIC 9(3).
        10  SORT-MARK2-CST12601 PIC 9(3).
        10  SORT-MARK2-CST12701 PIC 9(3).
    05  SORT-TOTAL-MARKS   PIC 9(3).
    05  SORT-GRADE         PIC X(2).
    05  SORT-RANK          PIC 9(3).

FD  TEMP-SEM2-FILE.
01  TEMP-RECORD.
    05  TEMP-DATA         PIC X(80).

WORKING-STORAGE SECTION.
01  WS-STUDENT-ID          PIC 9(5).
01  WS-STUDENT-NAME        PIC X(30).
01  WS-SEM-CHOICE          PIC 9.
01  WS-FOUND               PIC X VALUE 'N'.
01  WS-MORE-DATA           PIC X VALUE 'Y'.
01  WS-FILE-STATUS         PIC XX VALUE '00'.
01  WS-DUPLICATE           PIC X VALUE 'N'.
01  WS-IO-STATUS           PIC 99.
01  WS-VALID-ID            PIC X VALUE 'N'.
01  WS-INPUT-ID            PIC X(10).
01  WS-CONTINUE            PIC X VALUE 'Y'.
01  WS-HEADER.
    05 FILLER              PIC X(20) VALUE '2'.
    05 FILLER              PIC X(20) VALUE 'SEMESTER II'.
    05 FILLER              PIC X(40) VALUE SPACES.
01  WS-COLUMN-HEADER.
    05 FILLER              PIC X(10)  VALUE 'StudentID'.
    05 FILLER              PIC X(12) VALUE 'Name'.
    05 FILLER              PIC X(5)  VALUE '2101'.
    05 FILLER              PIC X(5)  VALUE '2201'.
    05 FILLER              PIC X(5)  VALUE '2401'.
    05 FILLER              PIC X(5)  VALUE '2501'.
    05 FILLER              PIC X(5)  VALUE '2601'.
    05 FILLER              PIC X(5)  VALUE '2701'.
    05 FILLER              PIC X(7)  VALUE 'Total'.
    05 FILLER              PIC X(7)  VALUE 'Grade'.
    05 FILLER              PIC X(5)  VALUE 'Rank'.
01  WS-DIVIDER             PIC X(80) VALUE ALL '-'.

PROCEDURE DIVISION.
MAIN-LOGIC.
    PERFORM UNTIL WS-CONTINUE = 'N'
        PERFORM GET-SEM-CHOICE
        PERFORM GET-STUDENT-ID
        IF WS-VALID-ID = 'Y'
            EVALUATE WS-SEM-CHOICE
                WHEN 1 PERFORM REGISTER-SEM1
                WHEN 2 PERFORM REGISTER-SEM2
                WHEN OTHER DISPLAY "Invalid choice. Please select 1 or 2."
            END-EVALUATE
        ELSE
            DISPLAY "Registration aborted due to invalid ID."
        END-IF
        DISPLAY "Do you want to continue? (Y/N): "
        ACCEPT WS-CONTINUE
        *> Convert input to uppercase
        IF WS-CONTINUE = 'n' OR 'y'
            MOVE FUNCTION UPPER-CASE(WS-CONTINUE) TO WS-CONTINUE
        END-IF
    END-PERFORM
    STOP RUN.

GET-SEM-CHOICE.
    PERFORM WITH TEST AFTER UNTIL WS-SEM-CHOICE = 1 OR WS-SEM-CHOICE = 2
        DISPLAY " "
        DISPLAY "Select semester to register for:"
        DISPLAY "1. SEM1 (New registration)"
        DISPLAY "2. SEM2"
        DISPLAY "Enter your choice (1 or 2): "
        ACCEPT WS-SEM-CHOICE
        IF WS-SEM-CHOICE NOT = 1 AND WS-SEM-CHOICE NOT = 2
            DISPLAY "Invalid choice. Please enter 1 or 2."
        END-IF
    END-PERFORM.

GET-STUDENT-ID.
    PERFORM WITH TEST AFTER UNTIL WS-VALID-ID = 'Y'
        DISPLAY "Enter Student ID (5 digits): "
        ACCEPT WS-INPUT-ID
        INSPECT WS-INPUT-ID TALLYING WS-IO-STATUS FOR LEADING SPACES
        IF FUNCTION LENGTH(FUNCTION TRIM(WS-INPUT-ID)) = 5 AND
           FUNCTION TRIM(WS-INPUT-ID) IS NUMERIC
            MOVE FUNCTION TRIM(WS-INPUT-ID) TO WS-STUDENT-ID
            MOVE 'Y' TO WS-VALID-ID
        ELSE
            DISPLAY "Invalid ID. Must be exactly 5 digits (0-9)."
            DISPLAY "You entered: '" WS-INPUT-ID "'"
            MOVE 'N' TO WS-VALID-ID
        END-IF
    END-PERFORM.

GET-STUDENT-NAME.
    DISPLAY "Enter Student Name: "
    ACCEPT WS-STUDENT-NAME.

REGISTER-SEM1.
    PERFORM CHECK-DUPLICATE-SEM1
    IF WS-DUPLICATE = 'N'
        PERFORM GET-STUDENT-NAME
        OPEN EXTEND STUDENT-FILE-SEM1
        MOVE WS-STUDENT-ID TO STUDENT-ID1
        MOVE WS-STUDENT-NAME TO STUDENT-NAME1
        MOVE 0 TO MARK1-CST11101 MARK1-CST11201 MARK1-CST11401
                 MARK1-CST11501 MARK1-CST11601 MARK1-CST11701
        MOVE 0 TO TOTAL-MARKS1
        MOVE "NA" TO GRADE1
        WRITE STUDENT-RECORD-SEM1
        CLOSE STUDENT-FILE-SEM1
        DISPLAY "Registration for SEM1 completed for ID: " WS-STUDENT-ID
    ELSE
        DISPLAY "Student ID " WS-STUDENT-ID " already registered for SEM1"
    END-IF.

REGISTER-SEM2.
    PERFORM CHECK-ELIGIBILITY-FOR-SEM2
    IF WS-FOUND = 'Y' AND WS-DUPLICATE = 'N'
        PERFORM WRITE-TO-SEM2
        DISPLAY "SEM2 file has been updated and sorted by student ID"
    END-IF.

CHECK-DUPLICATE-SEM1.
    MOVE 'N' TO WS-DUPLICATE
    OPEN INPUT STUDENT-FILE-SEM1
    MOVE 'Y' TO WS-MORE-DATA
    PERFORM UNTIL WS-MORE-DATA = 'N' OR WS-DUPLICATE = 'Y'
        READ STUDENT-FILE-SEM1
            AT END
                MOVE 'N' TO WS-MORE-DATA
            NOT AT END
                IF STUDENT-ID1 = WS-STUDENT-ID
                    MOVE 'Y' TO WS-DUPLICATE
                    MOVE 'N' TO WS-MORE-DATA
                END-IF
        END-READ
    END-PERFORM
    CLOSE STUDENT-FILE-SEM1.

CHECK-ELIGIBILITY-FOR-SEM2.
    PERFORM CHECK-DUPLICATE-SEM2
    IF WS-DUPLICATE = 'Y'
        DISPLAY "Student ID " WS-STUDENT-ID " already registered for SEM2"
        MOVE 'N' TO WS-FOUND
    ELSE
        OPEN INPUT STUDENT-FILE-SEM1
        MOVE 'N' TO WS-FOUND
        MOVE 'Y' TO WS-MORE-DATA
        MOVE 'N' TO WS-DUPLICATE *> Flag to track if we found but rejected due to grade

        PERFORM UNTIL WS-MORE-DATA = 'N'
            READ STUDENT-FILE-SEM1
                AT END
                    MOVE 'N' TO WS-MORE-DATA
                NOT AT END
                    IF STUDENT-ID1 = WS-STUDENT-ID
                        MOVE 'Y' TO WS-FOUND
                        MOVE STUDENT-NAME1 TO WS-STUDENT-NAME
                        EVALUATE GRADE1
    WHEN "A" WHEN "B" WHEN "C"
        DISPLAY "Student eligible for SEM2: " STUDENT-NAME1
    WHEN OTHER
        DISPLAY "Registration Failed for: " STUDENT-NAME1
        DISPLAY "Reason: Grade " GRADE1 " is not passing"
        MOVE 'N' TO WS-FOUND
        MOVE 'Y' TO WS-DUPLICATE
END-EVALUATE
                        MOVE 'N' TO WS-MORE-DATA
                    END-IF
            END-READ
        END-PERFORM

        *> Only show "not found" if we didn't find the student at all
        IF WS-FOUND = 'N' AND WS-DUPLICATE = 'N'
            DISPLAY "Student ID " WS-STUDENT-ID " not found in SEM1 records."
        END-IF

        CLOSE STUDENT-FILE-SEM1
    END-IF.

CHECK-DUPLICATE-SEM2.
    MOVE 'N' TO WS-DUPLICATE
    OPEN INPUT STUDENT-FILE-SEM2
    MOVE 'Y' TO WS-MORE-DATA
    PERFORM UNTIL WS-MORE-DATA = 'N' OR WS-DUPLICATE = 'Y'
        READ STUDENT-FILE-SEM2
            AT END
                MOVE 'N' TO WS-MORE-DATA
            NOT AT END
                IF STUDENT-ID2 = WS-STUDENT-ID
                    MOVE 'Y' TO WS-DUPLICATE
                    MOVE 'N' TO WS-MORE-DATA
                END-IF
        END-READ
    END-PERFORM
    CLOSE STUDENT-FILE-SEM2.

WRITE-TO-SEM2.
    *> First create a temporary file with all existing records
    OPEN INPUT STUDENT-FILE-SEM2
    OPEN OUTPUT TEMP-SEM2-FILE

    MOVE 'Y' TO WS-MORE-DATA
    PERFORM UNTIL WS-MORE-DATA = 'N'
        READ STUDENT-FILE-SEM2
            AT END
                MOVE 'N' TO WS-MORE-DATA
            NOT AT END
                MOVE STUDENT-RECORD-SEM2 TO TEMP-DATA
                WRITE TEMP-RECORD
        END-READ
    END-PERFORM

    *> Add the new registration with only ID and name
    MOVE SPACES TO STUDENT-RECORD-SEM2
    MOVE WS-STUDENT-ID TO STUDENT-ID2
    MOVE WS-STUDENT-NAME TO STUDENT-NAME2
    MOVE STUDENT-RECORD-SEM2 TO TEMP-DATA
    WRITE TEMP-RECORD

    CLOSE STUDENT-FILE-SEM2, TEMP-SEM2-FILE

    *> Sort the file by student ID
    SORT SORT-WORK-FILE
        ON ASCENDING KEY SORT-STUDENT-ID
        USING TEMP-SEM2-FILE
        GIVING STUDENT-FILE-SEM2

    *> Generate ranked results
    PERFORM GENERATE-RANKED-RESULTS

    DISPLAY "Registration completed for ID: " WS-STUDENT-ID
    DISPLAY "Ranked results for Semester 2 saved to ranked_results_sem2.dat".

GENERATE-RANKED-RESULTS.
    OPEN INPUT STUDENT-FILE-SEM2
    OPEN OUTPUT RANKED-RESULTS-FILE

    *> Write headers
    WRITE RANKED-RECORD FROM WS-HEADER
    WRITE RANKED-RECORD FROM WS-DIVIDER
    WRITE RANKED-RECORD FROM WS-COLUMN-HEADER
    WRITE RANKED-RECORD FROM WS-DIVIDER

    *> Process each record
    MOVE 'Y' TO WS-MORE-DATA
    MOVE 0 TO WS-IO-STATUS *> Using as rank counter
    PERFORM UNTIL WS-MORE-DATA = 'N'
        READ STUDENT-FILE-SEM2
            AT END
                MOVE 'N' TO WS-MORE-DATA
            NOT AT END
                ADD 1 TO WS-IO-STATUS
                MOVE WS-IO-STATUS TO RANK
                WRITE RANKED-RECORD FROM STUDENT-RECORD-SEM2
        END-READ
    END-PERFORM

    CLOSE STUDENT-FILE-SEM2, RANKED-RESULTS-FILE.
