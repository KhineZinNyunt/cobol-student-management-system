IDENTIFICATION DIVISION.
PROGRAM-ID. REGISTER-SEMS.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT SEM1-FILE ASSIGN TO 'student_sem1.dat'
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT SEM2-FILE ASSIGN TO 'student_sem2.dat'
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT SORT-FILE ASSIGN TO 'SORTWORK'.
    SELECT SORTED-SEM2 ASSIGN TO 'student_sem2_sorted.dat'
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD  SEM1-FILE.
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

FD  SEM2-FILE.
01  STUDENT-RECORD-SEM2.
    05  STUDENT-ID2        PIC X(10).
    05  STUDENT-NAME2      PIC X(30).

SD  SORT-FILE.
01  SORT-RECORD.
    05  SORT-ID           PIC X(10).
    05  SORT-NAME         PIC X(30).

FD  SORTED-SEM2.
01  SORTED-RECORD.
    05  SORTED-ID         PIC X(10).
    05  SORTED-NAME       PIC X(30).

WORKING-STORAGE SECTION.
01  WS-STUDENT-ID          PIC X(10).
01  WS-FOUND               PIC X VALUE 'N'.
01  WS-MORE-DATA           PIC X VALUE 'Y'.
01  WS-FILE-STATUS         PIC XX VALUE '00'.
01  WS-DUPLICATE           PIC X VALUE 'N'.
01  WS-IO-STATUS           PIC 99.
01  WS-TEMP-FILE           PIC X(20) VALUE 'temp_sem2.dat'.

PROCEDURE DIVISION.
MAIN-LOGIC.
    PERFORM GET-STUDENT-ID
    PERFORM CHECK-REGISTRATION
    IF WS-FOUND = 'Y' AND WS-DUPLICATE = 'N'
        PERFORM WRITE-TO-SEM2
        PERFORM SORT-SEM2-FILE
    END-IF
    STOP RUN.

GET-STUDENT-ID.
    DISPLAY "Enter Student ID to register for SEM2: "
    ACCEPT WS-STUDENT-ID.

CHECK-REGISTRATION.
    *> First check SEM2 file for duplicates
    PERFORM CHECK-DUPLICATE
    IF WS-DUPLICATE = 'Y'
        DISPLAY "Student ID " WS-STUDENT-ID " already registered for SEM2"
        MOVE 'N' TO WS-FOUND
    ELSE
        *> Then check SEM1 file for eligibility
        OPEN INPUT SEM1-FILE
        MOVE 'N' TO WS-FOUND
        MOVE 'Y' TO WS-MORE-DATA

        PERFORM UNTIL WS-MORE-DATA = 'N'
            READ SEM1-FILE
                AT END
                    MOVE 'N' TO WS-MORE-DATA
                NOT AT END
                    IF STUDENT-ID1 = WS-STUDENT-ID
                        MOVE 'Y' TO WS-FOUND
                        EVALUATE GRADE1
                            WHEN "A" WHEN "B" WHEN "C"
                                DISPLAY "Student eligible for SEM2: " STUDENT-NAME1
                            WHEN OTHER
                                DISPLAY "Registration Failed for: " STUDENT-NAME1
                                DISPLAY "Reason: Grade " GRADE1 " is not passing"
                                MOVE 'N' TO WS-FOUND
                        END-EVALUATE
                        MOVE 'N' TO WS-MORE-DATA
                    END-IF
            END-READ
        END-PERFORM

        IF WS-FOUND = 'N'
            DISPLAY "Student ID " WS-STUDENT-ID " not found in SEM1 records."
        END-IF

        CLOSE SEM1-FILE
    END-IF.

CHECK-DUPLICATE.
    MOVE 'N' TO WS-DUPLICATE
    OPEN INPUT SEM2-FILE
    MOVE 'Y' TO WS-MORE-DATA
    PERFORM UNTIL WS-MORE-DATA = 'N' OR WS-DUPLICATE = 'Y'
        READ SEM2-FILE
            AT END
                MOVE 'N' TO WS-MORE-DATA
            NOT AT END
                IF STUDENT-ID2 = WS-STUDENT-ID
                    MOVE 'Y' TO WS-DUPLICATE
                    MOVE 'N' TO WS-MORE-DATA
                END-IF
        END-READ
    END-PERFORM
    CLOSE SEM2-FILE.

WRITE-TO-SEM2.
    *> First create a temporary file with all existing records
    OPEN INPUT SEM2-FILE
    OPEN OUTPUT SORTED-SEM2

    MOVE 'Y' TO WS-MORE-DATA
    PERFORM UNTIL WS-MORE-DATA = 'N'
        READ SEM2-FILE
            AT END
                MOVE 'N' TO WS-MORE-DATA
            NOT AT END
                MOVE STUDENT-ID2 TO SORTED-ID
                MOVE STUDENT-NAME2 TO SORTED-NAME
                WRITE SORTED-RECORD
        END-READ
    END-PERFORM

    *> Add the new registration to the temporary file
    MOVE STUDENT-ID1 TO SORTED-ID
    MOVE STUDENT-NAME1 TO SORTED-NAME
    WRITE SORTED-RECORD

    CLOSE SEM2-FILE, SORTED-SEM2

    *> Now sort the combined records
    SORT SORT-FILE
        ON ASCENDING KEY SORT-ID
        USING SORTED-SEM2
        GIVING SEM2-FILE

    DISPLAY "Registration completed for ID: " WS-STUDENT-ID.

SORT-SEM2-FILE.
    *> This is now handled within WRITE-TO-SEM2
    *> DISPLAY "SEM2 file has been updated and sorted by student ID".
