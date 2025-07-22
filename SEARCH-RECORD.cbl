       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH-RECORD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
    SELECT RANKED-RESULTS-SEM1 ASSIGN TO "ranked_results_sem1.dat"
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT RANKED-RESULTS-SEM2 ASSIGN TO "ranked_results_sem2.dat"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD  RANKED-RESULTS-SEM1.
01  RANKED-RECORD-SEM1.
    05  RR1-STUDENT-ID     PIC X(10).
    05  RR1-STUDENT-NAME   PIC X(30).
    05  RR1-SUBJECT-MARKS  OCCURS 6 TIMES PIC 9(3).
    05  RR1-TOTAL-MARKS    PIC 9(4).
    05  RR1-GRADE          PIC X.
    05  RR1-RANK           PIC 9(4).

FD  RANKED-RESULTS-SEM2.
01  RANKED-RECORD-SEM2.
    05  RR2-STUDENT-ID     PIC X(10).
    05  RR2-STUDENT-NAME   PIC X(30).
    05  RR2-SUBJECT-MARKS  OCCURS 6 TIMES PIC 9(3).
    05  RR2-TOTAL-MARKS    PIC 9(4).
    05  RR2-GRADE          PIC X.
    05  RR2-RANK           PIC 9(4).

WORKING-STORAGE SECTION.
01 DISP-TOTAL-MARK       PIC Z(4).
01 DISP-RANK             PIC Z(4).
01 WS-CHOICE             PIC 9.
01 WS-MANAGE-CHOICE      PIC 9.
01 WS-ID-SEARCH          PIC X(10).
01 WS-EOF                PIC X VALUE 'N'.
    88 END-OF-FILE       VALUE 'Y'.
    88 NOT-END-OF-FILE   VALUE 'N'.
01 WS-FOUND              PIC X VALUE 'N'.
    88 RECORD-FOUND      VALUE 'Y'.
    88 RECORD-NOT-FOUND  VALUE 'N'.

01 STUDENT-ID            PIC X(10).
01 STUDENT-NAME          PIC X(30).
01 MARKS                 OCCURS 6 TIMES PIC 999.
01 IDX                   PIC 9(4) VALUE 1.
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
LINKAGE SECTION.
01  LS-ID-SEARCH            PIC X(10).
01  LS-SEMESTER             PIC 9.

PROCEDURE DIVISION USING LS-ID-SEARCH, LS-SEMESTER.
SEARCH-RECORD.
    MOVE 'N' TO WS-FOUND
    MOVE 'N' TO WS-EOF

    EVALUATE LS-SEMESTER
        WHEN 1
            OPEN INPUT RANKED-RESULTS-SEM1
            READ RANKED-RESULTS-SEM1 AT END SET END-OF-FILE TO TRUE END-READ
            PERFORM UNTIL END-OF-FILE
                IF RR1-STUDENT-ID = LS-ID-SEARCH
                    MOVE RR1-SUBJECT-MARKS(1) TO DISP-MARK1
                    MOVE RR1-SUBJECT-MARKS(2) TO DISP-MARK2
                    MOVE RR1-SUBJECT-MARKS(3) TO DISP-MARK3
                    MOVE RR1-SUBJECT-MARKS(4) TO DISP-MARK4
                    MOVE RR1-SUBJECT-MARKS(5) TO DISP-MARK5
                    MOVE RR1-SUBJECT-MARKS(6) TO DISP-MARK6
                    MOVE RR1-TOTAL-MARKS TO DISP-TOTAL-MARK
                    MOVE RR1-RANK TO DISP-RANK

                    DISPLAY "SEARCH RESULT - SEMESTER I"
                    DISPLAY "==============================================================================================="
                    DISPLAY "StudentID   Name                            1101 1201 1401 1501 1601 1701  Total  Grade  Rank"
                    DISPLAY "==============================================================================================="
                    DISPLAY RR1-STUDENT-ID "  " RR1-STUDENT-NAME "  "
                        DISP-MARK1 "  " DISP-MARK2 "  " DISP-MARK3 "  "
                        DISP-MARK4 "  " DISP-MARK5 "  " DISP-MARK6
                        "   " DISP-TOTAL-MARK "     " RR1-GRADE "   " DISP-RANK

                    SET RECORD-FOUND TO TRUE
                    SET END-OF-FILE TO TRUE
                END-IF
                READ RANKED-RESULTS-SEM1 AT END SET END-OF-FILE TO TRUE END-READ
            END-PERFORM
            CLOSE RANKED-RESULTS-SEM1

        WHEN 2
            OPEN INPUT RANKED-RESULTS-SEM2
            READ RANKED-RESULTS-SEM2 AT END SET END-OF-FILE TO TRUE END-READ
            PERFORM UNTIL END-OF-FILE
                IF RR2-STUDENT-ID = LS-ID-SEARCH
                    MOVE RR2-SUBJECT-MARKS(1) TO DISP-MARK1
                    MOVE RR2-SUBJECT-MARKS(2) TO DISP-MARK2
                    MOVE RR2-SUBJECT-MARKS(3) TO DISP-MARK3
                    MOVE RR2-SUBJECT-MARKS(4) TO DISP-MARK4
                    MOVE RR2-SUBJECT-MARKS(5) TO DISP-MARK5
                    MOVE RR2-SUBJECT-MARKS(6) TO DISP-MARK6
                    MOVE RR2-TOTAL-MARKS TO DISP-TOTAL-MARK
                    MOVE RR2-RANK TO DISP-RANK

                    DISPLAY "SEARCH RESULT - SEMESTER II"
                    DISPLAY "==============================================================================================="
                    DISPLAY "StudentID   Name                            2101 2201 2401 2501 2601 2701  Total  Grade  Rank"
                    DISPLAY "==============================================================================================="
                    DISPLAY RR2-STUDENT-ID "  " RR2-STUDENT-NAME "  "
                        DISP-MARK1 "  " DISP-MARK2 "  " DISP-MARK3 "  "
                        DISP-MARK4 "  " DISP-MARK5 "  " DISP-MARK6
                        "   " DISP-TOTAL-MARK "     " RR2-GRADE "   " DISP-RANK

                    SET RECORD-FOUND TO TRUE
                    SET END-OF-FILE TO TRUE
                END-IF
                READ RANKED-RESULTS-SEM2 AT END SET END-OF-FILE TO TRUE END-READ
            END-PERFORM
            CLOSE RANKED-RESULTS-SEM2

        WHEN OTHER
            DISPLAY "Invalid semester"
    END-EVALUATE

    IF RECORD-NOT-FOUND
        DISPLAY ESC COLOR-RED"Student not found in Semester " LS-SEMESTER ESC COLOR-RESET
    END-IF.
