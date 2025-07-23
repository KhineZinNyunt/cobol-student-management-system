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
       01 DISP-MARK1            PIC Z(3).
       01 DISP-MARK2            PIC Z(3).
       01 DISP-MARK3            PIC Z(3).
       01 DISP-MARK4            PIC Z(3).
       01 DISP-MARK5            PIC Z(3).
       01 DISP-MARK6            PIC Z(3).
       01 DISP-TOTAL-MARK       PIC Z(4).
       01 DISP-RANK             PIC Z(4).
       01 EOF-SEM1              PIC X VALUE 'N'.
           88 EOF-SEM1-YES      VALUE 'Y'.
           88 EOF-SEM1-NO       VALUE 'N'.
       01 EOF-SEM2              PIC X VALUE 'N'.
           88 EOF-SEM2-YES      VALUE 'Y'.
           88 EOF-SEM2-NO       VALUE 'N'.
       01 FOUND-IN-SEM1         PIC X VALUE 'N'.
           88 STUDENT-IN-SEM1   VALUE 'Y'.
       01 FOUND-IN-SEM2         PIC X VALUE 'N'.
           88 STUDENT-IN-SEM2   VALUE 'Y'.
       01 WS-SEMESTER-CHOICE    PIC 9.
       01 WS-ID-SEARCH          PIC X(10).

       LINKAGE SECTION.
       01 LS-ID-SEARCH          PIC X(10).

       PROCEDURE DIVISION USING LS-ID-SEARCH.

       MAIN-SEARCH.

           MOVE LS-ID-SEARCH TO WS-ID-SEARCH
           MOVE 'N' TO FOUND-IN-SEM1
           MOVE 'N' TO FOUND-IN-SEM2

           OPEN INPUT RANKED-RESULTS-SEM1
           MOVE 'N' TO EOF-SEM1
           PERFORM UNTIL EOF-SEM1-YES
               READ RANKED-RESULTS-SEM1
                   AT END MOVE 'Y' TO EOF-SEM1
                   NOT AT END
                       IF RR1-STUDENT-ID = WS-ID-SEARCH
                           SET STUDENT-IN-SEM1 TO TRUE
                           MOVE 'Y' TO EOF-SEM1
                       END-IF
               END-READ
           END-PERFORM
           CLOSE RANKED-RESULTS-SEM1

           OPEN INPUT RANKED-RESULTS-SEM2
           MOVE 'N' TO EOF-SEM2
           PERFORM UNTIL EOF-SEM2-YES
               READ RANKED-RESULTS-SEM2
                   AT END MOVE 'Y' TO EOF-SEM2
                   NOT AT END
                       IF RR2-STUDENT-ID = WS-ID-SEARCH
                           SET STUDENT-IN-SEM2 TO TRUE
                           MOVE 'Y' TO EOF-SEM2
                       END-IF
               END-READ
           END-PERFORM
           CLOSE RANKED-RESULTS-SEM2

           IF STUDENT-IN-SEM1 OR STUDENT-IN-SEM2
               DISPLAY "Student found in semester(s): "
               IF STUDENT-IN-SEM1
                   DISPLAY "1 , "with no ADVANCING
               END-IF
               IF STUDENT-IN-SEM2
                   DISPLAY "2 "
               END-IF
               DISPLAY "Select semester to view (1 or 2): "
               ACCEPT WS-SEMESTER-CHOICE

               EVALUATE WS-SEMESTER-CHOICE
                   WHEN 1
                       IF STUDENT-IN-SEM1
                           PERFORM DISPLAY-STUDENT-SEM1
                       ELSE
                           DISPLAY "Student not found in Semester 1."
                       END-IF
                   WHEN 2
                       IF STUDENT-IN-SEM2
                           PERFORM DISPLAY-STUDENT-SEM2
                       ELSE
                           DISPLAY "Student not found in Semester 2."
                       END-IF
                   WHEN OTHER
                       DISPLAY "Invalid semester selected."
               END-EVALUATE
           ELSE
               DISPLAY "Student not found in any semester."
           END-IF

           GOBACK.

       DISPLAY-STUDENT-SEM1.
           MOVE 'N' TO EOF-SEM1
           OPEN INPUT RANKED-RESULTS-SEM1
           PERFORM UNTIL EOF-SEM1-YES
               READ RANKED-RESULTS-SEM1
                   AT END MOVE 'Y' TO EOF-SEM1
                   NOT AT END
                       IF RR1-STUDENT-ID = WS-ID-SEARCH
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
                           DISPLAY "-----------------------------------------------------------------------------------------------"
                           DISPLAY "SUBJECT CODE EXPLANATION (SEMESTER I)"
                           DISPLAY "11101 - Basic Data Structures"
                           DISPLAY "11201 - Calculus I"
                           DISPLAY "11401 - Digital Fundamentals of Computer System"
                           DISPLAY "11501 - English Language Proficiency I"
                           DISPLAY "11601 - Myanmar Literature"
                           DISPLAY "11701 - Physics (Mechanics)"


                           MOVE 'Y' TO EOF-SEM1
                       END-IF
               END-READ
           END-PERFORM
           CLOSE RANKED-RESULTS-SEM1.

       DISPLAY-STUDENT-SEM2.
           MOVE 'N' TO EOF-SEM2
           OPEN INPUT RANKED-RESULTS-SEM2
           PERFORM UNTIL EOF-SEM2-YES
               READ RANKED-RESULTS-SEM2
                   AT END MOVE 'Y' TO EOF-SEM2
                   NOT AT END
                       IF RR2-STUDENT-ID = WS-ID-SEARCH
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
                           DISPLAY "-----------------------------------------------------------------------------------------------"
                           DISPLAY "SUBJECT CODE EXPLANATION (SEMESTER II)"
                           DISPLAY "12102 - Programming in C++"
                           DISPLAY "12201 - Calculus II"
                           DISPLAY "12301 - Web Technology"
                           DISPLAY "12502 - English Language Proficiency II"
                           DISPLAY "12601 - Myanmar Literature"
                           DISPLAY "12702 - Physics (Electromagnetism)"


                           MOVE 'Y' TO EOF-SEM2
                       END-IF
               END-READ
           END-PERFORM
           CLOSE RANKED-RESULTS-SEM2.
