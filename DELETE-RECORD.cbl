IDENTIFICATION DIVISION.
       PROGRAM-ID. DELETE-RECORD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE-SEM1 ASSIGN TO "student_sem1.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE-SEM1 ASSIGN TO "temp_sem1.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STUDENT-FILE-SEM2 ASSIGN TO "student_sem2.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE-SEM2 ASSIGN TO "temp_sem2.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  STUDENT-FILE-SEM1.
       01  STUDENT-RECORD-SEM1.
           05  STUDENT-ID1            PIC X(10).
           05  STUDENT-NAME1          PIC X(30).
           05  STUDENT-SUBJECTS1.
               10  MARK1-CST11101     PIC 9(3).
               10  MARK1-CST11201     PIC 9(3).
               10  MARK1-CST11401     PIC 9(3).
               10  MARK1-CST11501     PIC 9(3).
               10  MARK1-CST11601     PIC 9(3).
               10  MARK1-CST11701     PIC 9(3).
           05  TOTAL-MARKS1           PIC 9(3).
           05  GRADE1                 PIC X(2).

       FD  TEMP-FILE-SEM1.
       01  TEMP-RECORD-SEM1.
           05  TEMP-ID1            PIC X(10).
           05  TEMP-NAME1          PIC X(30).
           05  TEMP-SUBJECTS1.
               10  TEMP-MARK1-CST11101     PIC 9(3).
               10  TEMP-MARK1-CST11201     PIC 9(3).
               10  TEMP-MARK1-CST11401     PIC 9(3).
               10  TEMP-MARK1-CST11501     PIC 9(3).
               10  TEMP-MARK1-CST11601     PIC 9(3).
               10  TEMP-MARK1-CST11701     PIC 9(3).
           05  TEMP-TOTAL-MARKS1   PIC 9(3).
           05  TEMP-GRADE1         PIC X(2).

       FD  STUDENT-FILE-SEM2.
       01  STUDENT-RECORD-SEM2.
           05  STUDENT-ID2            PIC X(10).
           05  STUDENT-NAME2          PIC X(30).
           05  STUDENT-SUBJECTS2.
               10  MARK2-CST12101     PIC 9(3).
               10  MARK2-CST12201     PIC 9(3).
               10  MARK2-CST12401     PIC 9(3).
               10  MARK2-CST12501     PIC 9(3).
               10  MARK2-CST12601     PIC 9(3).
               10  MARK2-CST12701     PIC 9(3).
           05  TOTAL-MARKS2           PIC 9(3).
           05  GRADE2                 PIC X(2).

       FD  TEMP-FILE-SEM2.
       01  TEMP-RECORD-SEM2.
           05  TEMP-ID2            PIC X(10).
           05  TEMP-NAME2          PIC X(30).
           05  TEMP-SUBJECTS2.
               10  TEMP-MARK2-CST12101     PIC 9(3).
               10  TEMP-MARK2-CST12201     PIC 9(3).
               10  TEMP-MARK2-CST12401     PIC 9(3).
               10  TEMP-MARK2-CST12501     PIC 9(3).
               10  TEMP-MARK2-CST12601     PIC 9(3).
               10  TEMP-MARK2-CST12701     PIC 9(3).
           05  TEMP-TOTAL-MARKS2   PIC 9(3).
           05  TEMP-GRADE2         PIC X(2).

       WORKING-STORAGE SECTION.
       01  WS-EOF                  PIC X VALUE 'N'.
           88  FILE-END            VALUE 'Y'.
           88  FILE-NOT-END        VALUE 'N'.
       01  RECORD-DELETED          PIC X VALUE 'N'.

       LINKAGE SECTION.
       01  LS-ID-DELETE            PIC X(10).
       01  LS-SEMESTER             PIC 9.
       01  LS-DELETED              PIC X.

       PROCEDURE DIVISION USING LS-ID-DELETE, LS-SEMESTER, LS-DELETED.

       MAIN-LOGIC.
           EVALUATE LS-SEMESTER
               WHEN 1
                   PERFORM DELETE-SEM1-RECORD
               WHEN 2
                   PERFORM DELETE-SEM2-RECORD
               WHEN OTHER
                   DISPLAY "Invalid semester"
           END-EVALUATE

           MOVE RECORD-DELETED TO LS-DELETED
           GOBACK.

       DELETE-SEM1-RECORD.
           OPEN INPUT STUDENT-FILE-SEM1
           OPEN OUTPUT TEMP-FILE-SEM1

           MOVE 'N' TO WS-EOF
           MOVE 'N' TO RECORD-DELETED

           PERFORM UNTIL FILE-END
               READ STUDENT-FILE-SEM1
                   AT END
                       SET FILE-END TO TRUE
                   NOT AT END
                       IF STUDENT-ID1 NOT = LS-ID-DELETE
                           MOVE STUDENT-RECORD-SEM1 TO TEMP-RECORD-SEM1
                           WRITE TEMP-RECORD-SEM1
                       ELSE
                           MOVE 'Y' TO RECORD-DELETED
                       END-IF
               END-READ
           END-PERFORM

           CLOSE STUDENT-FILE-SEM1
           CLOSE TEMP-FILE-SEM1

           IF RECORD-DELETED = 'Y'
               CALL "CBL_DELETE_FILE" USING "student_sem1.dat"
               CALL "CBL_RENAME_FILE" USING "temp_sem1.dat", "student_sem1.dat"
           ELSE
               CALL "CBL_DELETE_FILE" USING "temp_sem1.dat"
           END-IF.

       DELETE-SEM2-RECORD.
           OPEN INPUT STUDENT-FILE-SEM2
           OPEN OUTPUT TEMP-FILE-SEM2

           MOVE 'N' TO WS-EOF
           MOVE 'N' TO RECORD-DELETED

           PERFORM UNTIL FILE-END
               READ STUDENT-FILE-SEM2
                   AT END
                       SET FILE-END TO TRUE
                   NOT AT END
                       IF STUDENT-ID2 NOT = LS-ID-DELETE
                           MOVE STUDENT-RECORD-SEM2 TO TEMP-RECORD-SEM2
                           WRITE TEMP-RECORD-SEM2
                       ELSE
                           MOVE 'Y' TO RECORD-DELETED
                       END-IF
               END-READ
           END-PERFORM

           CLOSE STUDENT-FILE-SEM2
           CLOSE TEMP-FILE-SEM2

           IF RECORD-DELETED = 'Y'
               CALL "CBL_DELETE_FILE" USING "student_sem2.dat"
               CALL "CBL_RENAME_FILE" USING "temp_sem2.dat", "student_sem2.dat"
           ELSE
               CALL "CBL_DELETE_FILE" USING "temp_sem2.dat"
           END-IF.
