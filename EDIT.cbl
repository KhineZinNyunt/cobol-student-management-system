       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDIT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE-SEM1 ASSIGN TO "student_sem1.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STUDENT-FILE-SEM2 ASSIGN TO "student_sem2.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO "temp.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

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
               10 MARK2-CST12101  PIC 999.
               10 MARK2-CST12201  PIC 999.
               10 MARK2-CST12401  PIC 999.
               10 MARK2-CST12501  PIC 999.
               10 MARK2-CST12601  PIC 999.
               10 MARK2-CST12701  PIC 999.
           05 TOTAL-MARKS2       PIC 999.
           05 GRADE2             PIC X(2).

       FD TEMP-FILE.
       01 TEMP-RECORD            PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-EOF                 PIC X VALUE 'N'.
           88 FILE-END           VALUE 'Y'.
           88 FILE-NOT-END       VALUE 'N'.

       01 WS-SEMESTER-CHAR      PIC X.
       01 WS-SEMESTER           PIC 9 VALUE 0.

       01 WS-STUDENT-ID          PIC X(10).
       01 WS-EDIT-OPTION         PIC X(10).
       01 WS-NEW-VALUE           PIC X(30).
       01 WS-LINE                PIC X(100).
       01 WS-FOUND               PIC X VALUE 'N'.

       01  WS-ID-VALID-FLAG       PIC X VALUE "N".
            88  ID-VALID            VALUE "Y".
            88  ID-NOT-VALID        VALUE "N".

       01  WS-NAME-VALID-FLAG     PIC X VALUE "N".
           88  NAME-VALID          VALUE "Y".
           88  NAME-NOT-VALID      VALUE "N".

       01  WS-MARK-VALID-FLAG     PIC X VALUE "N".
           88  MARK-VALID          VALUE "Y".
           88  MARK-NOT-VALID      VALUE "N".
       01  WS-MARK-NUMERIC        PIC 9(3).
       01  WS-TRIMMED-MARK      PIC X(3).
       01  WS-NUMERIC-ONLY      PIC X.
       01  WS-CHAR              PIC X.
       01  WS-INDEX             PIC 99.

       01  WS-ID-EXISTS-FLAG    PIC X VALUE "N".
           88  ID-EXISTS         VALUE "Y".
           88  ID-DOES-NOT-EXIST VALUE "N".
       LINKAGE SECTION.
       01 LS-STUDENT-ID         PIC X(10).
       01 LS-SEMESTER          PIC 9.
       01 LS-EDIT-RETURN-CODE  PIC X.

       PROCEDURE DIVISION USING LS-STUDENT-ID, LS-SEMESTER, LS-EDIT-RETURN-CODE.
       MAIN-PROCEDURE.
           MOVE LS-STUDENT-ID TO WS-STUDENT-ID
           MOVE LS-SEMESTER TO WS-SEMESTER
           EVALUATE WS-SEMESTER
               WHEN 1
                   PERFORM EDIT-SEM1
               WHEN 2
                   PERFORM EDIT-SEM2
               WHEN OTHER
                   DISPLAY "Invalid Semester."
                   MOVE 'N' TO LS-EDIT-RETURN-CODE
                   GOBACK
           END-EVALUATE
           MOVE 'Y' TO LS-EDIT-RETURN-CODE
           GOBACK.

       EDIT-SEM1.
           *> DISPLAY "Enter Student ID(eg. 5 digits-00001) to edit: ".
           *> ACCEPT WS-STUDENT-ID.
           OPEN INPUT STUDENT-FILE-SEM1
           OPEN OUTPUT TEMP-FILE
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-FOUND

           PERFORM UNTIL FILE-END
               READ STUDENT-FILE-SEM1
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF STUDENT-ID1 = WS-STUDENT-ID
                           DISPLAY "Student Found: " STUDENT-NAME1
                           MOVE 'Y' TO WS-FOUND
                           PERFORM HANDLE-EDIT-SEM1
                       END-IF
                       WRITE TEMP-RECORD FROM STUDENT-RECORD-SEM1
               END-READ
           END-PERFORM

           IF WS-FOUND NOT = 'Y'
               DISPLAY "Student Not Found!"
           END-IF
           CLOSE STUDENT-FILE-SEM1
           CLOSE TEMP-FILE

           CALL "CBL_DELETE_FILE" USING "student_sem1.dat"
           CALL "CBL_RENAME_FILE" USING "temp.dat" "student_sem1.dat".

       EDIT-SEM2.
           *> DISPLAY "Enter Student ID(eg. 5 digits-00001) to edit: ".
           *> ACCEPT WS-STUDENT-ID.
           OPEN INPUT STUDENT-FILE-SEM2
           OPEN OUTPUT TEMP-FILE
           MOVE 'N' TO WS-EOF
           MOVE 'N' TO WS-FOUND

           PERFORM UNTIL FILE-END
               READ STUDENT-FILE-SEM2
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       IF STUDENT-ID2 = WS-STUDENT-ID
                           DISPLAY "Student Found: " STUDENT-NAME2
                           MOVE 'Y' TO WS-FOUND
                           PERFORM HANDLE-EDIT-SEM2
                       END-IF
                       WRITE TEMP-RECORD FROM STUDENT-RECORD-SEM2
               END-READ
           END-PERFORM

           IF WS-FOUND NOT = 'Y'
               DISPLAY "Student Not Found!"
           END-IF
           CLOSE STUDENT-FILE-SEM2
           CLOSE TEMP-FILE

           CALL "CBL_DELETE_FILE" USING "student_sem2.dat"
           CALL "CBL_RENAME_FILE" USING "temp.dat" "student_sem2.dat".

       HANDLE-EDIT-SEM1.

           PERFORM UNTIL WS-EDIT-OPTION = "FINISH"
              DISPLAY "Which field do you want to edit?(NAME,SUB1 to 6)"
               DISPLAY "Or type FINISH to end editing."
               ACCEPT WS-EDIT-OPTION
               MOVE FUNCTION UPPER-CASE(WS-EDIT-OPTION)
               TO WS-EDIT-OPTION
               IF WS-EDIT-OPTION = "FINISH"
                DISPLAY "Finished editing."
               ELSE
                DISPLAY "Editing Started."


                EVALUATE WS-EDIT-OPTION



                   WHEN "NAME"
                    MOVE "N" TO WS-NAME-VALID-FLAG
                    PERFORM UNTIL NAME-VALID
                    DISPLAY "Enter new name (max 15 characters): "
                    ACCEPT WS-NEW-VALUE
                    IF FUNCTION LENGTH(FUNCTION TRIM(WS-NEW-VALUE))<= 15
                       MOVE FUNCTION TRIM(WS-NEW-VALUE) TO STUDENT-NAME1
                       MOVE "Y" TO WS-NAME-VALID-FLAG
                    ELSE
                    DISPLAY "Name must be 15 characters or fewer."

                    END-PERFORM

                   WHEN "SUB1"
                    MOVE "N" TO WS-MARK-VALID-FLAG
                    PERFORM UNTIL MARK-VALID
                     MOVE SPACES TO WS-NEW-VALUE
                     MOVE SPACES TO WS-TRIMMED-MARK
                     MOVE "Y" TO WS-NUMERIC-ONLY
                     MOVE 0 TO WS-MARK-NUMERIC
                     DISPLAY "Enter mark for CST-11101 [Basic Data Structures]: "
                     ACCEPT WS-NEW-VALUE

                     MOVE FUNCTION TRIM(WS-NEW-VALUE) TO WS-TRIMMED-MARK
                     MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK)
                      TO WS-MARK-NUMERIC
                      PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM
                      (WS-TRIMMED-MARK))
                       MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                       IF WS-CHAR < "0" OR WS-CHAR > "9"
                          MOVE "N" TO WS-NUMERIC-ONLY
                       END-IF
                      END-PERFORM
                      IF WS-NUMERIC-ONLY = "Y"

                      IF WS-MARK-NUMERIC>=0 AND WS-MARK-NUMERIC <= 100
                        MOVE WS-MARK-NUMERIC TO MARK1-CST11101
                        MOVE "Y" TO WS-MARK-VALID-FLAG
                       ELSE
                        DISPLAY "Mark must be between 0 and 100."
                       END-IF
                       ELSE
                      DISPLAY "Invalid input.Only numbers 0100."
                     END-IF

                    END-PERFORM

                   WHEN "SUB2"
                    MOVE "N" TO WS-MARK-VALID-FLAG
                    PERFORM UNTIL MARK-VALID
                     MOVE SPACES TO WS-NEW-VALUE
                      MOVE SPACES TO WS-TRIMMED-MARK
                     MOVE "Y" TO WS-NUMERIC-ONLY
                     MOVE 0 TO WS-MARK-NUMERIC
                     DISPLAY "Enter mark for CST-11201[Calculus I] : "
                     ACCEPT WS-NEW-VALUE
                     MOVE FUNCTION TRIM(WS-NEW-VALUE) TO WS-TRIMMED-MARK

                     MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK)
                      TO WS-MARK-NUMERIC
                      PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM
                      (WS-TRIMMED-MARK))
                       MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                       IF WS-CHAR < "0" OR WS-CHAR > "9"
                          MOVE "N" TO WS-NUMERIC-ONLY
                       END-IF
                      END-PERFORM
                      IF WS-NUMERIC-ONLY = "Y"

                      IF WS-MARK-NUMERIC>=0 AND WS-MARK-NUMERIC <= 100
                        MOVE WS-MARK-NUMERIC TO MARK1-CST11201
                        MOVE "Y" TO WS-MARK-VALID-FLAG
                       ELSE
                        DISPLAY "Mark must be between 0 and 100."
                       END-IF
                       ELSE
                      DISPLAY "Invalid input.Only numbers 0100."
                     END-IF

                    END-PERFORM

                    WHEN "SUB3"
                    MOVE "N" TO WS-MARK-VALID-FLAG
                    PERFORM UNTIL MARK-VALID
                     MOVE SPACES TO WS-NEW-VALUE
                      MOVE SPACES TO WS-TRIMMED-MARK
                     MOVE "Y" TO WS-NUMERIC-ONLY
                     MOVE 0 TO WS-MARK-NUMERIC
                     DISPLAY "Enter mark for CST-11401[Digital Fundamentals of Computer System]: "
                     ACCEPT WS-NEW-VALUE
                     MOVE FUNCTION TRIM(WS-NEW-VALUE) TO WS-TRIMMED-MARK

                     MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK)
                      TO WS-MARK-NUMERIC
                      PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM
                      (WS-TRIMMED-MARK))
                       MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                       IF WS-CHAR < "0" OR WS-CHAR > "9"
                          MOVE "N" TO WS-NUMERIC-ONLY
                       END-IF
                      END-PERFORM
                      IF WS-NUMERIC-ONLY = "Y"

                      IF WS-MARK-NUMERIC>=0 AND WS-MARK-NUMERIC <= 100
                        MOVE WS-MARK-NUMERIC TO MARK1-CST11401
                        MOVE "Y" TO WS-MARK-VALID-FLAG
                       ELSE
                        DISPLAY "Mark must be between 0 and 100."
                       END-IF
                       ELSE
                      DISPLAY "Invalid input.Only numbers 0100."
                     END-IF

                    END-PERFORM

                    WHEN "SUB4"
                    MOVE "N" TO WS-MARK-VALID-FLAG
                    PERFORM UNTIL MARK-VALID
                     MOVE SPACES TO WS-NEW-VALUE
                      MOVE SPACES TO WS-TRIMMED-MARK
                     MOVE "Y" TO WS-NUMERIC-ONLY
                     MOVE 0 TO WS-MARK-NUMERIC
                     DISPLAY "Enter mark forCST-11501 [English Language Proficiency I]: "
                     ACCEPT WS-NEW-VALUE
                     MOVE FUNCTION TRIM(WS-NEW-VALUE) TO WS-TRIMMED-MARK

                     MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK)
                      TO WS-MARK-NUMERIC
                      PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM
                      (WS-TRIMMED-MARK))
                       MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                       IF WS-CHAR < "0" OR WS-CHAR > "9"
                          MOVE "N" TO WS-NUMERIC-ONLY
                       END-IF
                      END-PERFORM
                      IF WS-NUMERIC-ONLY = "Y"

                      IF WS-MARK-NUMERIC>=0 AND WS-MARK-NUMERIC <= 100
                        MOVE WS-MARK-NUMERIC TO MARK1-CST11501
                        MOVE "Y" TO WS-MARK-VALID-FLAG
                       ELSE
                        DISPLAY "Mark must be between 0 and 100."
                       END-IF
                       ELSE
                      DISPLAY "Invalid input.Only numbers 0100."
                     END-IF

                    END-PERFORM

                    WHEN "SUB5"
                   MOVE "N" TO WS-MARK-VALID-FLAG
                    PERFORM UNTIL MARK-VALID
                     MOVE SPACES TO WS-NEW-VALUE
                     MOVE SPACES TO WS-TRIMMED-MARK
                     MOVE "Y" TO WS-NUMERIC-ONLY
                     MOVE 0 TO WS-MARK-NUMERIC
                     DISPLAY "Enter mark for CST-11601[Myanmar Literature]: "
                     ACCEPT WS-NEW-VALUE
                     MOVE FUNCTION TRIM(WS-NEW-VALUE) TO WS-TRIMMED-MARK

                     MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK)
                      TO WS-MARK-NUMERIC
                      PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM
                      (WS-TRIMMED-MARK))
                       MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                       IF WS-CHAR < "0" OR WS-CHAR > "9"
                          MOVE "N" TO WS-NUMERIC-ONLY
                       END-IF
                      END-PERFORM
                      IF WS-NUMERIC-ONLY = "Y"

                      IF WS-MARK-NUMERIC>=0 AND WS-MARK-NUMERIC <= 100
                        MOVE WS-MARK-NUMERIC TO MARK1-CST11601
                        MOVE "Y" TO WS-MARK-VALID-FLAG
                       ELSE
                        DISPLAY "Mark must be between 0 and 100."
                       END-IF
                       ELSE
                      DISPLAY "Invalid input.Only numbers 0100."
                     END-IF

                    END-PERFORM

                    WHEN "SUB6"
                    MOVE "N" TO WS-MARK-VALID-FLAG
                    PERFORM UNTIL MARK-VALID
                     MOVE SPACES TO WS-NEW-VALUE
                      MOVE SPACES TO WS-TRIMMED-MARK
                     MOVE "Y" TO WS-NUMERIC-ONLY
                     MOVE 0 TO WS-MARK-NUMERIC
                     DISPLAY "Enter mark forCST-11701 [Physics(Mechanics)]: "
                     ACCEPT WS-NEW-VALUE
                     MOVE FUNCTION TRIM(WS-NEW-VALUE) TO WS-TRIMMED-MARK

                     MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK)
                      TO WS-MARK-NUMERIC
                      PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM
                      (WS-TRIMMED-MARK))
                       MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                       IF WS-CHAR < "0" OR WS-CHAR > "9"
                          MOVE "N" TO WS-NUMERIC-ONLY
                       END-IF
                      END-PERFORM
                      IF WS-NUMERIC-ONLY = "Y"

                      IF WS-MARK-NUMERIC>=0 AND WS-MARK-NUMERIC <= 100
                        MOVE WS-MARK-NUMERIC TO MARK1-CST11701
                        MOVE "Y" TO WS-MARK-VALID-FLAG
                       ELSE
                        DISPLAY "Mark must be between 0 and 100."
                       END-IF
                       ELSE
                      DISPLAY "Invalid input.Only numbers 0100."
                     END-IF

                    END-PERFORM
                    WHEN OTHER
                     DISPLAY "Invalid Option. Try again."
                   END-EVALUATE
               END-IF
           END-PERFORM

           COMPUTE TOTAL-MARKS1 =
               MARK1-CST11101 + MARK1-CST11201 + MARK1-CST11401 +
               MARK1-CST11501 + MARK1-CST11601 + MARK1-CST11701

          EVALUATE TRUE
           WHEN TOTAL-MARKS1 >= 450 MOVE "A" TO GRADE1
           WHEN TOTAL-MARKS1 >= 400 MOVE "B" TO GRADE1
           WHEN TOTAL-MARKS1 >= 300 MOVE "C" TO GRADE1
           WHEN OTHER MOVE "F" TO GRADE1
       END-EVALUATE.

           DISPLAY "===== Edited Record ====="
       DISPLAY "ID: " STUDENT-ID1
       DISPLAY "Name: " STUDENT-NAME1
       DISPLAY "Marks: "
       DISPLAY "  CST11101-Basic Data Structures: " MARK1-CST11101
       DISPLAY "  CST11201-Calculus I: " MARK1-CST11201
       DISPLAY "  CST11401-Digital Fundamentals of Computer System: " MARK1-CST11401
       DISPLAY "  CST11501-English Language Proficiency I: " MARK1-CST11501
       DISPLAY "  CST11601-Myanmar Literature: " MARK1-CST11601
       DISPLAY "  CST11701-Physics(Mechanics): " MARK1-CST11701
       DISPLAY "Total: " TOTAL-MARKS1
       DISPLAY "Grade: " GRADE1
       DISPLAY "==========================".

       HANDLE-EDIT-SEM2.
           PERFORM UNTIL WS-EDIT-OPTION = "FINISH"
              DISPLAY "Which field do you want to edit?(NAME,SUB16)"
               DISPLAY "Or type FINISH to end editing."
               ACCEPT WS-EDIT-OPTION
               MOVE FUNCTION UPPER-CASE(WS-EDIT-OPTION)
               TO WS-EDIT-OPTION
               IF WS-EDIT-OPTION = "FINISH"
                DISPLAY "Finished editing."
               ELSE
                DISPLAY "Editing Started."


                EVALUATE WS-EDIT-OPTION
                  WHEN "ID"
                   MOVE "N" TO WS-ID-VALID-FLAG

                      PERFORM UNTIL ID-VALID
                       DISPLAY "Enter new value : "
                       ACCEPT WS-NEW-VALUE
                       IF FUNCTION LENGTH(FUNCTION TRIM(WS-NEW-VALUE))=5
                       AND FUNCTION TRIM(WS-NEW-VALUE) IS NUMERIC
                         MOVE FUNCTION TRIM(WS-NEW-VALUE) TO STUDENT-ID2
                         MOVE "Y" TO WS-ID-VALID-FLAG
                       ELSE
                         DISPLAY "ID must be exactly 5 digits."
                       END-IF
                      END-PERFORM


                   WHEN "NAME"
                    MOVE "N" TO WS-NAME-VALID-FLAG
                    PERFORM UNTIL NAME-VALID
                    DISPLAY "Enter new name (max 15 characters): "
                    ACCEPT WS-NEW-VALUE
                    IF FUNCTION LENGTH(FUNCTION TRIM(WS-NEW-VALUE))<= 15
                       MOVE FUNCTION TRIM(WS-NEW-VALUE) TO STUDENT-NAME2
                       MOVE "Y" TO WS-NAME-VALID-FLAG
                    ELSE
                    DISPLAY "Name must be 15 characters or fewer."
                    END-IF
                    END-PERFORM

                   WHEN "SUB1"

                    MOVE "N" TO WS-MARK-VALID-FLAG
                    PERFORM UNTIL MARK-VALID
                     MOVE SPACES TO WS-NEW-VALUE
                     MOVE SPACES TO WS-TRIMMED-MARK
                     MOVE "Y" TO WS-NUMERIC-ONLY
                     MOVE 0 TO WS-MARK-NUMERIC
                     DISPLAY "Enter mark for CST-12101[Programming in C++]:  "
                     ACCEPT WS-NEW-VALUE
                     MOVE FUNCTION TRIM(WS-NEW-VALUE) TO WS-TRIMMED-MARK

                      MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK)
                      TO WS-MARK-NUMERIC
                      PERFORM VARYING WS-INDEX FROM 1 BY 1
                      UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM
                      (WS-TRIMMED-MARK))
                       MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                       IF WS-CHAR < "0" OR WS-CHAR > "9"
                          MOVE "N" TO WS-NUMERIC-ONLY
                       END-IF
                      END-PERFORM
                      IF WS-NUMERIC-ONLY = "Y"
                       IF WS-MARK-NUMERIC>=0 AND WS-MARK-NUMERIC <= 100
                        MOVE WS-MARK-NUMERIC TO MARK2-CST12101
                        MOVE "Y" TO WS-MARK-VALID-FLAG
                       ELSE
                        DISPLAY "Mark must be between 0 and 100."
                       END-IF
                       ELSE
                      DISPLAY "Invalid input.Only numbers 0100."
                     END-IF

                    END-PERFORM

                   WHEN "SUB2"
                    MOVE "N" TO WS-MARK-VALID-FLAG
                    PERFORM UNTIL MARK-VALID
                     MOVE SPACES TO WS-NEW-VALUE
                     MOVE SPACES TO WS-TRIMMED-MARK
                     MOVE "Y" TO WS-NUMERIC-ONLY
                     MOVE 0 TO WS-MARK-NUMERIC
                     DISPLAY "Enter mark for CST-12201[Calculus II]: "
                     ACCEPT WS-NEW-VALUE
                     MOVE FUNCTION TRIM(WS-NEW-VALUE) TO WS-TRIMMED-MARK

                     MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK)
                      TO WS-MARK-NUMERIC
                      PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM
                      (WS-TRIMMED-MARK))
                       MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                       IF WS-CHAR < "0" OR WS-CHAR > "9"
                          MOVE "N" TO WS-NUMERIC-ONLY
                       END-IF
                      END-PERFORM
                      IF WS-NUMERIC-ONLY = "Y"

                      IF WS-MARK-NUMERIC>=0 AND WS-MARK-NUMERIC <= 100
                        MOVE WS-MARK-NUMERIC TO MARK2-CST12201
                        MOVE "Y" TO WS-MARK-VALID-FLAG
                       ELSE
                        DISPLAY "Mark must be between 0 and 100."
                       END-IF
                       ELSE
                      DISPLAY "Invalid input.Only numbers 0100."
                     END-IF

                    END-PERFORM

                    WHEN "SUB3"
                      MOVE "N" TO WS-MARK-VALID-FLAG
                    PERFORM UNTIL MARK-VALID
                     MOVE SPACES TO WS-NEW-VALUE
                     MOVE SPACES TO WS-TRIMMED-MARK
                     MOVE "Y" TO WS-NUMERIC-ONLY
                     MOVE 0 TO WS-MARK-NUMERIC
                     DISPLAY "Enter mark for CST-12401[Web Technology]: "
                     ACCEPT WS-NEW-VALUE
                     MOVE FUNCTION TRIM(WS-NEW-VALUE) TO WS-TRIMMED-MARK

                     MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK)
                      TO WS-MARK-NUMERIC
                      PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM
                      (WS-TRIMMED-MARK))
                       MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                       IF WS-CHAR < "0" OR WS-CHAR > "9"
                          MOVE "N" TO WS-NUMERIC-ONLY
                       END-IF
                      END-PERFORM
                      IF WS-NUMERIC-ONLY = "Y"

                      IF WS-MARK-NUMERIC>=0 AND WS-MARK-NUMERIC <= 100
                        MOVE WS-MARK-NUMERIC TO MARK2-CST12401
                        MOVE "Y" TO WS-MARK-VALID-FLAG
                       ELSE
                        DISPLAY "Mark must be between 0 and 100."
                       END-IF
                       ELSE
                      DISPLAY "Invalid input.Only numbers 0100."
                     END-IF

                    END-PERFORM

                    WHEN "SUB4"
                     MOVE "N" TO WS-MARK-VALID-FLAG
                    PERFORM UNTIL MARK-VALID
                     MOVE SPACES TO WS-NEW-VALUE
                      MOVE SPACES TO WS-TRIMMED-MARK
                     MOVE "Y" TO WS-NUMERIC-ONLY
                     MOVE 0 TO WS-MARK-NUMERIC
                     DISPLAY "Enter mark for CST-12501[English Language Proficiency II]: "
                     ACCEPT WS-NEW-VALUE
                     MOVE FUNCTION TRIM(WS-NEW-VALUE) TO WS-TRIMMED-MARK

                     MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK)
                      TO WS-MARK-NUMERIC
                      PERFORM VARYING WS-INDEX FROM 1 BY 1
                      UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM
                      (WS-TRIMMED-MARK))
                       MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                       IF WS-CHAR < "0" OR WS-CHAR > "9"
                          MOVE "N" TO WS-NUMERIC-ONLY
                       END-IF
                      END-PERFORM
                      IF WS-NUMERIC-ONLY = "Y"

                      IF WS-MARK-NUMERIC>=0 AND WS-MARK-NUMERIC <= 100
                        MOVE WS-MARK-NUMERIC TO MARK2-CST12501
                        MOVE "Y" TO WS-MARK-VALID-FLAG
                       ELSE
                        DISPLAY "Mark must be between 0 and 100."
                       END-IF
                       ELSE
                      DISPLAY "Invalid input.Only numbers 0100."
                     END-IF

                    END-PERFORM

                    WHEN "SUB5"
                      MOVE "N" TO WS-MARK-VALID-FLAG
                    PERFORM UNTIL MARK-VALID
                     MOVE SPACES TO WS-NEW-VALUE
                     MOVE SPACES TO WS-TRIMMED-MARK
                     MOVE "Y" TO WS-NUMERIC-ONLY
                     MOVE 0 TO WS-MARK-NUMERIC
                     DISPLAY "Enter mark for CST-12601[Myanmar Literature]: "
                     ACCEPT WS-NEW-VALUE
                     MOVE FUNCTION TRIM(WS-NEW-VALUE) TO WS-TRIMMED-MARK

                      MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK)
                      TO WS-MARK-NUMERIC
                      PERFORM VARYING WS-INDEX FROM 1 BY 1
                       UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM
                      (WS-TRIMMED-MARK))
                       MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                       IF WS-CHAR < "0" OR WS-CHAR > "9"
                          MOVE "N" TO WS-NUMERIC-ONLY
                       END-IF
                      END-PERFORM
                      IF WS-NUMERIC-ONLY = "Y"

                      IF WS-MARK-NUMERIC>=0 AND WS-MARK-NUMERIC <= 100
                        MOVE WS-MARK-NUMERIC TO MARK2-CST12601
                        MOVE "Y" TO WS-MARK-VALID-FLAG
                       ELSE
                        DISPLAY "Mark must be between 0 and 100."
                       END-IF
                       ELSE
                      DISPLAY "Invalid input.Only numbers 0100."
                     END-IF

                    END-PERFORM

                    WHEN "SUB6"
                    MOVE "N" TO WS-MARK-VALID-FLAG
                    PERFORM UNTIL MARK-VALID
                     MOVE SPACES TO WS-NEW-VALUE
                      MOVE SPACES TO WS-TRIMMED-MARK
                     MOVE "Y" TO WS-NUMERIC-ONLY
                     MOVE 0 TO WS-MARK-NUMERIC
                     DISPLAY "Enter mark for CST-12701[Physics (Electromagnetism)]: "
                     ACCEPT WS-NEW-VALUE
                     MOVE FUNCTION TRIM(WS-NEW-VALUE) TO WS-TRIMMED-MARK

                      MOVE FUNCTION NUMVAL(WS-TRIMMED-MARK)
                      TO WS-MARK-NUMERIC
                      PERFORM VARYING WS-INDEX FROM 1 BY 1
                      UNTIL WS-INDEX > FUNCTION LENGTH(FUNCTION TRIM
                      (WS-TRIMMED-MARK))
                       MOVE WS-TRIMMED-MARK(WS-INDEX:1) TO WS-CHAR
                       IF WS-CHAR < "0" OR WS-CHAR > "9"
                          MOVE "N" TO WS-NUMERIC-ONLY
                       END-IF
                      END-PERFORM
                      IF WS-NUMERIC-ONLY = "Y"

                      IF WS-MARK-NUMERIC>=0 AND WS-MARK-NUMERIC <= 100
                        MOVE WS-MARK-NUMERIC TO MARK2-CST12701
                        MOVE "Y" TO WS-MARK-VALID-FLAG
                       ELSE
                        DISPLAY "Mark must be between 0 and 100."
                       END-IF
                       ELSE
                      DISPLAY "Invalid input.Only numbers 0100."
                     END-IF

                    END-PERFORM

                    WHEN OTHER
                     DISPLAY "Invalid Option. Try again."
                   END-EVALUATE
               END-IF
           END-PERFORM

           COMPUTE TOTAL-MARKS2 =
               MARK2-CST12101 + MARK2-CST12201 + MARK2-CST12401 +
               MARK2-CST12501 + MARK2-CST12601 + MARK2-CST12701

       EVALUATE TRUE
           WHEN TOTAL-MARKS2 >= 450 MOVE "A" TO GRADE2
           WHEN TOTAL-MARKS2 >= 400 MOVE "B" TO GRADE2
           WHEN TOTAL-MARKS2 >= 300 MOVE "C" TO GRADE2
           WHEN OTHER MOVE "F" TO GRADE2
       END-EVALUATE.


       DISPLAY "===== Edited Record ====="
       DISPLAY "ID: " STUDENT-ID2
       DISPLAY "Name: " STUDENT-NAME2
       DISPLAY "Marks: "
       DISPLAY "  CST12101-Programming in C++: " MARK2-CST12101
       DISPLAY "  CST12201-CalculusII: " MARK2-CST12201
       DISPLAY "  CST12401-Web Technology: " MARK2-CST12401
       DISPLAY "  CST12501-English Language Proficiency II: " MARK2-CST12501
       DISPLAY "  CST12601-Myanmar Literature: " MARK2-CST12601
       DISPLAY "  CST12701-Physics (Electromagnetism): " MARK2-CST12701
       DISPLAY "Total: " TOTAL-MARKS2
       DISPLAY "Grade: " GRADE2
       DISPLAY "==========================".

       END-EDIT.
           STOP RUN.
