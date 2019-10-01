      PROGRAM MAIN
        IMPLICIT NONE

******COMMON_VARIABLES
        REAL x_min, x_max, x_step, y_min, y_max, y_step  
        COMMON x_min, x_max, x_step, y_min, y_max, y_step
            
******FUNCTIONS_DECLARATION
        LOGICAL input, tableFormation, inputChecker
        REAL round

******MAIN_BODY  
        IF (input() .AND. tableFormation()) THEN
          PRINT *, 'Completed.'
        ELSE
          PRINT *, 'Aborted.'
        END IF
        PAUSE
      END 

******FUNCTIONS_DEFINITION
      LOGICAL FUNCTION input()
        IMPLICIT NONE
        REAL x_min, x_max, x_step, y_min, y_max, y_step  
        COMMON x_min, x_max, x_step, y_min, y_max, y_step

        LOGICAL inputChecker
        
        OPEN(1, FILE = 'input.txt', STATUS = 'OLD', ERR = 1)
        READ(1, *, ERR = 2) x_min, x_max, x_step, 
     &                      y_min, y_max, y_step
        CLOSE(1)
        
        input = .TRUE.
        IF (inputChecker(x_min, x_max, x_step) .AND.
     &      inputChecker(y_min, y_max, y_step)) THEN 
          GOTO 5
        ELSE 
          GOTO 3
        END IF

    1   PRINT *, 'Error: file does not exist!' 
        GOTO 4
    2   PRINT *, 'Error: invalid input!' 
        GOTO 4
    3   PRINT *, 'Error: incorrect range or step size!'
    4   input = .FALSE.
    5 END

      LOGICAL FUNCTION inputChecker(min, max, step)
        IMPLICIT NONE
        REAL min, max, step, limit
        
        limit = 1E-38
        inputChecker = .FALSE.
        IF (min .LE. max .AND. 
     &      limit .LE. step .AND. min + step .LE. max) 
     &    inputChecker = .TRUE.
      END
      
      REAL FUNCTION func(x, y)
        IMPLICIT NONE
        REAL x, y, degToRad
        
        IF ((AMOD(y, 90.0) .NE. 0.0 .OR. y .EQ. 0.0) .AND. 
     &     (AMOD(x, 90.0) .NE. 0.0 .OR. x .EQ. 0.0)) THEN
          IF (AMOD(x, 180.0) .EQ. 0.0) THEN 
            func = 0
          ELSE
            func = tan(degToRad(x)) / cos(degToRad(y))
          END IF
        ELSE 
          func = ABS((y - y) / (y - y))
        END IF
      END

      LOGICAL FUNCTION tableFormation()
        IMPLICIT NONE
        REAL x_min, x_max, x_step, y_min, y_max, y_step  
        COMMON x_min, x_max, x_step, y_min, y_max, y_step

        REAL i, j, i_tmp, j_tmp, i_sum, j_sum, round, func
        
        OPEN(2, FILE = 'output.txt', STATUS = 'NEW', ERR = 1)
          WRITE(2, 103)
          i = x_min
          i_tmp = i
          DO WHILE (i .LT. x_max)
            i_sum = i + x_step
            IF (i_tmp .NE. round(i_sum)) THEN 
              WRITE(2, 105) i_tmp
              i_sum = round(i_sum)
              i_tmp = i_sum
            END IF 
             
            i = i_sum
          END DO
          WRITE(2, 105) x_max
          WRITE(2, 100)

          i = x_min
          i_tmp = i
          WRITE(2, 101)
          DO WHILE (i .LT. x_max)
            i_sum = i + x_step
            IF (i_tmp .NE. round(i_sum)) THEN 
              WRITE(2, 102)
              i_sum = round(i_sum)
              i_tmp = i_sum
            END IF
              
            i = i_sum
          END DO
          WRITE(2, 102)
            
          i = y_min
          i_tmp = i
          DO WHILE (i .LT. y_max)
            i_sum = i + y_step
            IF (i_tmp .NE. round(i_sum)) THEN          
              WRITE(2, 100)
              WRITE(2, 104) i_tmp
 
              j = x_min
              j_tmp = j
              DO WHILE (j .LT. x_max)
                j_sum = j + x_step
                IF (j_tmp .NE. round(j_sum)) THEN
                  WRITE(2, 105) func(j_tmp, i_tmp)
                  j_sum = round(j_sum)
                  j_tmp = j_sum
                END IF
                
                j = j_sum
              END DO
              WRITE(2, 105) func(x_max, i_tmp)
              
              i_sum = round(i_sum)
              i_tmp = i_sum
            END IF
            
              i = i_sum
          END DO
          WRITE(2, 100)
          WRITE(2, 104) y_max

          i = x_min
          i_tmp = i
          DO WHILE (i .LT. x_max)
            i_sum = i + x_step
            IF (i_tmp .NE. round(i_sum)) THEN
              WRITE(2, 105) func(i_tmp, y_max)
              i_sum = round(i_sum)
              i_tmp = i_sum
            END IF
            
            i = i_sum
          END DO
          WRITE(2, 105) func(x_max, y_max)  
        CLOSE(2)
        tableFormation = .TRUE.
        GOTO 2
        
    1   PRINT *, 'Error: file already exists!'
        tableFormation = .FALSE.
        GOTO 2  
  
  100   FORMAT(/1x\)
  101   FORMAT(13('-')\)
  102   FORMAT('|', 13('-')\)
  103   FORMAT('     y\x     '\)
  104   FORMAT(E12.4\, ' ')
  105   FORMAT('|', E12.4\, ' ')
    2 END 
       
      REAL FUNCTION degToRad(val)
        IMPLICIT NONE
        REAL val, pi
        pi = 3.14159265
        degToRad = val * pi / 180.0
      END
      
      REAL FUNCTION round(val)
        IMPLICIT NONE
        REAL val
        CHARACTER str_val*11
      
        WRITE(str_val, 100) val
        READ(str_val, *) round
        
  100   FORMAT(E11.4)  
      END
