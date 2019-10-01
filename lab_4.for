      PROGRAM MAIN
        IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      
        COMMON /partition/ lines
        COMMON /interval/ a, b, step 
    
        DIMENSION memory(100000000) ! 100 000 000
        
        CALL readData()
        CALL makePartition(memory(1))
        
        result   = GaussianQuadrature_3(memory(1))
*        result_2 = GaussianQuadrature_4(memory(1))
                
        PRINT *, 'Gauss_3: ', result
*        PRINT *, 'Gauss_4: ', result_2

        
        PAUSE
      END
      
******FUNCTIONS_DEFINITION
      DOUBLE PRECISION FUNCTION fun(x)
        IMPLICIT DOUBLE PRECISION(A-H, O-Z)
        !fun = 5*x**4
        !fun = 6*x**5
        !fun = 7*x**6
        !fun = 8*x**7
        !fun = 9*x**8
        !fun = 10*x**9
        fun = 11*x**10
      END
      
      SUBROUTINE readData()
        IMPLICIT DOUBLE PRECISION(A-H, O-Z)
        COMMON /partition/ lines
        COMMON /interval/ a, b, step 
        
        OPEN(1, FILE = 'input.txt', STATUS = 'OLD', ERR = 1)
        READ(1, *) a, b, lines
        CLOSE(1)
        
        IF (b .LT. a .OR. a .EQ. b) GOTO 2
        IF (lines .EQ. 0.0) GOTO 3
        
        step = (b - a) / lines
      
        PRINT *, 'Input data reading success.'
        RETURN  
    1   PAUSE 'Input error: "input.txt" file does not exist!'
        STOP 
    2   PAUSE 'Input error: invalid segment of integration!'
        STOP
    3   PAUSE 'Input error: zero step!'
        STOP
      END
      
      SUBROUTINE makePartition(partition)
        IMPLICIT DOUBLE PRECISION(A-H, O-Z)
        COMMON /partition/ lines
        COMMON /interval/ a, b, step 
        
        DIMENSION partition(*)
        
        DO i = 0, lines
          partition(i + 1) = a + i * step
        END DO
        IF (partition(lines) .NE. b) THEN
          lines = lines + 1
          partition(lines) = b
        END IF
        
        PRINT *, 'Partition created.'
        RETURN  
      END 

      DOUBLE PRECISION FUNCTION GaussianQuadrature_3(partition)
        IMPLICIT DOUBLE PRECISION(A-H, O-Z)
        COMMON /partition/ lines
        COMMON /interval/ a, b, step 
        
        DIMENSION partition(*), X(3), Q(3)
        
        X(1) = 0.77459666924148D0
        X(2) = 0.0D0
        X(3) = -0.77459666924148D0
        
        Q(1) = 0.55555555555555D0
        Q(2) = 0.88888888888888D0
        Q(3) = 0.55555555555555D0
        
        sum = 0.0D0
        DO i = 1, lines - 1
          segment = partition(i + 1) - partition(i)
          shift = partition(i + 1) + partition(i)

          tmp_sum = 0.0D0
          DO j = 1, 3
            value = (X(j) * segment + shift) / 2
            tmp_sum = tmp_sum + Q(j) * fun(value)
          END DO
          tmp_sum = tmp_sum * segment / 2
   
          sum = sum + tmp_sum
        END DO
        GaussianQuadrature_3 = sum
      END 

      DOUBLE PRECISION FUNCTION GaussianQuadrature_4(partition)
        IMPLICIT DOUBLE PRECISION(A-H, O-Z)
        COMMON /partition/ lines
        COMMON /interval/ a, b, step 
        
        DIMENSION partition(*), X(2), Q(2)
        
        X(1) = 0.861136311594052D0
        X(2) = 0.339981043584856D0
		
        Q(1) = 0.347854845137453D0
        Q(2) = 0.652145154862546D0      
  
        sum = 0.0D0
        DO i = 1, lines - 1
          segment = partition(i + 1) - partition(i)
          shift = partition(i + 1) + partition(i)
          
          tmp_sum = 0.0D0
          DO j = 1, 2
            value   = (-X(j) * segment + shift) / 2
            value_2 = ( X(j) * segment + shift) / 2
            tmp_sum = tmp_sum + Q(j) * (fun(value) + fun(value_2))
          END DO
          tmp_sum = tmp_sum * segment / 2
          
          sum = sum + tmp_sum
        END DO

        GaussianQuadrature_4 = sum
      END