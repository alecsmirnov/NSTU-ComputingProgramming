      PROGRAM MAIN
        COMMON /size/ N, nodes 
        DIMENSION mem(1000000)
  
        CALL readSizes(1000000)
        
        CALL readMatrix(mem(1), mem(N + 2), mem(2 * N + 3),
     &       mem(2 * N + nodes + 4)) 
     
        CALL readVector(mem(2 * N + 2 * nodes + 5))   
           
        CALL multiplication(mem(1), mem(N + 2), mem(2 * N + 3),
     &       mem(2 * N + nodes + 4), mem(2 * N + 2 * nodes + 5),
     &       mem(3 * N + 2 * nodes + 6))
           
        CALL matrixReconstruct(mem(1), mem(N + 2), mem(2*N+3),
     &       mem(2 * N + nodes + 4) , mem(4 * N + 2 * nodes + 7))

        PAUSE
      END

******FUNCTIONS_DEFINITION
      SUBROUTINE readSizes(mem_size)
        COMMON /size/ N, nodes 
        
        OPEN(1, FILE = 'matrix_size.txt', STATUS = 'OLD', ERR=1)
        READ(1, *) N
        CLOSE(1)
        
        OPEN(1, FILE = 'vector_size.txt', STATUS = 'OLD', ERR=2)
        READ(1, *) M
        CLOSE(1)
        
        IF (N .NE. M) GOTO 3
        OPEN(1, FILE = 'matrix_iA.txt', STATUS = 'OLD', ERR = 4)
        READ(1, *) (values, i = 1, N + 1)
        nodes = INT(values) – 1
        CLOSE(1)  
          
        IF (mem_size .LT. (4 * N + 2 * nodes + 2 + 100)) GOTO 5
        PRINT *, 'Sizes reading success' 
        RETURN

    1   PAUSE *, 'Error: matrix_size file does not exist!'
        STOP 
    2   PAUSE *, 'Error: vector_size file does not exist!'
        STOP
    3   PAUSE *, 'Error: invalid size of matrix or vector!'
        STOP
    4   PAUSE *, 'Error: nodes info file (matrix_iA) does not exist!'
        STOP
    5   PAUSE *, 'Error: out of memmory!'
        STOP
      END
      
      SUBROUTINE readMatrix(iA, Di, Al, Au)
        COMMON /size/ N, nodes 
        
        DIMENSION iA(*), Di(*), Al(*), Au(*)
        
        OPEN(1, FILE = 'matrix_iA.txt', STATUS = 'OLD', ERR = 1)
        READ(1, *) (iA(i), i = 1, N + 1)
        CLOSE(1)
        OPEN(1, FILE = 'matrix_Di.txt', STATUS = 'OLD', ERR = 2)
        READ(1, *) (Di(i), i = 1, N)
        CLOSE(1)
        OPEN(1, FILE = 'matrix_Al.txt', STATUS = 'OLD', ERR = 3)
        READ(1, *) (Al(i), i = 1, nodes)
        CLOSE(1)
        OPEN(1, FILE = 'matrix_Au.txt', STATUS = 'OLD', ERR = 4)
        READ(1, *) (Au(i), i = 1, nodes)
        CLOSE(1)
        PRINT *, 'Matrix reading success'
        RETURN 
        
    1   PAUSE *, 'Error: matrix_iA file does not exist!'
        STOP  
    2   PAUSE *, 'Error: matrix_Di file does not exist!'
        STOP 
    3   PAUSE *, 'Error: matrix_Al file does not exist!'
        STOP 
    4   PAUSE *, 'Error: matrix_Au file does not exist!'
        STOP 
      END      

      SUBROUTINE readVector(V)
        COMMON /size/ N, nodes 
        
        DIMENSION V(*)
        
        OPEN(1, FILE = 'vector.txt', STATUS = 'OLD', ERR = 1)
        READ(1, *) (V(i), i = 1, N)
        CLOSE(1)
        PRINT *, 'Vector reading success'
        RETURN 
        
    1   PAUSE *, 'Error: vector file does not exist!'
        STOP
      END
            
      SUBROUTINE matrixReconstruct(iA, Di, Al, Au, A)
        COMMON /size/ N, nodes 
        
        DIMENSION iA(*), Di(*), Al(*), Au(*), A(N, N)
       
        OPEN(1, FILE = 'matrix_rec.txt')
        DO i = 1, N
          A(i, i) = Di(i)
        END DO
        
        DO i = 1, N
          i_start = i - iA(i + 1)
          DO j = iA(i), iA(i + 1) - 1
            k = i_start + j 
            A(k, i) = Au(j)
            A(i, k) = Al(j)
          END DO
        END DO  
 
        DO i = 1, N
          WRITE(1, 100) (A(i, j), j = 1, N)
          WRITE(1, 200)
        END DO          
        CLOSE(1)
        PRINT *, 'Matrix reconstruction success'
        RETURN
        
  100   FORMAT(E10.4\, ' ')        
  200   FORMAT(/1x\)
      END
      
      SUBROUTINE multiplication(iA, Di, Al, Au, V, V_res)
        COMMON /size/ N, nodes 
        
        DIMENSION iA(*), Di(*), Al(*), Au(*), V(*), V_res(*)
        
        DO i = 1, N
          V_res(i) = Di(i) * V(i)
        END DO
        
        DO i = 1, N
          i_start = i - iA(i + 1)
          DO j = iA(i), iA(i + 1) - 1
            k = i_start + j 
            V_res(k) = V_res(k) + Au(j) * V(i)
            V_res(i) = V_res(i) + Al(j) * V(k)
          END DO
        END DO  
        
        CALL multResultWrite(V_res)
      END
      
      SUBROUTINE multResultWrite(V_res)
        COMMON /size/ N, nodes 
        
        DIMENSION V_res(*)
        
        OPEN(1, FILE = 'mult_result.txt')
        WRITE(1, 100) (V_res(i), i = 1, N)
        CLOSE(1)
        PRINT *, 'Multiplication success'
        RETURN
        
  100   FORMAT(E10.4\, ' ')        
  200   FORMAT(/1x\)
      END

******DIRECT_ANALOGUE_FUNCTIONS
      SUBROUTINE readSizesDir(mem_size)
        COMMON /size/ N, nodes 
        
        OPEN(1, FILE = 'matrix_size.txt', STATUS = 'OLD', ERR=1)
        READ(1, *) N
        CLOSE(1)
        
        OPEN(1, FILE = 'vector_size.txt', STATUS = 'OLD', ERR=2)
        READ(1, *) M
        CLOSE(1)
        
        IF (N .NE. M) GOTO 3
        OPEN(1,FILE='matrix_iA.dat',ACCESS='DIRECT',RECL=4,ERR=4)
        READ(1, REC = N + 1) values
        nodes = INT(values) - 1
        CLOSE(1)  
          
        IF (mem_size .LT. (4 * N + 2 * nodes + 2 + 100)) GOTO 5
        PRINT *, 'Sizes reading success' 
        RETURN

    1   PAUSE *, 'Error: matrix_size file does not exist!'
        STOP 
    2   PAUSE *, 'Error: vector_size file does not exist!'
        STOP
    3   PAUSE *, 'Error: invalid size of matrix or vector!'
        STOP
    4   PAUSE *, 'Error: nodes info file (matrix_iA) does not exist!'
        STOP
    5   PAUSE *, 'Error: out of memmory!'
        STOP
      END

      SUBROUTINE readMatrixDir(iA, Di, Al, Au)
        COMMON /size/ N, nodes 
        
        DIMENSION iA(*), Di(*), Al(*), Au(*)
        
        OPEN(1,FILE='matrix_iA.dat',ACCESS='DIRECT',RECL=4,ERR=1)
        DO i = 1, N + 1
          READ(1, REC = i) iA(i)
        END DO
        CLOSE(1)
        OPEN(1,FILE='matrix_Di.dat',ACCESS='DIRECT',RECL=4,ERR=2)
        DO i = 1, N
          READ(1, REC = i) Di(i)
        END DO
        CLOSE(1)
        OPEN(1,FILE='matrix_Al.dat',ACCESS='DIRECT',RECL=4,ERR=3)
        OPEN(2,FILE='matrix_Au.dat',ACCESS='DIRECT',RECL=4,ERR=4)
        DO i = 1, nodes
          READ(1, REC = i) Al(i)
          READ(2, REC = i) Au(i)
        END DO
        CLOSE(1)
        CLOSE(2)
        PRINT *, 'Matrix reading success'
        RETURN 
        
    1   PAUSE *, 'Error: matrix_iA file does not exist!'
        STOP  
    2   PAUSE *, 'Error: matrix_Di file does not exist!'
        STOP 
    3   PAUSE *, 'Error: matrix_Al file does not exist!'
        STOP 
    4   PAUSE *, 'Error: matrix_Au file does not exist!'
        STOP 
      END

      SUBROUTINE readVectorDir(V)
        COMMON /size/ N, nodes 
        
        DIMENSION V(*)
        
        OPEN(1,FILE='vector.dat',ACCESS='DIRECT',RECL=4,ERR=1)
        DO i = 1, N
          READ(1, REC = i) V(i)
        END DO
        CLOSE(1)
        PRINT *, 'Vector reading success'
        RETURN 
        
    1   PAUSE *, 'Error: vector file does not exist!'
        STOP
      END

******FILE_CONVERSION_FUNCTIONS
      SUBROUTINE sequentialToDirect(iA, Di, Al, Au, V)
        COMMON /size/ N, nodes 
        DIMENSION iA(*), Di(*), Al(*), Au(*)
        
        CALL matrixSTD(iA, Di, Al, Au)
        CALL vectorSTD(V)
      END
      
      SUBROUTINE directToSequential() 
        CALL matrixDTS()
        CALL vectorDTS()
      END
      
      SUBROUTINE matrixSTD(iA, Di, Al, Au)
        COMMON /size/ N, nodes 
        
        DIMENSION iA(*), Di(*), Al(*), Au(*)
        
        OPEN(1, FILE = 'matrix_iA.txt', STATUS = 'OLD', ERR = 1)
        OPEN(2, FILE='matrix_iA.dat', ACCESS='DIRECT', RECL=4)
        READ(1, *) (iA(i), i = 1, N + 1)
        DO i = 1, N + 1
          WRITE(2, REC = i) iA(i)
        END DO 
        CLOSE(1)
        CLOSE(2)
        
        OPEN(1, FILE = 'matrix_Di.txt', STATUS = 'OLD', ERR = 2)
        OPEN(2, FILE='matrix_Di.dat', ACCESS='DIRECT', RECL=4)
        READ(1, *) (Di(i), i = 1, N)
        DO i = 1, N
          WRITE(2, REC = i) Di(i)  
        END DO 
        CLOSE(1)
        CLOSE(2)
        
        OPEN(1, FILE = 'matrix_Al.txt', STATUS = 'OLD', ERR = 3)
        OPEN(2, FILE = 'matrix_Au.txt', STATUS = 'OLD', ERR = 3)
        OPEN(3, FILE='matrix_Al.dat', ACCESS='DIRECT', RECL=4)
        OPEN(4, FILE='matrix_Au.dat', ACCESS='DIRECT', RECL=4)
        READ(1, *) (Al(i), i = 1, nodes)
        READ(2, *) (Au(i), i = 1, nodes)
        DO i = 1, nodes
          WRITE(3, REC = i) Al(i)
          WRITE(4, REC = i) Au(i)  
        END DO 
        CLOSE(1)
        CLOSE(2)
        CLOSE(3)
        CLOSE(4) 
        PRINT *, 'STD matrix convertion success'
        RETURN          
    1   PAUSE *, 'Error STD: matrix_iA file does not exist!'
        STOP  
    2   PAUSE *, 'Error STD: matrix_Di file does not exist!'
        STOP 
    3   PAUSE *, 'Error STD: matrix_Al(Au) file does not exist!'
        STOP 
      END
      
      SUBROUTINE matrixDTS()
        COMMON /size/ N, nodes

        OPEN(1, FILE='matrix_iA.dat', ACCESS='DIRECT', RECL=4)
        OPEN(2, FILE = 'matrix_iA_1.txt')
        DO i = 1, N + 1
          READ(1, REC = i) tmp
          WRITE(2, 100) tmp 
        END DO 
        CLOSE(1)
        CLOSE(2)
        
        OPEN(1, FILE='matrix_Di.dat', ACCESS='DIRECT', RECL=4)
        OPEN(2, FILE = 'matrix_Di_1.txt')
        DO i = 1, N
          READ(1, REC = i) tmp
          WRITE(2, 100) tmp 
        END DO 
        CLOSE(1)
        CLOSE(2)  
      
        OPEN(1, FILE='matrix_Al.dat', ACCESS='DIRECT', RECL=4)
        OPEN(2, FILE='matrix_Au.dat', ACCESS='DIRECT', RECL=4)
        OPEN(3, FILE = 'matrix_Al_1.txt')
        OPEN(4, FILE = 'matrix_Au_1.txt')
        DO i = 1, nodes
          READ(1, REC = i) tmp
          READ(2, REC = i) tmp_2
          WRITE(3, 100) tmp 
          WRITE(4, 100) tmp_2
        END DO 
        CLOSE(1)
        CLOSE(2)
        CLOSE(3)
        CLOSE(4) 
        PRINT *, 'DTS matrix convertion success'
        RETURN
        
  100   FORMAT(E10.4\, ' ')
      END

      SUBROUTINE vectorSTD(V)
        COMMON /size/ N, nodes 
        
        DIMENSION V(*)
      
        OPEN(1, FILE = 'vector.txt', STATUS = 'OLD', ERR = 1)
        OPEN(2, FILE = 'vector.dat', ACCESS = 'DIRECT', RECL = 4)
        READ(1, *) (V(i), i = 1, N)
        DO i = 1, N
          WRITE(2, REC = i) V(i) 
        END DO 
        CLOSE(1)
        CLOSE(2)
        PRINT *, 'STD vector convertion success'
        RETURN
        
    1   PAUSE *, 'Error STD: vector input files does not exist!'
        STOP
      END
      
      SUBROUTINE vectorDTS()
        COMMON /size/ N, nodes

        OPEN(1, FILE = 'vector.dat', ACCESS = 'DIRECT', RECL = 4)
        OPEN(2, FILE = 'vector_1.txt')
        DO i = 1, N
          READ(1, REC = i) tmp
          WRITE(2, 100) tmp
        END DO 
        CLOSE(1)    
        CLOSE(2)
        PRINT *, 'DTS vector convertion success'
        RETURN

  100   FORMAT(E10.4\, ' ')
      END

******TEST_CREATOR
      SUBROUTINE createTest(matrix_size)
        CALL createMatrix(matrix_size)
        CALL createVector(matrix_size)
      END

      SUBROUTINE createMatrix(matrix_size) 
        lines = MOD(matrix_size, 2)
        
        items = 1
        OPEN(1, FILE = 'matrix_iA.txt')
        WRITE(1, 100) 1.0, 1.0, 1.0
        DO i = 1, (matrix_size - 2) / 2
          WRITE(1, 100) REAL(items + i), REAL(items + 2 * i)
          items = items + 2 * i
        END DO 
        IF (lines .NE. 0) THEN
          items = items + i
          WRITE(1, 100) REAL(items)
        END IF
        CLOSE(1)
        
        OPEN(1, FILE = 'matrix_Di.txt')
        DO i = 1, matrix_size / 2
          WRITE(1, 100) REAL(i), REAL(i)
        END DO 
        IF (lines .NE. 0) WRITE(1, 100) REAL(i)
        CLOSE(1)

        OPEN(1, FILE = 'matrix_Al.txt')
        OPEN(2, FILE = 'matrix_Au.txt')
        DO i = 1, (matrix_size - 2) / 2
          WRITE(1, 100) (REAL(j), j = 1, i)
          WRITE(1, 100) (REAL(j), j = 1, i)
        END DO 
        WRITE(2, 100) (0.0, j = 1, items - 1)
        IF (lines .NE. 0) WRITE(1, 100) (REAL(j), j = 1, i)
        CLOSE(1)
        CLOSE(2)
        
        OPEN(1, FILE = 'matrix_size.txt')
        WRITE(1, *) matrix_size 
        CLOSE(1)
        PRINT *, 'Matrix creation success'
        RETURN

  100   FORMAT(E10.4\, ' ')
      END
      
      SUBROUTINE createVector(matrix_size)
        OPEN(1, FILE = 'vector.txt')
        WRITE(1, 100) (1.0, j = 1, matrix_size)
        CLOSE(1)
        
        OPEN(1, FILE = 'vector_size.txt')
        WRITE(1, *) matrix_size 
        CLOSE(1)
        PRINT *, 'Vector creation success'
        RETURN
        
  100   FORMAT(E10.4\, ' ') 
      END