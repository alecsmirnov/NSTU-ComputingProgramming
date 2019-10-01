      PROGRAM MAIN
        IMPLICIT NONE
      
******COMMON_VARIABLES
        REAL x1, y1, x2, y2, x3, y3
        REAL a, b, c
        REAL pi, S, alpha
        COMMON /vertices/ x1, y1, x2, y2, x3, y3
        COMMON /sides/ a, b, c
        COMMON pi, S, alpha
      
******LOCAL_VARIABLES
        INTEGER i
      
******FUNCTIONS_DECLARATION
        REAL minAngle, minCos
    
******CONST
        pi = 3.14159265

******MAIN_BODY  
        DO WHILE (i .NE. 5)
          CALL menu()
          CALL input(i)
     
          SELECT CASE (i)
            CASE (1) 
              CALL setVertices()
              CALL calculateSides()
              IF (a.LT.b+c .AND. b.LT.a+c .AND. c.LT.a+b) THEN
                CALL calculateS()
                CALL calculateAlpha()
              ELSE
                PRINT *, 'Entered is not triangle! Try again.'
              END IF         
            CASE (2)
              CALL output('S = ', S)
            CASE (3)
              CALL output('Min angle = ', minAngle())
            CASE (4)
              CALL output('Min cos = ', minCos())
            CASE ( : 0, 6 : )
              PRINT *, 'Wrong menu item!'          
          END SELECT
          PRINT *, ' '
        END DO
        PRINT *, 'Completed'
        PAUSE
      END
******FUNCTIONS_DEFINITION
      SUBROUTINE menu()
        IMPLICIT NONE
        PRINT *, '"MENU"'
        PRINT *, '1. Enter the new triangle;'
        PRINT *, '2. Calculate the S of the triangle;'
        PRINT *, '3. Calculate minimum angle in degrees;'
        PRINT *, '4. Calculate cos of the minimum angle;'
        PRINT *, '5. Exit.'
      END
     
      SUBROUTINE input(item)
        IMPLICIT NONE
        INTEGER item
        PRINT *, ' '
        PRINT *, 'Select menu item: '
        READ *, item
      END
       
      SUBROUTINE output(text, result)
        IMPLICIT NONE
        REAL x1, y1, x2, y2, x3, y3
        REAL a, b, c
        REAL pi, S, alpha
        COMMON /vertices/ x1, y1, x2, y2, x3, y3
        COMMON /sides/ a, b, c
        COMMON pi, S, alpha
        
        CHARACTER *(*) text
        REAL result
        
        IF (S .NE. 0) THEN
          PRINT *, text, result
        ELSE
          PRINT *, 'Enter the coordinates of the triangle!'
        END IF
      END
       
      SUBROUTINE setVertices()
        IMPLICIT NONE
        REAL x1, y1, x2, y2, x3, y3
        REAL a, b, c
        REAL pi, S, alpha
        COMMON /vertices/ x1, y1, x2, y2, x3, y3
        COMMON /sides/ a, b, c
        COMMON pi, S, alpha
        
        PRINT *, 'Enter the vertices'
        PRINT *, 'x1, y1: '
        READ *, x1, y1
        PRINT *, 'x2, y2: '
        READ *, x2, y2
        PRINT *, 'x3, y3: '
        READ *, x3, y3
      END
      
      REAL FUNCTION sideLen(x1, y1, x2, y2)
        IMPLICIT NONE
        REAL x1, y1, x2, y2
        sideLen = sqrt((x2 - x1)**2 + (y2 - y1)**2)
      END
        
      SUBROUTINE calculateSides()
        IMPLICIT NONE
        REAL x1, y1, x2, y2, x3, y3
        REAL a, b, c
        REAL pi, S, alpha, sideLen
        COMMON /vertices/ x1, y1, x2, y2, x3, y3
        COMMON /sides/ a, b, c
        COMMON pi, S, alpha
       
        a = sideLen(x1, y1, x2, y2)
        b = sideLen(x1, y1, x3, y3)
        c = sideLen(x2, y2, x3, y3)
      END
        
      SUBROUTINE calculateS()
        IMPLICIT NONE
        REAL x1, y1, x2, y2, x3, y3
        REAL a, b, c
        REAL pi, S, alpha, p
        COMMON /vertices/ x1, y1, x2, y2, x3, y3
        COMMON /sides/ a, b, c
        COMMON pi, S, alpha
                
        p = (a + b + c) / 2
        S = sqrt(p * (p - a) * (p - b) * (p - c))
      END

      SUBROUTINE calculate Alpha()
        IMPLICIT NONE
        REAL x1, y1, x2, y2, x3, y3
        REAL a, b, c
        REAL pi, S, alpha, min_side 
        COMMON /vertices/ x1, y1, x2, y2, x3, y3
        COMMON /sides/ a, b, c
        COMMON pi, S, alpha

        min_side = min(a, b, c)
        alpha = asin(2 * S / (a * b * c / min_side))
      END
       
      REAL FUNCTION minAngle()
        IMPLICIT NONE
        REAL x1, y1, x2, y2, x3, y3
        REAL a, b, c
        REAL pi, S, alpha
        COMMON /vertices/ x1, y1, x2, y2, x3, y3
        COMMON /sides/ a, b, c
        COMMON pi, S, alpha

        minAngle = alpha * 180 / pi
      END

      REAL FUNCTION minCos()
        IMPLICIT NONE
        REAL x1, y1, x2, y2, x3, y3
        REAL a, b, c
        REAL pi, S, alpha
        COMMON /vertices/ x1, y1, x2, y2, x3, y3
        COMMON /sides/ a, b, c
        COMMON pi, S, alpha

        minCos = cos(alpha)
      END
