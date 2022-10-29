    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE RAND1(SEED,RAND)
    !
    ! DATA:  27/09/2019
    ! AUTOR: Boris N. Rojo Tanzi
    ! VERSION: 1.0
    !
    ! ESTA GENERA NUMEROS ALEATOREOS CON DISTRIBUCION UNIFORME.
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER, DIMENSION(:), ALLOCATABLE ::  SSEED
    
    DOUBLE PRECISION MIN

    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER SEED, K
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: RAND

    !--------------------------------------------------------------------------
    CALL RANDOM_SEED(SIZE=K)
    
    ALLOCATE(SSEED(K))
    
    SSEED = SEED
    CALL RANDOM_SEED(PUT=SSEED)
10  CALL RANDOM_NUMBER(RAND)
    
    DEALLOCATE(SSEED)
    
    MIN=MINVAL(1-RAND)
    IF (MIN.LT.1E-15) GO TO 10
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------