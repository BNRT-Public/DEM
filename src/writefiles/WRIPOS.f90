    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE WRIPOS
    !
    ! DATA:  06/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA ESCRIBE LAS BARRAS DANADAS EN EL ARCHIVO DISPLACEMENT_Txx.dat
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    INTEGER NPRINT

    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: DATAPRINT

    !--------------------------------------------------------------------------
    IF ((PR07.EQ.OU07).AND.(INCO.NE.0).AND.(OU07.GT.0)) THEN
        ! CARGAR VALORES
        NPRINT = INT(3*INCO)
        ALLOCATE(DATAPRINT(NPRINT))
        PR07=1

        DO I=1,INCO
            DATAPRINT(3*I-2) = U(2,NCON(I))-U0(NCON(I))
            DATAPRINT(3*I-1) = V(2,NCON(I))-V0(NCON(I))
            DATAPRINT(3*I)   = W(2,NCON(I))-W0(NCON(I))
        END DO
        
        WRITE (7,77) TIME,DATAPRINT
77      FORMAT((E15.8),',',<NPRINT>(3X,E15.8,','))
        DEALLOCATE(DATAPRINT)
    ELSE IF ((INCO.NE.0).AND.(OU07.GT.0)) THEN
        PR07=PR07+1
    END IF
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------