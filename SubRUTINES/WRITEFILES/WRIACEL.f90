    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE WRIACEL
    !
    ! DATA:  06/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA ESCRIBE LAS BARRAS DANADAS EN EL ARCHIVO ACELERATION_Txx.dat
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    INTEGER NPRINT

    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: DATAPRINT
    
    !--------------------------------------------------------------------------
    IF (OU05) 10,10,11
11  IF (PR05.EQ.OU05) THEN
        ! CARGAR VALORES
        NPRINT = INT(3*INCO)
        ALLOCATE(DATAPRINT(NPRINT))
        PR05=1

        DO I=1,INCO
            DATAPRINT(3*I-2) = ACX(NCON(I))
            DATAPRINT(3*I-1) = ACY(NCON(I))
            DATAPRINT(3*I)   = ACZ(NCON(I))
        END DO
        WRITE (5,55) TIME,DATAPRINT
55      FORMAT((E15.8),',',<NPRINT>(3X,E15.8,','))
        DEALLOCATE(DATAPRINT)
    ELSE
        PR05=PR05+1
    END IF

10  CONTINUE

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------