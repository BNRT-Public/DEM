    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!
    !
    SUBROUTINE WRIVEL
    !
    ! DATA:  06/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA ESCRIBE LAS BARRAS DANADAS EN EL ARCHIVO VELOCITY_Txx.dat
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    INTEGER NPRINT

    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: DATAPRINT

    !--------------------------------------------------------------------------
    IF (OU06) 10,10,11
11  IF (PR06.EQ.OU06) THEN
        ! CARGAR VALORES
        NPRINT = INT(3*INCO)
        ALLOCATE(DATAPRINT(NPRINT))
        PR06=1

        DO I=1,INCO
            DATAPRINT(3*I-2) = VLX(NCON(I))
            DATAPRINT(3*I-1) = VLY(NCON(I))
            DATAPRINT(3*I)   = VLZ(NCON(I))
        END DO
        WRITE (6,66) TIME,DATAPRINT
66      FORMAT((E15.8),',',<NPRINT>(3X,E15.8,','))
        DEALLOCATE(DATAPRINT)
    ELSE
        PR06=PR06+1
    END IF

10  CONTINUE

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------