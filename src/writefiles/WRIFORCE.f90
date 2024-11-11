    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE WRIFORCE(IFILE)
    !
    ! DATA:  07/11/2022
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA ESCRIBE LOS VALORES DE FUERZAS EN LAS CONDICIONES DE 
    ! BORDE EN EL ARCHIVO LOAD_Txx.dat
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,J
    INTEGER NPRINT

    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: DATAPRINT

    !--------------------------------------------------------------------------
    ! VARIABLES ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER IFILE

    !--------------------------------------------------------------------------
    IF (OU17) 10,10,11
11  IF (PR17.EQ.OU17) THEN
        ! CARGAR VALORES
        NPRINT=1
        NPRINT=NPRINT+SIZE(BDX,2)+SIZE(BDY,2)+SIZE(BDZ,2)
        NPRINT=NPRINT+SIZE(LOX,2)+SIZE(LOY,2)+SIZE(LOZ,2)
        ALLOCATE(DATAPRINT(NPRINT))
        PR17=1
        DATAPRINT(1)=TIME
        
        J = 1
        DO I=1,SIZE(BDX,2)
            J = J+1
            DATAPRINT(J)=SUM(FRX,MASK=BDX(:,I))
        END DO
        
        DO I=1,SIZE(BDY,2)
            J = J+1
            DATAPRINT(J)=SUM(FRY,MASK=BDY(:,I))
        END DO
        
        DO I=1,SIZE(BDZ,2)
            J = J+1
            DATAPRINT(J)=SUM(FRZ,MASK=BDZ(:,I))
        END DO
        
        DO I=1,SIZE(LOX,2)
            J = J+1
            DATAPRINT(J)=SUM(FRX,MASK=LOX(:,I))
        END DO
        
        DO I=1,SIZE(LOY,2)
            J = J+1
            DATAPRINT(J)=SUM(FRY,MASK=LOY(:,I))
        END DO
        
        DO I=1,SIZE(LOZ,2)
            J = J+1
            DATAPRINT(J)=SUM(FRZ,MASK=LOZ(:,I))
        END DO
        
        WRITE (IFILE,117) DATAPRINT
117     FORMAT((E15.8),',',<NPRINT-1>(3X,E15.8,','))
        DEALLOCATE(DATAPRINT)
    ELSE
        PR17=PR17+1
    END IF

10  CONTINUE

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------