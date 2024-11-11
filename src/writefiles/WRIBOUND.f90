    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE WRIBOUND(IFILE)
    !
    ! DATA:  07/11/2022
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA ESCRIBE LOS VALORES DE DESPLAZAMIENTO EN LAS CONDICIONES DE 
    ! BORDE EN EL ARCHIVO BOUND_Txx.dat
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,J
    INTEGER NPRINT

    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: DATAPRINT
    DOUBLE PRECISION TEMP

    !--------------------------------------------------------------------------
    ! VARIABLES ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER IFILE

    !--------------------------------------------------------------------------
    IF (OU18) 10,10,11
11  IF (PR18.EQ.OU18) THEN
        ! CARGAR VALORES
        NPRINT=1
        NPRINT=NPRINT+SIZE(BDX,2)+SIZE(BDY,2)+SIZE(BDZ,2)
        NPRINT=NPRINT+SIZE(LOX,2)+SIZE(LOY,2)+SIZE(LOZ,2)
        ALLOCATE(DATAPRINT(NPRINT))
        PR18=1
        DATAPRINT(1)=TIME
        
        J = 1
        DO I=1,SIZE(BDX,2)
            J = J+1
            TEMP = SUM(U(2,:)-U0(:),MASK=BDX(:,I))
            TEMP = TEMP/COUNT(BDX(:,I))
            DATAPRINT(J)=TEMP
        END DO
        
        DO I=1,SIZE(BDY,2)
            J = J+1
            TEMP = SUM(V(2,:)-V0(:),MASK=BDY(:,I))
            TEMP = TEMP/COUNT(BDY(:,I))
            DATAPRINT(J)=TEMP
        END DO
        
        DO I=1,SIZE(BDZ,2)
            J = J+1
            TEMP = SUM(W(2,:)-W0(:),MASK=BDZ(:,I))
            TEMP = TEMP/COUNT(BDZ(:,I))
            DATAPRINT(J)=TEMP
        END DO
        
        DO I=1,SIZE(LOX,2)
            J = J+1
            TEMP = SUM(U(2,:)-U0(:),MASK=LOX(:,I))
            TEMP = TEMP/COUNT(LOX(:,I))
            DATAPRINT(J)=TEMP
        END DO
        
        DO I=1,SIZE(LOY,2)
            J = J+1
            TEMP = SUM(V(2,:)-V0(:),MASK=LOY(:,I))
            TEMP = TEMP/COUNT(LOY(:,I))
            DATAPRINT(J)=TEMP
        END DO
        
        DO I=1,SIZE(LOZ,2)
            J = J+1
            TEMP = SUM(W(2,:)-W0(:),MASK=LOZ(:,I))
            TEMP = TEMP/COUNT(LOZ(:,I))
            DATAPRINT(J)=TEMP
        END DO

        WRITE (IFILE,118) DATAPRINT
118     FORMAT((E15.8),',',<NPRINT-1>(3X,E15.8,','))
        DEALLOCATE(DATAPRINT)
    ELSE
        PR18=PR18+1
    END IF

10  CONTINUE

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------