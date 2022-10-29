    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE WRITEN
    !
    ! DATA:  06/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESCRITURA DE TENSION PARA CADA CASO DE ENSAYO
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION SXX,SYY,SZZ,SYZ,SXZ,SXY,EXX,EYY,EZZ,EYZ,EXZ,EXY
    DOUBLE PRECISION FORCEX, FORCEY, FORCEZ
    INTEGER I, J

    !--------------------------------------------------------------------------
    ! INICIO DE VARIABLES
    !--------------------------------------------------------------------------
    SXX = 0.0D00
    SYY = 0.0D00
    SZZ = 0.0D00
    SYZ = 0.0D00
    SXZ = 0.0D00
    SXY = 0.0D00
    EXX = 0.0D00
    EYY = 0.0D00
    EZZ = 0.0D00
    EYZ = 0.0D00
    EXZ = 0.0D00
    EXY = 0.0D00

    FORCEX = 0.0D00
    FORCEY = 0.0D00
    FORCEZ = 0.0D00

    IF (OU11) 10,10,12
12  IF (PR11.EQ.OU11) THEN
        PR11=1
        !--------------------------------------------------------------------------
        !     ADAPTAR PARA CADA EXEMPLO TESTADO
        !--------------------------------------------------------------------------
        !$OMP PARALLEL DO PRIVATE(I) SHARED (LOX,LOY,LOZ,FRX,FRY,FRZ) REDUCTION(+:FORCEX, FORCEY,FORCEZ)
        DO I=1,NNT
            IF (ANY(LOX(I,:))) THEN
                FORCEX=FORCEX+FRX(I)
            END IF

            IF (ANY(LOY(I,:))) THEN
                FORCEY=FORCEY+FRY(I)
            END IF

            IF (ANY(LOZ(I,:))) THEN
                FORCEZ=FORCEZ+FRZ(I)
            END IF
        END DO
        !$OMP END PARALLEL DO

        SXX = FORCEX/(N1*LCO*L1*LCO)
        SYY = FORCEY/(M1*LCO*L1*LCO)
        SZZ = FORCEZ/(M1*LCO*N1*LCO)

        EYY = ((V(2,NCON(2))-(V(2,NCON(1))))-(V0(NCON(2))-V0(NCON(1))))/(V0(NCON(2))-V0(NCON(1)))

        !!!!
        !!!!    !EXX =(TIME*VELOCITY)/(M1*LCO)
        !!!!    !EYY =(TIME*VELOCITY)/(N1*LCO) !OK
        !!!!    !EZZ =(TIME*VELOCITY)/(L1*LCO)
        !!!!
        !!!!    EYY = ((V(2,NCON(2))-(V(2,NCON(1))))-(V0(NCON(2))-V0(NCON(1))))/(V0(NCON(2))-V0(NCON(1)))


        !--------------------------------------------------------------------------
        ! ESCRITURA DE ARCHIVO
        !--------------------------------------------------------------------------
        WRITE(11,11) TIME,SXX,SYY,SZZ,SYZ,SXZ,SXY,EXX,EYY,EZZ,EYZ,EXZ,EXY
11      FORMAT((E15.8),',',12(3X,E15.8,','))

    ELSE
        PR11=PR11+1
    END IF

10  CONTINUE

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------