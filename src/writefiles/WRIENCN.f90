    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE WRIENCN(IFILE)
    !
    ! DATA:  01/10/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA ESCRIBE LOS NODOS QUE SUPERAN UNA DETERMINADA (AENCN)
    !  Y LOS ESCRIBE EN EL ARCHIVO DeltaENCN_Txx.dat
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    DOUBLE PRECISION DELTA
    DOUBLE PRECISION DATAPRINT(5)

    !--------------------------------------------------------------------------
    ! VARIABLES ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER IFILE

    !--------------------------------------------------------------------------
    IF (OU16) 10,10,11
11  IF (PR16.EQ.OU16) THEN
        ! CARGAR VALORES
        DATAPRINT(1)=TIME
        DO I=1,NNT
            DELTA=DENCN(2,I)-DENCN(1,I)

            IF (ABS(DELTA).GE.AENCN) THEN
                DATAPRINT(1)=U(2,I)
                DATAPRINT(2)=V(2,I)
                DATAPRINT(3)=W(2,I)
                DATAPRINT(4)=DENCN(2,I)
                DATAPRINT(5)=DELTA

                WRITE (IFILE,16) TIME,I,DATAPRINT
16              FORMAT((E15.8),',',(I15),5(3X,E15.8,','))
            END IF
        END DO
        PR16=1
    ELSE
        PR16=PR16+1
    END IF

10  CONTINUE

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------