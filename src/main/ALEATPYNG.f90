    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!
    !
    SUBROUTINE ALEATPYNG
    !
    ! DATA:  15/09/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! CREA POLOS EN EL MODELO PARA EL MODULO DE ELASTISIDAD
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION AUX, BT,GM
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  RAND

    INTEGER I

    !--------------------------------------------------------------------------
    ! CREACION DE POLOS EN MODULO DE ELASTISIDAD
    !--------------------------------------------------------------------------
    CALL SIZEPOLO(M,N,L,LCO,LCXYNG,LCYYNG,LCZYNG,NPTYNG)
    
    ALLOCATE(RAND(NPTYNG(4)))
    ALLOCATE(PHIYNG(NPTYNG(4)))
    ALLOCATE(PPYNG(NPTYNG(4),3))
    PPYNG=0.0
    RAND=0.0
    AUX = 0.0
    PHIYNG=YNG

    CALL POSPOLO(M,N,L,LCO,LCXYNG,LCYYNG,LCZYNG,NPTYNG,PPYNG)

    IF (CVYNG.GE.0.05) THEN
        CALL FPROB(CVYNG,GM,BT)
        BT=YNG/BT

        CALL RAND1(SEED,RAND)

        !$OMP PARALLEL DO PRIVATE(I,AUX) SHARED(NPTYNG,PHIYNG,BT,GM,SEED,RAND)
        DO I=1,NPTYNG(4)
            AUX = 1.0-RAND(I)
            PHIYNG(I)=BT*(-LOG(AUX))**(1/GM)

            IF ((ISNAN(PHIYNG(I))).AND.(PHIYNG(I).LT.0.0))THEN
                !$OMP CRITICAL
                WRITE(*,*) PHIYNG(I), I
                WRITE(*,*) 'ERROR: VALUE YNG IN POLO'
                WRITE(*,*) 'DO YOU WISH TO CONTINUE?'
                PAUSE
                STOP
                !$OMP END CRITICAL
            END IF
        ENDDO
        !$OMP END PARALLEL DO
    END IF

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------