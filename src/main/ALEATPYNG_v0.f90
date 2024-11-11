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
    DOUBLE PRECISION AUX,NOR
    DOUBLE PRECISION BT,GM

    INTEGER IPT,IPTX,IPTY,IPTZ

    !--------------------------------------------------------------------------
    ! CREACION DE POLOS EN MODULO DE ELASTISIDAD
    !--------------------------------------------------------------------------
    CALL SIZEPOLO(M1,N1,L1,LCO,LCXYNG,LCYYNG,LCZYNG,NPTYNG)

    ALLOCATE(PHIYNG(NPTYNG(4)))
    ALLOCATE(PPYNG(NPTYNG(4),3))
    PPYNG = 0.0
    PHIYNG=YNG

    CALL POSPOLO(M1,N1,L1,LCO,LCXYNG,LCYYNG,LCZYNG,NPTYNG,PPYNG)

    AUX = 0.0
    IF (CVYNG.GE.0.05) THEN
        CALL FPROB(CVYNG,GM,BT)
        BT=YNG/BT

        !$OMP PARALLEL DO PRIVATE(IPT,IPTX,IPTY,IPTZ,AUX,NOR) SHARED(NPTYNG,PHIYNG,BT,GM,SEED)
        DO IPTX=1,NPTYNG(1)
            DO IPTY=1,NPTYNG(2)
                DO IPTZ=1,NPTYNG(3)
10                  AUX = 0.0
                    DO WHILE (AUX.LT.1.0D-15)
                        NOR = RAN0(SEED)
                        AUX=1.0D00-NOR
                    END DO

                    IPT=NPTYNG(1)*NPTYNG(2)*(IPTZ-1)+NPTYNG(1)*(IPTY-1)+IPTX
                    PHIYNG(IPT)=BT*(-LOG(AUX))**(1/GM)

                    IF (PHIYNG(IPT).LT.0.0) GO TO 10

                    IF (ISNAN(PHIYNG(IPT)))THEN
                        !$OMP CRITICAL
                        WRITE(*,*) PHIYNG(IPT), IPT
                        WRITE(*,*) 'ERROR: VALUE YNG IN POLO'
                        WRITE(*,*) 'DO YOU WISH TO CONTINUE?'
                        PAUSE
                        !$OMP END CRITICAL
                        GO TO 10
                    END IF

                END DO
            ENDDO
        ENDDO
        !$OMP END PARALLEL DO
    END IF

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------