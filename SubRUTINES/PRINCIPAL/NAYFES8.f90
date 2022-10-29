    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE NAYFES8
    IMPLICIT NONE
    !
    ! DATA:  15/09/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! CALCULA LAS PROPIEDADES EQUIVALENTE DE LAS BARRAS,
    ! PARA UN MEDIO CONTINUO, CONCIDERANDO QUE E Y GF TIENEN
    ! UNA DISTRIBUCION DE  WEIBULL
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    INTEGER IPT(8)

    DOUBLE PRECISION POA,ALFA,DELTA,PHIVR
    DOUBLE PRECISION DT_CRIT(2),DT_CMIN
    DOUBLE PRECISION LMIN
    DOUBLE PRECISION ROHMED
    DOUBLE PRECISION UU(3,1)

    !--------------------------------------------------------------------------
    ! ASIGNACION DE VARIABLES
    !--------------------------------------------------------------------------
    IPT=0
    UU=0.0
    
    !--------------------------------------------------------------------------
    ! CALCULO DE PARAMETROS
    !--------------------------------------------------------------------------
    DT_CMIN = 1.0D99
    LMIN = MINVAL(LI)
    POA=1.0D00-PSN**2.0
    DELTA=9.0D00*PSN/(4.0D00-8.0D00*PSN)
    ALFA=(9.0D00+8.0D00*DELTA)/(18.0D00+24.0D00*DELTA)
    LCR=2.0*CAF*POA/ALFA/RFC/RFC

    !--------------------------------------------------------------------------
    ! CALCULO DE PROPIEDADES DEL MATERIAL
    !--------------------------------------------------------------------------
    IF ((CVYNG.GE.0.05).OR.(CVGFR.GE.0.05)) THEN
        !$OMP PARALLEL DO PRIVATE(I,PHIVR,IPT,UU) &
        !$OMP SHARED(LCXYNG,LCYYNG,LCZYNG,LCXGFR,LCYGFR,LCZGFR,ROTYNG,ROTGFR,PHIYNG,YNG,YNGB,GFR,GFRB,NPTGFR,NPTYNG,PPYNG,PPGFR)
        DO I=1,NBT
            
            !--------------------------------------------------------------------------
            ! GENERACION DE DOS CAMPOS ALEATORIOS NO RELACIONADOS
            !--------------------------------------------------------------------------
            IF (CVYNG.GE.0.05) THEN
                CALL UBARPOLO(2,I,IPT,UU)!(LONG,MODEL,NPTYNG,LCXYNG,LCYYNG,LCZYNG,ROTYNG,PPYNG,IPT)
                CALL INTPOLO(2,IPT,UU,PHIVR)!(LONG,MODEL,NPTYNG,LCXYNG,LCYYNG,LCZYNG,PPYNG,IPT,PHIYNG,PHIV)
                YNGB(I)=PHIVR
            END IF
            
            IF (CVGFR.GE.0.05) THEN
                !3" GFRB(B)=PHIV*(GFR/YNG)
                CALL UBARPOLO(1,I,IPT,UU)!(LONG,MODEL,NPTGFR,LCXGFR,LCYGFR,LCZGFR,ROTGFR,PPGFR,IPT)
                CALL INTPOLO(1,IPT,UU,PHIVR)!(LONG,MODEL,NPTGFR,LCXGFR,LCYGFR,LCZGFR,PPGFR,IPT,PHIGFR,PHIV)
                GFRB(I)=PHIVR
                !3" GFRB(I)=(GFRB(I)+ PHIV)*0.5
            END IF
            
            !--------------------------------------------------------------------------
            ! GENERACION DE DOS CAMPOS ALEATORIOS RELACIONADOS(YNG MANDA)
            !--------------------------------------------------------------------------
            !IF (CVYNG.GE.0.05) THEN
            ! CALL RPOLO(NPTYNG,XB(I),YB(I),ZB(I),PHIV,LCXYNG,LCYYNG,LCZYNG,PHIYNG)
            ! YNGB(I)=PHIV
            ! GFRB(I)=PHIV*(GFR/YNG)
            !END IF
        END DO
        !$OMP END PARALLEL DO
    END IF

    !--------------------------------------------------------------------------
    ! CALCULO PARA DESABILITAR LAS BARRAS
    !--------------------------------------------------------------------------
    !CALL DEBBARRAS

    !--------------------------------------------------------------------------
    ! CALCULO DE LAS RIGIDECES DE LAS BARRAS
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I,DT_CRIT,ROHMED) &
    !$OMP SHARED(ENR,EDG,EP,YNGB,GFRB,LI,ALFA,POA,RFC,DELTA,CN,LMIN,ROHN,DT_CMIN)
    DO I=1,NBT
        ! ENR = RIGIDEZ DE LAS BARRAS NORMALES (FÓRMULA 3.16: IGNACIO)
        ENR(I)=YNGB(I)*(LI(I)**2.0)*ALFA
        ! EDG = RIGIDEZ DE LAS BARRAS DIAGONALES (FÓRMULA 3.16: IGNACIO)
        EDG(I)=2.0*DELTA*(4.0/3.0)*ENR(I)/DSQRT(3.0D00)

        !--------------------------------------------------------------------------
        ! CALCULO DE EP DE CADA BARRA
        !--------------------------------------------------------------------------
        EP(:,I)=RFC*DSQRT(GFRB(I)/YNGB(I)/POA)

        IF (ANY(ISNAN(EP(:,I)))) THEN
            !$OMP CRITICAL
            WRITE(*,*) 'EP: ', EP(:,I)
            WRITE(*,*) 'GFR: ', GFRB(I)
            WRITE(*,*) 'YNG: ', YNGB(I)
            WRITE(*,*) 'ERROR: VALUE EP'
            WRITE(*,*) 'DO YOU WISH TO CONTINUE?'
            PAUSE
            !$OMP END CRITICAL
        END IF

        !--------------------------------------------------------------------------
        ! CALCULO DE DT CRITICO PARA COMPRAR CON EL DT
        !--------------------------------------------------------------------------
        ROHMED=(ROHN(CN(1,I))+ROHN(CN(2,I)))*0.5

        DT_CRIT(1)=0.6*LCO/(DSQRT(YNGB(I)/ROHMED))
        DT_CRIT(2)=LMIN/(DSQRT(YNGB(I)/ROHMED))

        DT_CRIT(1) = MINVAL(DT_CRIT)

        IF(DT_CRIT(1).LT.DT_CMIN)THEN
            DT_CMIN=DT_CRIT(1)
        END IF
    END DO
    !$OMP END PARALLEL DO

    IF (DT_CMIN.LT.DT) THEN
        WRITE(*,*) 'ERROR. DT > DT_(CRIT)'
        WRITE(*,*) 'DT_CMIN = ',DT_CMIN
        WRITE(*,*) 'DT = ',DT
        PAUSE
        STOP
    END IF

    RETURN
    END