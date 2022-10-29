    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE NAYFES7
    !
    ! DATA:  04/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    ! CALCULA LAS PROPIEDADES EQUIVALENTE DE LAS BARRAS,
    ! PARA UN MEDIO CONTINUO
    ! SE APLICA WEIBULL??????????????
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I

    DOUBLE PRECISION POA,ALFA,DELTA
    DOUBLE PRECISION DT_CRIT(2),DT_CMIN
    DOUBLE PRECISION LMIN

    !--------------------------------------------------------------------------
    ! CALCULO DE PARAMETROS
    !--------------------------------------------------------------------------
    POA=1.0D00-PSN**2.0D00
    DELTA=9.0D00*PSN/(4.0D00-8.0D00*PSN)
    ALFA=(9.0D00+8.0D00*DELTA)/(18.0D00+24.0D00*DELTA)
    LCR=2.0*CAF*POA/ALFA/RFC/RFC
    EP(:,:)=RFC*DSQRT(GFR/YNG/POA)

    !--------------------------------------------------------------------------
    ! CALCULO DE LAS RIGIDECES DE LAS BARRAS
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I) SHARED(ENR,EDG,YNG,LI,ALFA,DELTA)
    DO I=1,NBT
        ! ENR = RIGIDEZ DE LAS BARRAS NORMALES (FÓRMULA 3.16: IGNACIO)
        ENR(I)=YNG*(LI(I)**2.0)*ALFA
        ! EDG = RIGIDEZ DE LAS BARRAS DIAGONALES (FÓRMULA 3.16: IGNACIO)
        EDG(I)=2.0D00*DELTA*(4.0D00/3.0D00)*ENR(I)/DSQRT(3.0D00)
    END DO
    !$OMP END PARALLEL DO

    !--------------------------------------------------------------------------
    ! CALCULO DE DT CRITICO PARA COMPRAR CON EL DT
    !--------------------------------------------------------------------------
    
    LMIN = MINVAL(LI)

    DT_CRIT(1)=0.6*LCO/(DSQRT(YNG/ROH))
    DT_CRIT(2)=LMIN/(DSQRT(YNG/ROH))

    DT_CMIN = MINVAL(DT_CRIT)

    IF (DT_CMIN.LT.DT) THEN
        WRITE(*,*) "DT_CMIN = ",DT_CMIN
        WRITE(*,*) "DT = ",DT
        PAUSE
        STOP
    END IF

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------