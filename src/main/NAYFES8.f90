    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE NAYFES8
    IMPLICIT NONE
    !
    ! DATA:  17/02/2023
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
    DOUBLE PRECISION DT_CRIT(3),DT_CMIN
    DOUBLE PRECISION LMIN
    DOUBLE PRECISION ROHMED
    DOUBLE PRECISION UU(3,1)
    DOUBLE PRECISION COEFDIAG
    
    double precision ygn_refuerzo
    double precision gf_refuerzo

    
    LOGICAL,ALLOCATABLE,DIMENSION(:) :: LOGIC

    !--------------------------------------------------------------------------
    ! ASIGNACION DE VARIABLES
    !--------------------------------------------------------------------------
    IPT=0
    UU=0.0
    
    SELECT CASE (MODELTYPE)
    CASE ('DEM') ! MODEL DEM
        
        !--------------------------------------------------------------------------
        ! CALCULO DE PARAMETROS
        !--------------------------------------------------------------------------
        DT_CMIN = 1.0D99
        LMIN = MINVAL(LI)
        POA=1.0D00-PSN**2.0
        DELTA=9.0D00*PSN/(4.0D00-8.0D00*PSN)
        ALFA=(9.0D00+8.0D00*DELTA)/(18.0D00+24.0D00*DELTA)
        !LCR=2.0*CAF*POA/ALFA/RFC/RFC
        LCR=2.0D0*CAF*POA*DEQ/ALFA
        COEFDIAG=2.0*DELTA*(4.0/3.0)/DSQRT(3.0D00)

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
        ! CALCULO PARA RIGIDIZAR LAS BARRAS
        !--------------------------------------------------------------------------
        IF ((test.eq.813).or.(test.eq.814).or.(test.eq.823).or.(test.eq.824).or.(test.eq.812)) THEN
            ALLOCATE(logic(2))
            ygn_refuerzo = 2.25e9
            gf_refuerzo = 112500
                        
            !$OMP PARALLEL DO PRIVATE(I) !$OMP SHARED(elemtype,GFRB,YNGB,gf_refuerzo,ygn_refuerzo)
            do i = 1,nbt
                if (elemtype(i) .eq. 7) then
                    GFRB(I) = gf_refuerzo
                    YNGB(I) = ygn_refuerzo
                end if
                
                if (elemtype(i) .eq. 6) then
                    GFRB(I) = gf_refuerzo
                    YNGB(I) = ygn_refuerzo
                end if
            end do
            !$OMP END PARALLEL DO
        END IF
        
        
        !--------------------------------------------------------------------------
        ! CALCULO DE LAS RIGIDECES DE LAS BARRAS
        !--------------------------------------------------------------------------
        !$OMP PARALLEL DO PRIVATE(I,DT_CRIT,ROHMED) &
        !$OMP SHARED(ENR,EDG,EP,YNGB,GFRB,LI,ALFA,POA,RFC,DELTA,CN,LMIN,ROHN,DT_CMIN,COEFDIAG)
        DO I=1,NBT
            ! ENR = RIGIDEZ DE LAS BARRAS NORMALES (FORMULA 3.16: IGNACIO)
            ENR(I)=YNGB(I)*(LI(I)**2.0)*ALFA
            ! EDG = RIGIDEZ DE LAS BARRAS DIAGONALES (FORMULA 3.16: IGNACIO)
            EDG(I)=COEFDIAG*ENR(I)

            !--------------------------------------------------------------------------
            ! CALCULO DE EP DE CADA BARRA
            !--------------------------------------------------------------------------
            EP(:,I)=DSQRT(1.00/DEQ)*DSQRT(GFRB(I)/YNGB(I)/POA)

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
        END DO
        !$OMP END PARALLEL DO
    
    END SELECT
    
    RETURN
    END
