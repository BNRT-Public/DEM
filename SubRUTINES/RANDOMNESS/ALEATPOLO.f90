    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!
    !
    SUBROUTINE ALEATPOLO
    !
    ! DATA:  05/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION AUX,NOR
    DOUBLE PRECISION NPTX,NPTY,NPTZ
    DOUBLE PRECISION PHI(NNT)
    INTEGER I,NPTOTAL

    DOUBLE PRECISION LCORX,LCORY,LCORZ
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    ! LONGITUD DE CORRELACION (VER DE PONER EN DATA)
    LCORX=8.00D-03
    LCORY=8.00D-03
    LCORZ=8.00D-03
    
    ! CANTIDAD DE POLOS EN CADA DIRECCION
    NPTX=(LCO*M1/LCORX)+1.0
    NPTY=(LCO*N1/LCORY)+1.0
    NPTZ=(LCO*l1/LCORZ)+1.0
    
    ! NUMERO TOTALES DE POLOS
    NPTOTAL=INT(NPTX*NPTY*NPTZ)
    
    DO I=1,NPTOTAL
        IF (CVGFR.GE.0.04) THEN
10          NOR = RAN0(SEED)
            AUX=1.0D00-NOR
            IF (AUX.LT.1.0D-15) GO TO 10
            PHI(I)=DSQRT((-LOG(AUX))**(1/GMA)/BTA)
        END IF
    END DO
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------