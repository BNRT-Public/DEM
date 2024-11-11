    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    FUNCTION DOMINF(DFT,LCOT,U0T,V0T,W0T,M1T,N1T,L1T)
    IMPLICIT NONE
    !
    ! DATA:  21/01/2023
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA CREA UN DOMINIO INFINITO EN LAS CARAS DEL MODELO
    !
    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA / SALIDA
    !--------------------------------------------------------------------------
    INTEGER M1T,N1T,L1T

    DOUBLE PRECISION U0T,V0T,W0T
    DOUBLE PRECISION LCOT
    DOUBLE PRECISION DFT
    DOUBLE PRECISION DOMINF
    
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION BORDER
    DOUBLE PRECISION AMPLI
    DOUBLE PRECISION DIST
    DOUBLE PRECISION DFN
    
    LOGICAL FACE(6) ! CARAS EN CUALES SE ACTIVA

    !--------------------------------------------------------------------------
    ! ASIGNACION DE CARAS PARA CREAR DOMINIO INFINITO
    !--------------------------------------------------------------------------
    BORDER = 0.1D0
    AMPLI=600.0D0

    FACE(1)=.FALSE. ! PLANE XY, Z=0
    FACE(2)=.FALSE. ! PLANE YZ, X=0
    FACE(3)=.FALSE. ! PLANE ZX, Y=0
    FACE(4)=.FALSE. ! PLANE XY, Z=z
    FACE(5)=.FALSE. ! PLANE YZ, X=x
    FACE(6)=.FALSE. ! PLANE ZX, Y=y


    !--------------------------------------------------------------------------
    ! CALCULA EL COEFICIENTE DE AMORTIGUAMIENTO PARA DOMINIO INFINITO
    !--------------------------------------------------------------------------
    DFN = DFT
    IF (FACE(1)) THEN
        DIST=BORDER*M1T*LCOT
        IF (U0T.LT.DIST)THEN
            DFN=(DFT+1.0D0)*(1.0D0+((DIST-U0T)/DIST)*AMPLI)
        END IF
    END IF

    IF (FACE(2)) THEN
        DIST=BORDER*N1T*LCOT
        IF (V0T.LT.DIST)THEN
            DFN=(DFT+1.0D0)*(1.0D0+((DIST-V0T)/DIST)*AMPLI)
        END IF
    END IF

    IF (FACE(3)) THEN
        DIST=BORDER*L1T*LCOT
        IF (W0T.LT.DIST)THEN
            DFN=(DFT+1.0D0)*(1.0D0+((DIST-W0T)/DIST)*AMPLI)
        END IF
    END IF

    IF (FACE(4)) THEN
        DIST=(1.0D0-BORDER)*M1T*LCOT
        IF (U0T.GT.DIST)THEN
            DFN=(DFT+1.0D0)*(1.0D0+((U0T-DIST)/(BORDER*M1T*LCOT))*AMPLI)
        END IF
    END IF

    IF (FACE(5)) THEN
        DIST=(1.0D0-BORDER)*N1T*LCOT
        IF (V0T.GT.DIST)THEN
            DFN=(DFT+1.0D0)*(1.0D0+((V0T-DIST)/(BORDER*N1T*LCOT))*AMPLI)
        END IF
    END IF

    IF (FACE(6)) THEN
        DIST=(1.0D0-BORDER)*N1T*LCOT
        IF (W0T.GT.DIST)THEN
            DFN=(DFT+1.0D0)*(1.0D0+((W0T-DIST)/(BORDER*L1T*LCOT))*AMPLI)
        END IF
    END IF

    DOMINF = DFN
    
    RETURN
    END FUNCTION DOMINF
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------