    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE DOMINF(DF1,DF2,STEP,NDAMF,DT,LCO,U0,V0,W0,M1,N1,L1,DF,TDMP1,TDMP2,TIME)
    IMPLICIT NONE
    !
    ! DATA:  28/09/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !        Ignacio Iturrioz
    !
    ! ESTA SUBRUTINA CREA UN DOMINIO INFINITO EN LAS CARAS DEL MODELO
    !
    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA / SALIDA
    !--------------------------------------------------------------------------
    INTEGER M1,N1,L1
    INTEGER STEP,NDAMF

    DOUBLE PRECISION U0,V0,W0
    DOUBLE PRECISION LCO
    DOUBLE PRECISION DF,DF1,DF2,TDMP1,TDMP2
    DOUBLE PRECISION DT,TIME
    
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION BORDER
    DOUBLE PRECISION AMPLI
    
    LOGICAL FACE(6) ! CARAS EN CUALES SE ACTIVA

    !--------------------------------------------------------------------------
    ! ASIGNACION DE CARAS PARA CREAR DOMINIO INFINITO
    !--------------------------------------------------------------------------
    BORDER = 0.1
    AMPLI=600.0

    FACE(1)=.FALSE. ! PLANE XY, Z=0
    FACE(2)=.FALSE. ! PLANE YZ, X=0
    FACE(3)=.FALSE. ! PLANE ZX, Y=0
    FACE(4)=.FALSE. ! PLANE XY, Z=z
    FACE(5)=.FALSE. ! PLANE YZ, X=x
    FACE(6)=.FALSE. ! PLANE ZX, Y=y

    !--------------------------------------------------------------------------
    ! CALCULA EL COEFICIENTE DE AMORTIGUAMIENTO
    !--------------------------------------------------------------------------
    ! CONTROL DE AMORTIGUAMIENTO
    IF (STEP.LT.NDAMF) THEN
        DF=DF1
    ELSE
        DF=DF2
    END IF

    !--------------------------------------------------------------------------
    ! CALCULA EL COEFICIENTE DE AMORTIGUAMIENTO PARA DOMINIO INFINITO
    !--------------------------------------------------------------------------
    IF (FACE(1)) THEN
        IF (U0.LT.(BORDER*M1*LCO))THEN
            DF=(DF+1.0)*(1+((BORDER*M1*LCO-U0)/(BORDER*M1*LCO))*AMPLI)
        END IF
    END IF

    IF (FACE(2)) THEN
        IF (V0.LT.(BORDER*N1*LCO))THEN
            DF=(DF+1.0)*(1+((BORDER*N1*LCO-V0)/(BORDER*N1*LCO))*AMPLI)
        END IF
    END IF

    IF (FACE(3)) THEN
        IF (W0.LT.(BORDER*L1*LCO))THEN
            DF=(DF+1.0)*(1+((BORDER*L1*LCO-W0)/(BORDER*L1*LCO))*AMPLI)
        END IF
    END IF

    IF (FACE(4)) THEN
        IF (U0.GT.((1.0-BORDER)*M1*LCO))THEN
            DF=(DF+1.0)*(1+((U0-(1.0-BORDER)*M1*LCO)/(BORDER*M1*LCO))*AMPLI)
        END IF
    END IF

    IF (FACE(5)) THEN
        IF (V0.GT.((1.0-BORDER)*N1*LCO))THEN
            DF=(DF+1.0)*(1+((V0-(1.0-BORDER)*N1*LCO)/(BORDER*N1*LCO))*AMPLI)
        END IF
    END IF

    IF (FACE(6)) THEN
        IF (W0.GT.((1.0-BORDER)*L1*LCO))THEN
            DF=(DF+1.0)*(1+((W0-(1.0-BORDER)*L1*LCO)/(BORDER*L1*LCO))*AMPLI)
        END IF
    END IF

    TDMP1=1.00-DF*DT*0.5
    TDMP2=1.00+DF*DT*0.5

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------