    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    FUNCTION SPECTRALROH(KI1,KJ2,KK3,ROH,CVROH,BROH)
    !
    ! DATA:  02/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    ! ESTA FUNCION VERIFICA LA DENSIDAD ESPECTRAL DE POTENCIA.
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    DOUBLE PRECISION PI
    DOUBLE PRECISION SPECTRALROH,DESVROH

    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    DOUBLE PRECISION KI1,KJ2,KK3,ROH,CVROH,BROH

    !--------------------------------------------------------------------------
    ! COEF. DE VARIACION = DESVIO PADRON / MEDIA => DESV = CV * MEDIA
    ! PARA QUE NO EXISTAN VALORES NEGATIVOS, CV DEVE SER COMO MAXIMO 0,28.
    !--------------------------------------------------------------------------
    PI  = 4.0*DATAN(1.0D00)
    DESVROH = CVROH*ROH

    IF ((KI1.EQ.0.0).OR.(KJ2.EQ.0.0).OR.(KK3.EQ.0.0)) THEN
        SPECTRALROH = 0.0
    ELSE
        SPECTRALROH = (DESVROH**2.0)*(BROH**3.0/(8.0*PI**(3.0D0/2.0D0)))* &
            & DEXP(-(BROH*KI1/2)**2.0-(BROH*KJ2/2)**2.0-(BROH*KK3/2)**2.0)
    END IF

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------