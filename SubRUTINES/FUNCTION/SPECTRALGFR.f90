    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    FUNCTION SPECTRALGFR (KI1,KJ2,KK3,GFR,CVGFR,BGFR)
    !     ====================
    !
    !  ESTA FUNÇÃO AVALIA A FUNÇÃO DENSIDADE ESPECTRAL DE POTÊNCIA.
    !
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    !
    DOUBLE PRECISION SPECTRALGFR,KI1,KJ2,KK3
    DOUBLE PRECISION PI,GFR,DESVGFR,CVGFR,BGFR
    !--------------------------------------------------------------------------
    !
    !     COEF DE VARIAÇÃO = DESVIO PADRÃO / MEDIA ===> DESV = CV * MEDIA
    !     PARA NÃO OCORRER VALORES NEGATIVOS, CV DEVE SER NO MÁXIMO 0,28.
    !
    !--------------------------------------------------------------------------
    PI  = 4.0D00*DATAN(1.0D00)
    DESVGFR = CVGFR*GFR
    !
    IF ((KI1.EQ.0.0D00).OR.(KJ2.EQ.0.0D00).OR.(KK3.EQ.0.0D00)) THEN
        SPECTRALGFR = 0.0D00
    ELSE
        SPECTRALGFR = (DESVGFR**2.0)*(BGFR**3.0/(8.0*PI**(3.0D0/2.0D0)))* &
            &      DEXP(-(BGFR*KI1/2)**2.0-(BGFR*KJ2/2)**2.0-(BGFR*KK3/2)**2.0)
    END IF
    !
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------