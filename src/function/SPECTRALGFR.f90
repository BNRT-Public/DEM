    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    FUNCTION SPECTRALGFR (KI1,KJ2,KK3,GFR,CVGFR,BGFR)
    !     ====================
    !
    !  ESTA FUN��O AVALIA A FUN��O DENSIDADE ESPECTRAL DE POT�NCIA.
    !
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    !
    DOUBLE PRECISION SPECTRALGFR,KI1,KJ2,KK3
    DOUBLE PRECISION PI,GFR,DESVGFR,CVGFR,BGFR
    !--------------------------------------------------------------------------
    !
    !     COEF DE VARIA��O = DESVIO PADR�O / MEDIA ===> DESV = CV * MEDIA
    !     PARA N�O OCORRER VALORES NEGATIVOS, CV DEVE SER NO M�XIMO 0,28.
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