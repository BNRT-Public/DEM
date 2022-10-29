    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    FUNCTION SPECTRALYNG (KI1,KJ2,KK3,YNG,CVYNG,BYNG)
    !     ====================
    !
    !  ESTA FUN��O AVALIA A FUN��O DENSIDADE ESPECTRAL DE POT�NCIA.
    !
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    !
    DOUBLE PRECISION SPECTRALYNG,KI1,KJ2,KK3
    DOUBLE PRECISION PI,YNG,DESVYNG,CVYNG,BYNG
    !--------------------------------------------------------------------------
    !
    !     COEF DE VARIA��O = DESVIO PADR�O / MEDIA ===> DESV = CV * MEDIA
    !     PARA N�O OCORRER VALORES NEGATIVOS, CV DEVE SER NO M�XIMO 0,28.
    !
    !--------------------------------------------------------------------------
    PI  = 4.0D00*DATAN(1.0D00)
    DESVYNG = CVYNG*YNG
    !
    IF ((KI1.EQ.0.0D00).OR.(KJ2.EQ.0.0D00).OR.(KK3.EQ.0.0D00)) THEN
        SPECTRALYNG = 0.0D00
    ELSE
        SPECTRALYNG = (DESVYNG**2.0)*(BYNG**3.0/(8.0*PI**(3.0D0/2.0D0)))* &
            &      DEXP(-(BYNG*KI1/2)**2.0-(BYNG*KJ2/2)**2.0-(BYNG*KK3/2)**2.0)
    END IF
    !
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------