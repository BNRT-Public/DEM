    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    FUNCTION SPECTRALYNG (KI1,KJ2,KK3,YNG,CVYNG,BYNG)
    !     ====================
    !
    !  ESTA FUNÇÃO AVALIA A FUNÇÃO DENSIDADE ESPECTRAL DE POTÊNCIA.
    !
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    !
    DOUBLE PRECISION SPECTRALYNG,KI1,KJ2,KK3
    DOUBLE PRECISION PI,YNG,DESVYNG,CVYNG,BYNG
    !--------------------------------------------------------------------------
    !
    !     COEF DE VARIAÇÃO = DESVIO PADRÃO / MEDIA ===> DESV = CV * MEDIA
    !     PARA NÃO OCORRER VALORES NEGATIVOS, CV DEVE SER NO MÁXIMO 0,28.
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