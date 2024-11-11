    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    FUNCTION RAN0(IDUM)
    !
    ! DATA:  02/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    ! VERSION: 0.0
    !
    ! ESTA GENERA NUMEROS ALEATOREOS CON DISTRIBUCION UNIFORME.
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER IA,IM,IQ,IR,MASK,K
    REAL RAN0,AM
    PARAMETER(IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836, &
            & MASK=123459876)
    
    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER IDUM
    
    !--------------------------------------------------------------------------
    IDUM=IEOR(IDUM,MASK)
    K=IDUM/IQ
    IDUM=IA*(IDUM-K*IQ)-IR*K
    IF (IDUM.LT.0) THEN
        IDUM=IDUM+IM
    END IF
    RAN0=AM*IDUM
    IDUM=IEOR(IDUM,MASK)

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------