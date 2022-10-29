    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE STATISTICS
    !
    ! DATA:  02/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA TRANSFORMA UN VALOR MEDIO EN SUS COEFICIENTE DE VARIACION
    ! EN LOS PARAMETROS DE WEIBULL EQUIVALENTES
    !
    !--------------------------------------------------------------------------
    IF (CVGFR.GE.0.04) THEN
        CALL FPROB (CVGFR,GMA,BTA)
    ELSE
        GMA=0.0
        BTA=0.0
    END IF

    RETURN
    END
    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------