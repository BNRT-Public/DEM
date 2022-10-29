    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE FPROB (CV,GM,BT)
    !
    ! DATA:  02/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA TRANSFORMA UN VALOR MEDIO EN SUS COEFICIENTE DE VARIACION
    ! EN LOS PARAMETROS DE WEIBULL EQUIVALENTES
    !
    !--------------------------------------------------------------------------
    DOUBLE PRECISION CV,GM,BT,DG,DE
    REAL GAMA
    INTEGER ER

    !--------------------------------------------------------------------------
    GM=35.0D00
    DG=0.1D00

10  GM=GM-DG
    DE=CV*CV+1.0D0-GAMA(1.0D0+2.0D0/GM)/GAMA(1.0D0+1.0D0/GM)**2.0D0
    ER=INT(10000*DE)

    IF (ER) 20,30,10

20  GM=GM+DG
    DG=DG*0.5
    GO TO 10

30  BT=GAMA(1.0D00+1.0D00/GM)

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------