    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE INTPOLO(TYPES,IPT,UU,PHIVR)!(LONG,MODEL,NPT,LCORX,LCORY,LCORZ,PP,IPT,PHIR,PHIVR)
    IMPLICIT NONE
    !
    ! DATA:  06/02/2020
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA CALCULA LOS VALORES ALEATORIOS SEGUN LOS
    ! POLOS POR UNA INTERPOLACION LINEAL
    !
    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER TYPES !1: SI ES GFR; 2: SI ES YNG; 3: SI ES RHO
    INTEGER I ! CONTADOR DE BARRAS
    INTEGER IPT(8) ! IND DE POSICION DE POLOS 
    
    DOUBLE PRECISION UU(3,1) ! COORDENADA ROTADA
    DOUBLE PRECISION PHIVR ! RESULTADO DE VALOR ALEATOREO
    
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION UB,VB,WB,PP(3) ! POSICION
    DOUBLE PRECISION X,Y,Z ! DIFERENCIA CON POLO INICIAL
    DOUBLE PRECISION CONJ(8) ! CLACULO DE VALOR ALEATOREO POR PARTE
    DOUBLE PRECISION LCORX,LCORY,LCORZ ! LONG DE COORELLACION
    DOUBLE PRECISION PHIR(8) ! VALORES ALEATOREOS EN LOS POLOS
    
        
    !--------------------------------------------------------------------------
    ! ASIGNACION DE VARIABLES
    !--------------------------------------------------------------------------
    SELECT CASE (TYPES)
    CASE (1) ! POLOS DE GFR
        LCORX=LCXGFR
        LCORY=LCYGFR
        LCORZ=LCZGFR
        PP(1)=PPGFR(IPT(1),1)
        PP(2)=PPGFR(IPT(1),2)
        PP(3)=PPGFR(IPT(1),3)
        PHIR=PHIGFR(IPT)
    CASE (2) ! POLOS DE YNG
        LCORX=LCXYNG
        LCORY=LCYYNG
        LCORZ=LCZYNG
        PP(1)=PPYNG(IPT(1),1)
        PP(2)=PPYNG(IPT(1),2)
        PP(3)=PPYNG(IPT(1),3)
        PHIR=PHIYNG(IPT)
    CASE (3) ! POLOS DE RHO
        LCORX=LCXROH
        LCORY=LCYROH
        LCORZ=LCZROH
        PP(1)=PPROH(IPT(1),1)
        PP(2)=PPROH(IPT(1),2)
        PP(3)=PPROH(IPT(1),3)
        PHIR=PHIROH(IPT)
    END SELECT
    
    !--------------------------------------------------------------------------
    ! INTERPOLACION LINEAL
    !--------------------------------------------------------------------------
    X=(UU(1,1)-PP(1))
    Y=(UU(2,1)-PP(2))
    Z=(UU(3,1)-PP(3))

    CONJ(1) = PHIR(1)
    CONJ(2) = ((PHIR(2)-PHIR(1))/LCORX)*X
    CONJ(3) = ((PHIR(3)-PHIR(1))/LCORY)*Y
    CONJ(4) = ((PHIR(5)-PHIR(1))/LCORZ)*Z
    CONJ(5) = ((PHIR(4)-PHIR(3)-PHIR(2)+PHIR(1))/(LCORX*LCORY))*X*Y
    CONJ(6) = ((PHIR(6)-PHIR(5)-PHIR(2)+PHIR(1))/(LCORX*LCORZ))*X*Z
    CONJ(7) = ((PHIR(7)-PHIR(5)-PHIR(3)+PHIR(1))/(LCORY*LCORZ))*Y*Z
    CONJ(8) = PHIR(8)-PHIR(7)-PHIR(6)+PHIR(5)-PHIR(4)
    CONJ(8) = CONJ(8) + PHIR(3)+PHIR(2)-PHIR(1)
    CONJ(8) = CONJ(8)/(LCORX*LCORY*LCORZ)
    CONJ(8) = CONJ(8)*X*Y*Z

    PHIVR=SUM(CONJ)

    IF (ISNAN(PHIVR).OR.(PHIVR.LE.0.0))THEN
        !$OMP CRITICAL
        WRITE(*,*) 'VALOR:',PHIVR
        WRITE(*,*) 'ERROR: VALUE GFR AND/OR YNG'
        WRITE(*,*) 'DO YOU WISH TO CONTINUE?'
        PAUSE
        STOP
        !$OMP END CRITICAL
    END IF

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------