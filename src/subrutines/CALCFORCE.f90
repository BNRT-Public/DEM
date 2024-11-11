    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE CALCFORCE(B,FORCE)
    !
    ! DATA:  06/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !  ESTA SUBROTINA CALCULA LOS VALORES DE FUERZA INTERNA DEL MODELO.
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION :: DI ! PORCENTAJE DE LA FUERZA EN LA DIRECCION I
    DOUBLE PRECISION :: LBAR ! LONGITUD DE LA BARRA

    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER B
    DOUBLE PRECISION :: FORCE

    !--------------------------------------------------------------------------
    ! LONGITUD DE LA BARRA EN EL TIEMPO ACTUAL
    LBAR=(U(2,CN(2,B))-U(2,CN(1,B)))**2.0
    LBAR=(V(2,CN(2,B))-V(2,CN(1,B)))**2.0+LBAR
    LBAR=(W(2,CN(2,B))-W(2,CN(1,B)))**2.0+LBAR
    LBAR=DSQRT(LBAR)

    ! CALCULO DE FUERZAS EN X DE LA BARRA
    DI=(U(2,CN(2,B))-U(2,CN(1,B)))/LBAR
    FRX(CN(1,B))=FRX(CN(1,B))+FORCE*DI
    FRX(CN(2,B))=FRX(CN(2,B))-FORCE*DI

    ! CALCULO DE FUERZAS EN Y DE LA BARRA
    DI=(V(2,CN(2,B))-V(2,CN(1,B)))/LBAR
    FRY(CN(1,B))=FRY(CN(1,B))+FORCE*DI
    FRY(CN(2,B))=FRY(CN(2,B))-FORCE*DI

    ! CALCULO DE FUERZAS EN Z DE LA BARRA
    DI=(W(2,CN(2,B))-W(2,CN(1,B)))/LBAR
    FRZ(CN(1,B))=FRZ(CN(1,B))+FORCE*DI
    FRZ(CN(2,B))=FRZ(CN(2,B))-FORCE*DI

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------