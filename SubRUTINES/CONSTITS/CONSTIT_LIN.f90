    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE CONSTIT_LIN
    !
    ! DATA:  14/09/2022
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    !  ESTA SUBROTINA É CHAMADA PELA SUBROTINA CABAR PARA MODELAR A RELAÇÃO
    !  CONSTITUTIVA DE CADA BARRA DE ACORDO COM UM DIAGRAMA LINEAR FORÇA VS.
    !  DEFORMAÇÃO.
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION :: AUX ! VERIFICACION DE ESTADOS
    INTEGER I

    !--------------------------------------------------------------------------
    ! CONCIDERANDO FUNCION LINEAL
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I) SHARED(STR,DSTR,EM,EP,KR,ECOMP,ETRAC,CON,FS,DM,FORCE)
    DO I=1,NBT
        FORCE(I)=ECOMP(I)*STR(I)
    END DO
    !$OMP END PARALLEL DO

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------