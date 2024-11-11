    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE CONSTIT_LIN
    implicit none
    !
    ! DATA:  01/03/2023
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !  ESTA SUBROTINA � CHAMADA PELA SUBROTINA CABAR PARA MODELAR A RELA��O
    !  CONSTITUTIVA DE CADA BARRA DE ACORDO COM UM DIAGRAMA LINEAR FOR�A VS.
    !  DEFORMA��O.
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I

    !--------------------------------------------------------------------------
    ! CONCIDERANDO FUNCION LINEAL
    !--------------------------------------------------------------------------
    !$omp parallel do private(i) shared(str,dstr,ecomp,etrac,con,force)
    do i=1,nbt
        if (str(i).ge.0.0d00) then
            ! en traccion
            force(1,i)=etrac(1,i)*str(i)+etrac(1,i)*con*dstr(i)
            elemtype(i) = 100
        else
            ! en compresion
            force(1,i)=ecomp(i)*str(i)+ecomp(i)*con*dstr(i)
            elemtype(i) = 110
        end if
    end do
    !$omp end parallel do

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------