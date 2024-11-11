    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE RESIZESYM(ANT)
    IMPLICIT NONE
    !
    ! DATA:  05/02/2020
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! SUBRUTINA QUE REDIMENCIONA LAS VARIABLES U0 Y CN
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: UTEMP ! POSICION NODO
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) :: CTEMP ! CONECTIVIDAD BARRA
    INTEGER NI

    !--------------------------------------------------------------------------
    ! VARIABLES ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER ANT ! CANTIDAD A AGRANDAR MATRIZ
    
    !--------------------------------------------------------------------------
    ! APLICAR LAS CONDICIONES DE BORDE PARA CADA CASO
    !--------------------------------------------------------------------------
    NI = SIZE(U0)    
    ALLOCATE(UTEMP(NNT))
    ! DESPLAZAMIENTO U
    UTEMP=U0
    DEALLOCATE(U0)
    ALLOCATE(U0(NNT+ANT))
    U0(1:NI)=UTEMP
    ! DESPLAZAMIENTO V
    UTEMP=V0
    DEALLOCATE(V0)
    ALLOCATE(V0(NNT+ANT))
    V0(1:NI)=UTEMP
    ! DESPLAZAMIENTO W
    UTEMP=W0
    DEALLOCATE(W0)
    ALLOCATE(W0(NNT+ANT))
    W0(1:NI)=UTEMP
    DEALLOCATE(UTEMP)

    ! CONECTIVIDAD
    NI = SIZE(CN,2)
    ALLOCATE(CTEMP(2,NBT))
    CTEMP=CN
    DEALLOCATE(CN)
    ALLOCATE(CN(2,NBT+ANT))
    CN(:,1:NI)=CTEMP
    DEALLOCATE(CTEMP)
    
    RETURN
    END
