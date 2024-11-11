    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE PRISMEDITGEOM(POS,ROT,DIST,FAEP,FAECOMP,FAETRAC,ISDM,ISFS,etype)
    IMPLICIT NONE
    !
    ! DATA:  17/10/2022
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! Edita la geometria cambiando los parametros del modelo con un prima
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    LOGICAL LOGIC(3,2)
    INTEGER I
    DOUBLE PRECISION UU0(3,1),UU1(3,1)
    DOUBLE PRECISION R(3,3)

    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    DOUBLE PRECISION POS(3,1) ! Posicion del centro de coordenadas
    DOUBLE PRECISION ROT(3) ! Angulos de rotacion en grados
    DOUBLE PRECISION DIST(3,2) ! Distancias hasta el eje (pos/neg)
    DOUBLE PRECISION FAEP
    DOUBLE PRECISION FAECOMP
    DOUBLE PRECISION FAETRAC
    LOGICAL ISDM
    LOGICAL ISFS
    integer etype
   
    !--------------------------------------------------------------------------
    
    R=ROTMAT(ROT)
 
    !$OMP PARALLEL DO PRIVATE(I,LOGIC, UU0, UU1) SHARED(POSB0,POS,R,DIST,ECOMP,ETRAC,EP,DM,FS)
    DO I=1,NBT
        UU0(:,1)=POSB0(:,I)
        UU0 = UU0-POS
        UU1=MATMUL(R,UU0)
            
        LOGIC(:,1)=(UU1(:,1).GE.DIST(:,1))
        LOGIC(:,2)=(UU1(:,1).LE.DIST(:,2))
            
        IF (ALL(LOGIC)) THEN
            ECOMP(I)=ECOMP(I)*FAECOMP
            ETRAC(:,I)=ETRAC(:,I)*FAETRAC
            EP(:,I)=EP(:,I)*FAEP
            er(I) = er(I)*FAEP
            EM(:,I) = EM(:,I)*FAEP
            DM(:,I)=ISDM
            FS(I)=ISFS
            elemtype(i) = etype
        END IF
                
    END DO
    !$OMP END PARALLEL DO
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------