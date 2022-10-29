    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE POSPOLO(M,N,L,LCO,LCX,LCY,LCZ,NP,PP)
    IMPLICIT NONE
    !
    ! DATA:  16/09/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! Calcula la posicion de polos para aleatoriedad
    !
    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER M,N,L
    DOUBLE PRECISION LCO, LCX, LCY, LCZ
    INTEGER NP(4)
    DOUBLE PRECISION PP(NP(4),3)
    
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION X0, Y0, Z0
    INTEGER I,J,K,IPT
    
    !--------------------------------------------------------------------------
    X0 = -((NP(1)-1)*LCX*0.5)
    Y0 = -((NP(2)-1)*LCY*0.5)
    Z0 = -((NP(3)-1)*LCZ*0.5)
    
    !$OMP PARALLEL DO PRIVATE(I,J,K,IPT) SHARED(NP,PP,X0,Y0,Z0,LCX,LCY,LCZ)
    DO K=1,NP(3)
        DO J=1,NP(2)
            DO I=1,NP(1)
                IPT = I+(J-1)*NP(1)+(K-1)*NP(1)*NP(2)
                PP(IPT,1)=X0+(I-1)*LCX
                PP(IPT,2)=Y0+(J-1)*LCY
                PP(IPT,3)=Z0+(K-1)*LCZ
            END DO
        END DO
    END DO
    !$OMP END PARALLEL DO
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------