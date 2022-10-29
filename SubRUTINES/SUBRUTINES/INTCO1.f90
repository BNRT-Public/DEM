    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE INTCO1 (MX,NX,LX,ST,NB)
    IMPLICIT NONE
    !
    ! DATA:  02/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA CREA LA MATRIZ DE CONECTIVIDAD DE UN ELEMENTO CUBICO
    ! CONECTANDO LOS ELEMENTOS INTERNOS
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,J,K
    INTEGER B
    INTEGER NO

    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER MX,NX,LX,ST,NB

    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I,J,K,NO,B) SHARED(CN,M1,N1,L1,MNL,MX,NX,LX,ST,NB)
    DO K=1,LX
        DO J=1,NX
            DO I=1,MX
                NO=MNL+(K-1)*M1*N1+(J-1)*M1+I
                B=NB+(K-1)*MX*NX+(J-1)*MX+I

                CN(1,B)=NO
                CN(2,B)=NO+ST
            END DO
        END DO
    END DO
    !$OMP END PARALLEL DO
    
    NB=NB+MX*NX*LX
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------