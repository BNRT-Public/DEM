    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!
    !
    SUBROUTINE EXTCO1 (MX,NX,LX,C1,C2,C3,ST,NB)
    IMPLICIT NONE
    !
    ! DATA:  14/11/2022
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA CREA LA MATRIZ DE CONECTIVIDAD DE UN ELEMENTO CUBICO
    ! CONECTANDO LOS ELEMENTOS EXTERIORES
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,J,K
    INTEGER B, NO
    INTEGER TYPES

    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER MX,NX,LX
    INTEGER ST,C1,C2,C3,NB

    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I,J,K,NO,B) SHARED(CN,M,N,L,MX,NX,LX,ST,NB)
    DO K=1,LX
        DO J=1,NX
            DO I=1,MX
                TYPES=0
                IF ((I.EQ.1).OR.(I.EQ.M)) TYPES=TYPES+C1
                IF ((J.EQ.1).OR.(J.EQ.N)) TYPES=TYPES+C2
                IF ((K.EQ.1).OR.(K.EQ.L)) TYPES=TYPES+C3
                                
                NO=(K-1)*M*N+(J-1)*M+I
                B=NB+(K-1)*MX*NX+(J-1)*MX+I

                CN(1,B)=NO
                CN(2,B)=NO+ST
                ELEMTYPE(B) = TYPES
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