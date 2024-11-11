    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE DIACO1(NB)
    IMPLICIT NONE
    !
    ! DATA:  02/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA CREA LA MATRIZ DE CONECTIVIDAD DE UN ELEMENTO CUBICO
    ! CONECTANDO LOS ELEMENTOS DIAGONALES
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,J,K,G
    INTEGER B
    INTEGER NOI,NOF,REF
    INTEGER ST(8)
    
    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER NB

    !--------------------------------------------------------------------------
   ! REF = M1*L*N+M*N1*L+M*N*L1+M2*N1*L1+M1*N2*L1+M1*N1*L2
    ST(1)=0
    ST(2)=1
    ST(3)=M
    ST(4)=M+1
    ST(5)=N*M
    ST(6)=N*M+1
    ST(7)=N*M+M
    ST(8)=N*M+M+1

    !$OMP PARALLEL DO PRIVATE(I,J,K,G,NOI,NOF,B) SHARED(CN,M1,N1,L1,MNL,ST,NB)
    DO K=1,L1
        DO J=1,N1
            DO I=1,M1
                NOI=(K-1)*M1*N1+(J-1)*M1+I
                NOF=(K-1)*M*N+(J-1)*M+I
                B=NB+(NOI-1)*8
                NOI=NOI+MNL

                DO G=1,8
                    B=B+1
                    CN(1,B)=NOI
                    CN(2,B)=NOF+ST(G)
                END DO

            END DO
        END DO
    END DO
    !$OMP END PARALLEL DO

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------