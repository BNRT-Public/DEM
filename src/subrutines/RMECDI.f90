    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE RMECDI(NBA)
    !
    ! DATA:  05/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    ! DEFINE LAS PROPIEDADES MECANICAS DE LAS BARRAS DIAGONALES
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,J,K,G
    INTEGER B
    INTEGER NOI,NOF,NOFF, REF
    INTEGER ST(8)
    
    !--------------------------------------------------------------------------
    ! VARIABLES ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER NBA

    !--------------------------------------------------------------------------
    REF=NBA
    ST(1)=0
    ST(2)=1
    ST(3)=M
    ST(4)=M+1
    ST(5)=N*M
    ST(6)=N*M+1
    ST(7)=N*M+M
    ST(8)=N*M+M+1

    !$OMP PARALLEL DO PRIVATE(I,J,K,G,NOI,NOF,B) SHARED(MNL,M1,N1,L1,REF,ST,EP,ER,EM,KR,REET,REYNG,ETRAC,ECOMP,LCR,LI,NBA)
    DO K=1,L1
        DO J=1,N1
            DO I=1,M1
                NOI=(K-1)*M1*N1+(J-1)*M1+I
                NOF=(K-1)*M*N+(J-1)*M+I
                B=REF+(NOI-1)*8
                NOI=NOI+MNL
                DO G=1,8
                    B=B+1
                     !NOFF=NOF+ST(G)

                    CALL RANDMEC (B,3)
                END DO
                NBA=NBA+8
            END DO
        END DO
    END DO
    !$OMP END PARALLEL DO

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------