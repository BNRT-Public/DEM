    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE RMECIN (MX,NX,LX,ST,NBA)
    !
    ! DATA:  05/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    ! DEFINE LAS PROPIEDADES MECANICAS DE LAS BARRAS INTERNAS
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,J,K
    INTEGER B,NOI,NOF,NO

    !--------------------------------------------------------------------------
    ! VARIABLES ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER MX,NX,LX,ST
    INTEGER NBA

    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I,J,K,NO,NOI,NOF,B) SHARED(MNL,M1,N1,L1,MX,NX,LX,ST,EP,ER,EM,KR,REET,REYNG,ETRAC,ECOMP,LCR,LI,NBA)
    DO K=1,LX
        DO J=1,NX
            DO I=1,MX
                NO=MNL+(K-1)*M1*N1+(J-1)*M1+I
                NOI=NO
                NOF=NO+ST
                B=NBA+(K-1)*MX*NX+(J-1)*MX+I

                CALL RANDMEC (B,0)

            END DO
        END DO
    END DO
    !$OMP END PARALLEL DO

    NBA=NBA+MX*NX*LX

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------