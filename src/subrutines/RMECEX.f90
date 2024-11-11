    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE RMECEX (MX,NX,LX,C1,C2,C3,ST,NBA)
    !
    ! DATA:  05/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    ! DEFINE LAS PROPIEDADES MECANICAS DE LAS BARRAS EXTERNAS
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,J,K
    INTEGER B,TYPES

    !--------------------------------------------------------------------------
    ! VARIABLES ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER MX,NX,LX
    INTEGER C1,C2,C3,ST
    INTEGER NBA
    
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I,J,K,TYPES,B) SHARED(M,N,L,MX,NX,LX,C1,C2,C3,EP,ER,EM,KR,REET,REYNG,ETRAC,ECOMP,LCR,LI,NBA)
    DO K=1,LX
        DO J=1,NX
            DO I=1,MX
                TYPES=0
                IF ((I.EQ.1).OR.(I.EQ.M)) TYPES=TYPES+C1
                IF ((J.EQ.1).OR.(J.EQ.N)) TYPES=TYPES+C2
                IF ((K.EQ.1).OR.(K.EQ.L)) TYPES=TYPES+C3

                B=NBA+(K-1)*MX*NX+(J-1)*MX+I

                CALL RANDMEC (B,TYPES)
                
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