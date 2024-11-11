    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE RMEC
    !
    ! DATA:  17/02/2023
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER NBA,I
    DOUBLE PRECISION FEP1

    !--------------------------------------------------------------------------
    ! Propriedades de los elementos
    !--------------------------------------------------------------------------
    SELECT CASE (MODELTYPE)
    CASE ('DEM') ! MODEL DEM
        !--------------------------------------------------------------------------
        NBA=0
        !    RMECEX (MX,NX,LX,C1,C2,C3, ST,NBA)
        CALL RMECEX (M1, N, L, 0, 1, 1,  1,NBA)
        CALL RMECEX (M, N1, L, 1, 0, 1,  M,NBA)
        CALL RMECEX (M,  N,L1, 1, 1, 0,M*N,NBA)

        CALL RMECIN (M2,N1,L1,1,NBA)
        CALL RMECIN (M1,N2,L1,M1,NBA)
        CALL RMECIN (M1,N1,L2,M1*N1,NBA)

        CALL RMECDI(NBA)

        !--------------------------------------------------------------------------
        ! CALCULO DE EPs PARA CADA BARRA
        !--------------------------------------------------------------------------
        !$OMP PARALLEL DO PRIVATE(I,FEP1) SHARED(EP,ER,KR,REET,REYNG,ETRAC,ECOMP)
        DO I=1,NBT
            SELECT CASE (LAW)
            CASE (2,4) ! LEY BILINEAL
                ER(I)=KR(1,I)*EP(1,I)
            CASE (3) ! LEY TRILINEAL
                KR(1,I)=KR(1,I)+1-REET-REET*REYNG+REYNG
                KR(1,I)=KR(1,I)/(1-REET*REYNG+REYNG)
                ER(I)=KR(1,I)*EP(1,I)
                EP(2,I)=REET*EP(1,I)
            
                FEP1=ECOMP(I)*EP(1,I)*(1-REYNG*REET+REYNG)
            
                 IF (EP(2,I).GT.ER(I)) THEN
                    !$OMP CRITICAL
                    WRITE(*,*) 'ERROR: ep1 > er'
                    WRITE(*,*) 'REET:',REET
                    WRITE(*,*) 'REYNG:',REYNG
                    WRITE(*,*) 'ECOMP:',ECOMP(I)
                    WRITE(*,*) 'EP:',EP(1,I)
                    WRITE(*,*) 'EP:',EP(2,I)
                    WRITE(*,*) 'ER:',ER(I)
                    PAUSE
                    STOP
                    !$OMP END CRITICAL
                END IF
                IF (FEP1.LT.0.0) THEN
                    !$OMP CRITICAL
                    WRITE(*,*) 'ERROR: F(ep'')<0:',FEP1
                    WRITE(*,*) 'REET:',REET
                    WRITE(*,*) 'REYNG:',REYNG
                    WRITE(*,*) 'ECOMP:',ECOMP(I)
                    WRITE(*,*) 'EP:',EP(1,I)
                    WRITE(*,*) 'ER:',ER(I)
                    PAUSE
                    STOP
                    !$OMP END CRITICAL
                END IF
            END SELECT
        END DO
        !$OMP END PARALLEL DO
    CASE ('PRO') ! MODEL PROTEIN
        !$OMP PARALLEL DO PRIVATE(I,FEP1) SHARED(EP,ER,KR,REET,REYNG,ETRAC,ECOMP)
        DO I=1,NBT
            SELECT CASE (LAW)
            CASE (2,4) ! LEY BILINEAL
                ER(I)=KR(1,I)*EP(1,I)
            CASE (3) ! LEY TRILINEAL
                ER(I)=KR(1,I)*EP(1,I)
            END SELECT
        END DO
        !$OMP END PARALLEL DO
        
    END SELECT
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------