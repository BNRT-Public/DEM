    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE WRIBARDAMAGED(IFILE)
    !
    ! DATA:  06/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA ESCRIBE LAS BARRAS DANADAS EN EL ARCHIVO BARDAMAGED_Txx.DAT
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER B, NOI, NOF
    INTEGER NPRINT
    
    !--------------------------------------------------------------------------
    ! VARIABLES ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER IFILE

    !--------------------------------------------------------------------------
    IF (OU10) 12,12,11
11  IF ((STEP.EQ.0).OR.(PR09.EQ.OU10)) THEN
        PR10=1
        DO B=1,NBT
            IF((DM(1,B)).AND.(.NOT.DMOLD(B))) THEN
                DMOLD(B)=.TRUE.
                NOI=CN(1,B)
                NOF=CN(2,B)

                SELECT CASE (LAW)
                CASE (2,4) ! LEY BILINEAL
                    NPRINT = 12
                    WRITE (IFILE,10) TIME,B,&
                        &(U(2,NOI)+U(2,NOF))*0.5,&
                        &(V(2,NOI)+V(2,NOF))*0.5,&
                        &(W(2,NOI)+W(2,NOF))*0.5,&
                        &U0(NOI),V0(NOI),W0(NOI),&
                        &U0(NOF),V0(NOF),W0(NOF),EP(1,B)
                CASE (3) ! LEY TRILINEAL
                    NPRINT = 13
                    WRITE (IFILE,10) TIME,B,&
                        &(U(2,NOI)+U(2,NOF))*0.5,&
                        &(V(2,NOI)+V(2,NOF))*0.5,&
                        &(W(2,NOI)+W(2,NOF))*0.5,&
                        &U0(NOI),V0(NOI),W0(NOI),&
                        &U0(NOF),V0(NOF),W0(NOF),EP(1,B),EP(2,B)
                END SELECT

10              FORMAT((E15.8),',',(3X,I15),',',<NPRINT>(3X,E15.8,','))
            END IF
        END DO
    ELSE
        PR10=PR10+1
    END IF

12  CONTINUE

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------