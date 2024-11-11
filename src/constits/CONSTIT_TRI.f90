    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE CONSTIT_TRI
    !
    ! DATA:  06/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !  ESTA SUBROTINA CALCULA LAS FUERZAS EN CADA BARRA CON LA LEY TRILINEAL,
    !   SIN CONSIDERAR AMORTIGUAMIENTO DENTRO DE LA LEY.
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION :: AUX ! VERIFICACION DE ESTADOS
    INTEGER I

    !--------------------------------------------------------------------------
    ! LEY TRILINEAL SIN AMORTIGUAMIENTO EN LA LETY CONSTITUTIVA
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I) SHARED(STR,DSTR,EM,EP,KR,ECOMP,ETRAC,CON,FS,DM,FORCE,REET,REYNG)
    DO I=1,NBT
        IF (STR(I).LE.0.0) THEN
            ! EN COMPRESION
10          FORCE(1,I)=ECOMP(I)*STR(I)!+ECOMP(I)*CON*DSTR(I)
            elemtype(i) = 110
            GO TO 100

        ELSEIF (FS(I)) THEN
            ! BARRA ROTA (e>er)
20          ETRAC(:,I)=0.0
            FORCE(1,I)=0.0
            EM(1,I)=ER(I)
            FS(I)=.TRUE.
            DM(:,I)=.TRUE.
            elemtype(i) = 300
            GO TO 100

        ELSEIF (DM(2,I)) THEN
            ! BARRA DANADA EN TRAMO 3
            IF (STR(I).LE.EM(1,I)) THEN
                ! EN TRACCION
30              FORCE(1,I)=ETRAC(1,I)*STR(I)!+ETRAC(I)*CON*DSTR(I)
                elemtype(i) = 100
                GO TO 100
            ELSEIF (STR(I).GE.ER(I)) THEN
                ! BARRA ROTA
                GO TO 20
            ELSE
                ! CAMBIA E EN EL TRAMO 3 (ep1<e<er)
40              AUX = (1-REET*REYNG+REYNG)/(KR(1,I)-REET)
                FORCE(1,I)=ECOMP(I)*EP(1,I)*(1-REYNG*REET+REYNG)-(STR(I)-REET*EP(1,I))*AUX*ECOMP(I)
                ETRAC(:,I)=FORCE(1,I)/STR(I)
                EM(1,I)=STR(I)
                DM(:,I)=.TRUE.
                elemtype(i) = 200
                GO TO 100
            END IF

        ELSEIF (DM(1,I)) THEN
            ! BARRA DANADA EN TRAMO 2
            IF (STR(I).LE.EM(1,I)) THEN
                ! EN TRACCION
                GO TO 30
            ELSEIF (STR(I).GE.ER(I)) THEN
                ! BARRA ROTA (e>er)
                GO TO 20
            ELSEIF (STR(I).GE.EP(2,I)) THEN
                ! CAMBIANDO E EN EL TRAMO 3 (ep1<e<er)
                GO TO 40
            ELSE
                ! CAMBIANDO E EN EL TRAMO 2 (ep<e<ep1)
50              FORCE(1,I)=ECOMP(I)*(EP(1,I)*(1+REYNG)-STR(I)*REYNG)
                ETRAC(:,I)=FORCE(1,I)/STR(I)
                EM(1,I)=STR(I)
                DM(1,I)=.TRUE.
                elemtype(i) = 200
                GO TO 100
            END IF

        ELSE
            ! BARRA SIN DANO
            IF (STR(I).GE.ER(I)) THEN
                ! BARRA ROTA (e>er)
                GO TO 20
            ELSEIF (STR(I).GE.EP(2,I)) THEN
                ! BARRA CAMBIANDO E EN EL TRAMO 3 (ep1<e<er)
                GO TO 40
            ELSEIF (STR(I).GE.EP(1,I)) THEN
                ! BARRA CAMBIANDO E EN E TRAMO 2 (ep<e<ep1)
                GO TO 50
            ELSE
                ! BARRA SIN DANO EN TRACCION
                GO TO 30
            END IF
        END IF

100     CONTINUE
    END DO
    !$OMP END PARALLEL DO

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------