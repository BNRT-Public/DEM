    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE ENERG
    !
    ! DATA:  02/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    ! CALCULA LOS VALORES DE ENERGIA EN CADA INTERVALO DE TIEMPO:
    ! - ENEX:  TRABAJO EXTERNO
    ! - ENIN:  ENERGIA INTERNA TOTAL
    ! - ENCN:  ENERGIA CINETICA
    ! - ENEL:  ENERGIA ELASTICA TOTAL
    ! - ENDP:  ENERGIA DISIPADA POR AMORTECIMENTO
    ! - ENGD:  ENERGIA DISIPADA (STRAIN SOFTENING)
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    DOUBLE PRECISION TEMP, KL!, FEP,FEP1!, AUX
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: AUX

    REAL INIT, FIN
    DOUBLE PRECISION TF,TI

    !--------------------------------------------------------------------------
    ! INICIO DE VARIABLES
    !-------------------------------------------------------------------------
    ALLOCATE(AUX(1))
    ENCNOLD=ENCN
    ENELOLD=ENEL
    ENGDOLD=ENGD
    ENDPOLD=ENDP
    ENCN=0.0
    ENEL=0.0
    ENGD=0.0
    TEMP=0.0
    AUX(:)=0.0

    !--------------------------------------------------------------------------
    ! CALCULO DE ENERGIA CINETICA ENCN
    ! CALCULO DE ENERGIA DISIPADA POR AMORTIGUAMIENTO ENDP (NO LEY CONSTITUTIVA EST=0)
    !--------------------------------------------------------------------------
    !$OMP PARALLEL DO PRIVATE(I,AUX,DF,DMP1,DMP2) REDUCTION(+:ENCN, TEMP) &
    !$OMP SHARED(M1,N1,L1,U0,V0,W0,U,V,W,VLX,VLY,VLZ,DF1,DF2,MAS,EST,DENCN,STEP,NDAMF,DT,LCO,TIME)
    DO I=1,NNT
        ! ENERGIA CINETICA
        DENCN(1,I) = DENCN(2,I)
        AUX(1) = 0.5*MAS(I)*(VLX(I)**2.0)     ! EN X
        AUX(1) = AUX(1)+0.5*MAS(I)*(VLY(I)**2.0) ! EN Y
        AUX(1) = AUX(1)+0.5*MAS(I)*(VLZ(I)**2.0) ! EN Z
        DENCN(2,I) = AUX(1)
        ENCN=ENCN+AUX(1)

        ! CALCULO DE AMORTIGUAMIENTO PARA DOMINIO INFINITO
        CALL DOMINF(DF1,DF2,STEP,NDAMF,DT,LCO,U0(I),V0(I),W0(I),M1,N1,L1,DF,DMP1,DMP2,TIME)
        
        ! ENERGIA DISIPADA
        TEMP=TEMP+0.5*DF*MAS(I)*VLX(I)*DELTAPOS(1,I) ! EN X
        TEMP=TEMP+0.5*DF*MAS(I)*VLY(I)*DELTAPOS(2,I) ! EN Y
        TEMP=TEMP+0.5*DF*MAS(I)*VLZ(I)*DELTAPOS(3,I) ! EN Z
    END DO
    !$OMP END PARALLEL DO
    ENDP=ENDP+TEMP

    !--------------------------------------------------------------------------
    ! CALCULO DE ENERGIA ELASTICA
    ! CALCULO DE ENERGIA DISIPADA POR AMORTIGUAMIENTO ENDP (LEY CONSTITUTIVA EST=1)
    !--------------------------------------------------------------------------
    TEMP=0.0
    !$OMP PARALLEL DO PRIVATE(I,KL) SHARED (ECOMP,ETRAC,CON,STR,DSTR,DT,LI,U,V,W,FRX,FRY,FRZ) REDUCTION(+:ENEL) REDUCTION(+:TEMP)
    DO I=1,NBT
        ! ENERGIA ELASTICA
        IF (STR(I).GE.0.0) THEN
            ENEL=ENEL+ETRAC(1,I)*((STR(I)**2.0)*LI(I)*0.5)
        ELSE
            ENEL=ENEL+ECOMP(I)*((STR(I)**2.0)*LI(I)*0.5)
        END IF

        ! ENERGIA DICIPADA CUANDO ESTA CONCIDERADA DENTRO DE LA ECUACION CONSTITUTIVA
        IF (EST.EQ.1) THEN
            KL=0.5
            ! IF ((I.GT.NBA).AND.(I.LE.NBS)) KL=0.25 PREGUNTAR
            IF (STR(I).LT.0.0D00) THEN
                TEMP=TEMP+2.0*(ECOMP(I)*CON*DSTR(I)*DSTR(I)*DT*LI(I)*KL)
            ELSE
                TEMP=TEMP+2.0*(ETRAC(1,I)*CON*DSTR(I)*DSTR(I)*DT*LI(I)*KL)
            END IF
        END IF

    END DO
    !$OMP END PARALLEL DO
    ENDP=ENDP+TEMP

    !--------------------------------------------------------------------------
    ! CALCULO DE ENERGIA DISIPADA POR RELAJACION (STRAIN SOFTENING) ENGD
    ! CALCULO DE ENERGIA DISIPADA POR LAS BARRAS ROTAS DEN
    !--------------------------------------------------------------------------
    TEMP=0.0
    SELECT CASE (LAW)
    CASE (2) ! LEY BILINEAL
        !$OMP PARALLEL DO PRIVATE(I,TEMP) SHARED (EP,KR,LI,EM,ETRAC,ECOMP) REDUCTION(+:ENGD)
        DO I=1,NBT
            ! ENERGIA POR RELAJACION ENGD
            IF (DM(1,I)) THEN
                TEMP=ECOMP(I)*(EP(1,I)**2.0)
                TEMP=TEMP-ETRAC(1,I)*(EM(1,I)**2.0)
                TEMP=TEMP+(ECOMP(I)*EP(1,I)+ETRAC(1,I)*EM(1,I))*(EM(1,I)-EP(1,I))
                !DANO(I)=TEMP/(ECOMP(I)*EP(I,1)*EP(I,1)*KR(1,I))
                ENGDB(I)=LI(I)*TEMP*0.5
            END IF
        END DO
        !$OMP END PARALLEL DO
        ENGD = SUM(ENGDB)

    CASE (3) ! LEY TRILINEAL
        DEALLOCATE(AUX)
        ALLOCATE(AUX(3))
        !$OMP PARALLEL DO PRIVATE(I,TEMP,AUX) SHARED (EP,ER,KR,LI,EM,ETRAC,ECOMP,REET,REYNG) REDUCTION(+:ENGD)
        DO I=1,NBT
            TEMP=0.0
            AUX(:)=0.0
            AUX(1)=ECOMP(I)*EP(1,I)
            AUX(2)=AUX(1)+ECOMP(I)*REYNG*(EP(2,I)-EP(1,I))
            AUX(3) = ETRAC(1,I)*EM(1,I)
            IF (FS(I))THEN ! (e>er)
                TEMP=AUX(1)*EP(1,I)+((EP(2,I)-EP(1,I))*(AUX(1)+AUX(2)))+((ER(I)-EP(2,I))*AUX(2))
            ELSEIF (DM(2,I))THEN ! (ep1<e<er)
                TEMP=AUX(1)*EP(1,I)+((EP(2,I)-EP(1,I))*(AUX(1)+AUX(2)))+((EM(1,I)-EP(2,I))*(AUX(2)+AUX(3)))-(AUX(3)*EM(1,I))
            ELSEIF (DM(1,I))THEN ! (ep<e<ep1)
                TEMP=AUX(1)*EP(1,I)+((EM(1,I)-EP(1,I))*(AUX(1)+AUX(3)))-AUX(3)*EM(1,I)
            END IF
            ENGDB(I)=LI(I)*TEMP*0.5
        END DO
        !$OMP END PARALLEL DO
        ENGD = SUM(ENGDB)

    CASE (4) ! LEY BILINEAL SHEAR
        !$OMP PARALLEL DO PRIVATE(I,TEMP) SHARED (EP,LI,STR,STROLD,ETRAC,ENGDB)
        DO I=1,NBT
            TEMP=0.0
            IF (ETRAC(1,I).NE.ETRAC(2,I)) THEN
                ! Primer dano
                IF (ETRAC(2,I).EQ.ECOMP(I)) THEN
                    TEMP=(ETRAC(2,I)-ETRAC(1,I))*EP(1,I)*STR(I)
                ELSE
                    TEMP=(ETRAC(2,I)-ETRAC(1,I))*STROLD(1,I)*STR(I)
                ENDIF
                ENGDB(I)=ENGDB(I)+TEMP*0.5*LI(I)
            END IF
        END DO
        !$OMP END PARALLEL DO
        ENGD = SUM(ENGDB)
        
    CASE (5) ! LEY BI-LINEAR EXPONENCIAL
    END SELECT

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------