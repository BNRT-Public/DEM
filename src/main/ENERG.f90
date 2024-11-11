    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE ENERG
    IMPLICIT NONE
    !
    ! DATA:  22/01/2023
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
    DOUBLE PRECISION DFN, DOMINF
    DOUBLE PRECISION TEMP, KL!, FEP,FEP1!, AUX
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: AUX1,AUX2

    REAL INIT, FIN
    DOUBLE PRECISION TF,TI

    !--------------------------------------------------------------------------
    ! INICIO DE VARIABLES
    !-------------------------------------------------------------------------
    DFN=DF
    ENCNOLD=ENCN
    ENELOLD=ENEL
    ENGDOLD=ENGD
    ENDPOLD=ENDP
    ENCN=0.0d0
    ENEL=0.0d0
    ENGD=0.0d0
    TEMP=0.0d0
    
    !--------------------------------------------------------------------------
    ! CALCULO DE ENERGIA CINETICA ENCN
    ! CALCULO DE ENERGIA DISIPADA POR AMORTIGUAMIENTO ENDP (NO LEY CONSTITUTIVA EST=0)
    !--------------------------------------------------------------------------
    DENCN(1,:) = DENCN(2,:)
    ALLOCATE(AUX1(3))
    ALLOCATE(AUX2(3))
    !$OMP PARALLEL DO PRIVATE(I,AUX1,AUX2,DFN) &
    !$OMP& REDUCTION(+:ENCN, TEMP) &
    !$OMP& SHARED(M1,N1,L1,U0,V0,W0,VLX,VLY,VLZ,DELTAPOS,DF,MAS,DENCN,LCO)
    DO I=1,NNT
        ! VELOCIDADES
        AUX1(1) = VLX(I)
        AUX1(2) = VLY(I)
        AUX1(3) = VLZ(I)
        AUX2=AUX1
        
        ! ENERGIA CINETICA
        AUX1=AUX1**2.0d0
        AUX1=AUX1*0.5d0*MAS(I)
        DENCN(2,I) = SUM(AUX1)
        ENCN=ENCN+SUM(AUX1)
        ! CALCULO DE AMORTIGUAMIENTO PARA DOMINIO INFINITO
        DFN = DOMINF(DF,LCO,U0(I),V0(I),W0(I),M1,N1,L1)
        
        ! ENERGIA DISIPADA
        AUX2=AUX2*DELTAPOS(:,I)
        AUX2=AUX2*+0.5*DFN*MAS(I)
        TEMP=TEMP+SUM(AUX2)
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
        DEALLOCATE(AUX1)
        ALLOCATE(AUX1(3))
        !$OMP PARALLEL DO PRIVATE(I,TEMP,AUX1) SHARED (EP,ER,KR,LI,EM,ETRAC,ECOMP,REET,REYNG) REDUCTION(+:ENGD)
        DO I=1,NBT
            TEMP=0.0
            AUX1(:)=0.0
            AUX1(1)=ECOMP(I)*EP(1,I)
            AUX1(2)=AUX1(1)+ECOMP(I)*REYNG*(EP(2,I)-EP(1,I))
            AUX1(3) = ETRAC(1,I)*EM(1,I)
            IF (FS(I))THEN ! (e>er)
                TEMP=AUX1(1)*EP(1,I)+((EP(2,I)-EP(1,I))*(AUX1(1)+AUX1(2)))+((ER(I)-EP(2,I))*AUX1(2))
            ELSEIF (DM(2,I))THEN ! (ep1<e<er)
                TEMP=AUX1(1)*EP(1,I)+((EP(2,I)-EP(1,I))*(AUX1(1)+AUX1(2)))+((EM(1,I)-EP(2,I))*(AUX1(2)+AUX1(3)))-(AUX1(3)*EM(1,I))
            ELSEIF (DM(1,I))THEN ! (ep<e<ep1)
                TEMP=AUX1(1)*EP(1,I)+((EM(1,I)-EP(1,I))*(AUX1(1)+AUX1(3)))-AUX1(3)*EM(1,I)
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