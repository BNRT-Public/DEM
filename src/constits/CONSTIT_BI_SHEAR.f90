    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE CONSTIT_BI_SHEAR
    !
    ! DATA:  15/10/2022
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !  ESTA SUBROTINA É CHAMADA PELA SUBROTINA CABAR PARA MODELAR A RELAÇÃO
    !  CONSTITUTIVA DE CADA BARRA DE ACORDO COM UM DIAGRAMA BILINEAR FORÇA VS.
    !  DEFORMAÇÃO CON SHEAR, COM AMOLECIMENTO ("STRAIN SOFTENING'). O PROCESSO DE
    !  DESCARGA É MODELADO DE ACORDO COM A HIPÓTESE DE DANO-ELÁSTICO.
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION :: AUX ! VERIFICACION DE ESTADOS
    INTEGER I
    
    LOGICAL,ALLOCATABLE,DIMENSION(:) :: BSEL
    
    !LOGICAL CNTEMP(4,NBT), !BSEL(NBT)
    LOGICAL,ALLOCATABLE,DIMENSION(:) :: STRBCAL
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: STRBSEL
    DOUBLE PRECISION STRCAVG, KRTEMP
    !Integer :: j
    !Integer, Dimension( : ), Allocatable :: indx
    
    !--------------------------------------------------------------------------
    ! CONCIDERANDO EL AMORTIGUAMIENTO EN LA RELACION CONSTITUTIVA: CON > 0.0D00
    !--------------------------------------------------------------------------
    ETRAC(2,:)=ETRAC(1,:) ! Mantiene estado anterior
    STROLD(1,:)=STR(:) ! Mantiene estado anterior
        
    !$OMP PARALLEL DO PRIVATE(I,AUX) SHARED(STR,DSTR,EM,EP,KR,ECOMP,ETRAC,CON,FS,DM,FORCE)
    DO I=1,NBT  
        IF (STR(I).GE.ER(I))  GO TO 30 ! ROTA
        IF (STR(I).GE.EM(1,I))  GO TO 20 ! CON DANO
        IF (STR(I).GE.0.0D00) GO TO 10 ! EN TRACCION

        !--------------------------------------------------------------------------
        ! EN COMPRESION
        FORCE(1,I)=ECOMP(I)*STR(I)+ECOMP(I)*CON*DSTR(I)
        GO TO 40

        !--------------------------------------------------------------------------
        ! EN TRACCION
10      FORCE(1,I)=ETRAC(1,I)*STR(I)+ETRAC(1,I)*CON*DSTR(I)
        GO TO 40

        !--------------------------------------------------------------------------
        ! CON DANO
20      AUX=(STR(I)-EP(1,I))/(KR(2,I)-1.0D00)
        AUX=ECOMP(I)*(EP(1,I)-AUX)
        ETRAC(1,I)=AUX/STR(I)
        EM(1,I)=STR(I)
        FORCE(1,I)=AUX+ETRAC(1,I)*CON*DSTR(I)
        DM(1,I)=.TRUE.
        GO TO 40

        !--------------------------------------------------------------------------
        ! ROTA
30      ETRAC(1,I)=0.0
        FORCE(1,I)=0.0
        EM(1,I)=ER(I)
        FS(I)=.TRUE.
        DM(:,I)=.TRUE.

40      CONTINUE
    END DO
    !$OMP END PARALLEL DO
    
    ALLOCATE(STRBSEL(SIZE(ELEMNTFAM, DIM=1)))
    ALLOCATE(STRBCAL(SIZE(ELEMNTFAM, DIM=1)))    
    ALLOCATE(BSEL(SIZE(ELEMNTFAM, DIM=1)))
    !$OMP PARALLEL DO PRIVATE(I,BSEL,STRBSEL,STRBCAL,STRCAVG,KRTEMP) SHARED(STR,EM,EP,KR,ECOMP,ETRAC,REET)
    DO I=1,NBT
        !--------------------------------------------------------------------------
        ! MODIFICACION POR SHEAR
        IF ((STR(I).GT.0.0).AND.(.NOT.FS(I))) THEN
            STRBSEL = 0.0
            BSEL=.NOT.(ELEMNTFAM(:,I).EQ.0)
            STRBSEL(1:COUNT(BSEL))=STR([PACK(ELEMNTFAM(:,I),BSEL)])
            
            !indx = Pack( [ ( j, j = Lbound( BSEL, Dim = 1 )    , &
            !              Ubound( BSEL, Dim = 1 ) ) ], BSEL )            
            STRBCAL=STRBSEL.LT.0.0D0
            IF (COUNT(STRBCAL).GT.0) THEN
                STRCAVG=SUM(STRBSEL, STRBCAL)/COUNT(STRBCAL)
                IF (EM(1,I).GT.EP(1,I)) THEN
                    KRTEMP=KR(1,I)*(-REET*STRCAVG/EP(1,I))
                    KR(2,I)=MAX(KRTEMP,KR(1,I))
                                        
                    ER(I)=KR(2,I)*EP(1,I)
                    AUX=ECOMP(I)*EP(1,I)-ETRAC(1,I)*EP(1,I)+ETRAC(1,I)*ER(I)
                    EM(1,I) = ECOMP(I)*EP(1,I)*ER(I)/AUX
                ELSE
                    KR(2,I) = KR(1,I)
                    ER(I)=KR(1,I)*EP(1,I)
                END IF
            END IF
        END IF
            
        
    END DO
    !$OMP END PARALLEL DO

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    