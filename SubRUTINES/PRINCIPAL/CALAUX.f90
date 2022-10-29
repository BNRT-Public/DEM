    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE CALAUX
    !
    ! DATA:  02/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    ! CALCULO DE LONGITUD Y COORDENADAS DE CADA BARRA
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    DOUBLE PRECISION :: DU,DV,DW
    
    !--------------------------------------------------------------------------
    ! ASIGNA TAMAÑO DE LAS VARIABLES
    !--------------------------------------------------------------------------
    ! POSICION/VELOCIDAD/ACELERACION
    ALLOCATE(U(3,NNT),V(3,NNT),W(3,NNT))
    ALLOCATE(DELTAPOS(3,NNT))
    ALLOCATE(VLX(NNT),VLY(NNT),VLZ(NNT))
    ALLOCATE(ACX(NNT),ACY(NNT),ACZ(NNT))
    ALLOCATE(FRX(NNT),FRY(NNT),FRZ(NNT))
    ! BARRA
    ALLOCATE(LI(NBT))
    ALLOCATE(XB(NBT),YB(NBT),ZB(NBT))
    ALLOCATE(XB0(NBT),YB0(NBT),ZB0(NBT))
    ALLOCATE(ENR(NBT))
    ALLOCATE(EDG(NBT))
    ALLOCATE(LCR(NBT))
    ALLOCATE(STR(NBT))
    ALLOCATE(DSTR(NBT))
    ALLOCATE(ER(NBT))
    ALLOCATE(YNGB(NBT))
    ALLOCATE(GFRB(NBT))
    ALLOCATE(LBAR(NBT))
    
    ALLOCATE(FS(NBT))
    ALLOCATE(FSOLD(NBT))
    ALLOCATE(DMOLD(NBT))
    ! ENERGIA
    ALLOCATE(DENCN(2,NNT))
    ALLOCATE(ENGDB(NBT))

    SELECT CASE (LAW)
    CASE (2) ! LEY BILINEAL
        ALLOCATE(EP(1,NBT))
        ALLOCATE(EM(1,NBT))
        ALLOCATE(DM(1,NBT))
        ALLOCATE(KR(1,NBT))
        ALLOCATE(EPMIN(1),EPMAX(1))
        ALLOCATE(MEAN(1),COEV(1))
        ALLOCATE(FORCE(1,NBT))
        ALLOCATE(ECOMP(NBT))
        ALLOCATE(ETRAC(1,NBT))
    CASE (3) ! LEY TRILINEAL
        ALLOCATE(EP(2,NBT))
        ALLOCATE(EM(1,NBT))
        ALLOCATE(DM(2,NBT))
        ALLOCATE(KR(1,NBT))
        ALLOCATE(EPMIN(2),EPMAX(2))
        ALLOCATE(MEAN(2),COEV(2))
        ALLOCATE(FORCE(1,NBT))
        ALLOCATE(ECOMP(NBT))
        ALLOCATE(ETRAC(1,NBT))
    CASE (4) ! LEY BILINEAL SHEAR
        ALLOCATE(EP(1,NBT))
        ALLOCATE(EM(1,NBT))
        ALLOCATE(DM(1,NBT))
        ALLOCATE(KR(2,NBT))
        ALLOCATE(EPMIN(1),EPMAX(1))
        ALLOCATE(MEAN(1),COEV(1))
        ALLOCATE(FORCE(1,NBT))
        ALLOCATE(ECOMP(NBT))
        ALLOCATE(ETRAC(2,NBT))
        ALLOCATE(STROLD(1,NBT))
    CASE DEFAULT
        WRITE(*,*) 'ERROR: SELECTED LAW NOT ALLOWED'
        PAUSE
        STOP
    END SELECT
    
    !--------------------------------------------------------------------------
    ! ASIGNA VALORES POR DEFECTO A VARIABLES
    !--------------------------------------------------------------------------
    ! MATERIAL
    YNGB=YNG
    GFRB=GFR
    ! BARRA
    EM=0.0
    FS=.FALSE.
    FSOLD=.FALSE.
    DM=.FALSE.
    DMOLD=.FALSE.
    STR=0.0
    IF (ALLOCATED(STROLD)) STROLD=0.0
    ! FUERZA
    FRX=0.0
    FRY=0.0
    FRZ=0.0
    
    !--------------------------------------------------------------------------
    ! CARGA DE VALORES Y CALCULOS DE POSICION DE ELEMENTOS
    !--------------------------------------------------------------------------
    U(1,:)=U0
    V(1,:)=V0
    W(1,:)=W0
    U(2,:)=U0
    V(2,:)=V0
    W(2,:)=W0
    U(3,:)=U0
    V(3,:)=V0
    W(3,:)=W0
    !!$OMP PARALLEL DO PRIVATE(I) SHARED(U,V,W,U0,V0,W0)
    !DO I=1,NNT
    !    U(:,I)=U0(I)
    !    V(:,I)=V0(I)
    !    W(:,I)=W0(I)
    !END DO
    !!$OMP END PARALLEL DO
                
    !$OMP PARALLEL DO PRIVATE(I,DU,DV,DW) SHARED(LI,XB0,YB0,ZB0,XB,YB,ZB,U0,V0,W0,CN)
    DO I=1,NBT
        DU=U0(CN(2,I))-U0(CN(1,I))
        DV=V0(CN(2,I))-V0(CN(1,I))
        DW=W0(CN(2,I))-W0(CN(1,I))

        LI(I)=DSQRT(DU**2.0+DV**2.0+DW**2.0)

        XB0(I)=(U0(CN(2,I))+U0(CN(1,I)))*0.5
        YB0(I)=(V0(CN(2,I))+V0(CN(1,I)))*0.5
        ZB0(I)=(W0(CN(2,I))+W0(CN(1,I)))*0.5
        
        XB(I)=(U(2,CN(2,I))+U(2,CN(1,I)))*0.5
        YB(I)=(V(2,CN(2,I))+V(2,CN(1,I)))*0.5
        ZB(I)=(W(2,CN(2,I))+W(2,CN(1,I)))*0.5
    END DO
    !$OMP END PARALLEL DO

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------