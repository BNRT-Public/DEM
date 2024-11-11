    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE CALAUX
    IMPLICIT NONE
    !
    ! DATA:  17/02/2023
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! CALCULO DE LONGITUD Y COORDENADAS DE CADA BARRA
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    INTEGER NOI,NOF
    DOUBLE PRECISION DIFF(3)
    
    !--------------------------------------------------------------------------
    ! ASIGNA TAMAÑO DE LAS VARIABLES
    !--------------------------------------------------------------------------
    ! POSICION/VELOCIDAD/ACELERACION
    IF (.not.ALLOCATED(U)) ALLOCATE(U(3,NNT))
    IF (.not.ALLOCATED(v)) ALLOCATE(v(3,NNT))
    IF (.not.ALLOCATED(w)) ALLOCATE(w(3,NNT))
    
    IF (.not.ALLOCATED(DELTAPOS)) ALLOCATE(DELTAPOS(3,NNT))
    
    IF (.not.ALLOCATED(VLX)) ALLOCATE(VLX(NNT))
    IF (.not.ALLOCATED(VLY)) ALLOCATE(VLY(NNT))
    IF (.not.ALLOCATED(VLZ)) ALLOCATE(VLZ(NNT))
    
    IF (.not.ALLOCATED(ACX)) ALLOCATE(ACX(NNT))
    IF (.not.ALLOCATED(ACY)) ALLOCATE(ACY(NNT))
    IF (.not.ALLOCATED(ACZ)) ALLOCATE(ACZ(NNT))
    
    IF (.not.ALLOCATED(FRX)) ALLOCATE(FRX(NNT))
    IF (.not.ALLOCATED(FRY)) ALLOCATE(FRY(NNT))
    IF (.not.ALLOCATED(FRZ)) ALLOCATE(FRZ(NNT))
    
    ! BARRA
    IF (.not.ALLOCATED(POSB0)) ALLOCATE(POSB0(3,NBT))
    IF (.not.ALLOCATED(POSB)) ALLOCATE(POSB(3,NBT))
    
    IF (.not.ALLOCATED(ENR)) ALLOCATE(ENR(NBT))
    IF (.not.ALLOCATED(EDG)) ALLOCATE(EDG(NBT))
    IF (.not.ALLOCATED(LCR)) ALLOCATE(LCR(NBT))
    IF (.not.ALLOCATED(STR)) ALLOCATE(STR(NBT))
    IF (.not.ALLOCATED(DSTR)) ALLOCATE(DSTR(NBT))
    IF (.not.ALLOCATED(YNGB)) ALLOCATE(YNGB(NBT))
    IF (.not.ALLOCATED(GFRB)) ALLOCATE(GFRB(NBT))
    IF (.not.ALLOCATED(LBAR)) ALLOCATE(LBAR(NBT))
    
    IF (.not.ALLOCATED(FS)) ALLOCATE(FS(NBT))
    IF (.not.ALLOCATED(FSOLD)) ALLOCATE(FSOLD(NBT))
    IF (.not.ALLOCATED(DMOLD)) ALLOCATE(DMOLD(NBT))
    ! ENERGIA
    IF (.not.ALLOCATED(DENCN)) ALLOCATE(DENCN(2,NNT))
    IF (.not.ALLOCATED(ENGDB)) ALLOCATE(ENGDB(NBT))

        
    !--------------------------------------------------------------------------
    ! ASIGNA VALORES POR DEFECTO A VARIABLES
    !--------------------------------------------------------------------------
    ! MATERIAL
    YNGB=YNG
    GFRB=GFR
    ! BARRA
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
                   
    !$OMP PARALLEL DO PRIVATE(I,NOI,NOF,DIFF) SHARED(LI,LBAR,POSB0,POSB,U0,V0,W0,U,V,W,CN)
    DO I=1,NBT
        NOI=CN(1,I)
        NOF=CN(2,I)
        DIFF(1)=U0(NOF)-U0(NOI)
        DIFF(2)=V0(NOF)-V0(NOI)
        DIFF(3)=W0(NOF)-W0(NOI)
        DIFF=DIFF**2.0

        LI(I)=DSQRT(SUM(DIFF))
        LBAR(I) = LI(I)

        POSB0(1,I)=(U0(NOF)+U0(NOI))*0.5
        POSB0(2,I)=(V0(NOF)+V0(NOI))*0.5
        POSB0(3,I)=(W0(NOF)+W0(NOI))*0.5
        
        POSB(1,I)=(U(2,NOF)+U(2,NOI))*0.5
        POSB(2,I)=(V(2,NOF)+V(2,NOI))*0.5
        POSB(3,I)=(W(2,NOF)+W(2,NOI))*0.5
        
    END DO
    !$OMP END PARALLEL DO

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------