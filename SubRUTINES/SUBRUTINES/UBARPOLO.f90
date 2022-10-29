    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE UBARPOLO(TYPES,I,IPT,UU)!(LONG,MODEL,NPT,LCORX,LCORY,LCORZ,ROT,PP,IPT)
    IMPLICIT NONE
    !
    ! DATA:  06/02/2020
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA CALCULA LOS POLOS QUE SE ENCUENTRA EL BARICENTRO
    !  DE CADA BARRA
    !
    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER TYPES !1: SI ES GFR; 2: SI ES YNG; 3: SI ES RHO
    INTEGER I ! CONTADOR DE BARRAS
    INTEGER IPT(8) ! IND DE POSICION DE POLOS 
    
    DOUBLE PRECISION UU(3,1) ! COORDENADA ROTADA

    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    DOUBLE PRECISION PI,G2R ! CONSTANTES
    DOUBLE PRECISION CX,SX,CY,SY,CZ,SZ ! FUNCIONES TRIGONOMETRICAS
    DOUBLE PRECISION U1(3,1),R(3,3),ROT(3) ! POSICION, ROTACION
    DOUBLE PRECISION LCX,LCY,LCZ ! LONG DE CORRELACION
    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: PP1,PP2,PP3 ! POS POLOS

    LOGICAL,ALLOCATABLE,DIMENSION(:) :: PFIND1,PFIND2,PFIND3 ! BUSCADOR DE POSICION
    
    INTEGER IDX,IDY,IDZ ! INDICES POLOS
    INTEGER NPT(4) ! NUMERO DE POLOS

    !--------------------------------------------------------------------------
    ! ASIGNACION DE VARIABLES
    !--------------------------------------------------------------------------
    PI=4.0*DATAN(1.0D0)
    G2R=PI/180.0
    
    !--------------------------------------------------------------------------
    ! CALCULA LA NUEVA POSICION ROTANDO EL MODELO
    !--------------------------------------------------------------------------
    SELECT CASE (TYPES)
    CASE (1) ! POLOS DE GFR
        ALLOCATE(PFIND1(NPTGFR(1)),PP1(NPTGFR(1)))
        ALLOCATE(PFIND2(NPTGFR(2)),PP2(NPTGFR(2)))
        ALLOCATE(PFIND3(NPTGFR(3)),PP3(NPTGFR(3)))
        PFIND1=.TRUE.
        PFIND2=.TRUE.
        PFIND3=.TRUE.
        
        PP1 = PPGFR(1:NPTGFR(1),1)
        PP2 = PPGFR(1:NPTGFR(1)*NPTGFR(2):NPTGFR(1),2)
        PP3 = PPGFR(1:NPTGFR(1)*NPTGFR(2)*NPTGFR(3):NPTGFR(1)*NPTGFR(2),3)
        
        ROT=ROTGFR
        LCX=LCXGFR
        LCY=LCYGFR
        LCZ=LCZGFR
        
        NPT=NPTGFR
    CASE (2) ! POLOS DE YNG
        ALLOCATE(PFIND1(NPTYNG(1)),PP1(NPTYNG(1)))
        ALLOCATE(PFIND2(NPTYNG(2)),PP2(NPTYNG(2)))
        ALLOCATE(PFIND3(NPTYNG(3)),PP3(NPTYNG(3)))
        PFIND1=.TRUE.
        PFIND2=.TRUE.
        PFIND3=.TRUE.
        
        PP1 = PPYNG(1:NPTYNG(1),1)
        PP2 = PPYNG(1:NPTYNG(1)*NPTYNG(2):NPTYNG(1),2)
        PP3 = PPYNG(1:NPTYNG(1)*NPTYNG(2)*NPTYNG(3):NPTYNG(1)*NPTYNG(2),3)
        
        ROT=ROTYNG
        LCX=LCXYNG
        LCY=LCYYNG
        LCZ=LCZYNG
        
        NPT=NPTYNG
    CASE (3) ! POLOS DE RHO
        ALLOCATE(PFIND1(NPTROH(1)),PP1(NPTROH(1)))
        ALLOCATE(PFIND2(NPTROH(2)),PP2(NPTROH(2)))
        ALLOCATE(PFIND3(NPTROH(3)),PP3(NPTROH(3)))
        PFIND1=.TRUE.
        PFIND2=.TRUE.
        PFIND3=.TRUE.
        
        PP1 = PPROH(1:NPTROH(1),1)
        PP2 = PPROH(1:NPTROH(1)*NPTROH(2):NPTROH(1),2)
        PP3 = PPROH(1:NPTROH(1)*NPTROH(2)*NPTROH(3):NPTROH(1)*NPTROH(2),3)
        
        ROT=ROTROH
        LCX=LCXROH
        LCY=LCYROH
        LCZ=LCZROH
        
        NPT=NPTROH
    END SELECT
        
    U1(1,1)=XB0(I)-(M1*LCO)*0.5
    U1(2,1)=YB0(I)-(N1*LCO)*0.5
    U1(3,1)=ZB0(I)-(L1*LCO)*0.5
    
    SX=-DSIN(ROT(1)*G2R)
    CX=DCOS(ROT(1)*G2R)
    SY=DSIN(ROT(2)*G2R)
    CY=DCOS(ROT(2)*G2R)
    SZ=-DSIN(ROT(3)*G2R)
    CZ=DCOS(ROT(3)*G2R)
    
    R(1,1)=CY*CZ
    R(2,1)=-CX*SZ-CZ*SX*SY
    R(3,1)=SX*SZ-CX*CZ*SY
    R(1,2)=CY*SZ
    R(2,2)=CX*CZ-SX*SY*SZ
    R(3,2)=-CZ*SX-CX*SY*SZ
    R(1,3)=SY
    R(2,3)=CY*SX
    R(3,3)=CX*CY
    
    UU=MATMUL(R,U1)
    
    !--------------------------------------------------------------------------
    ! BUSQUEDA DE POLOS
    !--------------------------------------------------------------------------
    IPT = -1
    
    PFIND1=(PP1.EQ.UU(1,1))
    PFIND2=(PP2.EQ.UU(2,1))
    PFIND3=(PP3.EQ.UU(3,1))
    
    IF (.NOT.(ANY(PFIND1,1))) THEN
        PFIND1=((PP1.GE.(UU(1,1)-LCX)).AND.(PP1.LE.UU(1,1)))
    END IF
    IF (.NOT.(ANY(PFIND2,1))) THEN
        PFIND2=((PP2.GE.(UU(2,1)-LCY)).AND.(PP2.LE.UU(2,1)))
    END IF
    IF (.NOT.(ANY(PFIND3,1))) THEN
        PFIND3=((PP3.GE.(UU(3,1)-LCZ)).AND.(PP3.LE.UU(3,1)))
    END IF
    IDX=FINDLOC(PFIND1,.TRUE.,1)
    IDY=FINDLOC(PFIND2,.TRUE.,1)
    IDZ=FINDLOC(PFIND3,.TRUE.,1)
        
    IPT(1)=(IDZ-1)*NPT(1)*NPT(2)+(IDY-1)*NPT(1)+IDX
    IPT(2)=IPT(1)+1
    IPT(3)=IPT(1)+NPT(1)
    IPT(4)=IPT(3)+1
    
    IPT(5)=IPT(1)+NPT(1)*NPT(2)
    IPT(6)=IPT(5)+1
    IPT(7)=IPT(5)+NPT(1)
    IPT(8)=IPT(7)+1
    
    !--------------------------------------------------------------------------
    ! VERIFICACION DE BUSQUEDA DE POLOS
    !--------------------------------------------------------------------------
    IF (ANY(IPT.EQ.0)) THEN
        !$OMP CRITICAL
        WRITE(*,*) 'ERROR: EN CALCULAR POSICION'
        PAUSE
        STOP
        !$OMP END CRITICAL
    END IF

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------