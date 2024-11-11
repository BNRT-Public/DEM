    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!
    !
    SUBROUTINE PREPVAR
    implicit none
    !
    ! DATA:  17/02/2023
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! CALCULO DE DIMENCIONES DEL MODELO Y ASIGNACION DE VARIABLES
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    LOGICAL LOGIC(3)
    CHARACTER TEXTO
    integer i
    
    !--------------------------------------------------------------------------
    ! VERIFICACION DEL MODELO MODELO
    !--------------------------------------------------------------------------
    SELECT CASE (MODELTYPE)
    CASE ('DEM') ! DEM
        WRITE (*,*) '  ************ GENERATING DEM MODEL ************  '
        !----------------------------------------------------------------------
        ! CALCULOS DE TAMANO DE MODELO
        !----------------------------------------------------------------------
        LOGIC(1)=(M.LE.1)
        LOGIC(2)=(N.LE.1)
        LOGIC(3)=(L.LE.1)
        IF (ANY(LOGIC)) THEN
            WRITE(*,*) 'ERROR: DIMENTIONS INVALID'
            PAUSE
            STOP
        END IF
    
        M1=M-1
        M2=M-2
        N1=N-1
        N2=N-2
        L1=L-1
        L2=L-2
        MNL=M*N*L
        NNT=MNL+M1*N1*L1
        NNA=NNT
        NBT=8*M1*N1*L1+M1*L*N+M*N1*L+M*N*L1+M2*N1*L1+M1*N2*L1+M1*N1*L2
        NBA=NBT
        
    CASE ('PRO') ! PROTEIN
        WRITE (*,*) '  ********** GENERATING PROTEIN MODEL **********  '
        !-----------------------------------------------------------------------
        ! LECTURA DE DATOS DE PROTEINAS
        !-----------------------------------------------------------------------
        ! LECTURA DEL ARCHIVO DE Nodos 'Node_List.DAT'
        OPEN (UNIT=1,ACCESS='SEQUENTIAL',FILE='Node_List.DAT',&
            &FORM='FORMATTED',STATUS='OLD')
        
        do i=1,4
           read (1,*) ! comentarios
        end do
        read (1,*) texto,texto,NNT
        CLOSE (1)
        
        ! LECTURA DEL ARCHIVO DE conexiones 'Connect_List.DAT'
        OPEN (UNIT=1,ACCESS='SEQUENTIAL',FILE='Connect_List.DAT',&
            &FORM='FORMATTED',STATUS='OLD')
        
        do i=1,4
           read (1,*) ! comentarios
        end do
        read (1,*) texto,texto,NBT
        CLOSE (1)
    
    CASE DEFAULT
        WRITE(*,*) 'ERROR: INVALID MODEL SELECT (MODELTYPE)'
        WRITE(*,*) '    - "DEM": DISCRETE ELEMENT METHOD'
        WRITE(*,*) '    - "OPT": DISCRETE ELEMENT METHOD FOR OPTIMIZATION'
        WRITE(*,*) '    - "BOU": BOUNDLE MODEL'
        WRITE(*,*) '    - "PRO": PROTEINE MODEL'
        PAUSE
        STOP
    END SELECT
    
    !--------------------------------------------------------------------------
    ! VERIFICACION DE DATOS
    !--------------------------------------------------------------------------

    IF (MAXVAL(NCON).GT.NNT) THEN
        WRITE(*,*) 'ERROR: INVALID SELECTED NODE NUMBER.'
        PAUSE
        STOP
    END IF

    IF (MAXVAL(BCON).GT.NBT) THEN
        WRITE(*,*) 'ERROR: INVALID SELECTED BAR NUMBER.'
        PAUSE
        STOP
    END IF
    
    LOGIC(1)=(LCXROH.LT.0.35*LCO)
    LOGIC(2)=(LCYROH.LT.0.35*LCO)
    LOGIC(3)=(LCZROH.LT.0.35*LCO)
    IF (ANY(LOGIC)) THEN
        WRITE(*,*) 'ERROR: CORRELATION LENGTH OF RHO OF NOT VALID.'
        PAUSE
        STOP
    END IF
    
    LOGIC(1)=(LCXYNG.LT.0.35*LCO)
    LOGIC(2)=(LCYYNG.LT.0.35*LCO)
    LOGIC(3)=(LCZYNG.LT.0.35*LCO)
    IF (ANY(LOGIC)) THEN
        WRITE(*,*) 'ERROR: CORRELATION LENGTH OF YNG OF NOT VALID.'
        PAUSE
        STOP
    END IF
    
    LOGIC(1)=(LCXGFR.LT.0.35*LCO)
    LOGIC(2)=(LCYGFR.LT.0.35*LCO)
    LOGIC(3)=(LCZGFR.LT.0.35*LCO)
    IF (ANY(LOGIC)) THEN
        WRITE(*,*) 'ERROR: CORRELATION LENGTH OF GFR OF NOT VALID.'
        PAUSE
        STOP
    END IF

    SELECT CASE (LAW)
    CASE (3)
        IF (REET.LT.1.0) THEN
            WRITE(*,*) 'ERROR: REET INTERVAL, REET>=1.'
            PAUSE
            STOP
        END IF
        IF ((REYNG.LT.-1.0)) THEN
            WRITE(*,*) 'ERROR: REYNG INTERVAL, REYNG>=-1.'
            PAUSE
            STOP
        END IF
        IF (((1-REYNG*REET+REET).LT.0.0)) THEN
            WRITE(*,*) 'ERROR: RELATIONSHIP BETWEEN REET AND REYNG NOT ALLOWED.'
            WRITE(*,*) 'F: PROP. ',(1-REYNG*REET+REET)
            PAUSE
            STOP
        END IF
    CASE (4)
        IF (REET.LT.0.0) THEN
            WRITE(*,*) 'ERROR: REET INTERVAL, REET>=0.0.'
            PAUSE
            STOP
        END IF
    END SELECT

    !--------------------------------------------------------------------------
    ! ASIGNAR DIMENCION A LAS VARIABLES
    !--------------------------------------------------------------------------
    ! POSICION/VELOCIDAD/ACELERACION
    ALLOCATE(U0(NNT),V0(NNT),W0(NNT))
    ! CONECTIVIDAD
    ALLOCATE(CN(2,NBT))
    ALLOCATE(ELEMTYPE(NBT))
    ALLOCATE(LI(NBT))
    ! LEY CONTITUTIVA
    SELECT CASE (LAW)
    CASE (1) ! LEY LINEAL
        ALLOCATE(FORCE(1,NBT))
        ALLOCATE(ECOMP(NBT))
        ALLOCATE(ETRAC(1,NBT))
        ALLOCATE(DM(1,NBT))
    CASE (2) ! LEY BILINEAL
        ALLOCATE(EP(1,NBT))
        ALLOCATE(EM(1,NBT))
        ALLOCATE(DM(1,NBT))
        ALLOCATE(KR(1,NBT))
        ALLOCATE(ER(NBT))
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
        ALLOCATE(ER(NBT))
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
        ALLOCATE(ER(NBT))
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
    
    ! CONTROL
    SELECT CASE (TEST)
    CASE (700) ! ENSAYO DE 3 PUNTOS
        ALLOCATE(NODECTR(3))
    END SELECT
    
    !--------------------------------------------------------------------------
    ! INICIAR LAS VARIABLES
    !--------------------------------------------------------------------------
    ! TIEMPO
    TIME=0.0D00
    STEP=0
    NSTEP=IDNINT(TTOT/DT)
    NDAMF=IDNINT(TDAF/DT)
    ! ENERGIA
    ENEX=0.0D00
    ENIN=0.0D00
    ENCN=0.0D00
    ENDP=0.0D00
    ENGD=0.0D00
    ENGDB=0.0D00
    ! MATERIAL
    !DMP1=1.0D00-DF1*DT*0.5D00
    !DMP2=1.0D00+DF1*DT*0.5D00
    DF=DF1
    CAF=(1/(3.0*SQRT(3.0)+2.0))*CORRCAF
    ! CONECTIVIDAD
    CN=0
    ! PROPIEDADES ELEMENTOS
    EPMIN = 0.0
    EPMAX = 0.0
    MEAN = 0
    COEV = 0
    FORCE = 0.0
    STROLD = 0.0
    ! ARCHIVOS
    PR03=0
    PR04=0
    PR05=0
    PR06=0
    PR07=0
    PR08=0
    PR09=0
    PR10=0
    PR11=0
    PR12=0
    PR13=0
    PR14=0
    PR16=0
    PR17=0
    PR18=0
    ! DESPLAZAMIENTO
    U0=0.0
    V0=0.0
    W0=0.0
    ! COND DE BORDE
    BDSYM=.FALSE.
    BDASYM=.FALSE.
    ! PROP. OLD FRACTURA
    RFC = DSQRT(1.00/DEQ)    
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------