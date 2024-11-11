    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    subroutine split(line, vals, i)
    implicit none
    character(*), intent(in) :: line
    integer  :: vals(*), buf(1000)
    integer :: i

    i = 1
    do
        read( line, *, end=100, err=100 ) buf(1 : i)
        vals( 1:i ) = buf(1:i)
        i = i + 1
    end do
100 continue
    i = i - 1
    end
    
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE READAT
    !
    ! DATA:  17/02/2023
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! LECTURA DE ARCHIVO DATA.dat, PARA CARGAR PARAMETROS DE MODELO
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    CHARACTER TEXTO
    
    character(10000) :: line
    integer          :: vals(1000)
    integer          :: nn
    !--------------------------------------------------------------------------
    ! LECTURA DE DATOS
    !--------------------------------------------------------------------------
    ! LECTURA DEL ARCHIVO DE ENTRADA 'DATA.DAT'
    OPEN (UNIT=1,ACCESS='SEQUENTIAL',FILE='DATA.dat',&
        &FORM='FORMATTED',STATUS='OLD')
        
    READ (1,*)            ! ' DEM SIMULATION '
    READ (1,*) MODELTYPE,TEXTO ! MODELTYPE: Tipo de modelo
    READ (1,*) TEST,TEXTO ! TEST: Numero de Test
    READ (1,*) NRUN,TEXTO ! NRUM: Numero de Corridas
    READ (1,*) SEED,TEXTO ! SEED: Numero de "Semilla"
    READ (1,*) LAW,TEXTO  ! LAW: Ley Constitutiva
    
    
    
    SELECT CASE (MODELTYPE)
    CASE ('DEM') ! DEM 
        READ (1,*)            ! ' GEOMETRY '
        READ (1,*) LCO,TEXTO  ! LCO: Tamano de celda
        READ (1,*) M,TEXTO    ! M: Numero de elementos en dir X
        READ (1,*) N,TEXTO    ! N: Numero de elementos en dir Y
        READ (1,*) L,TEXTO    ! L: Numero de elementos en dir Z
            
        READ (1,*)               ! ' MATERIAL '
        READ (1,*) PSN,TEXTO     ! PSN: Poisson
        READ (1,*) ROH,TEXTO     ! ROH: Densidad
        READ (1,*) YNG,TEXTO     ! YNG: Young
        READ (1,*) GFR,TEXTO     ! GFR: Energia específica de fratura
        READ (1,*) DEQ,TEXTO     ! DEQ: Dimencion de Falla
        READ (1,*) REET,TEXTO    ! REET: Factor de posicion de ep1 en trilineal
        READ (1,*) REYNG,TEXTO   ! REYNG: Relacion de E en trilineal
        READ (1,*) CORRCAF,TEXTO ! CORRCAF:
        READ (1,*) EST,TEXTO     ! EST:
        READ (1,*) DF1,TEXTO     ! DF1:
        READ (1,*) DF2,TEXTO     ! DF2:
        READ (1,*) TDAF,TEXTO    ! TDAF:
        READ (1,*) CON,TEXTO     ! CON:
        
        READ (1,*)               ! ' RANDOMNESS MESH '
        READ (1,*) CVUVW(1),&
            CVUVW(2),&
            CVUVW(3),TEXTO         ! CVUVW: COEFICIENTE DE VARIACION PARA IMPERFECCION EN MALLA
        READ (1,*) BORDERUVW,TEXTO ! BORDERUVW: BORDE SIN IMPERFECCION
        
        READ (1,*)               ! ' RANDOMNESS '
        READ (1,*) LCXROH,TEXTO  ! LCXROH: LONGITUD DE CORELACION PARA DENSIDAD
        READ (1,*) LCYROH,TEXTO  ! LCYROH: LONGITUD DE CORELACION PARA DENSIDAD
        READ (1,*) LCZROH,TEXTO  ! LCZROH: LONGITUD DE CORELACION PARA DENSIDAD
        READ (1,*) ROTROH(1),&
            ROTROH(2),&
            ROTROH(3),TEXTO      ! LCZROH: LONGITUD DE CORELACION PARA DENSIDAD
        READ (1,*) CVROH,TEXTO   ! CVROH: COEFICIENTE DE VARIACION PARA DENSIDAD
        READ (1,*) LCXYNG,TEXTO  ! LCXYNG: LONGITUD DE CORELACION PARA MODULO DE ELASTISIDAD
        READ (1,*) LCYYNG,TEXTO  ! LCYYNG: LONGITUD DE CORELACION PARA MODULO DE ELASTISIDAD
        READ (1,*) LCZYNG,TEXTO  ! LCZYNG: LONGITUD DE CORELACION PARA MODULO DE ELASTISIDAD
        READ (1,*) ROTYNG(1),&
            ROTYNG(2),&
            ROTYNG(3),TEXTO      ! LCZROH: LONGITUD DE CORELACION PARA DENSIDAD
        READ (1,*) CVYNG,TEXTO   ! CVYNG: COEFICIENTE DE VARIACION PARA MODULO DE ELASTISIDAD
        READ (1,*) LCXGFR,TEXTO  ! LCXGFR: LONGITUD DE CORELACION PARA ENERGIA ESPECIFICA
        READ (1,*) LCYGFR,TEXTO  ! LCYGFR: LONGITUD DE CORELACION PARA ENERGIA ESPECIFICA
        READ (1,*) LCZGFR,TEXTO  ! LCZGFR: LONGITUD DE CORELACION PARA ENERGIA ESPECIFICA
        READ (1,*) ROTGFR(1),&
            ROTGFR(2),&
            ROTGFR(3),TEXTO      ! LCZROH: LONGITUD DE CORELACION PARA DENSIDAD
        READ (1,*) CVGFR,TEXTO   ! CVGFR: COEFICIENTE DE VARIACION PARA ENERGIA ESPECIFICA
    
    CASE ('PRO') ! PROTEIN
        DO I=1,14
            READ (1,*) 
        END DO
        READ (1,*) EST,TEXTO     ! EST:
        READ (1,*) DF1,TEXTO     ! DF1:
        READ (1,*) DF2,TEXTO     ! DF2:
        READ (1,*) TDAF,TEXTO    ! TDAF:
        READ (1,*) CON,TEXTO     ! CON:
        DO I=1,16
            READ (1,*) 
        END DO
                
        LCO = 0
        M = 0
        N = 0
        L = 0
        PSN = 0.0
        ROH = 0.0
        YNG = 0.0
        GFR = 0.0
        DEQ = 0.0
        REET = 0.0
        REYNG = 0.0
        CORRCAF = 0.0
        LCXROH = 0.0
        LCYROH = 0.0
        LCZROH = 0.0
        ROTROH(:) = 0.0
        CVROH = 0.0
        LCXYNG = 0.0
        LCYYNG = 0.0
        LCZYNG = 0.0
        ROTYNG(:) = 0.0
        CVYNG = 0.0
        LCXGFR = 0.0
        LCYGFR = 0.0
        LCZGFR = 0.0
        ROTGFR(:) = 0.0
        CVGFR = 0.0
    END SELECT
    
    READ (1,*)            ! ' TIME SIMULATION '
    READ (1,*) DT,TEXTO   ! DT: Incremento de tiempo
    READ (1,*) TTOT,TEXTO ! TTOT: Tiempo total
    
    READ (1,*)             ! ' OUTPUT DATA '
    READ (1,*) AENCN,TEXTO ! DENCN: Delta de Energia Cinetica en cada paso
    
    read (1,*) texto ! NCON: lectura de nodos
    read( 1, "(a)") line
    call split( line, vals, Nn )
    if ( Nn .gt. 0 ) then
        allocate(ncon(Nn))
        ncon =  vals(1 : Nn)
        INCO = nN
    else
        allocate(ncon(0))
        INCO = 0
    endif
    
    read (1,*) texto ! BCON: Lectura de Barras
    read( 1, "(a)") line
    call split( line, vals, Nn )
    if ( Nn .gt. 0 ) then
        allocate(bcon(nN))
        bcon =  vals(1 : Nn)
        IbCO = Nn
    else
        allocate(bcon(0))
        ibco = 0
    endif
        
    READ (1,*) TEXTO       ! ' OUTPUT FILES '
    READ (1,*) OU02,TEXTO  ! Archivo de Control: CONTROL_OUT_Txx.dat
    READ (1,*) OU03,TEXTO  ! ANSYS: ANSYS_Txx_xxxxx.mac
    READ (1,*) OU04,TEXTO  ! ANSYS: ANSYSFEM_Txx_xxxxx.mac
    READ (1,*) OU05,TEXTO  ! Aceleracion: ACELERATION_Txx.dat
    READ (1,*) OU06,TEXTO  ! Velocidad: VELOCITY_Txx.dat
    READ (1,*) OU07,TEXTO  ! Desplazamiento: POSITION_Txx.dat
    READ (1,*) OU08,TEXTO  ! Energia: ENERGY_Txx.dat
    READ (1,*) OU09,TEXTO  ! Barras Rotas: BARBROKEN_Txx.dat
    READ (1,*) OU10,TEXTO  ! Barras Dañadas: BARDAMAGEN_Txx.dat
    READ (1,*) OU11,TEXTO  ! Tension Deformacion: STRESS_Txx.dat
    READ (1,*) OU12,TEXTO  ! PARAVIEW: PARAVIEW_Txx_xxxxx.vtk
    READ (1,*) OU13,TEXTO  ! Model: MODEL_Txx_xxxxx.dat
    READ (1,*) OU14,TEXTO  ! Constitutiva: CONSTIT_Txx_xxxxx.dat
    READ (1,*) OU15,TEXTO  ! Polos: POLOS_Type_Txx.dat
    READ (1,*) OU16,TEXTO  ! Delta Energia: DeltaENCN_Txx.dat
    READ (1,*) OU17,TEXTO  ! FORCE: FORCE_Txx.dat
    READ (1,*) OU18,TEXTO  ! DISPLECEMENT: BOUND_Txx.dat
    
    CLOSE (1)
        
    if (.fALSE.) THEN
        WRITE(*,*) ' DEM SIMULATION '
        WRITE(*,*) 'MODELTYPE:', MODELTYPE
        WRITE(*,*) 'TEST:',TEST
        WRITE(*,*) 'NRUN:',NRUN
        WRITE(*,*) 'SEED:',SEED
        WRITE(*,*) 'LAW:',LAW
        WRITE(*,*) 
        WRITE(*,*) ' GEOMETRY '
        WRITE(*,*) 'LCO:',LCO
        WRITE(*,*) 'M:',M
        WRITE(*,*) 'N:',N
        WRITE(*,*) 'L:',L
    
    END IF
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
