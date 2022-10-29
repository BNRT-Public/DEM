    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE READAT
    !
    ! DATA:  15/09/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! LECTURA DE ARCHIVO DATA.dat, PARA CARGAR PARAMETROS DE MODELO
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    CHARACTER TEXTO
    
    !--------------------------------------------------------------------------
    ! LECTURA DE DATOS
    !--------------------------------------------------------------------------
    READ (1,*) TEXTO ! ' DEM SIMULATION '
    READ (1,*) TEST,TEXTO ! TEST: Numero de Test
    READ (1,*) NRUN,TEXTO ! NRUM: Numero de Corridas
    READ (1,*) SEED,TEXTO ! SEED: Numero de "Semilla"
    READ (1,*) LAW,TEXTO  ! LAW: Ley Constitutiva
    
    READ (1,*) TEXTO      ! ' GEOMETRY '
    READ (1,*) LCO,TEXTO  ! LCO: Tamano de celda
    READ (1,*) M,TEXTO    ! M: Numero de elementos en dir X
    READ (1,*) N,TEXTO    ! N: Numero de elementos en dir Y
    READ (1,*) L,TEXTO    ! L: Numero de elementos en dir Z
    
    READ (1,*) TEXTO         ! ' MATERIAL '
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
    
    READ (1,*) TEXTO         ! ' RANDOMNESS '
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
    
    READ (1,*) TEXTO      ! ' TIME SIMULATION '
    READ (1,*) DT,TEXTO   ! DT: Incremento de tiempo
    READ (1,*) TTOT,TEXTO ! TTOT: Tiempo total
    
    READ (1,*) TEXTO       ! ' OUTPUT DATA '
    READ (1,*) AENCN,TEXTO ! DENCN: Delta de Energia Cinetica en cada paso
    READ (1,*) INCO,TEXTO  ! INCO: Cant. nodos para salida de datos
    ALLOCATE(NCON(INCO))
    DO I=1,INCO
        READ (1,*) NCON(I),TEXTO ! NCON: Lectura de Nodos
    END DO
    READ (1,*) IBCO,TEXTO  ! IBCO: Cant. barras para salida de datos
    ALLOCATE(BCON(IBCO))
    DO I=1,IBCO
        READ (1,*) BCON(I),TEXTO ! BCON: Lectura de Barras
    END DO
    
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
    
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------