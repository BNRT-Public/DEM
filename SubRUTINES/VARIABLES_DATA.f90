    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    MODULE VARIABLES_DATA
    IMPLICIT NONE
    !
    ! ESTE MODULO CREA LAS VARIABLES DE DATOS DE ENTRADA DEL PROGRAMA
    !
    ! DATA:  15/09/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------   
    INTEGER TEST            ! TEST: NUMERO DE TEST
    INTEGER NRUN            ! NRUM: NUMERO DE CORRIDAS
    INTEGER SEED            ! SEED: NUMERO DE "SEMILLA"
    INTEGER LAW             ! LAW: NUMERO DE LEY CONSTITUTIVA

    DOUBLE PRECISION  LCO   ! LCO: TAMANO DE CELDA
    INTEGER M               ! M: CANTIDAD DE CELDAS EN X
    INTEGER N               ! N: CANTIDAD DE CELDAS EN Y
    INTEGER L               ! L: CANTIDAD DE CELDAS EN Z

    DOUBLE PRECISION PSN     ! PSN: POISSON
    DOUBLE PRECISION ROH     ! ROH: DENSIDAD
    DOUBLE PRECISION YNG     ! YNG: MODULO DE ELASTISIDAD
    DOUBLE PRECISION GFR     ! GFR: ENERGIA DE FRACTURA
    DOUBLE PRECISION RFC     ! RFC: FACTOR DE FALLA --OLD--
    DOUBLE PRECISION DEQ     ! DEQ: DIMENCION DE FALLA
    DOUBLE PRECISION REET    ! REET: RELACION EP'/EP PARA LEY TRILINEAL
    DOUBLE PRECISION REYNG   ! REYNG: RELACION ENTRE E'/E PARA LEY TRILINEAL
    DOUBLE PRECISION CORRCAF ! CORRCAF: FACTOR DE MULTIPLICACION DE CAF PARA COMPATIBILIZAR MALLA
    INTEGER EST              ! EST: TIPO DE AMORTIGUAMIENTO, 0: PATRON, 1: PATRON + PROPORCIONAL A RIGIDEZ, 2: PROPORCIONAL A LA MASA
    DOUBLE PRECISION DF1     ! DF1: COEFICIENTE DE AMORTIGUAMIENTO PARA EL PRIMER INTERVALO (0 A TDAF)
    DOUBLE PRECISION DF2     ! DF2: COEFICIENTE DE AMORTIGUAMIENTO PARA EL SEGUNDO INTERVALO (TDAF A TTOT)
    DOUBLE PRECISION TDAF    ! TDAF: TIEMPO EN EL QUE CAMBIA EL AMORTIGUAMIENTO DF1 A DF2
    DOUBLE PRECISION CON     ! CON: AMORTIGUAMIENTO DENTRO DE LA ECUACION CONSTITUTIVA (EST = 1)

    DOUBLE PRECISION LCXROH    ! LCXROH: LONGITUD DE CORELACION PARA DENSIDAD
    DOUBLE PRECISION LCYROH    ! LCYROH: LONGITUD DE CORELACION PARA DENSIDAD
    DOUBLE PRECISION LCZROH    ! LCZROH: LONGITUD DE CORELACION PARA DENSIDAD
    DOUBLE PRECISION ROTROH(3) ! ROTROH: ANGULOS DE ROTACION EN EL CAMPO DE CORELACION PARA DENSIDAD
    DOUBLE PRECISION CVROH     ! CVROH: COEFICIENTE DE VARIACION PARA DENSIDAD
    DOUBLE PRECISION LCXYNG    ! LCXYNG: LONGITUD DE CORELACION PARA MODULO DE ELASTISIDAD
    DOUBLE PRECISION LCYYNG    ! LCYYNG: LONGITUD DE CORELACION PARA MODULO DE ELASTISIDAD
    DOUBLE PRECISION LCZYNG    ! LCZYNG: LONGITUD DE CORELACION PARA MODULO DE ELASTISIDAD
    DOUBLE PRECISION ROTYNG(3) ! ROTYNG: ANGULOS DE ROTACION EN EL CAMPO DE CORELACION PARA MODULO DE ELASTISIDAD
    DOUBLE PRECISION CVYNG     ! CVYNG: COEFICIENTE DE VARIACION PARA MODULO DE ELASTISIDAD
    DOUBLE PRECISION LCXGFR    ! LCXGFR: LONGITUD DE CORELACION PARA ENERGIA ESPECIFICA
    DOUBLE PRECISION LCYGFR    ! LCYGFR: LONGITUD DE CORELACION PARA ENERGIA ESPECIFICA
    DOUBLE PRECISION LCZGFR    ! LCZGFR: LONGITUD DE CORELACION PARA ENERGIA ESPECIFICA
    DOUBLE PRECISION ROTGFR(3) ! ROTYNG: ANGULOS DE ROTACION EN EL CAMPO DE CORELACION PARA ENERGIA ESPECIFICA
    DOUBLE PRECISION CVGFR     ! CVGFR: COEFICIENTE DE VARIACION PARA ENERGIA ESPECIFICA
    
    DOUBLE PRECISION DT      ! DT: INCREMENTO DE TIEMPO
    DOUBLE PRECISION TTOT    ! TTOT: TIEMPO TOTAL

    DOUBLE PRECISION AENCN     ! AENCN: AMPLITUD DE ENERGIA CINETICA EN CADA PASO
    INTEGER INCO  ! INCO: CANT. NODOS PARA SALIDA DE DATOS
    INTEGER,ALLOCATABLE,DIMENSION(:) :: NCON ! NCON: LECTURA DE NODOS
    INTEGER IBCO  ! INCO: CANT. NODOS PARA SALIDA DE DATOS
    INTEGER,ALLOCATABLE,DIMENSION(:) :: BCON ! NCON: LECTURA DE NODOS

    INTEGER OU02 ! CONTROL: CONTROL_OUT_TXX.DAT
    INTEGER OU03 ! ANSYS: ANSYS_TXX_pppppp.MAC
    INTEGER OU04 ! ANSYS: ANSYSFEM_TXX_pppppp.MAC
    INTEGER OU05 ! ACELERACION: ACELERATION_TXX.DAT
    INTEGER OU06 ! VELOCIDAD: VELOCITY_TXX.DAT
    INTEGER OU07 ! DESPLAZAMIENTO: POSITION_TXX.DAT
    INTEGER OU08 ! ENERGIA: ENERGY_TXX.DAT
    INTEGER OU09 ! BARRAS ROTAS: BARBROKEN_TXX.DAT
    INTEGER OU10 ! BARRAS DA�ADAS: BARDAMAGEN_TXX.DAT
    INTEGER OU11 ! TENSION DEFORMACION: STRESS_TXX.DAT
    INTEGER OU12 ! PARAVIEW: PARAVIEW_Txx_pppppp.vtk
    INTEGER OU13 ! MODELO DE BARRAS: MODEL_Txx_pppppp.dat
    INTEGER OU14 ! LEY CONTITUTIVA: CONSTIT_Txx_pppppp.dat
    INTEGER OU15 ! POLOS: POLOS_Type_Txx.dat
    INTEGER OU16 ! Delta Energia: DeltaENCN_Txx.dat
    INTEGER OU17 ! FORCE: FORCE_Txx.dat
    
    INCLUDE 'VARIABLES_DEM.f90'
    
    CONTAINS
    INCLUDE 'WRITEFILES/CLEARFILE.f90'
    INCLUDE 'PRINCIPAL/READAT.f90'
    INCLUDE 'PRINCIPAL/PREPVAR.f90'
    INCLUDE 'PRINCIPAL/COORD.f90'
    INCLUDE 'PRINCIPAL/IMPERF.f90'
    INCLUDE 'SUBRUTINES/COORDSUB.f90'
    INCLUDE 'PRINCIPAL/BOUND.f90'
    INCLUDE 'PRINCIPAL/LOAD.f90'
    INCLUDE 'PRINCIPAL/CREATENBSYM.f90'
    INCLUDE 'SUBRUTINES/RESIZESYM.f90'
    INCLUDE 'PRINCIPAL/NODMASS.f90'
    INCLUDE 'RANDOMNESS/INIROH.f90'
    INCLUDE 'FUNCTION/RAN0.f90'
    INCLUDE 'FUNCTION/RAND1.f90'
    INCLUDE 'FUNCTION/ROTMAT.f90'
    INCLUDE 'PRINCIPAL/CONECTI.f90'
    INCLUDE 'SUBRUTINES/EXTCO1.f90'
    INCLUDE 'SUBRUTINES/INTCO1.f90'
    INCLUDE 'SUBRUTINES/DIACO1.f90'
    INCLUDE 'PRINCIPAL/CALAUX.f90'
    INCLUDE 'PRINCIPAL/ALEATPYNG.f90'
    INCLUDE 'PRINCIPAL/ALEATPGFR.f90'
    INCLUDE 'PRINCIPAL/NAYFES8.f90'
    INCLUDE 'SUBRUTINES/UBARPOLO.f90'
    INCLUDE 'SUBRUTINES/INTPOLO.f90'
    INCLUDE 'SUBRUTINES/DEBBARRAS.f90'
    INCLUDE 'PRINCIPAL/STATISTICS.f90'
    INCLUDE 'SUBRUTINES/FPROB.f90'
    INCLUDE 'PRINCIPAL/RMEC.f90'
    INCLUDE 'SUBRUTINES/RMECEX.f90'
    INCLUDE 'SUBRUTINES/RMECIN.f90'
    INCLUDE 'SUBRUTINES/RMECDI.f90'
    INCLUDE 'SUBRUTINES/RANDMEC.f90'
    INCLUDE 'PRINCIPAL/CALCOEV.f90'
    INCLUDE 'PRINCIPAL/EDITGEOM.f90'
    INCLUDE 'WRITEFILES/OPEN_FILES.f90'
    INCLUDE 'WRITEFILES/OPEN_MODEL.f90'
    INCLUDE 'WRITEFILES/ANSYS.f90'
    INCLUDE 'WRITEFILES/MODEL.f90'
    INCLUDE 'WRITEFILES/PARAVIEW.f90'
    INCLUDE 'WRITEFILES/WRIBARDAMAGED.f90'
    INCLUDE 'WRITEFILES/WRIBARBROKEN.f90'
    INCLUDE 'WRITEFILES/WRIENCN.f90'
    INCLUDE 'PRINCIPAL/CALOAD.f90'
    INCLUDE 'PRINCIPAL/CABAR.f90'
    INCLUDE 'CONSTITS/CONSTIT_BI.f90'
    INCLUDE 'CONSTITS/CONSTIT_BI_SHEAR.f90'
    INCLUDE 'CONSTITS/CONSTIT_TRI.f90'
    INCLUDE 'PRINCIPAL/INTEGRATION.f90'
    INCLUDE 'SUBRUTINES/APPLYSYM.f90'
    INCLUDE 'PRINCIPAL/ENERG.f90'
    INCLUDE 'WRITEFILES/WRIENE.f90'
    INCLUDE 'WRITEFILES/WRITEN.f90'
    INCLUDE 'WRITEFILES/WRIPOS.f90'
    INCLUDE 'WRITEFILES/WRIVEL.f90'
    INCLUDE 'WRITEFILES/WRIACEL.f90'
    INCLUDE 'WRITEFILES/WRICONTR.f90'
    INCLUDE 'WRITEFILES/WRICONST.f90'
    INCLUDE 'WRITEFILES/WRIPOLOS.f90'
    INCLUDE 'WRITEFILES/WRIFORCE.f90'
    INCLUDE 'SUBRUTINES/DELAYTIME.f90'
    INCLUDE 'SUBRUTINES/PRISMEDITGEOM.f90'
    
    END MODULE VARIABLES_DATA