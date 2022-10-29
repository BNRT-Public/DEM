    !*************************************************************************!
    PROGRAM DEM
    !-------------------------------------------------------------------------!
    !-------------------------------------------------------------------------!
    !                                                                         !
    !                       " P R O G R A M A   D E M "                       !
    !                    =================================                    !
    !                                                                         !
    !                      DEM - DISCRETE ELEMENT METHOD                      !
    !                                                                         !
    !		            AUTORES: IGNACIO ITURRIOZ                             !
    !                            GABRIEL BIRCK                                !
    !                            BORIS N. ROJO TANZI & ET. AL.                !
    !                                                                         !
    !                                                                         !
    !            UFRGS - UNIVERSIDADE FEDERAL DO RIO GRANDE DO SUL            !
    !                       PORTO ALEGRE - RS - BRASIL                        !
    !                                                                         !
    !                                                                         !
    !                    ULTIMA ATUALIZACION: 16/09/2019                      !
    !                                                                         !
    !-------------------------------------------------------------------------!
    !--------------------------------------------------------------------------
    ! UNIDADES DE ARCHIVO UTILIZADAS
    !--------------------------------------------------------------------------
    !
    !  UNIT = 1  --> DATA.DAT
    !  UNIT = 2  --> CONTROL_OUT_Txx.DAT
    !  UNIT = 3  --> ANSYS_Txx_pppppp.MAC
    !  UNIT = 4  --> ANSYSFEM_Txx_pppppp.MAC
    !  UNIT = 5  --> ACELERATION_Txx.DAT
    !  UNIT = 6  --> VELOCITY_Txx.DAT
    !  UNIT = 7  --> DISPLACEMENT_Txx.DAT
    !  UNIT = 8  --> ENERGY_Txx.DAT
    !  UNIT = 9  --> BARBROKEN_Txx.DAT
    !  UNIT = 10 --> BARDAMAGED_Txx.DAT
    !  UNIT = 11 --> STRESS_Txx.DAT
    !  UNIT = 12 --> PARAVIEW_Txx_pppppp.vtk
    !  UNIT = 13 --> COSTIT_Txx.dat
    !
    !--------------------------------------------------------------------------
    ! LIBRERIAS A USAR
    !--------------------------------------------------------------------------
    #if def (_OPENMP)
    USE OMP_LIB
    #end if
    USE VARIABLES_DATA
    IMPLICIT NONE

    !--------------------------------------------------------------------------
    ! APERTURA DE PROGRAMA
    !--------------------------------------------------------------------------
    !CALL CLEARFILE
    CALL OMP_SET_NUM_THREADS(1);
    CALL WINDOWS

    !--------------------------------------------------------------------------
    ! REINICIA SIMULACION PARA CADA PASO
    !--------------------------------------------------------------------------
    RUN=0
20  RUN=RUN+1
    CALL DATE_AND_TIME(SDATEINFO, STIMEINFO)

    WRITE (*,*)
    WRITE (*,*) '  ** BEGINNING SIMULATION NUMBER',RUN,' **  '
    WRITE (*,*)

    ! LECTURA DEL ARCHIVO DE ENTRADA 'DATA.DAT'
    OPEN (UNIT=1,ACCESS='SEQUENTIAL',FILE='DATA.DAT',&
        &FORM='FORMATTED',STATUS='OLD')
    CALL READAT
    CLOSE (1)

    !--------------------------------------------------------------------------
    ! PREPARACION DE VARIABLES Y CALCULOS INICIALES
    !--------------------------------------------------------------------------
    ! INICIO DE VARIABLES
    CALL PREPVAR

    ! GENERACION DE COORDENADAS
    CALL COORD

    ! APLICA CONDICIONES DE CONTORNO Y CARGAS
    CALL BOUND
    CALL LOAD
    
    ! CREA LA MATRIZ DE CONECTIVIDAD DE LAS BARRAS
    CALL CONECTI
    
    ! CREA NODOS Y ELEMENTOS PARA SYM/ASYM
    CALL CREATENBSYM
    
    ! IMPONE ALEATORIEDAD EN LA MALLA
    CALL IMPERF

    ! CALCULAR LAS MASAS PUNTUALES
    CALL NODMASS

    ! CALCULOS AUXILIARES: POSICION, LONGITUD DE LAS BARRAS Y REORGANIZACION DE VARIABLES 
    CALL CALAUX

    ! GENERACION DE NUMEROS ALEATOREOS EN LOS POLOS
    CALL ALEATPYNG
    CALL ALEATPGFR

    ! CALCULO DE LAS PROPIEDADES DE CADA BARRA
    CALL NAYFES8

    ! CALCULO DE LAS PROPIEDADES ESTATICAS
    CALL STATISTICS

    ! GENERA PROPIEDADES MECANICAS ALEATOREAS
    CALL RMEC

    ! CALCULO DEL VALOR DE COEFICIENTE DE VARIACION DE EP
    CALL CALCOEV

    ! EDICION DE LA GEOMETRIA
    CALL EDITGEOM

    !--------------------------------------------------------------------------
    ! ESCRIBIENDO ARCHIVOS DEL MODELO
    !--------------------------------------------------------------------------
    WRITE (*,*) '  ******       WRITING MODEL FILES        ******  '
    WRITE (*,*)

    ! ESCRITURA ARCHIVO DEL MODELO
    CALL OPEN_MODEL

    ! ABRE E INICIA ARCHIVOS DE DATOS
    CALL OPEN_FILES

    !$OMP PARALLEL SECTIONS
    !$OMP SECTION
    ! ESCRITURA DEL ARCHIVO DE CONTROL
    CALL WRICONTR(2)
    !$OMP SECTION
    ! ESCRITURA BARRAS ROTAS
    CALL WRIBARBROKEN(9)
    !$OMP SECTION
    ! ESCRITURA BARRAS DANADAS
    CALL WRIBARDAMAGED(10)
    !$OMP SECTION
    ! ESCRITURA DE LEY CONSTITUTIVA
    CALL WRICONST
    !$OMP SECTION
    ! ESCRITURA DE VALOR Y POSICION DE LOS POLOS
    CALL WRIPOLOS
    !$OMP SECTION
    ! ESCRITURA DE DELTA DE ENERGIA CINETICA
    CALL WRIENCN(16)
    !$OMP SECTION
    ! ESCRITURA DE FUERZAS DESPLAZAMIENTOS
    CALL WRIFORCE(17)
    !$OMP END PARALLEL SECTIONS

    !--------------------------------------------------------------------------
    ! TERMINO LA CREACION DEL MODELO. INICIA LA INTEGRACION
    !--------------------------------------------------------------------------
    ! BUCLE DE INTEGRACION
    !--------------------------------------------------------------------------
    WRITE (*,*)
    WRITE (*,*) '  ** PERFORMING THE INTEGRATION STEP BY STEP **  '
    WRITE (*,*)

    DO STEP = 1,NSTEP
        ! INCREMENTO DE TIEMPO
        TIME=TIME+DT

        ! CONTROL DE AMORTIGUAMIENTO
        IF (STEP.LT.NDAMF) THEN
            DF=DF1
        ELSE
            DF=DF2
        END IF
        DMP1=1.0-DF*DT*0.5
        DMP2=1.0+DF*DT*0.5

        ! ESCRITURA DE INFORMACION DE PASO EN PANTALLA
        WRITE (*,40) STEP,RUN
        WRITE (*,50) TIME
        WRITE (*,*)
40      FORMAT ('   STEP     ======================>  ',I7,' / ',I2)
50      FORMAT ('   TIME     ======================>  ',F12.8)

        ! APLICACION DE CARGA (FUERZA/DESPLAZAMIENTO)
        CALL CALOAD

        ! CALCULO DE FUERZAS EN EL MODELO
        CALL CABAR
        
        ! INTEGRACION PARA OBTENCION DE LAS NUEVAS COORDENADAS
        CALL INTEGRATION
        
        ! CALCULO DE ENERGIA
        CALL ENERG

        ! ESCRITURA ARCHIVO DEL MODELO
        CALL OPEN_MODEL
        
        !$OMP PARALLEL SECTIONS
        !$OMP SECTION
        ! ESCRITURA DE ACELERACION
        CALL WRIACEL
        !$OMP SECTION
        ! ESCRITURA DE VELOCIDAD
        CALL WRIVEL
        !$OMP SECTION
        ! ESCRITURA DE POSICION
        CALL WRIPOS
        !$OMP SECTION
        ! ESCRITURA DE LOS VALORES DE ENERGIA
        CALL WRIENE(8)
        !$OMP SECTION
        ! ESCRITURA BARRAS ROTAS
        CALL WRIBARBROKEN(9)
        !$OMP SECTION
        ! ESCRITURA BARRAS DANADAS
        CALL WRIBARDAMAGED(10)
        !$OMP SECTION
        ! ESCRITURA DE TENSION -  DEFORMACION
        CALL WRITEN
        !$OMP SECTION
        ! ESCRITURA DE LEY CONSTITUTIVA
        CALL WRICONST
        !$OMP SECTION
        ! ESCRITURA DE DELTA DE ENERGIA CINETICA
        CALL WRIENCN(16)
        !$OMP SECTION
        ! ESCRITURA DE FUERZAS DESPLAZAMIENTOS
        CALL WRIFORCE(17)
        !$OMP END PARALLEL SECTIONS

    END DO

    ! CIERRA LOS ARCHIVOS ABIERTOS
    CALL CLOSE_FILES

    ! ESCRITURA DEL ARCHIVO DE CONTROL
    CALL WRICONTR(2)

    !--------------------------------------------------------------------------
    ! FINALIZA SIMULACION
    !--------------------------------------------------------------------------
    IF (RUN.EQ.NRUN) GO TO 70

    !--------------------------------------------------------------------------
    ! VUELVE AL INICIO PARA CALCULAR OTROS CASOS
    !--------------------------------------------------------------------------
    GO TO 20
70  CONTINUE

    WRITE (*,*)
    WRITE (*,*) '  ****************** THE END *******************  '
    WRITE (*,*)

    END PROGRAM DEM

