    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE WRICONTR(IFILE)
    !
    ! DATA:  07/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! DEFINICIONES
    !--------------------------------------------------------------------------
    #ifdef (_OPENMP)
    USE OMP_LIB
    #endif

    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    CHARACTER(LEN=24) :: DATFILE ! NOMBRE DE ARCHIVO
    CHARACTER(LEN=2)  :: NUMTEST ! NUMERO DE TEST EN CARACTER
    CHARACTER(LEN = 4) :: SYEAR
    CHARACTER(LEN = 2) :: SMONTH, SDAY, SHOUR, SMINUTE, SSECOND
    INTEGER DD, HH, MM
    REAL SS
    INTEGER I, NTHREAD
    DOUBLE PRECISION KRN,DELTA,ALFA,MASTOT,MASNOD

    !--------------------------------------------------------------------------
    ! VARIABLES ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER IFILE
    CHARACTER(LEN = 08) :: SDATECONT ! YYYYMMDD
    CHARACTER(LEN = 10) :: STIMECONT ! HHMMSS.SSS

    !--------------------------------------------------------------------------
    ! INICIO DE VARIABLES
    !--------------------------------------------------------------------------
    NTHREAD=0
    WRITE (NUMTEST,'(I2.2)') RUN

    CALL DELAYTIME(SDATEINFO, STIMEINFO, DD, HH, MM, SS)
    CALL DATE_AND_TIME(SDATECONT, STIMECONT)

    SYEAR = SDATEINFO(1:4)
    SMONTH = SDATEINFO(5:6)
    SDAY = SDATEINFO(7:8)
    SHOUR = STIMEINFO(1:2)
    SMINUTE = STIMEINFO(3:4)
    SSECOND = STIMEINFO(5:7)

    !--------------------------------------------------------------------------
    ! ESCRITURA DE ARCHIVO
    !--------------------------------------------------------------------------
    IF (OU02) 10,10,11

    ! FILE: CONTROL_OUT_Txx.dat
11  DATFILE = 'CONTROL_OUT_T'//NUMTEST//'.dat'
    OPEN (UNIT=IFILE,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')

    WRITE (IFILE, *)
    WRITE (IFILE, *) '  ********************* DEM *********************  '
    WRITE (IFILE, *)
    WRITE (IFILE, *) '              Discrete Element Method              '
    WRITE (IFILE, *)
    WRITE (IFILE, *) '       All Rights Reserved  -  PROMEC / UFRGS      '
    WRITE (IFILE, *)
    WRITE (IFILE, *) '           DATA CONTROL OUTPUT           '
    WRITE (IFILE, *) '           *******************           '
    WRITE (IFILE, *)
    WRITE (IFILE, *) '  Simulation   =======================>',RUN
    WRITE (IFILE, *)
    WRITE (IFILE,20) '   Test simulated...................... ',TEST
    WRITE (IFILE,20) '   Truss modules in X-direction........ ',M1
    WRITE (IFILE,20) '   Truss modules in Y-direction........ ',N1
    WRITE (IFILE,20) '   Truss modules in Z-direction........ ',L1
    WRITE (IFILE,*)
    !WRITE (IFILE,20) '   Principal nodes number.............. ',NNO
    !WRITE (IFILE,20) '   Symmetry nodes number............... ',NNS
    WRITE (IFILE,20) '   Total nodes number.................. ',NNT
    !WRITE (IFILE,20) '   Principal bars number............... ',NBA
    !WRITE (IFILE,20) '   Symmetry bars number................ ',NBS
    WRITE (IFILE,20) '   Total bars number................... ',NBT
    WRITE (IFILE,*)

    DO I=1,INCO
        WRITE (IFILE,20) '  Control node number................. ',NCON(I)
        WRITE (IFILE,25) '  Position node.... ',U0(NCON(I)),'; ',V0(NCON(I)),'; ',W0(NCON(I))
    END DO
    DO I=1,IBCO
        WRITE (IFILE,20) '  Control bars number................. ',BCON(I)
        WRITE (IFILE,25) '  Position node.... ',XB0(BCON(I)),'; ',YB0(BCON(I)),'; ',ZB0(BCON(I))
    END DO
    25  FORMAT (A21,E10.3,A02,E10.3,A02,E10.3)

    WRITE (IFILE, *)
    WRITE (IFILE,20) '   Damping condition................... ',EST
    WRITE (IFILE, *)
    WRITE (IFILE,24) '   Initial damping factor.............. ',DF1
    WRITE (IFILE,24) '   Final damping factor................ ',DF2
    WRITE (IFILE,24) '   Time of damping change.............. ',TDAF
    WRITE (IFILE,24) '   Stiffness proportional damping...... ',CON
    WRITE (IFILE, *)
    WRITE (IFILE,24) '   Truss module length................. ',LCO
    WRITE (IFILE,24) '   Time increment...................... ',DT
    WRITE (IFILE,24) '   Total time of integration........... ',TTOT

    KRN=SUM(KR(1,:))/NBT
    DELTA=9.0D00*PSN/(4.0D00-8.0D00*PSN)
    ALFA=(9.0D00+8.0D00*DELTA)/(18.0D00+24.0D00*DELTA)
    MASTOT=(M1*LCO*N1*LCO*L1*LCO)*ROH
    MASNOD=sum(MAS(:))

    WRITE (IFILE,*)
    WRITE (IFILE,24) '   Poissons ratio...................... ',PSN
    WRITE (IFILE,24) '   Mass density........................ ',ROH
    WRITE (IFILE,24) '   Nodal mass.......................... ',MASTOT
    WRITE (IFILE,24) '   Total mass.......................... ',MASTOT
    WRITE (IFILE,24) '   Youngs modulus...................... ',YNG
    WRITE (IFILE,24) '   Element normal stiffness............ ',ENR(1)
    WRITE (IFILE,24) '   Element diagonal stiffness.......... ',EDG(1)
    WRITE (IFILE,24) '   Specific fracture energy............ ',GFR
    WRITE (IFILE,24) '   Mean value for failure agent (rfc).. ',RFC
    WRITE (IFILE,24) '   Mean value for failure agent (deq).. ',DEQ
    WRITE (IFILE,24) '   Proportionality mean strain (ep).... ',EP(1,1)
    WRITE (IFILE,24) '   Critical value for Lc............... ',LCR(1)
    WRITE (IFILE,24) '   Kr value for normal bars............ ',KRN
    WRITE (IFILE,24) '   Delta coefficient................... ',DELTA
    WRITE (IFILE,24) '   Alfa coefficient.................... ',ALFA
    WRITE (IFILE, *)
    SELECT CASE (LAW)
    CASE (2) ! LEY BILINEAL
        WRITE (IFILE,24) '   Maximum value for ep................ ',EPMAX(1)
        WRITE (IFILE,24) '   Minimum value for ep................ ',EPMIN(1)
    CASE (3) ! LEY TRILINEAL
        WRITE (IFILE,24) '   Maximum value for ep(1)................ ',EPMAX(1)
        WRITE (IFILE,24) '   Maximum value for ep(2)................ ',EPMAX(2)
        WRITE (IFILE,24) '   Minimum value for ep(1)................ ',EPMIN(1)
        WRITE (IFILE,24) '   Minimum value for ep(2)................ ',EPMIN(2)
    CASE (4) ! LEY BILINEAL SHEAR
        WRITE (IFILE,24) '   Maximum value for ep................ ',EPMAX(1)
        WRITE (IFILE,24) '   Minimum value for ep................ ',EPMIN(1)
    END SELECT
    WRITE (IFILE, *)

    WRITE (IFILE, *)
    WRITE (IFILE, *) '             PROCESSING TIME             '
    WRITE (IFILE, *) '             ***************             '
    WRITE (IFILE,*)

    #ifdef (_OPENMP)
    WRITE (IFILE,20) '   Numbers of threads......................', omp_get_max_threads()
    #endif
    WRITE (IFILE,*) '   Time start the simulation......... ', SDATEINFO,' ',STIMEINFO
    WRITE (IFILE,*) '   Time write control file .......... ', SDATECONT,' ',STIMECONT
    WRITE (IFILE,21) '   Time of the process...............',DD,' - ',HH,':',MM,':',SS
    WRITE (IFILE,22) '   Data of the simulation..............   ',SDAY,'/',SMONTH,'/',SYEAR
    WRITE (IFILE,23) '   Hour of the simulation..............     ',SHOUR,':',SMINUTE,':',SSECOND
    WRITE (IFILE,*)

    WRITE (IFILE, *)
    WRITE (IFILE, *) '             GENERAL REMARKS             '
    WRITE (IFILE, *) '             ***************             '

    WRITE (IFILE,*)
    SELECT CASE (LAW)
    CASE (2) ! LEY BILINEAL
        WRITE (IFILE,24) '   Mean ep............................. ',MEAN(1)
        WRITE (IFILE,24) '   Coefficient of variation for ep..... ',COEV(1)
    CASE (3) ! LEY TRILINEAL
        WRITE (IFILE,24) '   Mean ep(1)............................. ',MEAN(1)
        WRITE (IFILE,24) '   Mean ep(2)............................. ',MEAN(2)
        WRITE (IFILE,24) '   Coefficient of variation for ep(1)..... ',COEV(1)
        WRITE (IFILE,24) '   Coefficient of variation for ep(2)..... ',COEV(2)
    CASE (4) ! LEY BILINEAL SHEAR
        WRITE (IFILE,24) '   Mean ep............................. ',MEAN(1)
        WRITE (IFILE,24) '   Coefficient of variation for ep..... ',COEV(1)
    END SELECT
    WRITE (IFILE,*)

    WRITE (IFILE,24) '   Coefficient of variation for roh.... ',CVROH
    WRITE (IFILE,24) '   Coefficient of variation for yng.... ',CVYNG
    WRITE (IFILE,24) '   Coefficient of variation for gf..... ',CVGFR
    WRITE (IFILE, *)

20  FORMAT (A40,I12)
21  FORMAT (A34,I03,A03,I02,A01,I02,A01,F6.3)
22  FORMAT (A42,A02,A01,A02,A01,A4)
23  FORMAT (A44,A02,A01,A02,A01,A02)
24  FORMAT (A40,E15.8)
    CLOSE(IFILE)

10  CONTINUE

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------