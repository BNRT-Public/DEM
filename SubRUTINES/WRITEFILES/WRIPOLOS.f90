    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE WRIPOLOS
    !
    ! DATA:  18/09/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    CHARACTER(LEN=24) :: DATFILE ! NOMBRE DE ARCHIVO
    CHARACTER(LEN=2)  :: NUMTEST ! NUMERO DE TEST EN CARACTER
    
    INTEGER I, IFILE

    !--------------------------------------------------------------------------
    ! INICIO DE VARIABLES
    !--------------------------------------------------------------------------
    WRITE (NUMTEST,'(I2.2)') RUN
    IFILE=15
    
    !--------------------------------------------------------------------------
    ! ESCRITURA DE ARCHIVO
    !--------------------------------------------------------------------------
    IF (OU15.GT.0) THEN

        ! FILE: POLOS_YNG_Txx.dat
        DATFILE = 'POLOS_YNG_T'//NUMTEST//'.dat'
        OPEN (UNIT=IFILE,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')

        WRITE (IFILE, 115) 'POSX','POSY','POSZ','YNG'
        DO I=1,NPTYNG(4)
            WRITE (IFILE, 215) PPYNG(I,1),PPYNG(I,2),PPYNG(I,3),PHIYNG(I)
        END DO

        CLOSE(IFILE)
        
        ! FILE: POLOS_GFR_Txx.dat
        DATFILE = 'POLOS_GFR_T'//NUMTEST//'.dat'
        OPEN (UNIT=IFILE,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')

        WRITE (IFILE, 115) 'POSX','POSY','POSZ','GFR'
        DO I=1,NPTGFR(4)
            WRITE (IFILE, 215) PPGFR(I,1),PPGFR(I,2),PPGFR(I,3),PHIGFR(I)
        END DO

        CLOSE(IFILE)
        
        ! FILE: POLOS_ROH_Txx.dat
        DATFILE = 'POLOS_ROH_T'//NUMTEST//'.dat'
        OPEN (UNIT=IFILE,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')

        WRITE (IFILE, 115) 'POSX','POSY','POSZ','ROH'
        DO I=1,NPTROH(4)
            WRITE (IFILE, 215) PPROH(I,1),PPROH(I,2),PPROH(I,3),PHIROH(I)
        END DO

        CLOSE(IFILE)

    END IF

115 FORMAT((A15),',',3(3X,A15,','))
215 FORMAT((E15.8),',',3(3X,E15.8,','))

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------