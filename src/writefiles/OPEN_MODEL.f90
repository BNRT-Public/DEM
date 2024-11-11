    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE OPEN_MODEL
    !
    ! DATA:  18/09/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! EN ESTA SUBRUTINA CREA EL ARCHIVO DEL MODELO PARA VISUALIZACION
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------    
    CHARACTER(LEN=25) :: DATFILE3  ! NOMBRE DE ARCHIVO: ANSYS_Txx_pppppp.mac
    CHARACTER(LEN=25) :: DATFILE4 ! NOMBRE DE ARCHIVO: ANSYSFEM_Txx_pppppp.dat
    CHARACTER(LEN=25) :: DATFILE12 ! NOMBRE DE ARCHIVO: PARAVIEW_Txx_pppppp.vtk
    CHARACTER(LEN=25) :: DATFILE13 ! NOMBRE DE ARCHIVO: MODEL_Txx_pppppp.dat
    
    CHARACTER(LEN=2)  :: NUMTEST ! NUMERO DE TEST EN CARACTER
    CHARACTER(LEN=8)  :: NUMSTEP ! NUMERO DE STEP EN CARACTER
    CHARACTER(LEN=6)  :: FMTRUN  ! FORMATO PARA ESCRITURA DE RUN
    CHARACTER(LEN=6)  :: FMTSTEP  ! FORMATO PARA ESCRITURA DE STEP
    
    LOGICAL LOGIC03(2),LOGIC04(2),LOGIC12(2),LOGIC13(2)
    
    !--------------------------------------------------------------------------
    FMTRUN='(I2.2)'
    FMTSTEP='(I8.8)'
    
    WRITE (NUMTEST,FMTRUN) RUN
    WRITE (NUMSTEP,FMTSTEP) STEP
    
    !$OMP PARALLEL SECTIONS

    ! ARCHIVO CON DATOS DE ANSYS
    ! FILE: ANSYS_Txx_pppppp.mac
    !$OMP SECTION
    LOGIC03(1)=((OU03.GT.0).AND.(STEP.EQ.0))
    LOGIC03(2)=((OU03.GT.0).AND.(PR03.EQ.OU03))
    
    IF (ANY(LOGIC03)) THEN
        DATFILE3 = 'ANSYS_T'//NUMTEST//'_'//NUMSTEP//'.mac'

        OPEN (UNIT=3,ACCESS='SEQUENTIAL',FILE=DATFILE3,STATUS='UNKNOWN')
        IF(STEP.EQ.0) THEN
            !CALL ANSYSMASTER(3)
            CALL ANSYS
        ELSE
            !CALL ANSYSSLAVE(3)
            CALL ANSYS
        END IF
        
        CLOSE (3)
        PR03=1
    ELSE
        PR03=PR03+1
    END IF
    
    ! ARCHIVO CON DATOS DE ANSYS PARA FEM
    ! FILE: ANSYSFEM_Txx_pppppp.dat
    !$OMP SECTION
    LOGIC04(1)=((OU04.GT.0).AND.(STEP.EQ.0))
    LOGIC04(2)=((OU04.GT.0).AND.(PR04.EQ.OU04))
    IF (ANY(LOGIC04)) THEN
        DATFILE4 = 'ANSYSFEM_T'//NUMTEST//'_'//NUMSTEP//'.mac'

        OPEN (UNIT=4,ACCESS='SEQUENTIAL',FILE=DATFILE4,STATUS='UNKNOWN')
        WRITE (4,*) '! TIME: ',TIME
        !CALL ANSYSFEM
        CLOSE (4)
        PR04=1
    ELSE
        PR04=PR04+1
    END IF

    ! ARCHIVO CON DATOS DE PARAVIEW
    ! FILE: PARAVIEW_Txx_pppppp.vtk
    !$OMP SECTION
    LOGIC12(1)=((OU12.GT.0).AND.(STEP.EQ.0))
    LOGIC12(2)=((OU12.GT.0).AND.(PR12.EQ.OU12))
    IF (ANY(LOGIC12)) THEN
        DATFILE12 = 'PARAVIEW_T'//NUMTEST//'_'//NUMSTEP//'.vtk'

        OPEN (UNIT=12,ACCESS='SEQUENTIAL',FILE=DATFILE12,STATUS='UNKNOWN')
        !WRITE (12,*) '! TIME: ',TIME
        CALL PARAVIEW
        CLOSE (12)
        PR12=1
    ELSE
        PR12=PR12+1
    END IF
    
    ! ARCHIVO CON DATOS DE PARAVIEW
    ! FILE: MODEL_Txx_pppppp.dat
    !$OMP SECTION
    LOGIC13(1)=((OU13.GT.0).AND.(STEP.EQ.0))
    LOGIC13(2)=((OU13.GT.0).AND.(PR13.EQ.OU13))
    IF (ANY(LOGIC13)) THEN
        DATFILE13 = 'MODEL_T'//NUMTEST//'_'//NUMSTEP//'.dat'

        OPEN (UNIT=13,ACCESS='SEQUENTIAL',FILE=DATFILE13,STATUS='UNKNOWN')
        CALL MODEL
        CLOSE (13)
        PR13=1
    ELSE
        PR13=PR13+1
    END IF
    !$OMP END PARALLEL SECTIONS

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
