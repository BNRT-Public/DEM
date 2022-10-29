    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE OPEN_FILES
    !
    ! DATA:  06/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! EN ESTA SUBRUTINA ABRE Y DA FORMATO A LOS ARCHIVOS DE SALIDA:
    ! - ACELERATION_Txx.dat
    ! - VELOCITY_Txx.dat
    ! - DISPLACEMENT_Txx.dat
    ! - ENERGY_Txx.dat
    ! - BARBROKEN_Txx.dat
    ! - BARDAMAGEN_Txx.dat
    ! - STRESS_Txx.dat
    ! - CONSTIT_Txx.dat
    ! - DeltaENCN_Txx.dat
    ! - FORCE_Txx.dat
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    CHARACTER(LEN=24) :: DATFILE ! NOMBRE DE ARCHIVO
    CHARACTER(LEN=2)  :: NUMTEST ! NUMERO DE TEST EN CARACTER
    CHARACTER(LEN=2)  :: NUMBDL  ! NUMERO DE BOUND-LOAD
    CHARACTER(LEN=8)  :: FMT     ! FORMATO DE GUARDADO DE NODO
    CHARACTER(LEN=15),ALLOCATABLE,DIMENSION(:) :: DATAPRINT ! DATOS A GRABAR
    CHARACTER(LEN=8)  :: SNODE   ! CACHE DE INTRODUCCION
    INTEGER NPRINT               ! NUMERO DE NODOS A IMPRIMIR
    INTEGER I,J

    !--------------------------------------------------------------------------
    ! CARGAR VARIABLES
    !--------------------------------------------------------------------------
    FMT = '(I8.8)'
    NPRINT = INT(3*INCO+1)
    WRITE (NUMTEST,'(I2.2)') RUN
    ALLOCATE(DATAPRINT(INT(3*INCO+1)))

    !--------------------------------------------------------------------------
    ! ARCHIVO CON VALORES DE ACELERACION DE CADA NODO
    ! FILE: ACELERATION_Txx.dat
    IF (OU05.GT.0) THEN
        DATFILE = 'ACELERATION_T'//NUMTEST//'.dat'
        OPEN (UNIT=5,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')
        DATAPRINT(1) = 'TIME'
        DO I=1,INCO
            WRITE (SNODE,FMT) NCON(I)
            DATAPRINT(INT(3*I-1))='AX N:'//SNODE
            DATAPRINT(INT(3*I))='AY N:'//SNODE
            DATAPRINT(INT(3*I+1))='AZ N:'//SNODE
        END DO
        WRITE (5,105) DATAPRINT
105     FORMAT((A15),',',<NPRINT-1>(3X,A15,','))
        PR05=1
    END IF

    ! ARCHIVO CON VALORES DE VELOCIDAD DE CADA NODO
    ! FILE: VELOCITY_Txx.dat
    IF (OU06.GT.0)THEN
        DATFILE = 'VELOCITY_T'//NUMTEST//'.dat'
        OPEN (UNIT=6,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')
        DATAPRINT(1) = 'TIME'
        DO I=1,INCO
            WRITE (SNODE,FMT) NCON(I)
            DATAPRINT(INT(3*I-1))='VX N:'//SNODE
            DATAPRINT(INT(3*I))='VY N:'//SNODE
            DATAPRINT(INT(3*I+1))='VZ N:'//SNODE
        END DO

        WRITE (6,106) DATAPRINT
106     FORMAT((A15),',',<NPRINT-1>(3X,A15,','))
        PR06=1
    END IF

    ! ARCHIVO CON VALORES DE DESPLAZAMIENTO DE CADA NODO
    ! FILE: DISPLACEMENT_Txx.dat
    IF (OU07.GT.0) THEN
        DATFILE = 'DISPLACEMENT_T'//NUMTEST//'.dat'
        OPEN (UNIT=7,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')
        DATAPRINT(1) = 'TIME'
        DO I=1,INCO
            WRITE (SNODE,FMT) NCON(I)
            DATAPRINT(INT(3*I-1))='UX N:'//SNODE
            DATAPRINT(INT(3*I))='UY N:'//SNODE
            DATAPRINT(INT(3*I+1))='UZ N:'//SNODE
        END DO
        WRITE (7,107) DATAPRINT
107     FORMAT((A15),',',<NPRINT-1>(3X,A15,','))
        PR07=1
    END IF

    ! ARCHIVO CON VALORES DE ENERGIA PARA CADA TIEMPO
    ! FILE: ENERGY_Txx.dat
    IF (OU08.GT.0) THEN
        DATFILE = 'ENERGY_T'//NUMTEST//'.dat'
        OPEN (UNIT=8,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')
        WRITE (8,108) 'TIME','ENEX','ENCN','ENEL','ENGD','ENDP','ENIN','ENCND','ENELD','ENGDD','ENDPD'
108     FORMAT((A15),',',10(2X,A15,','))
        PR08=1
    END IF

    ! ARCHIVO CON VALORES DE LAS BARRAS ROTAS PARA CADA TIEMPO
    ! FILE: BARBROKEN_Txx.dat
    IF (OU09.GT.0) THEN
        DATFILE = 'BARBROKEN_T'//NUMTEST//'.dat'
        OPEN (UNIT=9,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')
        SELECT CASE (LAW)
        CASE (2,4) ! LEY BILINEAL
            NPRINT=11
            WRITE (9,109) 'TIME','N BAR','POS X','POS Y','POS Z',&
                &'POS0 X NOI','POS0 Y NOI','POS0 Z NOI',&
                &'POS0 X NOF','POS0 Y NOF','POS0 Z NOF','EP'
        CASE (3) ! LEY TRILINEAL
            NPRINT=13
            WRITE (9,109) 'TIME','N BAR','POS X','POS Y','POS Z',&
                &'POS0 X NOI','POS0 Y NOI','POS0 Z NOI',&
                &'POS0 X NOF','POS0 Y NOF','POS0 Z NOF','EP','EP1'
        END SELECT
109     FORMAT((A15),',',<NPRINT>(3X,A15,','))
        PR09=1
    END IF

    ! ARCHIVO CON VALORES DE LAS BARRAS ROTAS PARA CADA TIEMPO
    ! FILE: BARDAMAGEN_Txx.dat
    IF (OU10.GT.0)THEN
        DATFILE = 'BARDAMAGEN_T'//NUMTEST//'.dat'
        OPEN (UNIT=10,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')
        SELECT CASE (LAW)
        CASE (2,4) ! LEY BILINEAL
            WRITE (10,110) 'TIME','N BAR','POS X','POS Y','POS Z',&
                &'POS0 X NOI','POS0 Y NOI','POS0 Z NOI',&
                &'POS0 X NOF','POS0 Y NOF','POS0 Z NOF','EP'
            NPRINT=11
        CASE (3) ! LEY TRILINEAL
            WRITE (10,110) 'TIME','N BAR','POS X','POS Y','POS Z',&
                &'POS0 X NOI','POS0 Y NOI','POS0 Z NOI',&
                &'POS0 X NOF','POS0 Y NOF','POS0 Z NOF','EP','EP1'
            NPRINT=12
        END SELECT
110     FORMAT((A15),',',<NPRINT>(3X,A15,','))
        PR10=1
    END IF

    ! ARCHIVO CON VALORES DE TENSION Y DEFORMACION PARA CADA TIEMPO
    ! FILE: STRESS_Txx.dat
    IF (OU11.GT.0)THEN
        DATFILE = 'STRESS_T'//NUMTEST//'.dat'
        OPEN (UNIT=11,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')
        WRITE (11,111) 'TIME','SXX','SYY','SZZ','SYZ','SXZ','SXY',&
            &'EXX','EYY','EZZ','EYZ','EXZ','EXY'
111     FORMAT((A15),',',12(3X,A15,','))
        PR11=1
    END IF

    ! ARCHIVO CON VALORES DE STR,EP,ER DE LAS BARRAS A CADA TIEMPO
    ! FILE: CONSTIT_Txx.dat
    IF (IBCO.GT.0)THEN
        DEALLOCATE(DATAPRINT)
        DATFILE = 'CONSTIT_T'//NUMTEST//'.dat'
        OPEN (UNIT=14,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')
        SELECT CASE (LAW)
        CASE (2) ! LEY BILINEAL
            NPRINT=INT(5*IBCO+1)
            ALLOCATE(DATAPRINT(NPRINT))
            DATAPRINT(1) = 'TIME'
            DO I=1,IBCO
                WRITE (SNODE,FMT) BCON(I)
                DATAPRINT(INT(5*I-3))='STR:'//SNODE
                DATAPRINT(INT(5*I-2))='FORCE:'//SNODE
                DATAPRINT(INT(5*I-1))='EP:'//SNODE
                DATAPRINT(INT(5*I))='ER:'//SNODE
                DATAPRINT(INT(5*I+1))='KR:'//SNODE
            END DO
            WRITE (14,114) DATAPRINT
        CASE (3) ! LEY TRILINEAL
            NPRINT=INT(5*IBCO+1)
            ALLOCATE(DATAPRINT(NPRINT))
            DATAPRINT(1) = 'TIME'
            DO I=1,IBCO
                WRITE (SNODE,FMT) BCON(I)
                DATAPRINT(INT(5*I-3))='STR:'//SNODE
                DATAPRINT(INT(5*I-2))='FORCE:'//SNODE
                DATAPRINT(INT(5*I-1))='EP:'//SNODE
                DATAPRINT(INT(5*I))='EP1:'//SNODE
                DATAPRINT(INT(5*I+1))='ER:'//SNODE
            END DO
            WRITE (14,114) DATAPRINT
        CASE (4) ! LEY BILINEAL SHEAR
            NPRINT=INT(5*IBCO+1)
            ALLOCATE(DATAPRINT(NPRINT))
            DATAPRINT(1) = 'TIME'
            DO I=1,IBCO
                WRITE (SNODE,FMT) BCON(I)
                DATAPRINT(INT(5*I-3))='STR:'//SNODE
                DATAPRINT(INT(5*I-2))='FORCE:'//SNODE
                DATAPRINT(INT(5*I-1))='EP:'//SNODE
                DATAPRINT(INT(5*I))='ER:'//SNODE
                DATAPRINT(INT(5*I+1))='KR:'//SNODE
            END DO
            WRITE (14,114) DATAPRINT
        END SELECT
114     FORMAT((A15),',',<NPRINT-1>(3X,A15,','))
    END IF

    ! ARCHIVO CON VALORES DE POSICION ENERGIA DE LOS NODOS A CADA TIEMPO
    ! FILE: DeltaENCN_Txx.dat
    IF (OU16.GT.0)THEN
        DEALLOCATE(DATAPRINT)
        DATFILE = 'DeltaENCN_T'//NUMTEST//'.dat'
        OPEN (UNIT=16,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')
        NPRINT=7
        ALLOCATE(DATAPRINT(NPRINT))
        DATAPRINT(1)='TIME'
        DATAPRINT(2)='NODE'
        DATAPRINT(3)='POSX'
        DATAPRINT(4)='POSY'
        DATAPRINT(5)='POSZ'
        DATAPRINT(6)='DENCN'
        DATAPRINT(7)='DENDA'
        WRITE (16,116) DATAPRINT
116     FORMAT((A15),',',<NPRINT-1>(3X,A15,','))
        DEALLOCATE(DATAPRINT)
    END IF

    ! ARCHIVO CON VALORES DE POSICION ENERGIA DE LOS NODOS A CADA TIEMPO
    ! FILE: FORCE_Txx.dat
    IF (OU17.GT.0)THEN
        DEALLOCATE(DATAPRINT)
        DATFILE = 'FORCE_T'//NUMTEST//'.dat'
        OPEN (UNIT=17,ACCESS='SEQUENTIAL',FILE=DATFILE,STATUS='UNKNOWN')
        
        NPRINT=1
        NPRINT=NPRINT+SIZE(BDX,2)+SIZE(BDY,2)+SIZE(BDZ,2)
        NPRINT=NPRINT+SIZE(LOX,2)+SIZE(LOY,2)+SIZE(LOZ,2)
        ALLOCATE(DATAPRINT(NPRINT))
        
        DATAPRINT(1)='TIME'
        J = 1
        DO I=1,SIZE(BDX,2)
            J = J+1
            WRITE (NUMBDL,'(I2.2)') I
            DATAPRINT(J)='BDX:'//NUMBDL
        END DO
        
        DO I=1,SIZE(BDY,2)
            J = J+1
            WRITE (NUMBDL,'(I2.2)') I
            DATAPRINT(J)='BDY:'//NUMBDL
        END DO
        
        DO I=1,SIZE(BDZ,2)
            J = J+1
            WRITE (NUMBDL,'(I2.2)') I
            DATAPRINT(J)='BDZ:'//NUMBDL
        END DO
        
        DO I=1,SIZE(LOX,2)
            J = J+1
            WRITE (NUMBDL,'(I2.2)') I
            DATAPRINT(J)='LOX:'//NUMBDL
        END DO
        
        DO I=1,SIZE(LOY,2)
            J = J+1
            WRITE (NUMBDL,'(I2.2)') I
            DATAPRINT(J)='LOY:'//NUMBDL
        END DO
        
        DO I=1,SIZE(LOZ,2)
            J = J+1
            WRITE (NUMBDL,'(I2.2)') I
            DATAPRINT(J)='LOZ:'//NUMBDL
        END DO
        
        WRITE (17,117) DATAPRINT
117     FORMAT((A15),',',<NPRINT-1>(3X,A15,','))
        DEALLOCATE(DATAPRINT)
    END IF

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
