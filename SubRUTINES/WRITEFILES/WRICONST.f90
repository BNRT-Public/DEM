    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE WRICONST
    !
    ! DATA:  06/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESTA SUBRUTINA ESCRIBE EL ESTADO DE LAS BARRAS EN EL ARCHIVO CONSTIT_Txx.dat
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    INTEGER NPRINT

    DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) :: DATAPRINT

    !--------------------------------------------------------------------------
    IF (IBCO.GT.0)THEN
        IF (OU14.GT.0) THEN
            IF (PR14.EQ.OU14) THEN
                SELECT CASE (LAW)
                CASE (2) ! LEY BILINEAL
                    NPRINT=INT(5*IBCO)
                    ALLOCATE(DATAPRINT(NPRINT))
                    DO I=1,IBCO
                        DATAPRINT(INT(5*I-4))=STR(BCON(I))
                        DATAPRINT(INT(5*I-3))=FORCE(1,BCON(I))
                        DATAPRINT(INT(5*I-2))=EP(1,BCON(I))
                        DATAPRINT(INT(5*I-1))=ER(BCON(I))
                        DATAPRINT(INT(5*I))=KR(1,BCON(I))
                    END DO
                    WRITE (14,114) TIME,DATAPRINT
                CASE (3) ! LEY TRILINEAL
                    NPRINT=INT(5*IBCO)
                    ALLOCATE(DATAPRINT(NPRINT))
                    DO I=1,IBCO
                        DATAPRINT(INT(5*I-4))=STR(BCON(I))
                        DATAPRINT(INT(5*I-3))=FORCE(1,BCON(I))
                        DATAPRINT(INT(5*I-2))=EP(1,BCON(I))
                        DATAPRINT(INT(5*I-1))=EP(2,BCON(I))
                        DATAPRINT(INT(5*I))=ER(BCON(I))
                    END DO
                    WRITE (14,114) TIME,DATAPRINT
                CASE (4) ! LEY BILINEAL SHEAR
                    NPRINT=INT(5*IBCO)
                    ALLOCATE(DATAPRINT(NPRINT))
                    DO I=1,IBCO
                        DATAPRINT(INT(5*I-4))=STR(BCON(I))
                        DATAPRINT(INT(5*I-3))=FORCE(1,BCON(I))
                        DATAPRINT(INT(5*I-2))=EP(1,BCON(I))
                        DATAPRINT(INT(5*I-1))=ER(BCON(I))
                        DATAPRINT(INT(5*I))=KR(2,BCON(I))
                    END DO
                    WRITE (14,114) TIME,DATAPRINT
                END SELECT
114             FORMAT((E15.8),',',<NPRINT>(3X,E15.8,','))
                DEALLOCATE(DATAPRINT)
                PR14=1
            ELSE
                PR14=PR14+1
            END IF

        END IF
    END IF

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------