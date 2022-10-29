    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE ANSYS
    !
    ! DATA:  02/07/2019
    ! AUTOR: Ignacio Iturrioz
    !        Boris N. Rojo Tanzi
    !
    ! ESCRIBE ARCHIVO PARA SER LEIDO EN ANSYS
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,J
    INTEGER BS, BD, BR

    !--------------------------------------------------------------------------
    ! INICIO DE VARIABLES
    !--------------------------------------------------------------------------
    BS = COUNT(.NOT.DM(1,:)) ! SIN DANO
    BD = COUNT((DM(1,:)).AND.(.NOT.FS)) ! CON DANO
    BR = COUNT(FS) ! ROTAS

    !--------------------------------------------------------------------------
    WRITE (3,*) '! TIME = ',TIME
    WRITE (3,*) '! ESTADO DE LAS BARRAS'
    !--------------------------------------------------------------------------
    WRITE (3,*)'! BARRAS INTACTAS =','    ',BS
    WRITE (3,*)'! BARRAS DANIFICADAS =',' ',BD
    WRITE (3,*)'! BARRAS ROMPIDAS =','    ',BR
    WRITE (3,*)
    !--------------------------------------------------------------------------
    WRITE (3,*)
    WRITE (3,*) 'FINISH'
    WRITE (3,*) '/CLEAR'
    WRITE (3,*) '/PREP7'
    WRITE (3,*) '/TITLE, TIME =',TIME
    WRITE (3,*) 'ET,1,4'
    WRITE (3,*) 'ET,2,4'
    WRITE (3,*) 'ET,IFILE,4'
    WRITE (3,*) 'ET,4,4'
    WRITE (3,*) 'ET,5,4'
    WRITE (3,*) 'ET,6,4'
    WRITE (3,*) 'MP,EX,1,2E10'
    WRITE (3,*) 'MP,PRXY,1,0.25'
    WRITE (3,*) 'MP,DENS,1,2500'
    WRITE (3,*) 'R,1,0.13,0.0014,0.36'
    WRITE (3,*)
    !--------------------------------------------------------------------------
    WRITE (3,*) '! COORDENADAS NODALES'
    !--------------------------------------------------------------------------
    DO I=1,NNT
        WRITE (3,*) 'N,',I,',',SNGL(U(3,I)),',',SNGL(V(3,I)),',',SNGL(W(3,I))
    END DO
    WRITE (3,*)
    !--------------------------------------------------------------------------
    WRITE (3,*) '! CONECTIVIDADES DE LOS ELEMENTOS'
    !--------------------------------------------------------------------------
    DO I=1,NBT
        IF (.NOT.DM(1,I)) THEN
            ! BARRAS SIN DANO
            WRITE (3,*) 'TYPE,1'
            WRITE (3,*) 'E',',',CN(1,I),',',CN(2,I),',!EL',I
        ELSE
            IF (.NOT.FS(I)) THEN
                ! BARRAS CON DANO
                WRITE (3,*) 'TYPE,2'
                WRITE (3,*) 'E',',',CN(1,I),',',CN(2,I),',!EL',I
            ELSE
                ! BARRAS ROTAS
                WRITE (3,*) 'TYPE,5'
                WRITE (3,*) 'E',',',CN(1,I),',',CN(2,I),',!EL',I
            END IF
        END IF
    END DO

    WRITE (3,*)
    WRITE (3,*) '! COLORES DE LOS ELEMENTOS'
    !--------------------------------------------------------------------------
    WRITE (3,*) 'ESEL,S,TYPE,,1'
    WRITE (3,*) '/COLOR,ELEM,WHIT'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'

    WRITE (3,*) 'ESEL,S,TYPE,,2'
    WRITE (3,*) '/COLOR,ELEM,ORAN'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'

    WRITE (3,*) 'ESEL,S,TYPE,,3'
    WRITE (3,*) '/COLOR,ELEM,YGREE'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'

    WRITE (3,*) 'ESEL,S,TYPE,,4'
    WRITE (3,*) '/COLOR,ELEM,DGRA'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'

    WRITE (3,*) 'ESEL,S,TYPE,,5'
    WRITE (3,*) '/COLOR,ELEM,RED'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'

    WRITE (3,*) 'ESEL,S,TYPE,,6'
    WRITE (3,*) '/COLOR,ELEM,GREE'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'
    WRITE (3,*)
    !--------------------------------------------------------------------------
    WRITE (3,*) '! CONDICIONES DE CONTORNO'
    !--------------------------------------------------------------------------
    DO I=1,NNT
        DO J=1,SIZE(BDX,2)
            IF (BDX(I,J)) THEN
                WRITE (3,*) 'D,',I,',','UX'
            END IF
        END DO
        DO J=1,SIZE(BDY,2)
            IF (BDY(I,J)) THEN
                WRITE (3,*) 'D,',I,',','UY'
            END IF
        END DO
        DO J=1,SIZE(BDZ,2)
            IF (BDZ(I,J)) THEN
                WRITE (3,*) 'D,',I,',','UZ'
            END IF
        END DO
    END DO
    WRITE (3,*)
    !--------------------------------------------------------------------------
    WRITE (3,*) '! CARGAS APLICADAS'
    !--------------------------------------------------------------------------
    DO I=1,NNT
        DO J=1,SIZE(LOX,2)
            IF (LOX(I,J)) THEN
                WRITE (3,*) 'F,',I,',','FX',',',10*(U(2,I)-U0(I))
            END IF
        END DO
        DO J=1,SIZE(LOY,2)
            IF (LOY(I,J)) THEN
                WRITE (3,*) 'F,',I,',','FY',',',10*(V(2,I)-V0(I))
            END IF
        END DO
        DO J=1,SIZE(LOZ,2)
            IF (LOZ(I,J)) THEN
                WRITE (3,*) 'F,',I,',','FZ',',',10*(W(2,I)-W0(I))
            END IF
        END DO
    END DO
    !
    WRITE (3,*)
    !--------------------------------------------------------------------------
    WRITE (3,*) '/VIEW,1,0,-1,0'
    WRITE (3,*) 'EPLOT'
    WRITE (3,*)

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------