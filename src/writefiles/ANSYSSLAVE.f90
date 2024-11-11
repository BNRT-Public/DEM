    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE ANSYSSLAVE(IFILE)
    !
    ! DATA:  02/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESCRIBE ARCHIVO PARA SER LEIDO EN ANSYS
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    INTEGER BS, BD, BR
    INTEGER*4 ELEMTYPE

    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    INTEGER IFILE

    !--------------------------------------------------------------------------
    ! INICIO DE VARIABLES
    !--------------------------------------------------------------------------
    BS = COUNT(.NOT.DM(1,:)) ! SIN DANO
    BD = COUNT((DM(1,:)).AND.(.NOT.FS)) ! CON DANO
    BR = COUNT(FS) ! ROTAS

    !--------------------------------------------------------------------------
    WRITE (IFILE,*) '! ESTADO DE LAS BARRAS'
    !--------------------------------------------------------------------------
    WRITE (IFILE,*)'! BARRAS INTACTAS =','    ',BS
    WRITE (IFILE,*)'! BARRAS DANIFICADAS =',' ',BD
    WRITE (IFILE,*)'! BARRAS ROMPIDAS =','    ',BR
    WRITE (IFILE,*)
    !--------------------------------------------------------------------------
    WRITE (IFILE,*)
    WRITE (IFILE,*) 'FINISH'
    WRITE (IFILE,*) '/CLEAR'
    WRITE (IFILE,*) '/PREP7'
    WRITE (IFILE,*) '/INPUT,''ANSYS_T01_00000000'',''mac'',,, 0' 
    WRITE (IFILE,*) '/TITLE, TEMPO =',TIME
    WRITE (IFILE,*)

    !--------------------------------------------------------------------------
    WRITE (IFILE,*) '! COORDENADAS NODALES'
    !--------------------------------------------------------------------------
    DO I=1,NNT
        WRITE (IFILE,*) 'NMODIF,',I,',',SNGL(U(3,I)),',',SNGL(V(3,I)),',',SNGL(W(3,I))
    END DO
    WRITE (IFILE,*)
    !--------------------------------------------------------------------------
    WRITE (IFILE,*) '! CONECTIVIDADES DE LOS ELEMENTOS'
    !--------------------------------------------------------------------------
    DO I=1,NBT
        IF (.NOT.DM(1,I)) THEN
            ! BARRAS SIN DANO
            ELEMTYPE=1
        ELSE
            IF (.NOT.FS(I)) THEN
                ! BARRAS CON DANO
                ELEMTYPE=2
            ELSE
                ! BARRAS ROTAS
                ELEMTYPE=5
            END IF
        END IF
        
        IF (CACHEANSYS(I).NE.ELEMTYPE) THEN
            WRITE (IFILE,*) 'EMODIF,',I,',TYPE,',ELEMTYPE,', !ELEMENT',I
        END IF
    END DO

    WRITE (IFILE,*)
    WRITE (IFILE,*) '! COLORES DE LOS ELEMENTOS'
    !--------------------------------------------------------------------------
    WRITE (IFILE,*) 'ESEL,S,TYPE,,1'
    WRITE (IFILE,*) '/COLOR,ELEM,WHIT'
    WRITE (IFILE,*) '/PNUM,TYPE,0'
    WRITE (IFILE,*) '/NUMBER,1'
    WRITE (IFILE,*) 'ESEL,ALL'

    WRITE (IFILE,*) 'ESEL,S,TYPE,,2'
    WRITE (IFILE,*) '/COLOR,ELEM,DGRA'
    WRITE (IFILE,*) '/PNUM,TYPE,0'
    WRITE (IFILE,*) '/NUMBER,1'
    WRITE (IFILE,*) 'ESEL,ALL'

    WRITE (IFILE,*) 'ESEL,S,TYPE,,3'
    WRITE (IFILE,*) '/COLOR,ELEM,YGREE'
    WRITE (IFILE,*) '/PNUM,TYPE,0'
    WRITE (IFILE,*) '/NUMBER,1'
    WRITE (IFILE,*) 'ESEL,ALL'

    WRITE (IFILE,*) 'ESEL,S,TYPE,,4'
    WRITE (IFILE,*) '/COLOR,ELEM,LGRA'
    WRITE (IFILE,*) '/PNUM,TYPE,0'
    WRITE (IFILE,*) '/NUMBER,1'
    WRITE (IFILE,*) 'ESEL,ALL'

    WRITE (IFILE,*) 'ESEL,S,TYPE,,5'
    WRITE (IFILE,*) '/COLOR,ELEM,GREE'
    WRITE (IFILE,*) '/PNUM,TYPE,0'
    WRITE (IFILE,*) '/NUMBER,1'
    WRITE (IFILE,*) 'ESEL,ALL'

    WRITE (IFILE,*) 'ESEL,S,TYPE,,6'
    WRITE (IFILE,*) '/COLOR,ELEM,GREE'
    WRITE (IFILE,*) '/PNUM,TYPE,0'
    WRITE (IFILE,*) '/NUMBER,1'
    WRITE (IFILE,*) 'ESEL,ALL'
    WRITE (IFILE,*)
    
    !--------------------------------------------------------------------------
    WRITE (IFILE,*)
    WRITE (IFILE,*) '/VIEW,1,0,-1,0'
    WRITE (IFILE,*) 'EPLOT'
    WRITE (IFILE,*)

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------