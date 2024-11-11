    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE ANSYSMASTER(IFILE)
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
    WRITE (IFILE,*) '/TITLE, TEMPO =',TIME
    WRITE (IFILE,*) 'ET,1,4'
    WRITE (IFILE,*) 'ET,2,4'
    WRITE (IFILE,*) 'ET,3,4'
    WRITE (IFILE,*) 'ET,4,4'
    WRITE (IFILE,*) 'ET,5,4'
    WRITE (IFILE,*) 'ET,6,4'
    WRITE (IFILE,*) 'MP,EX,1,2E10'
    WRITE (IFILE,*) 'MP,PRXY,1,0.25'
    WRITE (IFILE,*) 'MP,DENS,1,2500'
    WRITE (IFILE,*) 'R,1,0.13,0.0014,0.36'
    WRITE (IFILE,*)
    !--------------------------------------------------------------------------
    WRITE (IFILE,*) '! COORDENADAS NODALES'
    !--------------------------------------------------------------------------
    DO I=1,NNT
        WRITE (IFILE,*) 'N,',I,',',SNGL(U(3,I)),',',SNGL(V(3,I)),',',SNGL(W(3,I))
    END DO
    WRITE (IFILE,*)
    !--------------------------------------------------------------------------
    WRITE (IFILE,*) '! CONECTIVIDADES DE LOS ELEMENTOS'
    !--------------------------------------------------------------------------
    
    DO I=1,NBT
        IF (.NOT.DM(1,I)) THEN
            ! BARRAS SIN DANO
            CACHEANSYS(I)=1
        ELSE
            IF (.NOT.FS(I)) THEN
                ! BARRAS CON DANO
                CACHEANSYS(I)=2
            ELSE
                ! BARRAS ROTAS
                CACHEANSYS(I)=5
            END IF
        END IF
        
        WRITE (IFILE,*) 'TYPE,',CACHEANSYS(I)
        WRITE (IFILE,*) 'EN',',',I,',',CN(1,I),',',CN(2,I),', !ELEMENT',I
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
    WRITE (IFILE,*) '! CONDICIONES DE CONTORNO'
    !--------------------------------------------------------------------------
    DO I=1,NNT
        IF (BDX(I)) THEN
            WRITE (IFILE,*) 'D,',I,',','UX'
        END IF
        IF (BDY(I)) THEN
            WRITE (IFILE,*) 'D,',I,',','UY'
        END IF
        IF (BDZ(I)) THEN
            WRITE (IFILE,*) 'D,',I,',','UZ'
        END IF
    END DO
    WRITE (IFILE,*)
    !--------------------------------------------------------------------------
    WRITE (IFILE,*) '! CARGAS APLICADAS'
    !--------------------------------------------------------------------------
    !IF (TEST.EQ.801) THEN
    !    DO I=1,NNT
    !        IF (V0(I).GE.(N1-0.49D00)*LCO) THEN
    !            WRITE (IFILE,*) 'F,',I,',','FY',',',V(2,I)-V0(I)
    !        END IF
    !    END DO
    !END IF
    !!     TEST = 331 - CUBOS COMPRESSÃO TRIAXIAL
    !IF (TEST.EQ.331) THEN
    !    DO I=1,NNT
    !        IF ((W0(I).LE.0.51D00*LCO).OR.(W0(I).GE.(L1-0.51D00)*LCO))THEN
    !            WRITE (IFILE,*) 'F,',I,',','FZ',',',W(2,I)-W0(I)
    !        END IF
    !        IF (((U0(I).LE.0.51*LCO).AND.(U0(I).GE.0.49*LCO)).OR.((U0(I).GE.(M1-0.51)*LCO).AND.(U0(I).LE.(M1-0.49)*LCO))) THEN
    !            WRITE (IFILE,*) 'F,',I,',','FX',',',FRX(I)
    !        END IF
    !        IF (((V0(I).LE.0.51*LCO).AND.(V0(I).GE.0.49*LCO)).OR.((V0(I).GE.(N1-0.51)*LCO).AND.(V0(I).LE.(N1-0.49)*LCO)))THEN
    !            WRITE (IFILE,*) 'F,',I,',','FY',',',FRY(I)
    !        END IF
    !    END DO
    !END IF
    !
    WRITE (IFILE,*)
    
    !--------------------------------------------------------------------------
    WRITE (IFILE,*) '/VIEW,1,0,-1,0'
    WRITE (IFILE,*) 'EPLOT'
    WRITE (IFILE,*)

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------