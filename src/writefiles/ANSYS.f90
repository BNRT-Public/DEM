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
    integer i,j,k
    integer bs, bd, br

    !--------------------------------------------------------------------------
    ! INICIO DE VARIABLES
    !--------------------------------------------------------------------------
    bs = count(.not.dm(1,:)) ! sin dano
    bd = count((dm(1,:)).and.(.not.fs)) ! con dano
    br = count(fs) ! rotas
    k=0
    
    !--------------------------------------------------------------------------
    write (3,*) '! Time = ',time
    write (3,*) '! Estado de los elementos'
    !--------------------------------------------------------------------------
    write (3,*)'! Elementos intactas =','    ',bs
    write (3,*)'! Elementos danificadas =',' ',bd
    write (3,*)'! Elementos rompidas =','    ',br
    write (3,*)
    !--------------------------------------------------------------------------
    WRITE (3,*)
    WRITE (3,*) 'FINISH'
    WRITE (3,*) '/CLEAR'
    WRITE (3,*) '/PREP7'
    WRITE (3,*) '/TITLE, TIME =',TIME
    WRITE (3,*) '/PREP7'
    WRITE (3,*) 'ET,10,MASS21,0,0,2'
    WRITE (3,*) 'ET,1,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,2,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,3,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,4,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,5,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,6,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,7,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,8,COMBIN14,0,0,0'
    WRITE (3,*)
    WRITE (3,*) 'ET,100,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,110,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,200,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,300,COMBIN14,0,0,0'
    WRITE (3,*)
    WRITE (3,*) 'ET,1000,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,1100,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,2000,COMBIN14,0,0,0'
    WRITE (3,*) 'ET,3000,COMBIN14,0,0,0'
    WRITE (3,*)
    !--------------------------------------------------------------------------
    WRITE (3,*) '! COORDENADAS NODALES'
    write (3,*) '! MASAS NODALES'
    !--------------------------------------------------------------------------
    write (3,*)
    write (3,*) 'type,10' 
    do i=1,nnt
        k=k+1
        write (3,31) i
        write (3,32,advance='yes') k,mas(i),mas(i),mas(i)
        write (3,33) k
        write (3,34,advance='yes') i,u(3,i),v(3,i),w(3,i)
        write (3,35) i
    end do
31  format('! node',i15)
32  format('r,',i15,',',3(e21.14,','),',,,')
33  format('real,',i15)
34  format('n,',i15,',',3(e21.14,','))
35  format('e,',i15)
    
    write (3,*)
    !--------------------------------------------------------------------------
    write (3,*) '! Conectividades de los elementos'
    write (3,*) '! Rigideces'
    !--------------------------------------------------------------------------
    write (3,*)
    DO I=1,NBT
        k = k+1
        write (3,*) '! element',i
        !
        write (3,*) 'r,',k,',',etrac(1,i)/lbar(i),',,,,,,'
        !
        write (3,*) 'real,',k,' $ type,',elemtype(i)
        write (3,*) 'e,',cn(1,i),',',cn(2,i)
            
        !IF (.NOT.DM(1,I)) THEN
        !    ! BARRAS SIN DANO
        !    WRITE (3,*) 'TYPE,1'
        !    WRITE (3,*) 'E',',',CN(1,I),',',CN(2,I),',!EL',I
        !ELSE
        !    IF (.NOT.FS(I)) THEN
        !        ! BARRAS CON DANO
        !        WRITE (3,*) 'TYPE,2'
        !        WRITE (3,*) 'E',',',CN(1,I),',',CN(2,I),',!EL',I
        !    ELSE
        !        ! BARRAS ROTAS
        !        WRITE (3,*) 'TYPE,5'
        !        WRITE (3,*) 'E',',',CN(1,I),',',CN(2,I),',!EL',I
        !    END IF
        !END IF
    END DO

    WRITE (3,*)
    !--------------------------------------------------------------------------
    WRITE (3,*) '! COLORES DE LOS ELEMENTOS'
    !--------------------------------------------------------------------------
    ! Elementos masa
    write (3,*) 'esel,s,type,,10'
    write (3,*) '/color,elem,whit'
    write (3,*) '/pnum,type,0'
    write (3,*) '/number,1'
    write (3,*) 'esel,all'
    
    ! Elementos resorte
    WRITE (3,*) 'ESEL,S,TYPE,,1'
    WRITE (3,*) '/COLOR,ELEM,LGRA'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'

    WRITE (3,*) 'ESEL,S,TYPE,,2'
    WRITE (3,*) '/COLOR,ELEM,BLUE'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'

    WRITE (3,*) 'ESEL,S,TYPE,,3'
    WRITE (3,*) '/COLOR,ELEM,RED'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'
    
    WRITE (3,*) 'ESEL,S,TYPE,,4'
    WRITE (3,*) '/COLOR,ELEM,ORAN'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'

    WRITE (3,*) 'ESEL,S,TYPE,,5'
    WRITE (3,*) '/COLOR,ELEM,cyan'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'

    WRITE (3,*) 'ESEL,S,TYPE,,6'
    WRITE (3,*) '/COLOR,ELEM,bmag'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'
    
    WRITE (3,*) 'ESEL,S,TYPE,,7'
    WRITE (3,*) '/COLOR,ELEM,mred'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'

    WRITE (3,*) 'ESEL,S,TYPE,,8'
    WRITE (3,*) '/COLOR,ELEM,ygree'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'
    
    ! Elementos sanos a traccion
    WRITE (3,*) 'ESEL,S,TYPE,,100'
    WRITE (3,*) '/COLOR,ELEM,LGRA'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'
    
    ! Elementos sanos a traccion
    WRITE (3,*) 'ESEL,S,TYPE,,110'
    WRITE (3,*) '/COLOR,ELEM,LGRA'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'
    
    ! Elementos con dano
    WRITE (3,*) 'ESEL,S,TYPE,,200'
    WRITE (3,*) '/COLOR,ELEM,BLUE'
    WRITE (3,*) '/PNUM,TYPE,0'
    WRITE (3,*) '/NUMBER,1'
    WRITE (3,*) 'ESEL,ALL'
    
    ! Elementos rotos
    WRITE (3,*) 'ESEL,S,TYPE,,300'
    WRITE (3,*) '/COLOR,ELEM,RED'
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