    !-------------------------------------------------------------------------
    !-------------------------------------------------------------------------
    !
    SUBROUTINE PARAVIEW
    !
    ! DATA:  02/07/2019
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! ESCRIBE ARCHIVO PARA SER LEIDO EN PARAVIEW
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I
    INTEGER BS, BD, BR

    !--------------------------------------------------------------------------
    ! INICIO DE VARIABLES
    !--------------------------------------------------------------------------
    BS = COUNT(.NOT.DM(1,:))
    BD = COUNT((DM(1,:)).AND.(.NOT.FS))
    BR = COUNT(FS)
    
    !--------------------------------------------------------------------------
    WRITE (12,'(A26)')'# vtk DataFile Version 3.0'
    WRITE (12,'(A10)')'vtk output'
    WRITE (12,'(A5)')'ASCII'
    !--------------------------------------------------------------------------
    ! COORDENADAS NODALES
    !--------------------------------------------------------------------------
    WRITE (12,'(A16)')'DATASET POLYDATA'
    WRITE (12,'(A6,I8,A6)')'POINTS',NNT,' float'
    DO I=1,NNT
        WRITE (12,*) U(2,I),V(2,I),W(2,I)
    END DO
!122 FORMAT((F15.8),',',2(3X,F15.8,','))    
    !--------------------------------------------------------------------------
    ! CONECTIVIDADES
    !--------------------------------------------------------------------------
    WRITE (12,*)
    WRITE (12,*)'LINES',NBT,' ',INT(3*NBT)
    DO I=1,NBT
       WRITE (12,*)'2 ',CN(1,I),CN(2,I)
    END DO
    !--------------------------------------------------------------------------
    ! BARRAS
    !--------------------------------------------------------------------------
    WRITE (12,*)'CELL_DATA ',NBT
    WRITE (12,*)'SCALARS BARRA float'
    WRITE (12,*)'LOOKUP_TABLE cores_barras'
    
    DO I=1,NBT
        IF (.NOT.DM(1,I)) THEN
            !     BARRAS SEM DANO - COR CYAN
            WRITE (12,*) '0.0'
        ELSE
            IF (.NOT.FS(I)) THEN
                !     BARRAS DANIFICADAS - COR AMARELA
                WRITE (12,*) '0.5'
            ELSE
                !     BARRAS ROMPIDAS - COR VERMELHA
                WRITE (12,*) '1.0'
            END IF
        END IF
    END DO
    
    WRITE (12,*)'LOOKUP_TABLE cores_barras 3'
    WRITE (12,*)'0.0 1.0 1.0 1.0'
    WRITE (12,*)'1.0 1.0 0.0 1.0'
    WRITE (12,*)'1.0 0.0 0.0 1.0'
    
    !--------------------------------------------------------------------------
    ! BARARAS SANAS
    !--------------------------------------------------------------------------
    WRITE (12,*)'SCALARS INTEIRAS float'
    WRITE (12,*)'LOOKUP_TABLE barras_inteiras'
    
    DO I=1,NBT
        IF (DM(1,I)) THEN
            !     BARRAS SEM DANO - COLORIDA
            WRITE (12,*) '1.0'
        ELSE
            IF (FS(I)) THEN
                !     BARRAS DANIFICADAS - TRANSPARENTE
                WRITE (12,*) '0.0'
            ELSE
                !     BARRAS ROMPIDAS - TRANSPARENTE
                WRITE (12,*) '0.0'
            END IF
        END IF
    END DO
    
    WRITE (12,*)'LOOKUP_TABLE barras_inteiras 2'
    WRITE (12,*)'0.0 0.0 0.0 0.0'
    WRITE (12,*)'0.0 1.0 1.0 1.0'
    
    !--------------------------------------------------------------------------
    ! BARRAS DANADAS
    !--------------------------------------------------------------------------
    WRITE (12,*)'SCALARS DANIFICADAS float'
    WRITE (12,*)'LOOKUP_TABLE barras_danificadas'
    
    DO I=1,NBT
        IF (DM(1,I)) THEN
            !     BARRAS SEM DANO - TRANSPARENTE
            WRITE (12,*) '0.0'
        ELSE
            IF (FS(I)) THEN
                !     BARRAS DANIFICADAS - COLORIDA
                WRITE (12,*) '1.0'
            ELSE
                !     BARRAS ROMPIDAS - TRANSPARENTE
                WRITE (12,*) '0.0'
            END IF
        END IF
        !
    END DO
    
    WRITE (12,*)'LOOKUP_TABLE barras_danificadas 2'
    WRITE (12,*)'0.0 0.0 0.0 0.0'
    WRITE (12,*)'1.0 1.0 0.0 1.0'
    
    !--------------------------------------------------------------------------
    ! BARRAS ROTAS
    !--------------------------------------------------------------------------
    WRITE (12,*)'SCALARS ROMPIDAS float'
    WRITE (12,*)'LOOKUP_TABLE barras_rompidas'
    
    DO I=1,NBT
        IF (.NOT.DM(1,I)) THEN
            !     BARRAS SEM DANO - TRANSPARENTE
            WRITE (12,*) '0.0'
        ELSE
            IF (.NOT.FS(I)) THEN
                !     BARRAS DANIFICADAS - TRANSPARENTE
                WRITE (12,*) '0.0'
            ELSE
                !     BARRAS ROMPIDAS - COLORIDA
                WRITE (12,*) '1.0'
            END IF
        END IF
    END DO
    
    WRITE (12,*)'LOOKUP_TABLE barras_rompidas 2'
    WRITE (12,*)'0.0 0.0 0.0 0.0'
    WRITE (12,*)'0.0 1.0 1.0 1.0'

    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------