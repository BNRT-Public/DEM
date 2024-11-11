!--------------------------------------------------------------------------
    !--------------------------------------------------------------------------
    !
    SUBROUTINE ELEMENTFAMILY(ISNODE,NUM)
    IMPLICIT NONE
    !
    ! DATA:  17/10/2022
    ! AUTOR: Boris N. Rojo Tanzi
    !
    ! Edita la geometria cambiando los parametros del modelo con un prima
    !
    !--------------------------------------------------------------------------
    ! VARIABLES LOCALES
    !--------------------------------------------------------------------------
    INTEGER I,J,K
    INTEGER NOI,NOF
    !INTEGER IX
    !LOGICAL CNTEMP(4,NBT), BSEL(NBT)
    LOGICAL LOGIC
    LOGICAL CNTEMP(4)
    
    !--------------------------------------------------------------------------
    ! VARIABLES DE ENTRADA SALIDA
    !--------------------------------------------------------------------------
    DOUBLE PRECISION NUM
    LOGICAL ISNODE
   
    !--------------------------------------------------------------------------
    IF (LAW.EQ.4) THEN
    WRITE (*,*)
    WRITE (*,*) '  ****** GENERATING THE ELEMENT FAMILY    ******  '
    WRITE (*,*) 
    IF (ISNODE) THEN
        ALLOCATE(ELEMNTFAM(26,NBT))
        ELEMNTFAM=0
        
        !$OMP PARALLEL DO PRIVATE(I,LOGIC,J,K,CNTEMP,NOI,NOF) SHARED(ELEMNTFAM,CN)
        DO I=1,NBT
            K=1
            NOI=CN(1,I)
            NOF=CN(2,I)
            
            !!$OMP PARALLEL DO PRIVATE(J,LOGIC,CNTEMP) SHARED(ELEMNTFAM,I,NOI,NOF) FIRSTPRIVATE(K) 
            DO J=1,NBT
                CNTEMP(1:2)=CN(:,J).EQ.NOI
                CNTEMP(3:4)=CN(:,J).EQ.NOF
                LOGIC = ANY(CNTEMP)
                
                IF (I.EQ.J) LOGIC=.FALSE.
                
                IF (LOGIC) THEN    
                    ELEMNTFAM(K,I) = J
                    K=K+1
                END IF
            END DO
            !!$OMP END PARALLEL DO
                        
            !ELEMNTFAM(1:COUNT(BSEL),I)=PACK([(IX,IX=1,SIZE(BSEL))],BSEL)
        END DO
        !$OMP END PARALLEL DO
    END IF
    END IF
    WRITE (*,*) '  **** OUT GENERATING THE ELEMENT FAMILY    ****  '
    RETURN
    END
    !
    !--------------------------------------------------------------------------
    !--------------------------------------------------------------------------