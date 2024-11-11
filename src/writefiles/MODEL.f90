	!-------------------------------------------------------------------------
	!-------------------------------------------------------------------------
	!
	SUBROUTINE MODEL
	!
	! DATA:  18/09/2019
	! AUTOR: Boris N. Rojo Tanzi
	!
	! ESCRIBE ARCHIVO CON LOS DATOS DEL MODELO
	!
	!--------------------------------------------------------------------------
	! VARIABLES LOCALES
	!--------------------------------------------------------------------------
	INTEGER I
	INTEGER BS, BD, BR, NPRINT
	CHARACTER(LEN=12),ALLOCATABLE,DIMENSION(:) :: DATAPRINT ! DATOS A GRABAR

	!--------------------------------------------------------------------------
	! INICIO DE VARIABLES
	!--------------------------------------------------------------------------
	BS = COUNT(.NOT.DM(1,:)) ! SIN DANO
	BD = COUNT((DM(1,:)).AND.(.NOT.FS)) ! CON DANO
	BR = COUNT(FS) ! ROTAS

	!--------------------------------------------------------------------------
	WRITE (13,*) '! TIME = ',TIME
	WRITE (13,*) '! ESTADO DE LAS BARRAS'
	!--------------------------------------------------------------------------
	WRITE (13,*)'! BARRAS INTACTAS =','    ',BS
	WRITE (13,*)'! BARRAS DANIFICADAS =',' ',BD
	WRITE (13,*)'! BARRAS ROMPIDAS =','    ',BR
	WRITE (13,*)
	!--------------------------------------------------------------------------
	WRITE (13,*)

	! TITULO
	SELECT CASE (LAW)
	CASE (2,4) ! LEY BILINEAL
		NPRINT=INT(14)
		ALLOCATE(DATAPRINT(NPRINT))
		DATAPRINT(1)= 'NBAR'
        DATAPRINT(2)= 'CN1'
        DATAPRINT(3)= 'CN2'
		DATAPRINT(4)= 'UBAR'
		DATAPRINT(5)= 'VBAR'
		DATAPRINT(6)= 'WBAR'
		DATAPRINT(7)= 'GFR'
		DATAPRINT(8)= 'YNG'
		DATAPRINT(9)= 'EP'
		DATAPRINT(10)= 'ER'
        DATAPRINT(11)= 'ETRAC'
        DATAPRINT(12)= 'ECOMP'
		DATAPRINT(13)= 'DM'
		DATAPRINT(14)='FS'
	CASE (3) ! LEY TRILINEAL
		NPRINT=INT(15)
		ALLOCATE(DATAPRINT(NPRINT))
		DATAPRINT(1)= 'NBAR'
        DATAPRINT(2)= 'CN1'
        DATAPRINT(3)= 'CN2'
		DATAPRINT(4)= 'UBAR'
		DATAPRINT(5)= 'VBAR'
		DATAPRINT(6)= 'WBAR'
		DATAPRINT(7)= 'GFR'
		DATAPRINT(8)= 'YNG'
		DATAPRINT(9)= 'EP'
		DATAPRINT(10)= 'EP1'
		DATAPRINT(11)= 'ER'
        DATAPRINT(12)= 'ETRAC'
        DATAPRINT(13)= 'ECOMP'
		DATAPRINT(14)='DM'
		DATAPRINT(15)='FS'
	END SELECT
	WRITE (13,113) DATAPRINT
113	FORMAT((A15),',',<NPRINT>(3X,A15,','))

	! DATOS
	SELECT CASE (LAW)
	CASE (2,4) ! LEY BILINEAL
		DO I=1,NBT
			WRITE (13,213) I,CN(1,I),CN(2,I),POSB(:,I),GFRB(I),YNGB(I),EP(1,I),ER(I),ETRAC(1,I),ECOMP(I),DM(1,I),FS(I)
		END DO
213		FORMAT((I15),',',2(3X,I15,','),9(3X,E15.8,','),2(3X,L15,','))
	CASE (3) ! LEY TRILINEAL
		DO I=1,NBT
			WRITE (13,313) I,CN(1,I),CN(2,I),POSB(:,I),GFRB(I),YNGB(I),EP(1,I),EP(2,I),ER(I),ETRAC(1,I),ECOMP(I),DM(1,I),FS(I)
		END DO
313		FORMAT((I15),',',2(3X,I15,','),10(3X,E15.8,','),2(3X,L15,','))
	END SELECT

	RETURN
	END
	!
	!--------------------------------------------------------------------------
	!--------------------------------------------------------------------------