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
		NPRINT=INT(10)
		ALLOCATE(DATAPRINT(NPRINT))
		DATAPRINT(1)= 'NBAR'
		DATAPRINT(2)= 'UBAR'
		DATAPRINT(3)= 'VBAR'
		DATAPRINT(4)= 'WBAR'
		DATAPRINT(5)= 'GFR'
		DATAPRINT(6)= 'YNG'
		DATAPRINT(7)= 'EP'
		DATAPRINT(8)= 'ER'
		DATAPRINT(9)= 'DM'
		DATAPRINT(10)='FS'
	CASE (3) ! LEY TRILINEAL
		NPRINT=INT(11)
		ALLOCATE(DATAPRINT(NPRINT))
		DATAPRINT(1)= 'NBAR'
		DATAPRINT(2)= 'UBAR'
		DATAPRINT(3)= 'VBAR'
		DATAPRINT(4)= 'WBAR'
		DATAPRINT(5)= 'GFR'
		DATAPRINT(6)= 'YNG'
		DATAPRINT(7)= 'EP'
		DATAPRINT(8)= 'EP1'
		DATAPRINT(9)= 'ER'
		DATAPRINT(10)='DM'
		DATAPRINT(11)='FS'
	END SELECT
	WRITE (13,113) DATAPRINT
113	FORMAT((A12),',',<NPRINT>(3X,A12,','))

	! DATOS
	SELECT CASE (LAW)
	CASE (2,4) ! LEY BILINEAL
		DO I=1,NBT
			WRITE (13,213) I,XB(I),YB(I),ZB(I),GFRB(I),YNGB(I),EP(1,I),ER(I),DM(1,I),FS(I)
		END DO
213		FORMAT((I12),',',7(3X,E13.6,','),2(3X,L12,','))
	CASE (3) ! LEY TRILINEAL
		DO I=1,NBT
			WRITE (13,313) I,XB(I),YB(I),ZB(I),GFRB(I),YNGB(I),EP(1,I),EP(2,I),ER(I),DM(1,I),FS(I)
		END DO
313		FORMAT((I12),',',8(3X,E13.6,','),2(3X,L12,','))
	END SELECT

	RETURN
	END
	!
	!--------------------------------------------------------------------------
	!--------------------------------------------------------------------------