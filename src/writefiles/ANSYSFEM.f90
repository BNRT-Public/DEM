!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
      SUBROUTINE ANSYSFEM (IFILE)
!     ================
!
!  DATA:  14/10/1992
!  AUTOR: IGNACIO ITURRIOZ
!
!  ESTA SUBROTINA GERA ARQUIVOS *.ANS (ARQUIVOS QUE PERMITEM REALIZAR
!  SAÍDA GRÁFICA COMPATÍVEL COM O ANSYS).
!
!--------------------------------------------------------------------------
      PARAMETER (INN=1.2E6,INB=6E6,IRU=6,INCON=10)
!--------------------------------------------------------------------------
!     DEFINIÇÃO DE VARIÁVEIS E GRUPOS COMUNS
      INCLUDE 'COMMON.FOR'
!--------------------------------------------------------------------------
      INTEGER II,JJ,KK,IMOD,I,NELEM
      INTEGER, DIMENSION(M*N*L,9) :: NOD
      INTEGER, DIMENSION(NBT,26) :: BAR
      
      INTEGER M2,N2,L2
      INTEGER AUXX,AUXY,AUXZ,AUXZ1
      INTEGER B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16,B17,B18,B19,B20,B21,B22,B23,B24,B25,B26
      INTEGER NN,NNA,NNB,NDIAG
      
	DOUBLE PRECISION FAUX1,FAUX2,FAUXA,FAUXB,F11,F22,F33,F12,F13,F23
      
!
!--------------------------------------------------------------------------
      WRITE (IFILE,*) '/PREP7'
      WRITE (IFILE,*) '/TITLE, TEMPO =',TIME
      WRITE (IFILE,*) 'ET,1,SOLID45,,,,0'
      WRITE (IFILE,*)
!--------------------------------------------------------------------------
      WRITE (IFILE,*) '!COORDENADAS NODALES'
!--------------------------------------------------------------------------
      DO I=1,NNT
        WRITE (IFILE,*) 'N,',I,',',SNGL(U(2,I)),',',SNGL(V(2,I)),',',SNGL(W(2,I))
      END DO
      WRITE (IFILE,*)
!
!-------------------------------------------------------------------------
! CREACION DE ELEMENTOS
!-------------------------------------------------------------------------
      WRITE (IFILE,*) 
      WRITE (IFILE,*) '!ELEMENTOS'
      WRITE (IFILE,*)
      
      NELEM=1
      DO 25 II=1,M1
	DO 25 JJ=1,N1
	DO 25 KK=1,L1
          I=(KK-1)*(N-1)*(M-1)+(JJ-1)*(M-1)+II
      
          NOD(NELEM,1)=M*(JJ-1)+N*M*(KK-1)+II
	    NOD(NELEM,2)=M*(JJ-1)+N*M*(KK-1)+II+1
	    NOD(NELEM,3)=M*(JJ-1)+N*M*(KK-1)+II+1+M*N
	    NOD(NELEM,4)=M*(JJ-1)+N*M*(KK-1)+II+M*N
	    NOD(NELEM,5)=M*(JJ-1)+N*M*(KK-1)+II+M
	    NOD(NELEM,6)=M*(JJ-1)+N*M*(KK-1)+II+1+M
	    NOD(NELEM,7)=M*(JJ-1)+N*M*(KK-1)+II+1+M+M*N
	    NOD(NELEM,8)=M*(JJ-1)+N*M*(KK-1)+II+M+M*N
          NOD(NELEM,9)=M*N*L+I
	    NELEM=NELEM+1
   25 CONTINUE
      NELEM = NELEM-1
 
      DO 35 I=1,NELEM
      WRITE(IFILE,34) 'E',NOD(I,1),NOD(I,2),NOD(I,3),NOD(I,4),NOD(I,5),NOD(I,6),NOD(I,7),NOD(I,8)
   34 FORMAT(A2,',',7(I7,','),I7)
   35 CONTINUE
!
!-------------------------------------------------------------------------
! CARGA DE DESPLAZAMIENTOS
!-------------------------------------------------------------------------
      WRITE (IFILE,*) 
      WRITE (IFILE,*) '!CAMPO DE DESPLAZAMIENTO'
      WRITE (IFILE,*) '/POST1'
      WRITE (IFILE,*)
      
      DO I=1,NNT
          WRITE (IFILE,*) 'DNSOL,',I,',U,X,',SNGL(U(2,I)),',',SNGL(V(2,I)),',',SNGL(W(2,I))
      END DO
      WRITE (IFILE,*) 'FINISH'
!
!-------------------------------------------------------------------------
! CALCULO DE TENSIONES
!-------------------------------------------------------------------------     
      
      WRITE (IFILE,*) 
      WRITE (IFILE,*) '!TENSIONES ELEMENTALES'
      WRITE (IFILE,*) '/POST1'
      WRITE (IFILE,*) 
!-------------------------------------------------------------------------
!
!	IMOD=1
!      M2=M1-1
!      N2=N1-1
!      L2=L1-1
!      
!      DO 10 II=1,M1
!	DO 10 JJ=1,N1
!	DO 10 KK=1,L1
!!-------------------------------------------------------------------------
!! OBTENCION DE LAS BARRAS EN CADA ELEMENTO
!          IF(II.EQ.1)THEN
!              AUXX=M2*N1*(KK-1)+M2*(JJ-1)+II
!          ELSE
!              AUXX=M2*N1*(KK-1)+M2*(JJ-1)+II-1
!          END IF
!       
!          IF(JJ.EQ.1)THEN
!              AUXY=M2*N1*L1+M1*N2*(KK-1)+M1*(JJ-1)+II
!          ELSE
!              AUXY=M2*N1*L1+M1*N2*(KK-1)+M1*(JJ-1)+II-M1
!          END IF
!     
!          IF(KK.EQ.1)THEN
!              AUXZ1=M1*N1*(KK-1)+M1*(JJ-1)+II
!          ELSE
!              AUXZ1=M1*N1*(KK-1)+M1*(JJ-1)+II-(M1*N1)
!          END IF
!          
!          AUXZ=M2*N1*L1+N2*M1*L1+AUXZ1
!
!          NNNA=M1*N*L+N1*M*L+L1*M*N 
!          NNNB=NNNA+(M1-1)*N1*L1+(N1-1)*L1*M1+(L1-1)*M1*N1
!
!          I=(KK-1)*(N-1)*(M-1)+(JJ-1)*(M-1)+II
!          B1=M1*N*(KK-1)+M1*(JJ-1)+II 
!          B2=B1+N*M1 
!          B3=B2+M1 
!          B4=B1+M1 
!          NN=M1*N*L 
!          B5=NN+M*N1*(KK-1)+M*(JJ-1)+II 
!          B6=B5+1 
!          B7=B6+M*N1 
!          B8=B5+M*N1 
!          NN=NN+N1*M*L 
!          B9=NN+M*N*(KK-1)+M*(JJ-1)+II 
!          B10=B9+1 
!          B11=B10+M 
!          B12=B9+M 
!
!          IF(II.EQ.1)THEN
!              B13=0
!              B14=NNNA+AUXX
!          ELSE
!              B13=NNNA+AUXX
!              B14=B13+1
!          ENDIF
!          IF(II.EQ.M1)THEN
!              B14=0
!          ENDIF
!          IF(JJ.EQ.1)THEN         
!              B15=0                  
!              B16=NNNA+AUXY
!          ELSE
!              B15=NNNA+AUXY
!              B16=B15+M1
!          ENDIF
!          IF(JJ.EQ.N1)THEN
!              B16=0
!          ENDIF
!          IF(KK.EQ.1)THEN
!              B17=0
!              B18=NNNA+AUXZ
!          ELSE
!              B17=NNNA+AUXZ
!              B18=B17+M1*N1
!          ENDIF
!          IF(KK.EQ.L1)THEN
!              B18=0
!          ENDIF  
!
!          NDIAG=NN+L1*M*N+(M1-1)*N1*L1+(N1-1)*M1*L1+(L1-1)*M1*N1+8*(I-1)
!          B19=NDIAG+1 
!          B20=NDIAG+2 
!          B21=NDIAG+3 
!          B22=NDIAG+4 
!          B23=NDIAG+5 
!          B24=NDIAG+6 
!          B25=NDIAG+7 
!          B26=NDIAG+8
!!-------------------------------------------------------------------------
!! CALCULO DE F11
!!      BARRAS NORMALES EXTERNAS 
!          IF((JJ.EQ.1).AND.(KK.EQ.1))THEN
!              FAUX1=((FORCE(B1)*4)+(FORCE(B2)*2)+(FORCE(B3))+
!     &            (FORCE(B4)*2))*0.25
!          END IF
!          IF((JJ.EQ.1).AND.(KK.GT.1).AND.(B18.NE.0))THEN
!              FAUX1=((FORCE(B1)*2)+(FORCE(B2)*2)+(FORCE(B3))+
!     &            (FORCE(B4)))*0.25
!          END IF
!          IF((JJ.EQ.1).AND.(KK.EQ.L1))THEN
!              FAUX1=((FORCE(B1)*2)+(FORCE(B2)*4)+(FORCE(B3)*2)+
!     &            (FORCE(B4)))*0.25
!          END IF
!	    IF((JJ.GT.1).AND.(KK.EQ.1).AND.(B16.NE.0))THEN
!      FAUX1=((FORCE(B1)*2)+(FORCE(B2))+(FORCE(B3))+(FORCE(B4)*2))/4
!	 END IF
!	IF(JJ.EQ.N1.AND.KK.EQ.1)THEN
!      FAUX1=((FORCE(B1)*2)+(FORCE(B2))+(FORCE(B3)*2)+(FORCE(B4)*4))/4
!	END IF
!	IF(JJ.EQ.N1.AND.KK.GT.1.AND.B18.NE.0)THEN
!      FAUX1=((FORCE(B1))+(FORCE(B2))+(FORCE(B3)*2)+(FORCE(B4)*2))/4
!	END IF
!	IF(JJ.EQ.N1.AND.KK.EQ.L1)THEN
!      FAUX1=((FORCE(B1))+(FORCE(B2)*2)+(FORCE(B3)*4)+(FORCE(B4)*2))/4
!	 END IF
!	IF(JJ.GT.1.AND.KK.EQ.L1.AND.B16.NE.0)THEN
!      FAUX1=((FORCE(B1))+(FORCE(B2)*2)+(FORCE(B3)*2)+(FORCE(B4)))/4
!	 END IF
!       IF (JJ.GT.1.AND.JJ.LT.N1.AND.KK.GT.1.AND.KK.LT.L1.AND.B15.NE.0
!     &.AND.B16.NE.0.AND.B17.NE.0.AND.B18.NE.0 )THEN
!      FAUX1=((FORCE(B1))+(FORCE(B2))+(FORCE(B3))+(FORCE(B4)))/4
!	END IF
!!      FAUX1=((FORCE(B1))+(FORCE(B2))+(FORCE(B3))+(FORCE(B4)))/4
!   
!!       BARRAS DIAGONALES
!	FAUX2=0.577*(FORCE(B20)+FORCE(B24)+FORCE(B26)+FORCE(B22))
!      IF (B14.EQ.0) THEN
!	  NORM=0
!	ELSE
!	  NORM=FORCE(B14)
!      END IF
!!     SOMA DE UN LADO
!	FAUXA=FAUX1+FAUX2+NORM
!!  DEL OTRO LADO
!        
!!      BARRAS NORMALES EXTERNAS 
!!      MESMA QUE DEL PRIMER CALCULO      
!!     BARRAS DIAGONALES
!	FAUX2=0.577*(FORCE(B19)+FORCE(B21)+FORCE(B23)+FORCE(B25))
!!     SOMA DEL OTRO  LADO
!      IF (B13.EQ.0) THEN
!	  NORM=0
!	ELSE
!	  NORM=FORCE(B13)
!      END IF
!	FAUXB=FAUX1+FAUX2+NORM
!!  FINALMETE FICA
!!
!!	WRITE(*,*)'PARA O OUTRO LADO'
!!      WRITE(*,*)'FAUX1,NORM,FAUX2',FAUX1,NORM,FAUX2
!!	READ(*,*)
!      F11=(FAUXA+FAUXB)/2
!!      READ(*,*)
!          
!!!--------------------------     
!!!   *******************   CALCULO F22 *************
!!      IF(II.EQ.1.AND.KK.EQ.1)THEN !MARCA4
!!      FAUX1=((FORCE(B8)*2)+(FORCE(B7))+(FORCE(B5)*4)+(FORCE(B6)*2))/4
!!	 END IF 
!!	IF(II.EQ.1.AND.KK.GT.1.AND.B18.NE.0)THEN
!!      FAUX1=((FORCE(B8)*2)+(FORCE(B7))+(FORCE(B5)*2)+(FORCE(B6)))/4
!!	 END IF
!!	IF(II.EQ.1.AND.KK.EQ.L1)THEN
!!      FAUX1=((FORCE(B8)*4)+(FORCE(B7)*2)+(FORCE(B5)*2)+(FORCE(B6)))/4
!!	END IF
!!	IF(II.GT.1.AND.KK.EQ.1.AND.B14.NE.0)THEN
!!      FAUX1=((FORCE(B8))+(FORCE(B7))+(FORCE(B5)*2)+(FORCE(B6)*2))/4
!!	 END IF
!!	IF(II.EQ.M1.AND.KK.EQ.1)THEN
!!      FAUX1=((FORCE(B8))+(FORCE(B7)*2)+(FORCE(B5)*2)+(FORCE(B6)*4))/4
!!	END IF
!!	IF(II.EQ.M1.AND.KK.GT.1.AND.B18.NE.0)THEN
!!      FAUX1=((FORCE(B8))+(FORCE(B7)*2)+(FORCE(B5))+(FORCE(B6)*2))/4
!!	END IF
!!	IF(II.EQ.M1.AND.KK.EQ.L1)THEN
!!      FAUX1=((FORCE(B8)*2)+(FORCE(B7)*4)+(FORCE(B5))+(FORCE(B6)*2))/4
!!	 END IF
!!	IF(II.GT.1.AND.KK.EQ.L1.AND.B14.NE.0)THEN
!!      FAUX1=((FORCE(B8)*2)+(FORCE(B7)*2)+(FORCE(B5))+(FORCE(B6)))/4
!!	 END IF
!!       IF (II.GT.1.AND.II.LT.M1.AND.KK.GT.1.AND.KK.LT.L1.AND.B13.NE.0&
!!     &.AND.B14.NE.0.AND.B17.NE.0.AND.B18.NE.0 )THEN
!!      FAUX1=((FORCE(B8))+(FORCE(B7))+(FORCE(B5))+(FORCE(B6)))/4
!!	END IF
!!!      FAUX1=((FORCE(B8))+(FORCE(B7))+(FORCE(B5))+(FORCE(B6)))/4
!!
!!      FAUX2=0.577*(FORCE(B21)+FORCE(B25)+FORCE(B26)+FORCE(B22))
!!!     SOMA DE UM LADO 
!!      IF (B16.EQ.0) THEN
!!	  NORM=0
!!	ELSE
!!	  NORM=FORCE(B16)
!!      END IF
!!!      WRITE(*,*)'FORCE(B8),FORCE(B7),FORCE(B5),FORCE(B6),NORM,FORCE(B21)
!!!     &,FORCE(B25),FORCE(B26),FORCE(B22),IMOD',FORCE(B8),FORCE(B7)
!!!     &,FORCE(B5),FORCE(B6),NORM,FORCE(B21),FORCE(B25),FORCE(B26)
!!!     & ,FORCE(B22),IMOD
!!!      READ(*,*)
!!!!      WRITE(*,*)'NA DIREÇÃO Y PARA UM LADO'
!!!      WRITE(*,*)'FAUX1,NORM,FAUX2',FAUX1,NORM,FAUX2
!!!      PAUSE
!!	FAUXA=FAUX1+FAUX2+NORM
!!!     SOMA DE OUTRO LADO
!!      FAUX2=0.577*(FORCE(B23)+FORCE(B24)+FORCE(B19)+FORCE(B20))
!!      IF (B15.EQ.0) THEN
!!	  NORM=0
!!	ELSE
!!	  NORM=FORCE(B15)
!!      END IF
!!!      WRITE(*,*)'NA DIREÇÃO Y PARA O OUTRO LADO'
!!!      WRITE(*,*)'FAUX1,NORM,FAUX2',FAUX1,NORM,FAUX2
!!!      READ(*,*)
!!      FAUXB=FAUX1+FAUX2+NORM
!!!     FINALMENTE FICA
!!          F22=(FAUXA+FAUXB)/2
!!
!!!-------------------------------------	
!!!    *****************  CALCULO F33   ******************
!!       IF(II.EQ.1.AND.JJ.EQ.1)THEN 
!!      FAUX1=(FORCE(B9)*4+FORCE(B10)*2+FORCE(B11)+FORCE(B12)*4)/4
!!	 END IF 
!!	IF(II.EQ.1.AND.JJ.GT.1.AND.B16.NE.0)THEN
!!      FAUX1=(FORCE(B9)*2+FORCE(B10)+FORCE(B11)+FORCE(B12)*2)/4
!!	 END IF
!!	IF(II.EQ.1.AND.JJ.EQ.N1)THEN
!!      FAUX1=(FORCE(B9)*2+FORCE(B10)+FORCE(B11)*2+FORCE(B12)*4)/4
!!	END IF
!!	IF(II.GT.1.AND.JJ.EQ.1.AND.B14.NE.0)THEN
!!      FAUX1=(FORCE(B9)*2+FORCE(B10)*2+FORCE(B11)+FORCE(B12))/4
!!	 END IF
!!	IF(II.GT.1.AND.JJ.EQ.N1.AND.B14.NE.0)THEN
!!      FAUX1=(FORCE(B9)+FORCE(B10)+FORCE(B11)*2+FORCE(B12)*2)/4
!!	END IF
!!	IF(II.EQ.M1.AND.JJ.EQ.1)THEN
!!      FAUX1=(FORCE(B9)*2+FORCE(B10)*4+FORCE(B11)*2+FORCE(B12))/4
!!	END IF
!!	IF(II.EQ.M1.AND.JJ.GT.1.AND.B16.NE.0)THEN
!!      FAUX1=(FORCE(B9)+FORCE(B10)*2+FORCE(B11)*2+FORCE(B12))/4
!!	 END IF
!!	IF(II.EQ.M1.AND.JJ.EQ.N1)THEN
!!      FAUX1=(FORCE(B9)+FORCE(B10)*2+FORCE(B11)*4+FORCE(B12)*2)/4
!!	 END IF
!!       IF (II.GT.1.AND.II.LT.M1.AND.JJ.GT.1.AND.JJ.LT.N1.AND.B13.NE.0&
!!     &.AND.B14.NE.0.AND.B15.NE.0.AND.B16.NE.0 )THEN
!!      FAUX1=(FORCE(B9)+FORCE(B10)+FORCE(B11)+FORCE(B12))/4
!!	END IF
!!!      FAUX1=(FORCE(B9)+FORCE(B10)+FORCE(B11)+FORCE(B12))/4
!! 
!!      FAUX2=0.577*(FORCE(B23)+FORCE(B24)+FORCE(B25)+FORCE(B26))
!!
!!!     SOMA DE UM LADO 
!!      IF (B18.EQ.0) THEN
!!	  NORM=0
!!	ELSE
!!	  NORM=FORCE(B18)
!!      END IF
!!
!!!      WRITE(*,*)'NA DIREÇÃO Z PARA UM LADO'
!!!      WRITE(*,*)'FAUX1,NORM,FAUX2',FAUX1,NORM,FAUX2
!!     
!!	FAUXA=FAUX1+FAUX2+NORM
!!!     SOMA DE OUTRO LADO
!!      FAUX2=0.577*(FORCE(B19)+FORCE(B20)+FORCE(B21)+FORCE(B22))
!!
!!      IF (B17.EQ.0) THEN
!!	  NORM=0
!!	ELSE
!!	  NORM=FORCE(B17)
!!      END IF
!!
!!      FAUXB=FAUX1+FAUX2+NORM
!!!     FINALMENTE FICA
!!!      WRITE(*,*)'NA DIREÇÃO Z PARA UM LADO'
!!!      WRITE(*,*)'FAUX1,NORM,FAUX2',FAUX1,NORM,FAUX2
!!!      READ(*,*)
!!          F33=(FAUXA+FAUXB)/2
!!!-------------------------------------
!!	
!!!	******************CALCULO F12***************
!!
!!	LOAD1=0.577*(FORCE(B22)-FORCE(B20))
!!      LOAD2=0.577*(FORCE(B26)-FORCE(B24))
!!	FAUX2=(LOAD1+LOAD2)/2 
!!!     BARRAS DIAGONALES
!!!	FAUX2=0.577*(FORCE(B20)+FORCE(B22)+FORCE(B24)+FORCE(B26))
!!!     SOMA DE UN LADO
!!	FAUXA=FAUX2
!!!  DEL OTRO LADO
!!!     
!!	LOAD1=0.577*(FORCE(B21)-FORCE(B19))
!!	LOAD2=0.577*(FORCE(B25)-FORCE(B23))
!!	FAUX2=(LOAD1+LOAD2)/2 
!!	FAUXB=FAUX2
!!  
!!!     BARRAS DIAGONALES
!!!	FAUX2=0.577*(FORCE(B19)+FORCE(B21)+FORCE(B23)+FORCE(B25))
!!!     SOMA DEL OTRO  LADO
!!!     FINALMENTE FICA
!!!
!!!      WRITE(*,*)'FAUXA,FAUXB',FAUXA,FAUXB
!!!	READ(*,*)
!!      F12=((FAUXA)-(FAUXB))
!!	IF(FAUXA.LT.0)THEN
!!	F12=-F12
!!	ELSE
!!	F12=F12
!!	END IF
!!!--------------------------     
!!
!!!	  *********************	CALCULO F13 ************** 
!!
!!	LOAD1=0.577*(FORCE(B22)-FORCE(B26))
!!	LOAD2=0.577*(FORCE(B20)-FORCE(B24))
!!	
!!	FAUX2=(LOAD1+LOAD2)/2 
!!     
!!!     BARRAS DIAGONALES
!!!	FAUX2=(0.577)*(FORCE(B20)+FORCE(B22)+FORCE(B24)+FORCE(B26))
!!!     SOMA DE UN LADO
!!	FAUXA=FAUX2
!!!  DEL OTRO LADO
!!!     
!!	LOAD1=0.577*(FORCE(B21)-FORCE(B25))
!!	LOAD2=0.577*(FORCE(B19)-FORCE(B23))
!!	FAUX2=(LOAD1+LOAD2)/2 
!!   
!!!     BARRAS DIAGONALES
!!!	FAUX2=(0.577)*(FORCE(B19)+FORCE(B21)+FORCE(B23)+FORCE(B25))
!!!     SOMA DEL OTRO  LADO
!!	FAUXB=FAUX2
!!
!!!  FINALMENTE FICA
!!!
!!      F13=((FAUXA)-(FAUXB))
!!	IF(FAUXA.LT.0)THEN
!!	F13=-F13
!!	ELSE
!!	F13=F13
!!	END IF
!!!-------------------------------------------------------------
!!!   ***********************   CALCULO F23 *****************
!!	LOAD1=0.577*(FORCE(B22)-FORCE(B20))
!!	LOAD2=0.577*(FORCE(B21)-FORCE(B19))
!!	FAUX2=(LOAD1+LOAD2)/2 
!!
!!!      FAUX2=0.577*(FORCE(B21)+FORCE(B22)+FORCE(B25)+FORCE(B26))
!!!     SOMA DE UM LADO      
!!	FAUXA=FAUX2
!!!     SOMA DE OUTRO LADO
!!	LOAD1=0.577*(FORCE(B25)-FORCE(B23))
!!	LOAD2=0.577*(FORCE(B26)-FORCE(B24))
!!	FAUX2=(LOAD1+LOAD2)/2 
!!
!!!      FAUX2=0.577*(FORCE(B19)+FORCE(B20)+FORCE(B23)+FORCE(B24))
!!      FAUXB=FAUX2
!!!     FINALMENTE FICA
!!          F23=((FAUXA)-(FAUXB))/2
!!	IF(FAUXA.LT.0)THEN
!!	F23=-F23
!!	ELSE
!!	F23=F23
!!	END IF
!!!-------------------------------------------------------------------
!!!       CALCULO DAS TENSÕES
!!       S11(IMOD)=F11/AM
!!	S22(IMOD)=F22/AM
!!	S33(IMOD)=F33/AM
!!	S12(IMOD)=F12/AM
!!	S13(IMOD)=F13/AM
!!	S23(IMOD)=F23/AM
!!!        WRITE(*,*) S11(IMOD), S22(IMOD), S33(IMOD)
!!!        PAUSE
!!!      ---------------------------------------------------------------------
!!!      CALCULO DAS DEFORMAÇÕES
!!!       WRITE(*,*)"IMOD=",IMOD
!!!	WRITE(*,*) "I1=",I1, "U0(I1)=",U0(I1),"V0(I1)=",V0(I1),"W0(I1)="&
!!!    &,W0(I1) 
!!!	WRITE(*,*) "I2=",I2, "U0(I2)=",U0(I2),"V0(I2)=",V0(I2),"W0(I2)="&
!!!    &,W0(I2) 
!!!	WRITE(*,*) "I3=",I3, "U0(I3)=",U0(I3),"V0(I3)=",V0(I3),"W0(I3)="&
!!!     &,W0(I3) 
!!!	WRITE(*,*) "I4=",I4, "U0(I4)=",U0(I4),"V0(I4)=",V0(I4),"W0(I4)="&
!!!     &,W0(I4) 
!!!	WRITE(*,*) "I5=",I5, "U0(I5)=",U0(I5),"V0(I5)=",V0(I5),"W0(I5)="&
!!!     &,W0(I5) 
!!!	WRITE(*,*) "I6=",I6, "U0(I6)=",U0(I6),"V0(I6)=",V0(I6),"W0(I6)="&
!!!     &,W0(I6) 
!!!	WRITE(*,*) "I7=",I7, "U0(I7)=",U0(I7),"V0(I7)=",V0(I7),"W0(I7)="&
!!!     &,W0(I7) 
!!!	WRITE(*,*) "I8=",I8, "U0(I8)=",U0(I8),"V0(I8)=",V0(I8),"W0(I8)="&
!!!     &,W0(I8) 
!!!    PAUSE
!!!     -------------------------
!!!     CALCULO DO E11
!!      AU=0.25*((U2(I6)-U2(I5))+(U2(I7)-U2(I8))+(U2(I3)-U2(I4))+&
!!     & (U2(I2)-U2(I1)))
!!	AU0=0.25*((U0(I6)-U0(I5))+(U0(I7)-U0(I8))+(U0(I3)-U0(I4))+&
!!     &	(U0(I2)-U0(I1)))
!!!	E11(IMOD)=(AU-AU0)     !DESLOCAMENTO ENGANANAR ANSYS
!!	E11(IMOD)=(AU-AU0)/AU0 !DEFORMAÇÃO
!!!
!!!	WRITE(*,*) "E11", IMOD,"=",E11(IMOD)
!!!       WRITE(*,*) "EM SRTEN"
!!!	WRITE(*,*) "AU=", AU, "AU0=", AU0
!!!	WRITE(*,*) "U2(I5)=",U2(I5),"U2(I6)=",U2(I6),"DU=",(U2(I6)-U2(I5))
!!!	WRITE(*,*) "I5=", I5,"I6=",I6
!!!	WRITE(*,*) "U2(I8)=",U2(I8),"U2(I7)=",U2(I7),"DU=",(U2(I7)-U2(I8))
!!!	WRITE(*,*) "I7=", I7,"I8=",I8
!!!	WRITE(*,*) "U2(I4)=",U2(I4),"U2(I3)=",U2(I3),"DU=",(U2(I3)-U2(I4))
!!!	WRITE(*,*) "I4=", I4,"I3=",I3
!!!	WRITE(*,*) "U2(I2)=",U2(I2),"U2(I1)=",U2(I1),"DU=",(U2(I2)-U2(I1))
!!!	WRITE(*,*) "I2=", I2,"I1=",I1
!!!       PAUSE
!!
!!	IF(DABS(E11(IMOD)).GT.1) THEN
!!	E11(IMOD)=1.
!!	END IF 
!!!     CALCULO DO E22
!!
!!      AV=0.25*((V2(I7)-V2(I3))+(V2(I8)-V2(I4))+(V2(I5)-V2(I1))&
!!     &	+(V2(I6)-V2(I2)))
!!	AV0=0.25*((V0(I7)-V0(I3))+(V0(I8)-V0(I4))+(V0(I5)-V0(I1))+&
!!     &	(V0(I6)-V0(I2)))
!!!     	E22(IMOD)=(AV-AV0)      !DESLOCAMENTO ENGANANAR ANSYS
!!	E22(IMOD)=(AV-AV0)/AV0   !DEFORMAÇÃO
!!!        WRITE(*,*) "E22", IMOD,"=",E22(IMOD)
!!!        WRITE(*,*) "EM SRTEN"
!!        
!!        IF(DABS(E22(IMOD)).GT.1) THEN
!!	    E22(IMOD)=1.
!!	 END IF    
!!
!!!	WRITE(*,*) "E22", IMOD,"=",E22(IMOD)
!!!	WRITE(*,*) "AV=", AV, "AV0=", AV0
!!!	WRITE(*,*) "V2(I7)=",V2(I7),"V2(I5)=",V2(I5),"DV=",(V2(I7)-V2(I5))
!!!	WRITE(*,*) "I7=", I7,"I5=",I5
!!!	WRITE(*,*) "V2(I8)=",V2(I8),"V2(I6)=",V2(I6),"DV=",(V2(I8)-V2(I6))
!!!	WRITE(*,*) "I8=", I8,"I6=",I6
!!!	WRITE(*,*) "V2(I3)=",V2(I3),"V2(I1)=",V2(I1),"DV=",(V2(I3)-V2(I1))
!!!	WRITE(*,*) "I3=", I3,"I1=",I1
!!!	WRITE(*,*) "V2(I4)=",V2(I4),"V2(I2)=",V2(I2),"DV=",(V2(I4)-V2(I2))
!!!	WRITE(*,*) "I4=", I4,"I2=",I2
!!!       WRITE(*,*) "EM SRTEN"
!!!     CALCULO DO E33
!!
!!      AW=0.25*((W2(I8)-W2(I5))+(W2(I7)-W2(I6))+(W2(I4)-W2(I1))&
!!     &	+(W2(I3)-W2(I2)))
!!      AW0=0.25*((W0(I8)-W0(I5))+(W0(I7)-W0(I6))+(W0(I4)-W0(I1))+&
!!     &	(W0(I3)-W0(I2)))
!!!      E33(IMOD)=(AW-AW0)     !DESLOCAMENTO ENGANANAR ANSYS
!!      E33(IMOD)=(AW-AW0)/AW0 !DEFORMAÇÃO
!!	
!!!    	WRITE(*,*) "E33", IMOD,"=",E33(IMOD)
!!!       WRITE(*,*) "EM SRTEN"
!!
!!       IF(DABS(E33(IMOD)).GT.1) THEN
!!    	    E33(IMOD)=1.
!!	END IF   
!! !      WRITE(*,*) "W2(I8)-W2(I5)=",W2(I8)-W2(I5) 
!! !     WRITE(*,*) "W0(I8)-W0(I5)=",W0(I8)-W0(I5) 
!!
!!
!!!	WRITE(*,*) "E33", IMOD,"=",E33(IMOD)
!!!     CALCULO DO E12
!!      E12(IMOD)=0.5*((AU-AU0)/AV0+ (AV-AV0)/AU0)
!!	IF(DABS(E12(IMOD)).GT.1) THEN
!!	E12(IMOD)=1.
!!	END IF   
!!	E23(IMOD)=0.5*((AV-AV0)/AW0+ (AW-AW0)/AV0)
!!	IF(DABS(E23(IMOD)).GT.1) THEN
!!	E23(IMOD)=1.
!!	END IF   
!!      E13(IMOD)=0.5*((AU-AU0)/AW0+ (AW-AW0)/AU0)
!!	IF(DABS(E13(IMOD)).GT.1) THEN
!!	E13(IMOD)=1.
!!	END IF   
!!
!!
!!
!!!*********************** FIN DE ARQUIVOS DOS CAMINHOS ************************
!!!*************** ARQUIVOS COM OS TENSORES DE TENSÃO POR CADA MODULO **********
!!	WRITE(6,40)IMOD,S11(IMOD),S22(IMOD),S33(IMOD),S12(IMOD)
!!     &	,S23(IMOD),S13(IMOD)
!!   40 FORMAT(I6,',',E12.6,',',E12.6,',',E12.6,',',E12.6,',',E12.6
!!     &,',',E12.6)
!!	WRITE(7,45)IMOD,E11(IMOD),E22(IMOD),E33(IMOD),E12(IMOD)
!!     &,E23(IMOD),E13(IMOD)
!!   45 FORMAT(I6,',',E12.6,',',E12.6,',',E12.6,',',E12.6,',',E12.6
!!     &,',',E12.6)
!!!***************************FIN DE TENSORES DE TENSÃO POR CADA MODULO***********     	 
!!
!!
!   10 CONTINUE
!!      
!!      
!!      
!!      
!!      
!!      
!!!
!! !     SUBROUTINE NODCUBO(M,N,L,M1,N1,L1,NOD)
!! !     IMPLICIT NONE
!!	!INTEGER M,N,L,M1,N1,L1,B,II,JJ,KK, I1, I2, I3, I4, 
!! !    &I5, I6, I7, I8, I9,I
!!	!INTEGER NOD(50*M*N*L)
!! !     B=1
!! !     DO 25 II=1,M1
!!	!DO 25 JJ=1,N1
!!	!DO 25 KK=1,L1
!! !
!! !         I=(KK-1)*(N-1)*(M-1)+(JJ-1)*(M-1)+II ! ISTO AQUI NÃO E IGUAL DO QUE IMOD QUE ESTA NO PROGRAMA PRINCIPAL
!! !     I1=M*(JJ-1)+N*M*(KK-1)+II 
!! !     I2=I1+1 
!! !     I3=I2+M*N 
!! !     I4=I1+M*N 
!! !     I5=I1+M 
!! !     I6=I2+M 
!! !     I7=I6+M*N 
!! !     I8=I5+M*N 
!! !     I9=M*N*L+I
!! !         
!! !         
!! !     NOD(B)=I1
!!	!NOD(B+1)=I2
!!	!NOD(B+2)=I3
!!	!NOD(B+3)=I4
!!	!NOD(B+4)=I5
!!	!NOD(B+5)=I6
!!	!NOD(B+6)=I7
!!	!NOD(B+7)=I8
!!	!B=B+8   
!! !  25 CONTINUE
!! !
!!	!RETURN 
!!	!END SUBROUTINE
      
      RETURN
      END
!
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------