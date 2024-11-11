    SUBROUTINE CONSTIT_WEIB (B,STR,STRRT,R)
    !     =======================================

    !
    !     THIS  FUNCTION  MODELS  THE  CONSTITUTIVE  RELATION  OF  EACH  BAR
    !     ACCORDING TO A  WEIBULL LAW WITH THREE PARAMETERS FORCE VS. STRAIN  DIAGRAM,
    !     WITH  STRAIN SOFTENING. THE  UNLOADING  PROCESS  IS  MODELED  ACCORDING  TO  THE
    !     ELASTIC-DAMAGE HIPOTHESIS.
    !      IN THIS VESION THE CURVE COEFICIENTS ARE ALEATORIES FUNCTION
    ! ******************************************************************** C
    !
    !	AUTORES: J. D. RIERA,IGNACIO ITURRIOZ, R. D. RIOS
    !	LAST UPDATE:/II/07
    !     MODIFICACIÓN LUIS KOSTESKI, FECHA JULIO/2011
    !	-----------------------
    !  STRAIN_AUX=10000*STRAIN
    !FORMA DA CURVA PROPOSTA  FORCE_AUX=A*(STRAIN-AUX)*EXP[-BB*(STRAIN-AUX)^C]
    !  FORCE =FORCE_AUX *100
    !  ONDE A= IS THE STIFFNESS IN THE ORIGIN( THE MAXIMUM VALUE OF THIS PARAMETER IS 3*E1(B),
    !  IF IN THE RANDOM GENERATION THIS LIMIT IS VILOLATED WILL BE FIXED A=3*E1(B)
    !
    !    STRAIN (PARA FMAX)=(1/BB*C)^[1/C]
    !
    !
    !
    !         F  !
    !            !           *  * *
    !            !         *   !    *
    !            !       *     !      *
    !            !     *       !        *
    !            !    *        !          *
    !            !   *         !            *
    !            !  *          !              *
    !            ! *           !                 *
    !            !-------------!---------------------*--------------
    !                         STRAIN(P/FMAX)                 STRAIN
    !
    !
    !
    !
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



    !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    PARAMETER (INN=1.2E6,INB=6E6,IRU=8,INCON=8)
    !--------------------------------------------------------------------------
    !     DEFINIÇÃO DE VARIÁVEIS E GRUPOS COMUNS
    INCLUDE 'COMMON.FOR'
    REAL GAMA2

    INTEGER B,ERRO,R

    DOUBLE PRECISION AUX,STR,STRRT,ER,KK

    !    ----------------------------------------------------------
    DOUBLE PRECISION C,A,STR_AUX,FORCE_AUX,FMAX,CVA,CVB,CVC,GM,BT,AUX1,UNIF,RAN0,AF,F1,F2,ERR
    !--------------------------------------------------------------
    CON=0.0
    SS(B)=STR
    KK=0.0
    STR_AUX= STR/EP(B)

    !ESTO PUEDE ESTAR EN OTRO LADO PARA QUE SE CALCULE SOLAMENTE UNA VEZ Y NO EN CADA PASO!
    IF (STEP .EQ.1) THEN

        C=5.0D00
        DG=0.1D00

11      C=C-DG
        !CONDICIÓN: MODULO, FMAX Y AREA
        F1=(2/(KR(B)*EP(B)**2)/C*GAMA2(2.0D0/C))**(C/2)
        F2=(EXP(-1/C)/(C**(1/C)*EP(B)))**C
        ERR=(F1-F2)


        !CONDICIÓN: EMAX, FMAX Y AREA
        !C	F1=1/C/EP(B)**C
        !C	F2=(2*EXP(1/C)/(KR(B)*EP(B)**2)/C*GAMA2(2.0D0/C))**(C/2)
        !C	ERR=(F2-F1)


        ERRO=INT(10000*ERR)

        IF (ERRO) 11,31,21

21      C=C+DG
        DG=DG/2.0D00
        GO TO 11

        !CONDICIÓN: MODULO, FMAX Y AREA
31      BB(B)=(2/(KR(B)*EP(B)**2)/C*GAMA2(2.0D0/C))**(C/2)
        AA(B)=E1(B)

        !CONDICION: EMAX, FMAX Y AREA
        !C   31 BB(B)=1/C/EP(B)**C
        !C   	AA(B)=E1(B)*EXP(1/C)

        CC(B)=C
    END IF

    IF ((E2(B).EQ.0.0).AND.(STR.GT.0.0)) GO TO 40 !BARRA ROMPIDA A TRACAO
    IF (STR.GE.EM(B)) GO TO 20 !COMPORTAMENTO EM CARGA
    IF (STR.GE.0.0D00) GO TO 10  !COMPORTAMENTO ELASTICO

    !!!!!!!!!!!!!!!!! COMPORTAMENTO A COMPRESSÃO
    FORCE(B)=E1(B)*STR+E1(B)*CON*STRRT*KK

    GOTO 40
    !!!!!!!!!!!!!!!!!!!!

    !   !
10  FORCE(B)=E2(B)*STR  !COMPORTAMENTO ELASTICO NO CAMINHO DA DESCARGA

    GO TO 40
    ! -----------------------------------------------------------
20  CONTINUE
    !C      FORCE_AUX=A*STR_AUX*EXP(-BB*STR_AUX**C)
    FORCE(B)=AA(B)*STR*EXP(-BB(B)*STR**CC(B))
    E2(B)=FORCE(B)/STR
    EM(B)=STR

    !----------------------------------------------------------------------
    !C      IF (STR.GT.EP(B)) THEN
    IF (STR.GT.((BB(B)*CC(B))**(-1/CC(B)))) THEN

        IF (DM(B)) THEN
            DM(B)=.FALSE.
            !C	  WRITE(*,*)'BARRA DANADA=',B,DM(B)
        END IF
    END IF
    !
    !CCC      IF(EM(B).GT.(KR(B)*10*(BB*C)**(-1/C)))THEN
    IF(EM(B).GT.(15*(BB(B)*CC(B))**(-1/CC(B))))THEN

        !CCC      E2(B)=0.0D00
        !CCC      FORCE(B)=0.0D00
        !C
        IF (FS(B)) THEN
            FS(B)=.FALSE.
            DM(B)=.FALSE.
            !C	  WRITE(*,*) 'BARRA ROMPIDA:' ,B,FS(B)
        END IF
    END IF
    !-------------------------------------------------------------------------

    !      WRITE(*,*)'BARRA=',B,FORCE,FORCE_AUX,STR,STR_AUX

40  CONTINUE
    ! IF (B.EQ.489) THEN
    !  WRITE(*,*) FORCE,STR
    !	 END IF
    END

    REAL FUNCTION GAMA2(ARG)
    !
    INTEGER N,I
    !
    DOUBLE PRECISION ARG,P,K,ARGG,FACT
    !
    K=ARG*(ARG+1)*(ARG+2)*(ARG+3)
    ARGG=ARG
    ARGG=ARGG+4
    N=INT(ARGG)
    P=ARGG-N
    !
    FACT=1
    DO 10 I=1,N
        FACT=FACT*I
10  CONTINUE
    !
    GAMA2=FACT*(((N+P/2)**2+(2-P)*P/12)**((P-1)/2))/K
    !
    RETURN
    END