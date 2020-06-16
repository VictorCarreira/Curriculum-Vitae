MODULE activation
IMPLICIT NONE
  PUBLIC
  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
  INTEGER(KIND=SP):: epoch   
  INTEGER(KIND=SP):: fid
  INTEGER(KIND=SP), ALLOCATABLE, DIMENSION(:)::rockcode
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:)::pp
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: xxi,xxi1,xxi2, w
  REAL(KIND=DP):: x, aa, n, eta, a1, a2, SC1, SC2
 

CONTAINS

!------------------------------------------------------
REAL(KIND=DP) FUNCTION degrau(x)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10) 
REAL(KIND=DP), INTENT(IN):: x

  IF(x >= 0.0d0 ) THEN
   degrau = +1.0d0   
  ELSE IF (x < 0.0d0) THEN
   degrau = -1.0d0 
  END IF

END FUNCTION degrau

!-------------------------------------------------------------------------------

REAL(KIND=DP) FUNCTION Robbins(c,nn)
!Taxa de aprendizagem calculada por Robbins and Monro(1951).
!c é uma constante e nn é o número de iterações.
 IMPLICIT NONE
  !INTEGER, PARAMETER::PS = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10) 
  INTEGER(KIND=DP), INTENT(IN):: nn
  REAL(KIND=DP), INTENT(IN):: c
  REAL(KIND=DP):: nnn
 
  nnn = DFLOAT(nn) ! converte o inteiro em um real de dupla precisão

  Robbins=c/nnn

END FUNCTION Robbins

!--------------------------------------------------------------------------------

REAL(KIND=DP) FUNCTION Darken(etaO,nn,tau)
!Taxa de aprendizagem calculada por Darken and Moody(1992)
!Evita que o eta dispare para valores altos de c e n.
!Onde eta0 e tau são constantes definidas pelo usuário.
!c = tau.etaO
 IMPLICIT NONE
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10) 
  !INTEGER, PARAMETER::PS = SELECTED_INT_KIND(r=4)
  INTEGER(KIND=DP), INTENT(IN):: nn
  REAL(KIND=DP), INTENT(IN):: etaO,tau
  REAL(KIND=DP):: nnn
 
  nnn = DFLOAT(nn)


 Darken=etaO/(1.0d0+(nnn/tau))

END FUNCTION Darken

!-------------------------------------------------------------------------

!SUBROUTINE Etraining(nome,xi1, xi2)! uso pro arquivao
SUBROUTINE Etraining(fid,xi1,xi2) ! uso pra uma rocha só
!Esta subrotina visa automatizar a entrada de dados do perceptron.
!As duas primeiras informações a serem fornecidas são as matrizes
!que armazenam as informações de propriedades físicas das rochas. 
!Estas entram vazias e retornam preenchidas.

IMPLICIT NONE
 INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
 INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
 INTEGER(KIND=SP):: i
 INTEGER(KIND=SP), INTENT(IN):: fid
 REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT):: xi1, xi2
 REAL(KIND=DP)::a1, a2, a3, a4
 REAL(KIND=DP):: icod, iprof
 CHARACTER(LEN=80):: cab
 !CHARACTER(LEN=30), INTENT(IN):: nome
 CHARACTER(LEN=20):: linha(4)

 !ALLOCATE(xi1(8,4),xi2(8,4))

 !Zera variáveis
  xi1=0.0d0 !Treinamento de um único tipo litológico (subclasse1) 
  xi2=0.0d0 !Treinamento de multipadrões litológicos (subclasse2) 

 !fid=22
 !OPEN(fid,file='inputs/' // nome) ! para o arquivao

 READ(fid,FMT=15) cab 
 READ(fid,FMT=15) cab 

 DO i =1,8
  READ(fid,FMT=*) linha(1), icod, iprof, a1, a2, a3, a4
    xi1(i,1)=a1
    xi1(i,2)=a2
    xi1(i,3)=a3
    xi1(i,4)=a4
  END DO 


 READ(fid,FMT=15) cab 
 READ(fid,FMT=15) cab 

 DO i =1,8
  READ(fid,FMT=*) linha(2), icod, iprof, a1, a2, a3, a4
    xi2(i,1)=a1
    xi2(i,2)=a2
    xi2(i,3)=a3
    xi2(i,4)=a4
 END DO 


CLOSE(fid)

!FORMATOS UTILIZADOS
15 FORMAT(A71)
!16 FORMAT(4(ES9.2E2,2x))

END SUBROUTINE Etraining 


!-------------------------------------------------------

SUBROUTINE Eclassification(fid,csi,nd,rockcode)
!Esta subrotina tem como objetivo ler o dado de poços que serão
!utilizados na estapa de classificação de uma rede. Entra uma matriz
!csi adimensional com linhas e colunas a serem preenchidas. Ela re-
!torna uma matriz preenchida e dimensionada de acordo com o dado de
!entrada na pasta input e o número de linhas desta matriz.
IMPLICIT NONE
 INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
 INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
 INTEGER(KIND=SP):: i,iprof,icod
 INTEGER(KIND=SP), INTENT(IN):: fid
 INTEGER(KIND=SP), ALLOCATABLE, DIMENSION(:),INTENT(OUT)::rockcode
 INTEGER(KIND=SP),INTENT(OUT):: nd
 CHARACTER(LEN=80):: cab, litologia
 REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT)::csi
 REAL(KIND=DP):: a1,a2,a3,a4

   !Leitura do arquivo de dados a ser classificado
   READ(fid,FMT=15) cab !Cabeçalho
    !WRITE(fid,FMT=15) cab
   READ(fid,FMT=15) cab !Espaço em branco em baixo do cabeçalho
     !WRITE(fid,FMT=15) cab
   
  i=1
   DO WHILE(.TRUE.) ! Lê entrada até que existam linhas
    READ(fid,*,END=60)
     i=i+1
   END DO
  60 CONTINUE
  CLOSE(fid)
  
  nd=i-1 !linhas do arquivo de entrada (nd)

  WRITE(*,FMT=*)"Quantidade de dados a serem classificados=", nd

   ALLOCATE(csi(nd,4), rockcode(nd)) !Dimensiona csi com o número de linhas

   OPEN(fid,FILE='inputs/dados_sint_c1.txt')! Lê a segunda vez para preencher csi
   READ(fid,FMT=15) cab !Cabeçalho
   READ(fid,FMT=15) cab !Espaço em branco em baixo do cabeçalho
 
  DO i =1,nd ! Armazena os dados de propriedades na matriz csi
    READ(fid,FMT=*) litologia, icod, iprof, a1, a2, a3, a4 
     csi(i,1)=a1
     csi(i,2)=a2
     csi(i,3)=a3 
     csi(i,4)=a4
     rockcode(i)=icod
  END DO
  
  CLOSE(fid)

 WRITE(*,*)'Sinal de classificação'
  DO i=1,size(csi,1)
    WRITE(*,FMT=16)csi(i,:)
  END DO
 WRITE(*,*)'======================================================='


!FORMATOS UTILIZADOS
15 FORMAT(A71)
16 FORMAT(4(ES9.2E2,2x))

END SUBROUTINE Eclassification


!-------------------------------------------------------

SUBROUTINE weight(m,n,x,peso)
!Define valor constante para a matriz de pesos, 
!bem como a sua dimensão do vetor de pesos. Caso
!"n" seja 1 omega assumirá posição de vetor coluna.
!"m" é o número de linhas de omega. e "x" é o preen-
!chimento de valores constantes da matriz.
IMPLICIT NONE 
 INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
 INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
 INTEGER(KIND=SP), INTENT(IN)::m, n
 REAL(KIND=DP), INTENT(IN)::x
 REAL(KIND=DP), DIMENSION(m,n), INTENT(INOUT):: peso
 INTEGER(KIND=SP):: i,j 


  DO i=1,m ! Linhas. No percetron é sempre 1. 
   DO j=1,n! número de propriedades 
     peso(i,j)=x
   END DO 
  END DO 

END SUBROUTINE weight

!-------------------------------------------------------

!SUBROUTINE synaptic(xxi1,xxi2,eta,epoch,w) !sem critério de parada
SUBROUTINE synaptic(xxi1,xxi2,eta,w) !com ritério de parada
IMPLICIT NONE
 INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
 INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
 INTEGER(KIND=DP):: epoch
 REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(IN):: xxi1,xxi2
 REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT):: w
 REAL(KIND=DP), INTENT(IN):: eta
 INTEGER(KIND=SP):: i,j!,k
 REAL(KIND=DP)::a1, a2, SC1, SC2

a1=0.0d0
a2=0.0d0
SC1=0.0d0
SC2=0.0d0
epoch=0

DO i=1,8
 DO j=1,4 !chute inicial
  a1 = a1 + w(j,1)*xxi1(i,j)
  a2 = a2 + w(j,1)*xxi2(i,j)
 END DO 
END DO 

!DO k=1,epoch !épocas

DO WHILE (a1 < 0d0 .or. a2 > 0d0)

epoch=epoch+1 ! Conta quantas épocas são necessárias para finalizar o treinamento (automatização)

DO i=1,8 ! número de linhas da matriz de entrada

SC1= degrau(a1)
SC2= degrau(a2)

  DO j=1,4 ! número de propriedades (colunas) da matriz de entrada 

    IF(SC1 < 0d0)THEN 
     w(j,1)=w(j,1)+eta*xxi1(i,j) 
    ELSE
     w(j,1)=w(j,1)  
    END IF

    IF(SC2 > 0d0)THEN
     w(j,1)=w(j,1)-eta*xxi2(i,j) 
    ELSE
     w(j,1)=w(j,1)  
    END IF

  END DO 

a1=0d0
a2=0d0

 DO j=1,4 !atualização dos a's
  a1 = a1 + w(j,1)*xxi1(i,j)
  a2 = a2 + w(j,1)*xxi2(i,j)
 END DO 

END DO !laço das amostras

 IF(epoch > 1000000)THEN
    GO TO 55
 END IF 

END DO !do while. Final do treinamento.

55 CONTINUE 

WRITE(*,FMT=*),'número de épocas=',epoch

END SUBROUTINE synaptic

!-------------------------------------------------------------------------------------------------------

SUBROUTINE classification(fid,dado,w,nd,rockcode,rcclassification)
!Faz uso matriz omega ajustada em associação com o sinal csi
!de entradada na fase de classificação
IMPLICIT NONE
 INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
 INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
 REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(IN)::dado
 REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(IN) :: w
 CHARACTER(LEN=30)::avali
 INTEGER(KIND=SP)::aval, i
 INTEGER(KIND=SP), INTENT(IN)::nd, fid
 INTEGER(KIND=SP), ALLOCATABLE, DIMENSION(:), INTENT(IN)::rockcode
 INTEGER(KIND=SP), ALLOCATABLE , DIMENSION(:), INTENT(OUT)::rcclassification
 !REAL(KIND=DP), DIMENSION(1,4):: wT
 INTEGER(KIND=SP):: j
 REAL(KIND=DP)::a, c

OPEN(fid, FILE='outputs/saida_P_UERJ.txt')


 ALLOCATE(rcclassification(nd))


 a=0.0d0
 !wT= 0.0d00
 !wT = transpose(w)

 DO i=1,nd
   DO j=1,4
     a=a+w(j,1)*dado(i,j)
   END DO

  c=degrau(a)


  IF(c > 0d0)THEN
    aval= +1 
    avali= 'Pertence a subclasse'
    rcclassification(i)=rockcode(i)
   ELSE
    aval= -1 
    avali='Nao pertence a subclasse'
    rcclassification(i)=0
  END IF

  WRITE(*,*)i, 'Avaliação: ', ' ', aval, ' ',avali 
  WRITE(fid,*)i, 'Avaliação: ', ' ', aval, '', avali, rcclassification(i)
 END DO

END SUBROUTINE classification 

!--------------------------------------------------------------------------------------------------------

SUBROUTINE Error(rcclassification,rockcode,nd,err)
!Subrotina que contabiliza os erros de classicação do poço.
!Entradas são a avaliação da etapa de classificação,
!o código da rocha, objeto de classificação, vindo do dado
!de perfilagem, o código lido no dado de poço. E a saída
!o número de erros totais de classificação.
IMPLICIT NONE
 INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
 INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
 INTEGER(KIND=SP), DIMENSION(:), INTENT(IN) ::rcclassification, rockcode
 INTEGER(KIND=SP), INTENT(IN):: nd
 INTEGER(KIND=SP), ALLOCATABLE, DIMENSION(:), INTENT(OUT):: err
 INTEGER(KIND=SP)::i

  ALLOCATE(err(nd))

  err= 0

 DO i=1,nd-1
  IF(rcclassification(i) /= rockcode(i)) THEN
     err(i+1)=err(i)+1
  ELSE
    err(i+1)=err(i)
  END IF
! WRITE(*,*)'erros =', err(i)
END DO 

 
END SUBROUTINE Error




!------SUBROTINAS NÃO UTILIZADAS---!

SUBROUTINE treinamento(xxi,aa,eta,epoch,w)
!Rotina de treinamento para somente um padrão sendo W uma matriz de pesos
!Este conceito pode ser aproveitado no futuro para rede multicamadas. 
IMPLICIT NONE
INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(IN):: xxi
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:), INTENT(INOUT):: w
REAL(KIND=DP), INTENT(IN):: aa, eta
INTEGER(KIND=SP), INTENT(IN):: epoch
INTEGER(KIND=SP):: i

!Critério de parada (BRUTAL EM 100 PELO +oo)
!IF(aa>100)THEN
!  epoch=0
! DO WHILE (aa>100)
!   epoch=epoch+1
! END DO
!END IF

DO i=1,epoch
 !Atualização do w para somente um padrão 
  IF(aa>0) THEN 
    w(i+1,i+1)=w(i,i)
  ELSE IF(aa<=0) THEN
    w(i+1,i+1)=w(i,i)+eta*xxi(i+1,i+1) 
  END IF
END DO 


END SUBROUTINE treinamento

END MODULE activation
