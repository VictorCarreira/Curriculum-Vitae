PROGRAM Estatisticos

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
    !Programa de teste e comparação de duas diferentes distâncias                !
    !Orientador: Cosme Ferreira da Ponte Neto                                    !
    !Aluno: Victor Ribeiro Carreira                                              !
    !Categoria: classificador                                                    !
    !Subrotina Teste                                                             !
    !Para usar compilação com flags utilize:                                     !
    !"pasta/subpasta/nomedopragrama.f95" -o nomedoexecutável                     !
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

                         !***********TABELA DE VARIÁVEIS***********!
                          !lito: classe de rocha específica         !
                          !nt: número de dados de treinamento       !
                          !ndclass: número de dados de classificação!
                          !codtr: codigos de rocha do treinamento   !
                          !codcl: codigos de rocha da classificação !
                          !proftr: vetor com as profs de treinamento!
                          !profcl: vetor com as profs de classific. !
                          !tr: matriz com os dados de treianemto    !
                          !cl: matriz com os dados de classificação !
                          !-----------------------------------------!

  IMPLICIT NONE
  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)
  INTEGER(KIND=SP):: i,j,i1,i2,i3,i4,i5,i6,i7,i8,i9, ie, ij, nt, ndclass, imelhor
  INTEGER(KIND=SP)::acertos, erro
  INTEGER(KIND=SP), ALLOCATABLE, DIMENSION(:):: lito_maha
  REAL(KIND=DP)::maha1,maha2,maha3, eucli, a1, a2, a3, a4, a5, a6, inicio, fim, soma, grande, menor
  REAL(KIND=SP), ALLOCATABLE, DIMENSION(:):: proftr, profcl, codtr, codcl,dist, xx, rock
  REAL*8, DIMENSION(4):: xmin, xmax
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: tr, cl, lito1,lito2,lito3,lito4, lito5, lito6, lito7, lito8, lito9, centroide &
         , distMaha
  CHARACTER(LEN=80):: cab, litologia ,litologiatr, litologiacl
  
  CALL CPU_TIME(inicio)

  !Entradas
   OPEN(1,FILE='../inputs/Real/1RCH0001SC/perfis_1RCH0001SC.txt')
   OPEN(2,FILE='../inputs/Real/1TP0003SC/perfis_1TP0003SC.txt')
  !Saídas 
   OPEN(3,FILE='../outputs/Real/ERclass020720.txt')
   OPEN(4,FILE='../outputs/Real/MRclass020720.txt')
   OPEN(5,FILE='../log/Real/MLlog020720.txt')
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!$$$$$$$$$$$$$$$$$$$$$ LEITURA DOS ARQUIVOS DE ENTRADA $$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!! CONTA N DADOS ARQUIVO DE TREINAMENTO UNIDADE 1 !!!!

     READ(1,15) cab    ! cabeçalho
     READ(1,15) cab    ! linha em branco abaixo do cabeçalho
     ! Conta linhas para dimensionalizar o dado
     ij=1 
     DO WHILE (.true.) 
       READ(1,*,end=10) litologia, a1, a2, a3, a4, a5, a6 ! só conta
        ij=ij+1  
     END DO 
     10 CONTINUE 
     CLOSE(1) 

     nt=ij-1! A variável nt vai armazenar a contagem de linhas em ij. Fazendo a operação ij-1 obtem-se o número de dados de treinamento
     WRITE(6,*) "N de dados de treinamento",nt 

!!!!! CONTA N DADOS ARQUIVO DE ENTRADA UNIDADE 2 CLASSIFICAÇÂO!!!!

  READ(2,15) cab   !lê linha referente ao cabeçalho
  READ(2,15) cab   !Lê linha em branco abaixo do cabeçalho

  ! Conta linhas para dimensionalizar o dado
   ie=1
  DO WHILE (.TRUE.)
     READ(2,*,END=8) litologia, a1,a2,a3,a4,a5,a6 !só conta os dados de classificação
     ie=ie+1
  END DO
  8 CONTINUE
  CLOSE(2)


ndclass=ie-1
WRITE(6,*)"N dados a serem classificadonsons",ndclass


!!!!!!!!!! DIMENSIONA A MATRIZES E VETORES PARA O TAMANHO DO DADO !!!!!!!!!!!!!!!!

!Armazena vetores de profundidades e codigos rochosos
ALLOCATE(codtr(1:nt),codcl(1:ndclass),proftr(1:nt),profcl(1:ndclass))
!Armazena matrizes de propriedades físicas
ALLOCATE(tr(1:nt,4),cl(1:ndclass,4),rock(8))


!!!! ARMAZENA OS DADOS NAS MATRIZES E VETORES DIMENSIONADOS PARA OS DADOS !!!!!!!!!!!!!!! 

OPEN(1,FILE='../inputs/Real/1RCH0001SC/perfis_1RCH0001SC.txt')

READ(1,15) cab 
READ(1,15) cab

! leitura do dado de treinamento:
i=0
DO i=1,nt
  READ(1,*) litologiatr, codtr(i), proftr(i),tr(i,1),tr(i,2),tr(i,3),tr(i,4)
END DO 

OPEN(2,FILE='../inputs/Real/1TP0003SC/perfis_1TP0003SC.txt')
READ(2,15) cab  
READ(2,15) cab   

! leitura do dado de classificacao:
i=0
DO i=1,ndclass
  READ(2,*) litologiacl, codcl(i), profcl(i), cl(i,1), cl(i,2), cl(i,3), cl(i,4)
END DO

! Normalizacao dos dados de treinamento:
 
  xmin = MINVAL(cl,1) ! dados de classificacao (deve ser utilizado os mesmos coeficientes de normalizacao!!!!!)
  xmax = MAXVAL(cl,1) 
  CALL minmax(tr, nt, 4, xmin, xmax)  ! dados de treinamento
  CALL minmax(cl, ndclass,4, xmin, xmax)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!! ETAPA DE TREINAMENTO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!! VIZUALIZA DOS DADOS NA TELA PARA DETERMINAÇÃO DOS CLUSTERS !!!!!!!!!

!!!!!!!!!!!!!!!!!!! 1RHC0001SC !!!!!!!!!!!!!!!!
!   6  CALCILUTITO      =    16.0 M       .49 %
!  44  DIAMICTITO       =   361.0 M     11.04 %
!  49  ARENITO          =   689.0 M     21.08 %
!  54  SILTITO          =   452.0 M     13.83 %
!  56  ARGILITO         =    68.0 M      2.08 %
!  57  FOLHELHO         =   523.0 M     16.00 %
!  58  MARGA            =     3.0 M       .09 %
!  65  DIABASIO         =   179.0 M      5.48 %
!  66  BASALTO          =   978.0 M     29.92 %


rock(1)=6
rock(2)=44
rock(3)=49
rock(4)=54
rock(5)=56
rock(6)=57
rock(7)=58
rock(8)=65
!rock(9)=66


!!!!!!!!!!!! Divide o dado em subsets !!!!!!!!!!!!!
!Contador:
i=0 
!Rochas:
i1=0
i2=0
i3=0
i4=0
i5=0
i6=0
i7=0
i8=0
i9=0





!Conta linhas dos subsets
DO i=1,nt
 IF (codtr(i) == 49.0 ) THEN !arenito
     i1=i1+1
   ELSE IF (codtr(i) == 44.0) THEN !diamictito
     i2=i2+1
   ELSE IF (codtr(i) == 57.0) THEN !folhelho
      i3=i3+1
   ELSE IF (codtr(i) == 54.0) THEN !siltito
      i4=i4+1  
   ELSE IF (codtr(i) == 6.0) THEN !calcilutito 
      i5=i5+1
   ELSE IF (codtr(i) == 58.0) THEN !marga
      i6=i6+1
   ELSE IF (codtr(i) == 65.0) THEN !diabasio
      i7=i7+1
   ELSE IF (codtr(i) == 56.0) THEN ! argilito
      i8=i8+1
   ELSE IF (codtr(i) == 66.0) THEN ! basalto   
      i9=i9+1
  END IF
END DO


ALLOCATE(lito1(i1,4),lito2(i2,4),lito3(i3,4),lito4(i4,4),lito5(i5,4) )
ALLOCATE(lito6(i6,4),lito7(i7,4),lito8(i8,4),lito9(i9,4))

i=0
i1=0
i2=0
i3=0
i4=0
i5=0
i6=0
i7=0
i8=0
i9=0 

!!!!!! Preenche as matrizes de subsets com o dado !!!!!!!

DO i=1, nt
 IF (codtr(i) == 49.0 ) THEN
      i1=i1+1   
      lito1(i1,1)= tr(i,1)
      lito1(i1,2)= tr(i,2)
      lito1(i1,3)= tr(i,3)
      lito1(i1,4)= tr(i,4) 

   ELSE IF (codtr(i) == 44.0) THEN
     i2=i2+1   
      lito2(i2,1)= tr(i,1)
      lito2(i2,2)= tr(i,2)
      lito2(i2,3)= tr(i,3)
      lito2(i2,4)= tr(i,4) 

   ELSE IF (codtr(i) == 57.0) THEN
      i3=i3+1   
      lito3(i3,1)= tr(i,1)
      lito3(i3,2)= tr(i,2)
      lito3(i3,3)= tr(i,3)
      lito3(i3,4)= tr(i,4) 

   ELSE IF (codtr(i) == 54.0) THEN
      i4=i4+1   
      lito4(i4,1)= tr(i,1)
      lito4(i4,2)= tr(i,2)
      lito4(i4,3)= tr(i,3)
      lito4(i4,4)= tr(i,4) 

   ELSE IF (codtr(i) == 6.0) THEN
      i5=i5+1   
      lito5(i5,1)= tr(i,1)
      lito5(i5,2)= tr(i,2)    
      lito5(i5,3)= tr(i,3)
      lito5(i5,4)= tr(i,4) 

   ELSE IF (codtr(i) == 58.0) THEN
      i6=i6+1   
      lito6(i6,1)= tr(i,1)
      lito6(i6,2)= tr(i,2)
      lito6(i6,3)= tr(i,3)
      lito6(i6,4)= tr(i,4) 

   ELSE IF (codtr(i) == 65.0) THEN
      i7=i7+1   
      lito7(i7,1)= tr(i,1)
      lito7(i7,2)= tr(i,2)
      lito7(i7,3)= tr(i,3)
      lito7(i7,4)= tr(i,4) 

   ELSE IF (codtr(i) == 56.0 ) THEN
      i8=i8+1   
      lito8(i8,1)= tr(i,1)
      lito8(i8,2)= tr(i,2)
      lito8(i8,3)= tr(i,3)
      lito8(i8,4)= tr(i,4) 

   ELSE IF (codtr(i) == 66.0 ) THEN
      i9=i9+1   
      lito9(i9,1)= tr(i,1)
      lito9(i9,2)= tr(i,2)
      lito9(i9,3)= tr(i,3)
      lito9(i9,4)= tr(i,4) 
   END IF
END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!! EUCLIDEAN MACHINE LEARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Este classificador determina a menor distância entre a matriz de centroides por
!tipo rochoso em relação a cada linha do arquivo de classificação.


!!!!!!!!!! DETERMINAÇÃO DA MATRIZ DOS CENTRÓIDES = centroide (9,4) !!!!!!!!!!!!

 ALLOCATE(centroide(8,4))

DO i=1,4 
  soma=0d0
  DO j=1,i1
   soma=soma+lito1(j,i)
  END DO 
 centroide(1,i)= soma/DFLOAT(i1)
END DO

DO i=1,4 
  soma=0d0
  DO j=1,i2
   soma=soma+lito2(j,i)
  END DO 
 centroide(2,i)= soma/DFLOAT(i2)
END DO

DO i=1,4 
  soma=0d0
  DO j=1,i3
   soma=soma+lito3(j,i)
  END DO 
 centroide(3,i)= soma/DFLOAT(i3)
END DO

DO i=1,4 
  soma=0d0
  DO j=1,i4
   soma=soma+lito4(j,i)
  END DO 
 centroide(4,i)= soma/DFLOAT(i4)
END DO

DO i=1,4 
  soma=0d0
  DO j=1,i5
   soma=soma+lito5(j,i)
  END DO 
 centroide(5,i)= soma/DFLOAT(i5)
END DO

DO i=1,4 
  soma=0d0
  DO j=1,i6
   soma=soma+lito6(j,i)
  END DO 
 centroide(6,i)= soma/DFLOAT(i6)
END DO


DO i=1,4 
  soma=0d0
  DO j=1,i7
   soma=soma+lito7(j,i)
  END DO 
 centroide(7,i)= soma/DFLOAT(i7)
END DO

DO i=1,4 
  soma=0d0
  DO j=1,i8
   soma=soma+lito8(j,i)
  END DO 
 centroide(8,i)= soma/DFLOAT(i8)
END DO


!DO i=1,4 
!  soma=0d0
!  DO j=1,i9
!   soma=soma+lito9(j,i)
!  END DO 
! centroide(9,i)= soma/DFLOAT(i9)
!END DO




PRINT*,'************************ MATRIZ DE CENTROIDES *****************************'
DO i=1,8
 WRITE(6,*)(centroide(i,j),j=1,4)
END DO
PRINT*,'***************************************************************************'
!!!!!!!!!!!!!!!!!!!!!!!! CLASSIFICACAO DOS DADOS  !!!!!!!!!!!!!!!!!!!!!!!!!!!

ALLOCATE(dist(8),xx(ndclass))

DO j=1,ndclass
 DO i=1,8
   dist(i)=(cl(j,1)-centroide(i,1))**2 + &
           (cl(j,2)-centroide(i,2))**2 + &
           (cl(j,3)-centroide(i,3))**2 + &
           (cl(j,4)-centroide(i,4))**2
   dist(i)=SQRT(dist(i))
   !WRITE(6,*) dist(i)
  END DO 
! WRITE(6,*) MINVAL(dist)
! Rotina Cósmica para seleção do menor índice
grande=0.0d0

 DO i=1,8
  grande=grande+abs(dist(i))
 END DO
 
 menor=grande  !1.d10   !grande					!deve ser um número grande
! print*,menor,minval(dist,1)
 
  
  DO i=1,8
   IF(dist(i).LT.menor) THEN
    menor=dist(i)
    imelhor=rock(i)
   print*,imelhor
    END IF
   END DO

!!!!!! SALVA ARQUIVO DE SAÍDA !!!!!!!

  IF (imelhor == 49.0) THEN 
      xx(j) = 49.0
  END IF

  IF (imelhor == 44.0) THEN
      xx(j) = 44.0
  END IF

  IF (imelhor == 57.0) THEN
      xx(j) = 57.0
  END IF

  IF (imelhor == 54.0) THEN
      xx(j) = 54.0
  END IF

  IF (imelhor == 6.0) THEN
      xx(j) = 6.0
  END IF

  IF (imelhor == 58.0) THEN
      xx(j) = 58.0
  END IF

  IF (imelhor == 65.0) THEN
      xx(j) = 65.0
  END IF

  IF (imelhor == 56.0) THEN
      xx(j) = 56.0
  END IF

  IF (imelhor == 66.0) THEN
      xx(j) = 66.0
  END IF

  WRITE(3,*) j, 'lito=', xx(j)

END DO


! Conta os erros e os acertos do Euclides

acertos=0

DO i=1,ndclass
  IF(xx(i) == codcl(i) ) THEN
          acertos = acertos +1
  ELSE 
          acertos = acertos
  END IF
END DO

PRINT*,'Acertos da classificação euclideana=>',acertos

erro=0

DO i=1,ndclass
  IF(xx(i) /= codcl(i)) THEN
          erro = erro+1
  ELSE
          erro = erro
  END IF
END DO



PRINT*,'Erros da classificação euclideana=>',erro

PRINT*,'+++++++++++++++++++++++++++++++++++++++++++++++++'

WRITE(5,FMT=*)'1TP0003SC'
WRITE(5,FMT=*)'ACERTOS(MLE)=',acertos
WRITE(5,FMT=*)'ERROS(MLE)=',erro


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!! MAHALANOBEAN MACHINE LEARNING !!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Calculo entre 1 nuvem de treinamento e 1 ponto pertencente aos dados de classificacao:

  ALLOCATE(distMaha(ndclass,8), lito_maha(ndclass)) ! numero de dados de classificacao x numero de litologias a serem comparadas

  DO i=1,ndclass ! numero total de dados de classificacao
      CALL mahalanobeana(lito1, i1, cl(i,:),  1, 4, distMaha(i,1) ) ! distancia do i-esimo ponto dos dados de classificaco em relacao a lito1
      CALL mahalanobeana(lito2, i2, cl(i,:),  1, 4, distMaha(i,2) )
      CALL mahalanobeana(lito3, i3, cl(i,:),  1, 4, distMaha(i,3) )
      CALL mahalanobeana(lito4, i4, cl(i,:),  1, 4, distMaha(i,4) )
      CALL mahalanobeana(lito5, i5, cl(i,:),  1, 4, distMaha(i,5) )
      CALL mahalanobeana(lito6, i6, cl(i,:),  1, 4, distMaha(i,6) )
      CALL mahalanobeana(lito7, i7, cl(i,:),  1, 4, distMaha(i,7) )
      CALL mahalanobeana(lito8, i8, cl(i,:),  1, 4, distMaha(i,8) )
      !CALL mahalanobeana(lito9, i9, cl(i,:),  1, 4, distMaha(i,9) )
   END DO
   
   ! Localizacao do indice dos menores valores de distmaha e guardando no vetor lito_maha: 
   lito_maha= MINLOC(distMaha,2)
   
   
   ! escrevendo o arquivo de saida:
   xx=0
   DO j=1,ndclass

     IF (rock(lito_maha(j)) == 49.0) THEN 
         xx(j) = 49.0
     END IF

     IF (rock(lito_maha(j)) == 44.0) THEN
         xx(j) = 44.0
     END IF

     IF (rock(lito_maha(j)) == 57.0) THEN
         xx(j) = 57.0
     END IF

     IF (rock(lito_maha(j)) == 54.0) THEN
         xx(j) = 54.0
     END IF

     IF (rock(lito_maha(j)) == 6.0) THEN
         xx(j) = 6.0
     END IF

     IF (rock(lito_maha(j)) == 58.0) THEN
         xx(j) = 58.0
     END IF

     IF (rock(lito_maha(j)) == 65.0) THEN
         xx(j) = 65.0
     END IF

      IF (rock(lito_maha(j)) == 56.0) THEN
       xx(j) = 56.0
      END IF

      !IF (lito_maha(j) == 66.0) THEN
      ! xx(j) = 66.0
      !END IF

       WRITE(4,*) j, 'lito=', xx(j)
       PRINT*,xx(j)

    END DO



! Conta os erros e os acertos de classificação de mahalanobis:

acertos=0

DO i=1,ndclass
  IF(xx(i) == codcl(i) ) THEN
          acertos = acertos +1
  ELSE 
          acertos = acertos
  END IF
END DO

PRINT*,'Acertos da classificação mahalanobeana=>',acertos

erro=0

DO i=1,ndclass
  IF(xx(i) /= codcl(i)) THEN
          erro = erro+1
  ELSE
          erro = erro
  END IF
END DO



PRINT*,'Erros da classificação mahalanobeana=>',erro

PRINT*,'++++++++++++++++++++++++++++++++++++++++++++'

WRITE(5,FMT=*)'ACERTOS(MLM)=',acertos
WRITE(5,FMT=*)'ERROS(MLM)=',erro


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ FORMAT $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !Formatos dos arquivos de saida
  11 FORMAT(F12.1)
  21 FORMAT(A9,2x,E12.2)
  22 FORMAT(A20,2x,I10)
  15 FORMAT(A71)


  CLOSE(1)
  CLOSE(2)
  CLOSE(3)
  CLOSE(4)
  CLOSE(5)
  

  CALL CPU_TIME(fim)
  PRINT*,'tempo de máquina=',fim-inicio


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ FIM $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  CONTAINS

  !------------------------------------------------------------------------------
  SUBROUTINE euclideana(lito1,lito2,eucli)

   IMPLICIT NONE
   INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
   INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)

   REAL(KIND=DP), DIMENSION(:,:), INTENT(IN)::lito1, lito2
   REAL(KIND=DP), INTENT(OUT):: eucli
   REAL(KIND=DP)::media1, media2

   INTEGER(KIND=SP):: k

    eucli=0d0

    IF(SIZE(lito1(1,:)) /= SIZE(lito2(1,:)))THEN
      PRINT*,'WARNING! THE PROPERTIES NUMBER´S OF lito1 AND lito2 MUST BE THE SAME.'
      STOP
      RETURN
    END IF

   eucli=0.0
    DO k=1,SIZE(lito1(1,:))  ! Inicia o laço da primeira até a última propriedade que é dado pelo size de lito
     media1=0d0 !zera as variáveis
     media2=0d0 !zera as variáveis para a cálculo do centróide 2
     media1=SUM(lito1(:,k))/SIZE(lito1(:,k)) !calcula as médias para as k propriedades
     media2=SUM(lito2(:,k))/SIZE(lito2(:,k)) !calcula as médias para k propriedades para uma segunda nuvem de pontos
     !eucli= eucli + (lito2(1,k)-media1)**2 ! Cálculo da medida de semelhança de euclides para um conjunto de pontos e um centróide
     eucli= eucli + (media2-media1)**2
    END DO ! Final do laço das k propriedades
    eucli=SQRT(eucli)

  
  END SUBROUTINE euclideana
 !------------------------------------------------------------------------------------

  SUBROUTINE mahalanobeana(g11,np1,dado,np2,ndim,dist)

  !  	subrotina que calcula a distância de mahalanobis entre
  !  	dois agrupamentos de elementos com dimensão ndim

     IMPLICIT NONE
      INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
      INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)

      INTEGER(KIND=SP), INTENT(IN):: np1
      INTEGER(KIND=SP), INTENT(IN):: np2, ndim
      REAL(KIND=DP),INTENT(OUT):: dist

      INTEGER(KIND=SP):: i,j,k
      REAL(KIND=DP), DIMENSION(ndim), INTENT(IN) :: dado
      REAL(KIND=DP),ALLOCATABLE, DIMENSION(:):: soma, xm1, xm2, gg2
      REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:):: g1, g2, g1T, g2T, cov1, cov2, &
      covag, g11, g22, md, mdT, alfa, d2

      ALLOCATE(soma(ndim),xm1(ndim),xm2(ndim), gg2(ndim))

      ALLOCATE(g1(np1,ndim),g2(np2,ndim),g1T(ndim,np1),g2T(ndim,np2),&
      cov1(ndim,ndim),cov2(ndim,ndim),covag(ndim,ndim),md(ndim,1),&
      mdT(1,ndim),alfa(1,ndim),d2(1,1) )

      g1=g11
      !g2=g22
      gg2 = dado
	

  !  	grupo 1
  
    DO j=1,ndim
      soma(j)=0d0
      DO i=1,np1
        soma(j)=soma(j)+g1(i,j)
      END DO
    END DO

    DO i=1,ndim
      xm1(i)=soma(i)/dfloat(np1)
    END DO

  !  	grupo 2
    DO j=1,ndim
      soma(j)=0d0
      soma(j)=soma(j)+gg2(j)
    END DO
   

    DO i=1,ndim
      xm2(i)=soma(i)/dfloat(np2)
    END DO

  !  	vetor das diferenças - será escrito sobre a matrizes g1 e g2


    DO j=1,ndim
      DO i=1,np1
        g1(i,j)=g1(i,j)-xm1(j)
      END DO
    END DO

    DO  j=1,ndim
      !DO i=1,np2
      gg2(j)=gg2(j)-xm2(j)
      !END DO
    END DO

  !      --------GRUPO 1 ---------------------
  !  	criando a matriz transposta g1T
  !  	-------------- -------------------
    DO i=1,np1    !107 ! número de equações
      DO j=1,ndim   !2
        g1T(j,i)=g1(i,j)
      END DO
    END DO
  !  ----------------------------------------------------
  !  	 - multiplicação de matrizes
  !  	   multiplicação de g1T por g1

    DO k=1,ndim
      DO j=1,ndim
        cov1(j,k)=0.d0
        DO i=1,np1
          cov1(j,k)=cov1(j,k)+g1T(j,i)*g1(i,k)
        END DO
      END DO
    END DO

   ! write(6,*) '======covariância 1 ======'
   ! write(6,*) cov1(1,1),cov1(1,2)
   ! write(6,*) cov1(2,1),cov1(2,2)

    DO i=1,ndim
      DO j=1,ndim
        cov1(i,j)=cov1(i,j)/dfloat(np1)
      END DO
    END DO



  !      --------GRUPO 2 ---------------------
  !  	criando a matriz transposta g2T

    !DO i=1,np2
     ! DO j=1,ndim
     !   g2T(j,i)=g2(i,j)
     ! END DO
    !END DO

  !  ---------------------------------------------------
  !  	 - multiplicação de matrizes
  !  	   multiplicação de g2T por g2

     DO k=1,ndim
       DO j=1,ndim
         cov2(j,k)=0.d0
         !DO i=1,np2
         cov2(j,k)=cov2(j,k)+gg2(k)**2
         !END DO
       END DO
     END DO

     DO  i=1,ndim
       DO j=1,ndim
         cov2(i,j)=cov2(i,j)/dfloat(np2)
       END DO
     END DO

    !  WRITE(6,*) '======covariância 2 ======'
    !  WRITE(6,*) cov2(1,1),cov2(1,2)
    !  WRITE(6,*) cov2(2,1),cov2(2,2)


  !  	-------- covariância agrupada------

     DO i=1,ndim
       DO j=1,ndim
         covag(i,j)=dfloat(np1)*cov1(i,j)/(dfloat(np1+np2))+ &
         dfloat(np2)*cov2(i,j)/(dfloat(np1+np2))
       END DO
     END DO

    !  WRITE(6,*) '======covariância agrupada ======'
    !  WRITE(6,*) covag(1,1),covag(1,2)
    !  WRITE(6,*) covag(2,1),covag(2,2)

  !  	inversao da matriz covag - usando subrotina



     CALL INVERT(covag,ndim)
    ! Teste: dexando a matriz de cov =1
   !covag=0d0
    ! DO i=1,ndim
    !   covag(i,i)=1.0
     !END DO

    !  WRITE(6,*) '====== inv covariância agrupada ======'
    !  WRITE(6,*) covag(1,1),covag(1,2)
    !  WRITE(6,*) covag(2,1),covag(2,2)

  !  	diferenicas médias

     DO i=1,ndim
       md(i,1)=xm1(i)-xm2(i)
     END DO

  !  	write(6,*) '====== diferencias medias ======'
  !  	write(6,*) md(1,1)
  !  	write(6,*) md(2,1)

  !  	criando a matriz transposta mdT
  !  	---------------------------

    DO i=1,ndim
      DO j=1,1
        mdT(j,i)=md(i,j)
      END DO
    END DO

  !  ----------------------------------------------------
  !  	multiplicação de mdT por cov^-1
  !  	 - multiplicação de matrizes


    DO k=1,ndim
      DO j=1,1
        alfa(j,k)=0.d0
        DO i=1,ndim
          alfa(j,k)=alfa(j,k)+mdT(j,i)*covag(i,k)
        END DO
      END DO
    END DO

  !  ----------------------------------------------------
  !  	multiplicação de alfa por md
  !  	 - multiplicação de matrizes

    DO k=1,1
      DO j=1,1
        d2(j,k)=0.d0
        DO i=1,ndim  !2	!
          d2(j,k)=d2(j,k)+alfa(j,i)*md(i,k)
        END DO
      END DO
    END DO

    dist=dsqrt(d2(1,1))

  !! Desalocando as variaveis dentro da subrotina:
   DEALLOCATE(g1,g2,g1T,g2T,cov1,cov2,covag,md,mdT,alfa,d2, soma, xm1, xm2)


  END SUBROUTINE mahalanobeana


  !--------------------------------------------------------------------------


     SUBROUTINE INVERT(A,i)
        integer i,im,j,k,l
        real*8 A(i,i),B(i)

         IM=I-1

         DO 5 K=1,I
           DO 2 J=1,IM
             2 B(J)=A(1,J+1)/A(1,1)
             B(I)=1.d0/A(1,1)
             DO 4 L=1,IM
               DO 3 J=1,IM
                 3 A(L,J)=A(L+1,J+1)-A(L+1,1)*B(J)
                 4 A(L,I)=-A(L+1,1)*B(I)
                 DO 5 J=1,I
                   5 A(I,J)=B(J)

     END SUBROUTINE INVERT


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 SUBROUTINE minmax(x, nx, ny, a, b) ! normalizacao dos perfis a partir do metodo min max scaling (igual ao do calculdo do IGR)
 ! inputs:
 INTEGER*4, INTENT(IN):: nx,ny ! dimensoes 
 REAL*8, INTENT(INOUT), DIMENSION(nx,ny) :: x
 REAL*8, DIMENSION( ny ):: a, b ! limites inferior e superior dos dados de classificacao.
 INTEGER*4:: i,j 

 ! loop da normalizacao para cada propriedade:
 DO i=1, nx
   DO j=1, ny
     x(i,j) = ( x(i,j) - a(j) )/ ( b(j) - a(j) )
   END DO
 END DO

 RETURN
 END SUBROUTINE minmax


SUBROUTINE minmax_standard(x, nx, ny, mean, std) ! normalizacao dos perfis a partir do metodo standardization Z-core
 ! inputs:
 INTEGER*4, INTENT(IN):: nx,ny ! dimensoes 
 REAL*8, INTENT(INOUT), DIMENSION(nx,ny) :: x
 REAL*8, INTENT(IN), DIMENSION( ny ):: mean, std
 INTEGER*4:: i,j 
 REAL*8:: soma

  ! loop da normalizacao:
 DO i=1,nx
   DO j=1,ny
     x(i,j) = ( x(i,j) - mean(j) )/ ( std(j) )
   END DO
 END DO
 
 RETURN
 END SUBROUTINE minmax_standard

END PROGRAM Estatisticos
