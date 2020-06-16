PROGRAM sintetico
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !Projeto de Doutorado                                                     !
  !Orientador: Cosme Ferreira da Ponte Neto                                 !
  !Aluno: Victor Ribeiro Carreira                                           !
  !Este programa tem o propósito de simular dados de well-logging           !
  !Gerador de dados sintéticos de uma Base de Dados de dados de poços       !
  !Propriedades modeladas em rocha: RHOB, SP, GR, DT                        !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

     !##################### LISTA DE VARIÁVEIS ###########################!
     !nd - número de dados                                                !
     !vp - Matriz com valor da propriedade física			  !
     !delta - despersão da valor medidos (desvio padrão)		  !
     !ic - contador universal de profundidade                             !
     !####################################################################!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$ DECLARAÇÃO DAS VARIÁVEIS GLOBAIS $$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
 INTEGER, PARAMETER:: SGL = SELECTED_REAL_KIND(p=8, r=8)
 INTEGER, PARAMETER:: DBL = SELECTED_REAL_KIND(p=4, r=4)
 INTEGER(KIND=DBL):: Lito(90), nd(90), i, j, ic, ia, ia1, ia2, ia3, ia4
 CHARACTER(LEN=12):: L(90)
 CHARACTER(LEN=80):: cab
 REAL(KIND=SGL):: a, a5, a4, a3, a2, inicial, final 
 REAL(KIND=SGL):: custocomputacional, b1, b2, b3, b4, alfa1, alfa2, alfa3, alfa4
 REAL(KIND=SGL), ALLOCATABLE, DIMENSION(:):: x1, x2
 REAL(KIND=SGL), ALLOCATABLE, DIMENSION(:,:):: ds, vp, delta, vc


ALLOCATE(ds(10000,5), x1(10000), x2(10000), vp(4,10000), delta(4,10000), vc(4,10000))


    CALL cpu_time(inicial)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$ ARQUIVOS DE SAÍDA $$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

OPEN(1,file='../inputs/BDsintetico.txt')

!! Cabeçalho

WRITE(1,15) 'Litologia','codigo','prof','RHOB','GR','SP','DT'
WRITE(1,*) '   '   ! Cria um separador (linha branca) em baixo do cabeçalho

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ POÇO T1 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PRINT*,'********************* BANCO DE DADOS ****************************'
!-----------------------------------------
!Folhelho 2 é uma rocha com baixo teor de matéria orgânica GR entre 60-150
 Lito(1)=1 !folhelho 2
  L(1)='shale2'

  vp(1,1)=2.50d0!densidade
  vp(2,1)=110d0!raio-gama
  vp(3,1)=70d0!potencial espontaneo 
  vp(4,1)=550d0!sonico

  delta(1,1)=vp(1,1)*0.02d0
  delta(2,1)=vp(2,1)*0.05d0
  delta(3,1)=vp(3,1)*0.1d0
  delta(4,1)=vp(4,1)*0.02d0

  nd(1)=INT(0.5/1d-2)   !numero de medidas na camada 1
WRITE(6,*) 'nd(t1-folhelho2)=',nd(1)

!----------------------------------------
Lito(2)=2   !dolomita
 L(2)='dolomite'
 vp(1,2)=2.5d0!RHOB
 vp(2,2)=40d0!GR
 vp(3,2)=-60d0!SP
 vp(4,2)=142d0!DT

 delta(1,2)=vp(1,2)*0.02d0
 delta(2,2)=vp(2,2)*0.1d0
 delta(3,2)=vp(3,2)*0.1d0
 delta(4,2)=vp(4,2)*0.1d0


 nd(2)=INT(0.9/1d-2)   !numero de medidas na camada 2
 WRITE(6,*) 'nd(t1-dolomita)=',nd(2)

!--------------------------------------
 Lito(3)=3     !diabásio
  L(3)='diabase'
  vp(1,3)=2.8d0!RHOB
  vp(2,3)=30d0!GR
  vp(3,3)=5d0!SP
  vp(4,3)=50d0!DT

  delta(1,3)=vp(1,3)*0.02d0
  delta(2,3)=vp(2,3)*0.1d0
  delta(3,3)=vp(3,3)*0.1d0
  delta(4,3)=vp(4,3)*0.1d0

  nd(3)=INT(0.2/1d-2)   !numero de medidas na camada 3
WRITE(6,*) 'nd(t1-diabásio)=',nd(3)


!--------------------------------------

Lito(4)=2   !dolomita
 L(4)='dolomite'
 vp(1,4)=2.5d0!RHOB
 vp(2,4)=40d0!GR
 vp(3,4)=-60d0!SP
 vp(4,4)=142d0!DT

 delta(1,4)=vp(1,4)*0.02d0
 delta(2,4)=vp(2,4)*0.1d0
 delta(3,4)=vp(3,4)*0.1d0
 delta(4,4)=vp(4,4)*0.1d0

 nd(4)=INT(0.4/1d-2)   !numero de medidas na camada 4
 WRITE(6,*) 'nd(t1-dolomita)=',nd(4)


!-------------------------------------
Lito(5)=4      !conglomerado
 L(5)='conglomerate'
 vp(1,5)=2.3d0!RHOB
 vp(2,5)=20d0!GR
 vp(3,5)=-40d0!SP
 vp(4,5)=110d0!DT

 delta(1,5)=vp(1,5)*0.02d0
 delta(2,5)=vp(2,5)*0.01d0
 delta(3,5)=vp(3,5)*0.01d0
 delta(4,5)=vp(4,5)*0.01d0

 nd(5)=INT(2.3/1d-2)   !numero de medidas na camada 5
WRITE(6,*) 'nd(t1-conglomerado)=',nd(5)


!Adiciona ruído aleatório randômico

 ic=0

DO j=1,5  ! numero de camadas
 DO i=1,nd(j)
  ic=ic+1
  CALL n_rand(vp(1,j),delta(1,j),vc(1,j))
  CALL n_rand(vp(2,j),delta(2,j),vc(2,j))
  CALL n_rand(vp(3,j),delta(3,j),vc(3,j))
  CALL n_rand(vp(4,j),delta(4,j),vc(4,j))

  a2=vc(1,j)
  a3=vc(2,j)
  a4=vc(3,j)
  a5=vc(4,j)

  WRITE(1,14) L(j),Lito(j),ic,a2,a3,a4,a5

  ia1=ic
 END DO
END DO


!------------------------------------


!Propriedades do embasamento
 vp(1,7)=2.8d0!RHOB
 vp(2,7)=40d0!GR
 vp(3,7)=70d0!SP
 vp(4,7)=55d0!DT


nd(6)=INT(0.9/1d-2)   !numero de medidas na camada 6
 ia3=0

DO i=1,nd(6)
 ia3=ia3+1
END DO

ia2=ia1+ia3

!!!!!!!!!!!



alfa1=(vp(1,5)-vp(1,7))/dfloat(ia1-ia2)        !-(2.8d0-b1)
alfa2=(vp(2,5)-vp(2,7))/dfloat(ia1-ia2)
alfa3=(vp(3,5)-vp(3,7))/dfloat(ia1-ia2)
alfa4=(vp(4,5)-vp(4,7))/dfloat(ia1-ia2)



b1=vp(1,5)-alfa1*dfloat(ia1)    !2.3d0
b2=vp(2,5)-alfa2*dfloat(ia1)
b3=vp(3,5)-alfa3*dfloat(ia1)
b4=vp(4,5)-alfa4*dfloat(ia1)

WRITE(6,*) 'nd(t1-falha normal)=',nd(6)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ FALHA $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!! Mistura 1
Lito(6)=451    ! camada mista conglomerado+embasamento
L(6)='fault1'

DO i=1,nd(6)/4
  ic=ic+1

  vp(1,6)=alfa1*dfloat(ic)+b1
  vp(2,6)=alfa2*dfloat(ic)+b2
  vp(3,6)=alfa3*dfloat(ic)+b3
  vp(4,6)=alfa4*dfloat(ic)+b4

  delta(1,6)=vp(1,6)*0.02d0
  delta(2,6)=vp(2,6)*0.1d0
  delta(3,6)=vp(3,6)*0.2d0
  delta(4,6)=vp(4,6)*0.1d0

  CALL n_rand(vp(1,6),delta(1,6),vc(1,6))
  CALL n_rand(vp(2,6),delta(2,6),vc(2,6))
  CALL n_rand(vp(3,6),delta(3,6),vc(3,6))
  CALL n_rand(vp(4,6),delta(4,6),vc(4,6))

  a2=vc(1,6)
  a3=vc(2,6)
  a4=vc(3,6)
  a5=vc(4,6)

 WRITE(1,14) L(6),Lito(6),ic,a2,a3,a4,a5
END DO

!!!!!!!!! Mistura  2
Lito(6)=452    ! camada mista conglomerado+embasamento
 L(6)='fault2'

DO i=1,nd(6)/4
 ic=ic+1

  vp(1,6)=alfa1*dfloat(ic)+b1
  vp(2,6)=alfa2*dfloat(ic)+b2
  vp(3,6)=alfa3*dfloat(ic)+b3
  vp(4,6)=alfa4*dfloat(ic)+b4

  delta(1,6)=vp(1,6)*0.02d0
  delta(2,6)=vp(2,6)*0.1d0
  delta(3,6)=vp(3,6)*0.2d0+3 !fator de correção ao passar o SP pelo 0
  delta(4,6)=vp(4,6)*0.1d0

  CALL n_rand(vp(1,6),delta(1,6),vc(1,6))
  CALL n_rand(vp(2,6),delta(2,6),vc(2,6))
  CALL n_rand(vp(3,6),delta(3,6),vc(3,6))
  CALL n_rand(vp(4,6),delta(4,6),vc(4,6))

  a2=vc(1,6)
  a3=vc(2,6)
  a4=vc(3,6)
  a5=vc(4,6)

  WRITE(1,14) L(6),Lito(6),ic,a2,a3,a4,a5
END DO

!!!!!!!!!!!!!! mistura  3
Lito(6)=453    ! camada mista conglomerado+embasamento
 L(6)='fault3'

DO i=1,nd(6)/4
 ic=ic+1

  vp(1,6)=alfa1*dfloat(ic)+b1
  vp(2,6)=alfa2*dfloat(ic)+b2
  vp(3,6)=alfa3*dfloat(ic)+b3
  vp(4,6)=alfa4*dfloat(ic)+b4

  delta(1,6)=vp(1,6)*0.02d0
  delta(2,6)=vp(2,6)*0.1d0
  delta(3,6)=vp(3,6)*0.1d0
  delta(4,6)=vp(4,6)*0.1d0

  CALL n_rand(vp(1,6),delta(1,6),vc(1,6))
  CALL n_rand(vp(2,6),delta(2,6),vc(2,6))
  CALL n_rand(vp(3,6),delta(3,6),vc(3,6))
  CALL n_rand(vp(4,6),delta(4,6),vc(4,6))

  a2=vc(1,6)
  a3=vc(2,6)
  a4=vc(3,6)
  a5=vc(4,6)

  WRITE(1,14) L(6),Lito(6),ic,a2,a3,a4,a5
END DO

!!!!!!!! mistura  4
Lito(6)=454    ! camada mista conglomerado+embasamento
 L(6)='fault4'


DO i=1,nd(6)/4
 ic=ic+1

 vp(1,6)=alfa1*dfloat(ic)+b1
 vp(2,6)=alfa2*dfloat(ic)+b2
 vp(3,6)=alfa3*dfloat(ic)+b3
 vp(4,6)=alfa4*dfloat(ic)+b4

  delta(1,6)=vp(1,6)*0.02d0
  delta(2,6)=vp(2,6)*0.1d0
  delta(3,6)=vp(3,6)*0.1d0
  delta(4,6)=vp(4,6)*0.1d0

  CALL n_rand(vp(1,6),delta(1,6),vc(1,6))
  CALL n_rand(vp(2,6),delta(2,6),vc(2,6))
  CALL n_rand(vp(3,6),delta(3,6),vc(3,6))
  CALL n_rand(vp(4,6),delta(4,6),vc(4,6))

  a2=vc(1,6)
  a3=vc(2,6)
  a4=vc(3,6)
  a5=vc(4,6)

  WRITE(1,14) L(6),Lito(6),ic,a2,a3,a4,a5
END DO

!!!!  final das misturas


!-----------------------------------



Lito(7)=5      !embasamento
 L(7)='crystalline'
 vp(1,7)=2.75d0!RHOB
 vp(2,7)=40d0!GR
 vp(3,7)=70d0!SP
 vp(4,7)=55d0!DT

 delta(1,7)=vp(1,7)*0.02d0
 delta(2,7)=vp(2,7)*0.1d0
 delta(3,7)=vp(3,7)*0.1d0
 delta(4,7)=vp(4,7)*0.1d0

 nd(7)=INT(2.8/1d-2)   !numero de medidas na camada 7
WRITE(6,*) 'nd(t1-embasameto)=',nd(7)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 DO i=1,nd(7)
  ic=ic+1

  CALL n_rand(vp(1,7),delta(1,7),vc(1,7))
  CALL n_rand(vp(2,7),delta(2,7),vc(2,7))
  CALL n_rand(vp(3,7),delta(3,7),vc(3,7))
  CALL n_rand(vp(4,7),delta(4,7),vc(4,7))

  ia1=ic
  a2=vc(1,7)
  a3=vc(2,7)
  a4=vc(3,7)
  a5=vc(4,7)


 WRITE(1,14) L(7),Lito(7),ia1,a2,a3,a4,a5
END DO


!Adicionar o poço t2 aqui


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ Poço T2 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!----------------------------
!Folhelho 2 é uma rocha com baixo teor de matéria orgânica GR entre 60-150
 Lito(8)=1 !folhelho 2
  L(8)='shale2'

  vp(1,8)=2.50d0!densidade
  vp(2,8)=110d0!raio-gama
  vp(3,8)=70d0!potencial espontaneo 
  vp(4,8)=550d0!sonico

  delta(1,8)=vp(1,8)*0.02d0
  delta(2,8)=vp(2,8)*0.1d0
  delta(3,8)=vp(3,8)*0.1d0
  delta(4,8)=vp(4,8)*0.1d0

  nd(8)=INT(0.38/1d-2)   !numero de medidas na camada 1 do poço t2 ! 40 medidas
WRITE(6,*) 'nd(t2-folhelho2)=',nd(8)




!----------------------------------
Lito(9)=2   !dolomita
 L(9)='dolomite'
 vp(1,9)=2.5d0!RHOB
 vp(2,9)=40d0!GR
 vp(3,9)=-60d0!SP
 vp(4,9)=142d0!DT

 delta(1,9)=vp(1,9)*0.02d0
 delta(2,9)=vp(2,9)*0.1d0
 delta(3,9)=vp(3,9)*0.1d0
 delta(4,9)=vp(4,9)*0.1d0


 nd(9)=INT(1.2/1d-2)   !numero de medidas na camada 2 ! 120 medidas
 WRITE(6,*) 'nd(t2-dolomita)=',nd(9)


!----------------------------------
 Lito(10)=3    !diabásio
  L(10)='diabase'
  vp(1,10)=2.8d0!RHOB
  vp(2,10)=30d0!GR
  vp(3,10)=5d0!SP
  vp(4,10)=50d0!DT

  delta(1,10)=vp(1,10)*0.02d0
  delta(2,10)=vp(2,10)*0.1d0
  delta(3,10)=vp(3,10)*0.1d0
  delta(4,10)=vp(4,10)*0.1d0

  nd(10)=INT(0.3/1d-2)   !numero de medidas na camada 3! 30 medidas
WRITE(6,*) 'nd(t2-diabásio)=',nd(10)


!----------------------------------

Lito(11)=2   !dolomita
 L(11)='dolomite'
 vp(1,11)=2.5d0!RHOB
 vp(2,11)=40d0!GR
 vp(3,11)=-60d0!SP
 vp(4,11)=142d0!DT

 delta(1,11)=vp(1,11)*0.02d0
 delta(2,11)=vp(2,11)*0.1d0
 delta(3,11)=vp(3,11)*0.1d0
 delta(4,11)=vp(4,11)*0.1d0

 nd(11)=INT(0.8/1d-2)   !numero de medidas na camada 4
 WRITE(6,*) 'nd(t2-dolomita)=',nd(11)



!----------------------------------
!Folhelho 1 é uma rocha com alto teor de matéria orgânica GR entre 150-250
 Lito(12)=6 !folhelho 
  L(12)='shale1'

  vp(1,12)=2.580d0!densidade
  vp(2,12)=230d0!raio-gama
  vp(3,12)=70d0!potencial espontaneo 
  vp(4,12)=520d0!sonico

  delta(1,12)=vp(1,12)*0.02d0
  delta(2,12)=vp(2,12)*0.1d0
  delta(3,12)=vp(3,12)*0.1d0
  delta(4,12)=vp(4,12)*0.1d0

  nd(12)=INT(2.0/1d-2)   !numero de medidas na camada 1 do poço t2
WRITE(6,*) 'nd(t2-folhelho1)=',nd(12)



!-----------------------------------
!Halita 
 Lito(13)=7 !halita
  L(13)='halite'

  vp(1,13)=2.168d0!densidade
  vp(2,13)=11d0!raio-gama
  vp(3,13)=6d0!potencial espontaneo. Confirmar estes valores
  vp(4,13)=216d0!sonico. Confirmar estes valores. 

  delta(1,13)=vp(1,13)*0.02d0
  delta(2,13)=vp(2,13)*0.1d0
  delta(3,13)=vp(3,13)*0.1d0
  delta(4,13)=vp(4,13)*0.1d0

  nd(13)=INT(1.8/1d-2)   !numero de medidas na camada 1 do poço t2
WRITE(6,*) 'nd(t2-halita)=',nd(13)




!----------------------------------
!Granito 
 Lito(14)=8 !Granito
  L(14)='granite'

  vp(1,14)=2.67d0!densidade
  vp(2,14)=150d0!raio-gama
  vp(3,14)=-60d0!potencial espontaneo. Confirmar estes valores
  vp(4,14)=68d0!sonico. Confirmar estes valores. 

  delta(1,14)=vp(1,14)*0.02d0
  delta(2,14)=vp(2,14)*0.1d0
  delta(3,14)=vp(3,14)*0.1d0
  delta(4,14)=vp(4,14)*0.1d0

  nd(14)=INT(0.5/1d-2)   !numero de medidas na camada 1 do poço t2
WRITE(6,*) 'nd(t2-granito)=',nd(14)



!Adiciona ruído aleatório randômico. Este bloco deve vir após a inserção das propriedades físicas.

 ic=0

DO j=8,14  ! numero de camadas
 DO i=8,nd(j)
  ic=ic+1
  CALL n_rand(vp(1,j),delta(1,j),vc(1,j))
  CALL n_rand(vp(2,j),delta(2,j),vc(2,j))
  CALL n_rand(vp(3,j),delta(3,j),vc(3,j))
  CALL n_rand(vp(4,j),delta(4,j),vc(4,j))

  a2=vc(1,j)
  a3=vc(2,j)
  a4=vc(3,j)
  a5=vc(4,j)

  WRITE(1,FMT=14) L(j),Lito(j),ic,a2,a3,a4,a5

  ia1=ic
 END DO
END DO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ POÇO T3 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!----------------------------
!Folhelho 2 é uma rocha com baixo teor de matéria orgânica GR entre 60-150
 Lito(15)=1 !folhelho 2
  L(15)='shale2'

  vp(1,15)=2.50d0!densidade
  vp(2,15)=110d0!raio-gama
  vp(3,15)=70d0!potencial espontaneo 
  vp(4,15)=550d0!sonico

  delta(1,15)=vp(1,15)*0.02d0
  delta(2,15)=vp(2,15)*0.1d0
  delta(3,15)=vp(3,15)*0.1d0
  delta(4,15)=vp(4,15)*0.1d0

  nd(15)=INT(0.3/1d-2)   !numero de medidas na camada 1 do poço t3
WRITE(6,*) 'nd(t3-folhelho2)=',nd(15)


!----------------------------------
Lito(16)=2   !dolomita
 L(16)='dolomite'
 vp(1,16)=2.5d0!RHOB
 vp(2,16)=40d0!GR
 vp(3,16)=-60d0!SP
 vp(4,16)=142d0!DT

 delta(1,16)=vp(1,16)*0.02d0
 delta(2,16)=vp(2,16)*0.1d0
 delta(3,16)=vp(3,16)*0.1d0
 delta(4,16)=vp(4,16)*0.1d0


 nd(16)=INT(1.0/1d-2)   !numero de medidas na camada 2
 WRITE(6,*) 'nd(t3-dolomita)=',nd(16)


!----------------------------------
 Lito(17)=3     !diabásio
  L(17)='diabase'
  vp(1,17)=2.8d0!RHOB
  vp(2,17)=30d0!GR
  vp(3,17)=5d0!SP
  vp(4,17)=50d0!DT

  delta(1,17)=vp(1,17)*0.02d0
  delta(2,17)=vp(2,17)*0.1d0
  delta(3,17)=vp(3,17)*0.1d0
  delta(4,17)=vp(4,17)*0.1d0

  nd(17)=INT(0.3/1d-2)   !numero de medidas na camada 3
WRITE(6,*) 'nd(t3-diabásio)=',nd(17)


!----------------------------------

Lito(18)=2   !dolomita
 L(18)='dolomite'
 vp(1,18)=2.5d0!RHOB
 vp(2,18)=40d0!GR
 vp(3,18)=-60d0!SP
 vp(4,18)=142d0!DT

 delta(1,18)=vp(1,18)*0.02d0
 delta(2,18)=vp(2,18)*0.1d0
 delta(3,18)=vp(3,18)*0.1d0
 delta(4,18)=vp(4,18)*0.1d0

 nd(18)=INT(0.2/1d-2)   !numero de medidas na camada 4
 WRITE(6,*) 'nd(t3-dolomita)=',nd(18)

!Adiciona ruído aleatório randômico. Este bloco deve vir após a inserção das propriedades físicas.

! ic=0

DO j=15,18  ! numero de camadas
 DO i=8,nd(j)
  ic=ic+1
  CALL n_rand(vp(1,j),delta(1,j),vc(1,j))
  CALL n_rand(vp(2,j),delta(2,j),vc(2,j))
  CALL n_rand(vp(3,j),delta(3,j),vc(3,j))
  CALL n_rand(vp(4,j),delta(4,j),vc(4,j))

  a2=vc(1,j)
  a3=vc(2,j)
  a4=vc(3,j)
  a5=vc(4,j)

  WRITE(1,FMT=14) L(j),Lito(j),ic,a2,a3,a4,a5

  ia1=ic
 END DO
END DO



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ POÇO T4 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!----------------------------
!Folhelho 2 é uma rocha com baixo teor de matéria orgânica GR entre 60-150
 Lito(19)=1 !folhelho 2
  L(19)='shale2'

  vp(1,19)=2.50d0!densidade
  vp(2,19)=110d0!raio-gama
  vp(3,19)=70d0!potencial espontaneo 
  vp(4,19)=60d0!sonico

  delta(1,19)=vp(1,19)*0.02d0
  delta(2,19)=vp(2,19)*0.1d0
  delta(3,19)=vp(3,19)*0.1d0
  delta(4,19)=vp(4,19)*0.1d0

  nd(19)=INT(0.5/1d-2)   !numero de medidas na camada 1 do poço t2
WRITE(6,*) 'nd(t4-folhelho2)=',nd(19)


!----------------------------------
Lito(20)=2   !dolomita
 L(10)='dolomite'
 vp(1,20)=2.5d0!RHOB
 vp(2,20)=40d0!GR
 vp(3,20)=-60d0!SP
 vp(4,20)=550d0!DT

 delta(1,20)=vp(1,20)*0.02d0
 delta(2,20)=vp(2,20)*0.1d0
 delta(3,20)=vp(3,20)*0.1d0
 delta(4,20)=vp(4,20)*0.1d0


 nd(20)=INT(0.4/1d-2)   !numero de medidas na camada 2
 WRITE(6,*) 'nd(t4-dolomita)=',nd(20)


!----------------------------------
 Lito(20)=3     !diabásio
  L(20)='diabase'
  vp(1,20)=2.8d0!RHOB
  vp(2,20)=30d0!GR
  vp(3,20)=5d0!SP
  vp(4,20)=50d0!DT

  delta(1,20)=vp(1,20)*0.02d0
  delta(2,20)=vp(2,20)*0.1d0
  delta(3,20)=vp(3,20)*0.1d0
  delta(4,20)=vp(4,20)*0.1d0

  nd(20)=INT(0.3/1d-2)   !numero de medidas na camada 3
WRITE(6,*) 'nd(t4-diabásio)=',nd(20)

!----------------------------------

Lito(21)=2   !dolomita
 L(21)='dolomite'
 vp(1,21)=2.5d0!RHOB
 vp(2,21)=40d0!GR
 vp(3,21)=-60d0!SP
 vp(4,21)=142d0!DT

 delta(1,21)=vp(1,21)*0.02d0
 delta(2,21)=vp(2,21)*0.1d0
 delta(3,21)=vp(3,21)*0.1d0
 delta(4,21)=vp(4,21)*0.1d0

 nd(21)=INT(0.4/1d-2)   !numero de medidas na camada 4
 WRITE(6,*) 'nd(t4-dolomita)=',nd(21)


!----------------------------------
!Folhelho 1 é uma rocha com alto teor de matéria orgânica GR entre 150-250
 Lito(22)=6 !folhelho 
  L(22)='shale1'

  vp(1,22)=2.580d0!densidade
  vp(2,22)=230d0!raio-gama
  vp(3,22)=70d0!potencial espontaneo 
  vp(4,22)=520d0!sonico

  delta(1,22)=vp(1,22)*0.02d0
  delta(2,22)=vp(2,22)*0.1d0
  delta(3,22)=vp(3,22)*0.1d0
  delta(4,22)=vp(4,22)*0.1d0

  nd(22)=INT(2.0/1d-2)   !numero de medidas na camada 1 do poço t2
WRITE(6,*) 'nd(t4-folhelho1)=',nd(22)



!-----------------------------------
!Halita 
 Lito(23)=7 !halita
  L(23)='halite'

  vp(1,23)=2.168d0!densidade
  vp(2,23)=11d0!raio-gama
  vp(3,23)=6d0!potencial espontaneo. Confirmar estes valores
  vp(4,23)=216d0!sonico. Confirmar estes valores. 

  delta(1,23)=vp(1,23)*0.02d0
  delta(2,23)=vp(2,23)*0.1d0
  delta(3,23)=vp(3,23)*0.1d0
  delta(4,23)=vp(4,23)*0.1d0

  nd(23)=INT(1.0/1d-2)   !numero de medidas na camada 1 do poço t2
WRITE(6,*) 'nd(t4-halita)=',nd(23)


!-------------------------------------

Lito(24)=5      !embasamento
 L(24)='crystalline'
 vp(1,24)=2.75d0!RHOB
 vp(2,24)=40d0!GR
 vp(3,24)=70d0!SP
 vp(4,24)=55d0!DT

 delta(1,24)=vp(1,24)*0.02d0
 delta(2,24)=vp(2,24)*0.1d0
 delta(3,24)=vp(3,24)*0.1d0
 delta(4,24)=vp(4,24)*0.1d0

 nd(24)=INT(0.2/1d-2)   !numero de medidas na camada 7
WRITE(6,*) 'nd(t4-embasameto)=',nd(24)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!Adiciona ruído aleatório randômico. Este bloco deve vir após a inserção das propriedades físicas.

! ic=0

DO j=19,24  ! numero de camadas
 DO i=8,nd(j)
  ic=ic+1
  CALL n_rand(vp(1,j),delta(1,j),vc(1,j))
  CALL n_rand(vp(2,j),delta(2,j),vc(2,j))
  CALL n_rand(vp(3,j),delta(3,j),vc(3,j))
  CALL n_rand(vp(4,j),delta(4,j),vc(4,j))

  a2=vc(1,j)
  a3=vc(2,j)
  a4=vc(3,j)
  a5=vc(4,j)

  WRITE(1,FMT=14) L(j),Lito(j),ic,a2,a3,a4,a5

  ia1=ic
 END DO
END DO



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ POÇO T5 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!----------------------------------

Lito(25)=9   !ARENITO
 L(25)='sandstone'
 vp(1,25)=2.315d0!RHOB
 vp(2,25)=20.0d0!GR
 vp(3,25)=120.0d0!SP
 vp(4,25)=170d0!DT

 delta(1,25)=vp(1,25)*0.02d0
 delta(2,25)=vp(2,25)*0.1d0
 delta(3,25)=vp(3,25)*0.1d0
 delta(4,25)=vp(4,25)*0.1d0

 nd(25)=INT(1.98/1d-2)   !numero de medidas na camada 4
 WRITE(6,*) 'nd(t5-arenito)=',nd(25)


!----------------------------------

Lito(26)=1   !FOLHELHO 2
 L(26)='shale2'

  vp(1,26)=2.50d0!densidade
  vp(2,26)=110d0!raio-gama
  vp(3,26)=70d0!potencial espontaneo 
  vp(4,26)=550d0!sonico

 delta(1,26)=vp(1,26)*0.02d0
 delta(2,26)=vp(2,26)*0.1d0
 delta(3,26)=vp(3,26)*0.1d0
 delta(4,26)=vp(4,26)*0.1d0

 nd(26)=INT(3.0/1d-2)   !numero de medidas na camada 4
 WRITE(6,*) 'nd(t5-folhelho 2)=',nd(26)


 !-------------------------------------

Lito(27)=4      !conglomerado
 L(27)='conglomerate'
 vp(1,27)=2.3d0!RHOB
 vp(2,27)=20d0!GR
 vp(3,27)=-40d0!SP
 vp(4,27)=110d0!DT

 delta(1,27)=vp(1,27)*0.02d0
 delta(2,27)=vp(2,27)*0.1d0
 delta(3,27)=vp(3,27)*0.1d0
 delta(4,27)=vp(4,27)*0.1d0

 nd(27)=INT(2.0/1d-2)   !numero de medidas na camada 5
WRITE(6,*) 'nd(t5-conglomerado)=',nd(27)


!Adiciona ruído aleatório randômico

 ic=0

DO j=25,27  ! numero de camadas
 DO i=1,nd(j)
  ic=ic+1
  CALL n_rand(vp(1,j),delta(1,j),vc(1,j))
  CALL n_rand(vp(2,j),delta(2,j),vc(2,j))
  CALL n_rand(vp(3,j),delta(3,j),vc(3,j))
  CALL n_rand(vp(4,j),delta(4,j),vc(4,j))

  a2=vc(1,j)
  a3=vc(2,j)
  a4=vc(3,j)
  a5=vc(4,j)

  WRITE(1,14) L(j),Lito(j),ic,a2,a3,a4,a5

  ia1=ic
 END DO
END DO


!------------------------------------







!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$ FORMATAÇÃO $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

11 FORMAT(4(ES12.4E3,2x))
12 FORMAT(I3,2x,3(f6.2,2x))
13 FORMAT(I2,3x,I10,2x,4(ES9.2E2,2x))
14 FORMAT(A12,2x,I3,2x,I10,2x,4(ES9.2E2,2x))
15 FORMAT(A9,5x,A6,6x,A4,2x,A4,7x,A4,7x,A3,8x,A3)
16 FORMAT(A11,8(ES9.2E3))


!Cálculo do Custo Computacional
CALL cpu_time(final)
  custocomputacional=final-inicial

PRINT*,'**************************** FIM *********************************'
  PRINT*, 'Custo Computacional=',custocomputacional, 'segundos'
 STOP

CONTAINS

SUBROUTINE media_desvio(x,n,media,desvio_padrao)
    IMPLICIT NONE
    INTEGER(KIND=DBL), INTENT(IN)::n
    INTEGER(KIND=DBL)::i
    REAL(KIND=SGL), DIMENSION(:), ALLOCATABLE, INTENT(INOUT):: x
    REAL(KIND=SGL)::soma, soma2, media
    REAL(KIND=SGL), INTENT(OUT)::desvio_padrao

                      ALLOCATE(x(n))

  soma = 0d0
 DO i=1,n
  soma = soma + x(i)
 END DO

 media=soma/dfloat(n)

  soma2 = 0d0

 DO i=1,n
  soma2 = soma2 + (x(i)-media)**2
 END DO

  soma2=soma2/(dfloat(n)-1d0)

 desvio_padrao=dsqrt(soma2)

 RETURN
END SUBROUTINE media_desvio


!#########################################
SUBROUTINE soma(e,x,w)
 integer:: e,i
	real*8 :: x(e),w

  w=0
 DO i=1,e
  w= w + x(i)
 END DO

END SUBROUTINE soma

!#########################################
SUBROUTINE n_rand(vp,dp,vc)
 real*8 vp,dp,vc,tq(100),vmax,vmin
 integer i

 DO i=1,38
  vmax=vp+0.5d0*dp
  vmin=vp-0.5d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
  END DO

 DO i=39,62
  vmax=vp+1.5d0*dp
  vmin=vp+0.5d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO

 DO i=63,86
  vmax=vp-0.5d0*dp
  vmin=vp-1.5d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 DO i=87,92
  vmax=vp+2.5d0*dp
  vmin=vp+1.5d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 DO i=93,98
  vmax=vp-1.5d0*dp
  vmin=vp-2.5d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 vmax=vp+3.5d0*dp
 vmin=vp+2.5d0*dp
 tq(99)=rand()*(vmax-vmin)+vmin

 vmax=vp-2.5d0*dp
 vmin=vp-3.5d0*dp
 tq(100)=rand()*(vmax-vmin)+vmin

 vc=tq(int(100.d0*rand()+1.d0))

 return
END SUBROUTINE n_rand

!#########################################

SUBROUTINE n_rand2(vp,dp,vc)
 real*8 vp,dp,vc,tq(100),vmax,vmin
 integer i

 DO i=1,22
  vmax=vp+0.28d0*dp
  vmin=vp-0.28d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 DO i=23,28
  vmax=vp+0.44d0*dp
  vmin=vp+0.28d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO

 DO i=29,34
  vmax=vp-0.28d0*dp
  vmin=vp-0.44d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO

 DO i=35,44
  vmax=vp+0.74d0*dp
  vmin=vp+0.44d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 DO i=45,54
  vmax=vp-0.44d0*dp
  vmin=vp-0.74d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 DO i=55,58
  vmax=vp+0.88d0*dp
  vmin=vp+0.74d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 DO i=59,62
  vmax=vp-0.74d0*dp
  vmin=vp-0.88d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO

 DO i=63,70
  vmax=vp+1.23d0*dp
  vmin=vp+0.88d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO

 DO i=71,78
  vmax=vp-0.88d0*dp
  vmin=vp-1.23d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 DO i=79,83
  vmax=vp+1.56d0*dp
  vmin=vp+1.23d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 DO i=84,88
  vmax=vp-1.23d0*dp
  vmin=vp-1.56d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 DO i=89,90
  vmax=vp+1.76d0*dp
  vmin=vp+1.56d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 DO i=91,92
  vmax=vp-1.56d0*dp
  vmin=vp-1.76d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO

 DO i=93,94
  vmax=vp+2.06d0*dp
  vmin=vp+1.76d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO  

 DO i=95,96
  vmax=vp-1.76d0*dp
  vmin=vp-2.06d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 DO i=97,98
  vmax=vp+4.51d0*dp
  vmin=vp+2.06d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 DO i=99,100
  vmax=vp-2.06d0*dp
  vmin=vp-4.51d0*dp
  tq(i)=rand()*(vmax-vmin)+vmin
 END DO 

 vc=tq(int(100.d0*rand()+1.d0))


 return
END SUBROUTINE n_rand2

END PROGRAM sintetico
