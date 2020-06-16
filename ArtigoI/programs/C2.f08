PROGRAM C2
     !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
     !Projeto de Doutorado                                                     !
     !Orientador: Cosme Ferreira da Ponte Neto                                 !
     !Aluno: Victor Ribeiro Carreira                                           !
     !Este programa tem o propósito de simular dados de well-logging           !
     !Gerador de dados sintéticos de uma Base de Dados de dados de poços       !
     !Propriedades modeladas em rocha: RHOB, SP, GR, DT                        !
     !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
    
        !##################### LISTA DE VARIÁVEIS ################!
        !Np - número de propriedades                              !
        !Nx - variação lateral                                    !
        !Nz - profundidade                                        !
        !Nr - número de rochas (litotipos)                        !
        !vp - Hipermatriz com valor da propriedade física         !
        !vr - Hipermatriz com valor de prop física com ruído      !  
        !delta - dispersão da valor medidos (desvio padrão)       !
        !i - contador universal de profundidade                   !
        !j - contador universão de propriedades físicas           !
        !cod - codigo litológico numérico                         !
        !#########################################################!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$ DECLARAÇÃO DAS VARIÁVEIS GLOBAIS $$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
 INTEGER, PARAMETER:: SGL = SELECTED_REAL_KIND(p=8, r=8)
 INTEGER, PARAMETER:: DBL = SELECTED_REAL_KIND(p=4, r=4)
 INTEGER(KIND=DBL):: Lito(1000), nd(1000), i, j, ic, ia, ia1, ia2, ia3, ia4
 CHARACTER(LEN=12):: L(1000)
 CHARACTER(LEN=80):: cab
 REAL(KIND=SGL):: a, a5, a4, a3, a2, inicial, final, esp1, T_amostra, cota0, cota1
 REAL(KIND=SGL):: cota2, cota3, cota4, cota5, cota6, cota7, cota8, esp2, esp3, esp4
 REAL(KIND=SGL):: esp5, esp6, esp7, esp8
 REAL(KIND=SGL):: custocomputacional, b1, b2, b3, b4, alfa1, alfa2, alfa3, alfa4
 REAL(KIND=SGL), ALLOCATABLE, DIMENSION(:):: x1, x2
 REAL(KIND=SGL), ALLOCATABLE, DIMENSION(:,:):: ds, vp, delta, vc



ALLOCATE(ds(10000,5), x1(10000), x2(10000), vp(4,10000), delta(4,10000), vc(4,10000))


    CALL cpu_time(inicial)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$ ARQUIVOS DE SAÍDA $$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

OPEN(1,file='../inputs/C2.txt')

!! Cabeçalho

WRITE(1,15) 'Lithology','Code','Depth(m)','RHOB','GR','SP','DT'
WRITE(1,*) '   '   ! Cria um separador (linha branca) em baixo do cabeçalho

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ POÇO C2 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

PRINT*,'************************* POÇO C2 **************************'


!-----------------------------------------
!Folhelho 2 é uma rocha com baixo teor de matéria orgânica GR entre 60-150
 Lito(1)=1 !folhelho 2
  L(1)='Shale2'

  vp(1,1)=2.50d0!densidade
  vp(2,1)=110d0!raio-gama
  vp(3,1)=70d0!potencial espontaneo 
  vp(4,1)=550d0!sonico

  delta(1,1)=vp(1,1)*0.02d0
  delta(2,1)=vp(2,1)*0.1d0
  delta(3,1)=vp(3,1)*0.1d0
  delta(4,1)=vp(4,1)*0.1d0



   cota0=0d0  !topo da camada
   cota1=1.0d0  !base da camada
   esp1=cota1-cota0 !espessura da camada
   T_amostra=1d-2 !Taxa de amostragem
  nd(1)=INT(esp1/T_amostra)   !numero de medidas na camada 1
WRITE(6,*) 'nd(c2-folhelho2)=',nd(1)

!----------------------------------------
Lito(2)=2   !dolomita
 L(2)='Dolomite'
 vp(1,2)=2.5d0!RHOB
 vp(2,2)=40d0!GR
 vp(3,2)=-60d0!SP
 vp(4,2)=142d0!DT

 delta(1,2)=vp(1,2)*0.02d0
 delta(2,2)=vp(2,2)*0.1d0
 delta(3,2)=vp(3,2)*0.1d0
 delta(4,2)=vp(4,2)*0.1d0

  cota2=1.6d0 !base da camada
  esp2=cota2-cota1 !Espessura da camada
  T_amostra=1d-2 !Taxa de amostragem 
 nd(2)=INT(esp2/T_amostra)   !numero de medidas na camada 2
 WRITE(6,*) 'nd(c2-dolomita)=',nd(2)

!--------------------------------------
 Lito(3)=3     !diabásio
  L(3)='Diabase'
  vp(1,3)=2.8d0!RHOB
  vp(2,3)=30d0!GR
  vp(3,3)=5d0!SP
  vp(4,3)=50d0!DT

  delta(1,3)=vp(1,3)*0.02d0
  delta(2,3)=vp(2,3)*0.1d0
  delta(3,3)=vp(3,3)*0.1d0
  delta(4,3)=vp(4,3)*0.1d0

cota3=1.9d0 !base do diabásio
esp3=cota3-cota2 !Espessura da diabásio
T_amostra=1d-2 !Taxa de amostragem do diabásio
nd(3)=INT(esp3/T_amostra)   !numero de medidas na camada 3 (diabásio)
WRITE(6,*) 'nd(c2-diabásio)=',nd(3)


!--------------------------------------

Lito(4)=2   !dolomita
 L(4)='Dolomite'
 vp(1,4)=2.5d0!RHOB
 vp(2,4)=40d0!GR
 vp(3,4)=-60d0!SP
 vp(4,4)=142d0!DT

 delta(1,4)=vp(1,4)*0.02d0
 delta(2,4)=vp(2,4)*0.1d0
 delta(3,4)=vp(3,4)*0.1d0
 delta(4,4)=vp(4,4)*0.1d0

 cota4=2.6 !base 
 esp4=cota4-cota3 !Espessura da dolomita
 T_amostra=1d-2 !Taxa de amostragem 
 nd(4)=INT(esp4/T_amostra)   !numero de medidas na camada 4
 WRITE(6,*) 'nd(c2-dolomita)=',nd(4)


!-------------------------------------
Lito(5)=6      !Folhelho 1
 L(5)='Shale1'
 vp(1,5)=2.58d0!RHOB
 vp(2,5)=230d0!GR
 vp(3,5)=70d0!SP
 vp(4,5)=520d0!DT

 delta(1,5)=vp(1,5)*0.02d0
 delta(2,5)=vp(2,5)*0.1d0
 delta(3,5)=vp(3,5)*0.1d0
 delta(4,5)=vp(4,5)*0.1d0

 cota5=3.0d0 !base 
 esp5=cota5-cota4 !Espessura da dolomita
 T_amostra=1d-2 !Taxa de amostragem 
 nd(5)=INT(esp5/T_amostra)   !numero de medidas na camada 5
WRITE(6,*) 'nd(c2- folhelho1)=',nd(5)


!-------------------------------------
Lito(6)=7      !Halita
 L(6)='Halite'
 vp(1,6)=2.16d0!RHOB
 vp(2,6)=11d0!GR
 vp(3,6)=6d0!SP
 vp(4,6)=216d0!DT

 delta(1,6)=vp(1,6)*0.02d0
 delta(2,6)=vp(2,6)*0.1d0
 delta(3,6)=vp(3,6)*0.1d0
 delta(4,6)=vp(4,6)*0.1d0

 cota6=3.5d0 !base 
 esp6=cota6-cota5 !Espessura da dolomita
 T_amostra=1d-2 !Taxa de amostragem 
 nd(6)=INT(esp6/T_amostra)   !numero de medidas na camada 6
WRITE(6,*) 'nd(c2- halita)=',nd(6)

!-------------------------------------
Lito(7)=10      !Basalto
 L(7)='Basalt'
 vp(1,7)=2.87d0!RHOB
 vp(2,7)=15d0!GR
 vp(3,7)=50d0!SP
 vp(4,7)=65d0!DT

 delta(1,7)=vp(1,7)*0.02d0
 delta(2,7)=vp(2,7)*0.1d0
 delta(3,7)=vp(3,7)*0.1d0
 delta(4,7)=vp(4,7)*0.1d0

 cota7=3.9d0 !base 
 esp7=cota7-cota6 !Espessura da dolomita
 T_amostra=1d-2 !Taxa de amostragem 
 nd(7)=INT(esp7/T_amostra)   !numero de medidas na camada 7
WRITE(6,*) 'nd(c2- Basalto)=',nd(7)


!-------------------------------------
Lito(8)=4      !conglomerado
 L(8)='Conglomerate'
 vp(1,8)=2.30d0!RHOB
 vp(2,8)=20d0!GR
 vp(3,8)=-40d0!SP
 vp(4,8)=110d0!DT

 delta(1,8)=vp(1,8)*0.02d0
 delta(2,8)=vp(2,8)*0.1d0
 delta(3,8)=vp(3,8)*0.1d0
 delta(4,8)=vp(4,8)*0.1d0

 cota8=5.61d0 !base 
 esp8=cota8-cota7 !Espessura da dolomita
 T_amostra=1d-2 !Taxa de amostragem 
 nd(8)=INT(esp8/T_amostra)   !numero de medidas na camada 8
WRITE(6,*) 'nd(c2- conglomerado)=',nd(8)

!---------------------------------------

!FINAL DOS LITOTIPOS


!Adiciona ruído aleatório randômico. Este bloco deve vir após a inserção das propriedades físicas.

ic=0

DO j=1,8  ! numero de camadas
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
PRINT*,'************************** FIM *****************************'
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



END PROGRAM C2
