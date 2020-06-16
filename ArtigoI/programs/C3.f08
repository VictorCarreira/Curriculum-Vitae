PROGRAM C3
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
 INTEGER(KIND=DBL):: Lito(90), nd(90), i, j, ic, ia, ia1, ia2, ia3, ia4
 CHARACTER(LEN=12):: L(90)
 CHARACTER(LEN=80):: cab
 REAL(KIND=SGL):: a, a5, a4, a3, a2, inicial, final, esp1, T_amostra, cota0, cota1
 REAL(KIND=SGL):: cota2, cota3, cota4, esp2, esp3, esp4
 REAL(KIND=SGL):: custocomputacional, b1, b2, b3, b4, alfa1, alfa2, alfa3, alfa4
 REAL(KIND=SGL), ALLOCATABLE, DIMENSION(:):: x1, x2
 REAL(KIND=SGL), ALLOCATABLE, DIMENSION(:,:):: ds, vp, delta, vc


ALLOCATE(ds(10000,5), x1(10000), x2(10000), vp(4,10000), delta(4,10000), vc(4,10000))


    CALL cpu_time(inicial)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$ ARQUIVOS DE SAÍDA $$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

OPEN(1,file='../inputs/Sintetico/C3.txt')

!! Cabeçalho

WRITE(1,15) 'Litologia','codigo','prof','RHOB','GR','SP','DT'
WRITE(1,*) '   '   ! Cria um separador (linha branca) em baixo do cabeçalho

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ POÇO C3 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

PRINT*,'************************* POÇO C3 **************************'

!-----------------------------------------

 Lito(1)=9 !Arenito (cod.)
  L(1)='Sandstone'

  vp(1,1)=2.315d0!densidade
  vp(2,1)=20.0d0!raio-gama
  vp(3,1)=120d0!potencial espontaneo 
  vp(4,1)=170d0!sonico

  delta(1,1)=vp(1,1)*0.02d0
  delta(2,1)=vp(2,1)*0.1d0
  delta(3,1)=vp(3,1)*0.1d0
  delta(4,1)=vp(4,1)*0.1d0



   cota0=0d0  !topo da camada
   cota1=1.2d0  !base da camada
   esp1=cota1-cota0 !espessura da camada
   T_amostra=1d-2 !Taxa de amostragem
  nd(1)=INT(esp1/T_amostra)   !numero de medidas na camada 1
WRITE(6,*) 'nd(C3-arenito)=',nd(1)

!----------------------------------------
Lito(2)=1   !Folhelho 2
 L(2)='Shale2'
 vp(1,2)=2.5d0!RHOB
 vp(2,2)=110d0!GR
 vp(3,2)=70d0!SP
 vp(4,2)=550d0!DT

 delta(1,2)=vp(1,2)*0.02d0
 delta(2,2)=vp(2,2)*0.01d0
 delta(3,2)=vp(3,2)*0.01d0
 delta(4,2)=vp(4,2)*0.01d0

  cota2=1.5d0 !base da camada
  esp2=cota2-cota1 !Espessura da camada
  T_amostra=1d-2 !Taxa de amostragem 
 nd(2)=INT(esp2/T_amostra)   !numero de medidas na camada 2
 WRITE(6,*) 'nd(C2-Folhelho2)=',nd(2)

!Adiciona ruído aleatório randômico nas camadas no arenito e no folhelho 2

 ic=0

DO j=1,2  ! numero de camadas
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



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ FALHA $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!Propriedades do cristalino da mistura
 vp(1,4)=2.8d0!RHOB
 vp(2,4)=40.0d0!GR
 vp(3,4)=70d0!SP
 vp(4,4)=55d0!DT

cota3=3.5d0 !base da falha
esp3=cota3-cota2 !Espessura da falha
T_amostra=1d-2 !Taxa de amostragem na falha
nd(3)=INT(esp3/T_amostra)   !numero de medidas na camada 3 (falha)
 

ia3=0
DO i=1,nd(3)
 ia3=ia3+1
END DO

ia2=ia1+ia3

!!!!!!!!!!!
alfa1=(vp(1,2)-vp(1,4))/dfloat(ia1-ia2)        !-(2.8d0-b1)
alfa2=(vp(2,2)-vp(2,4))/dfloat(ia1-ia2)
alfa3=(vp(3,2)-vp(3,4))/dfloat(ia1-ia2)
alfa4=(vp(4,2)-vp(4,4))/dfloat(ia1-ia2)

b1=vp(1,2)-alfa1*dfloat(ia1)    !2.3d0
b2=vp(2,2)-alfa2*dfloat(ia1)
b3=vp(3,2)-alfa3*dfloat(ia1)
b4=vp(4,2)-alfa4*dfloat(ia1)

WRITE(6,*) 'nd(C3-falha normal)=',nd(3)



!!!! Mistura 1
Lito(3)=151    ! camada mista conglomerado+embasamento
L(3)='Fault1'

DO i=1,nd(3)/4
  ic=ic+1

  vp(1,3)=alfa1*dfloat(ic)+b1
  vp(2,3)=alfa2*dfloat(ic)+b2
  vp(3,3)=alfa3*dfloat(ic)+b3
  vp(4,3)=alfa4*dfloat(ic)+b4

  delta(1,3)=vp(1,3)*0.02d0
  delta(2,3)=vp(2,3)*0.1d0
  delta(3,3)=vp(3,3)*0.1d0
  delta(4,3)=vp(4,3)*0.1d0

  CALL n_rand(vp(1,3),delta(1,3),vc(1,3))
  CALL n_rand(vp(2,3),delta(2,3),vc(2,3))
  CALL n_rand(vp(3,3),delta(3,3),vc(3,3))
  CALL n_rand(vp(4,3),delta(4,3),vc(4,3))

  a2=vc(1,3)
  a3=vc(2,3)
  a4=vc(3,3)
  a5=vc(4,3)

 WRITE(1,14) L(3),Lito(3),ic,a2,a3,a4,a5
END DO

!!!!!!!!! Mistura  2
Lito(3)=152    ! camada mista conglomerado+embasamento
 L(3)='Fault2'

DO i=1,nd(3)/4
 ic=ic+1

  vp(1,3)=alfa1*dfloat(ic)+b1
  vp(2,3)=alfa2*dfloat(ic)+b2
  vp(3,3)=alfa3*dfloat(ic)+b3
  vp(4,3)=alfa4*dfloat(ic)+b4

  delta(1,3)=vp(1,3)*0.02d0
  delta(2,3)=vp(2,3)*0.1d0
  delta(3,3)=vp(3,3)*0.1d0
  delta(4,3)=vp(4,3)*0.1d0

  CALL n_rand(vp(1,3),delta(1,3),vc(1,3))
  CALL n_rand(vp(2,3),delta(2,3),vc(2,3))
  CALL n_rand(vp(3,3),delta(3,3),vc(3,3))
  CALL n_rand(vp(4,3),delta(4,3),vc(4,3))

  a2=vc(1,3)
  a3=vc(2,3)
  a4=vc(3,3)
  a5=vc(4,3)

  WRITE(1,14) L(3),Lito(3),ic,a2,a3,a4,a5
END DO

!!!!!!!!!!!!!! mistura  3
Lito(3)=153    ! camada mista conglomerado+embasamento
 L(3)='Fault3'

DO i=1,nd(3)/4
 ic=ic+1

  vp(1,3)=alfa1*dfloat(ic)+b1
  vp(2,3)=alfa2*dfloat(ic)+b2
  vp(3,3)=alfa3*dfloat(ic)+b3
  vp(4,3)=alfa4*dfloat(ic)+b4

  delta(1,3)=vp(1,3)*0.02d0
  delta(2,3)=vp(2,3)*0.1d0
  delta(3,3)=vp(3,3)*0.1d0
  delta(4,3)=vp(4,3)*0.1d0

  CALL n_rand(vp(1,3),delta(1,3),vc(1,3))
  CALL n_rand(vp(2,3),delta(2,3),vc(2,3))
  CALL n_rand(vp(3,3),delta(3,3),vc(3,3))
  CALL n_rand(vp(4,3),delta(4,3),vc(4,3))

  a2=vc(1,3)
  a3=vc(2,3)
  a4=vc(3,3)
  a5=vc(4,3)

  WRITE(1,14) L(3),Lito(3),ic,a2,a3,a4,a5
END DO

!!!!!!!! mistura  4
Lito(3)=154    ! camada mista conglomerado+embasamento
 L(3)='Fault4'


DO i=1,nd(3)/4
 ic=ic+1

 vp(1,3)=alfa1*dfloat(ic)+b1
 vp(2,3)=alfa2*dfloat(ic)+b2
 vp(3,3)=alfa3*dfloat(ic)+b3
 vp(4,3)=alfa4*dfloat(ic)+b4

  delta(1,3)=vp(1,3)*0.02d0
  delta(2,3)=vp(2,3)*0.1d0
  delta(3,3)=vp(3,3)*0.1d0
  delta(4,3)=vp(4,3)*0.1d0

  CALL n_rand(vp(1,3),delta(1,3),vc(1,3))
  CALL n_rand(vp(2,3),delta(2,3),vc(2,3))
  CALL n_rand(vp(3,3),delta(3,3),vc(3,3))
  CALL n_rand(vp(4,3),delta(4,3),vc(4,3))

  a2=vc(1,3)
  a3=vc(2,3)
  a4=vc(3,3)
  a5=vc(4,3)

  WRITE(1,14) L(3),Lito(3),ic,a2,a3,a4,a5
END DO

!!!!  final das misturas



!-------------------------------------
Lito(4)=5      !Cristalino
 L(4)='Crystalline'

 vp(1,4)=2.8d0!RHOB
 vp(2,4)=40d0!GR
 vp(3,4)=70d0!SP
 vp(4,4)=55d0!DT

 delta(1,4)=vp(1,4)*0.02d0
 delta(2,4)=vp(2,4)*0.1d0
 delta(3,4)=vp(3,4)*0.1d0
 delta(4,4)=vp(4,4)*0.1d0


 cota4=4.5d0 !base final do poço
 esp4=cota4-cota3 !Espessura do conglomerado 
 T_amostra=1d-2 !Taxa de amostragem do conglomerado
 nd(4)=INT(esp4/T_amostra)   !numero de medidas na camada 4
WRITE(6,*) 'nd(C3-embasamento)=',nd(4)


!randomiza o conglomerado

 DO i=1,nd(4)
  ic=ic+1

  CALL n_rand(vp(1,4),delta(1,4),vc(1,4))
  CALL n_rand(vp(2,4),delta(2,4),vc(2,4))
  CALL n_rand(vp(3,4),delta(3,4),vc(3,4))
  CALL n_rand(vp(4,4),delta(4,4),vc(4,4))

  ia1=ic
  a2=vc(1,4)
  a3=vc(2,4)
  a4=vc(3,4)
  a5=vc(4,4)


 WRITE(1,14) L(4),Lito(4),ia1,a2,a3,a4,a5
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



END PROGRAM C3


