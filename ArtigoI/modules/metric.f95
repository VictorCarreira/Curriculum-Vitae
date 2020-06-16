MODULE metric
IMPLICIT NONE
  PUBLIC
  INTEGER, PARAMETER::PS = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::PD = SELECTED_REAL_KIND(8,10)
  INTEGER(KIND=PS):: n
  REAL(KIND=PD), ALLOCATABLE, DIMENSION(:,:):: A, bt
  REAL(KIND=PD):: c,nn, tau, etaO
  
USE algebra, ONLY: invert

CONTAINS

!#############################################################################################

SUBROUTINE euclideana(a1,a2,a3,a4,b1,b2,b3,b4,eucli)
  IMPLICIT NONE
  REAL(KIND=PD), INTENT(IN):: a1,a2,a3,a4,b1,b2,b3,b4
  REAL(KIND=PD), INTENT(OUT):: eucli

    d=DSQRT((a1-b1)**2+(a2-b2)**2+(a3-b3)**2+(a4-b4)**2)

END SUBROUTINE euclideana

!------------------------------------------------------------------------------------

SUBROUTINE mahalanobis(g11,np1,g22,np2,ndim,dist)      
	
!! subrotina que calcula a distância de mahalanobis entre
!! dois agrupamentos de elementos com dimensão ndim 	

 IMPLICIT NONE
 REAL(KIND=PD),DIMENSION(:,:), ALLOCATABLE:: g11, g22
 REAL(KIND=PD):: dist
 INTEGER:: np1, np2, ndim
 REAL(KIND=PD),DIMENSION(:,:), ALLOCATABLE:: g1, g2, g1T, g2T, cov1, &
 cov2, covag, md, mdT, alfa, d2, tr1, nt1, tr2, nt2
 REAL(KIND=PD),DIMENSION(:), ALLOCATABLE::	soma, xm1,xm2, m2
 INTEGER(KIND=PD):: i, j, k  

 ALLOCATE(g1(np1,ndim),g2(np2,ndim),&
 g1T(ndim,np1),g2T(ndim,np2),cov1(ndim,ndim),cov2(ndim,ndim),&
 covag(ndim,ndim),soma(ndim),xm1(ndim),m2(ndim),g22(np2,ndim),&
 md(ndim,1),mdT(1,ndim),alfa(1,ndim),d2(1,1),g11(np1,ndim)) 

  
!implicit real*8(a-h,o-z)

!	real*8,intent(in)::g1(np1,ndim),g2(np2,ndim)
!	real*8,intent(out)::dist
!	integer,intent(in)::np1,np2,ndim 


!REAL*8 g1(np1,ndim),g2(np2,ndim),g1T(ndim,np1),g2T(ndim,np2),&
!cov1(ndim,ndim),cov2(ndim,ndim),covag(ndim,ndim),soma(ndim),xm1(ndim),&
!m2(ndim),g22(np2,ndim),md(ndim,1),mdT(1,ndim),alfa(1,ndim),d2(1,1),g11(np1,ndim)


	g1=g11
	g2=g22

!	grupo 1	

DO j=1,ndim
  soma(j)=0d0
   DO i=1,np1
    soma(j)=soma(j)+g1(i,j)
   END DO 
END DO

DO i=1,ndim
  xm1(i)=soma(i)/dfloat(np1)
END DO	

!	grupo 2	

DO j=1,ndim
  soma(j)=0d0
  DO i=1,np2
   soma(j)=soma(j)+g2(i,j)
  END DO
END DO

DO i=1,ndim
  xm2(i)=soma(i)/dfloat(np2)
  !xm2(i)=soma(i)/REAL(np2)
END DO	

!vetor das diferenças - será escrito sobre a matrizes g1 e g2

DO j=1,ndim
 DO i=1,np1
  g1(i,j)=g1(i,j)-xm1(j)
  END DO 
END DO 

DO j=1,ndim
 DO i=1,np2
  g2(i,j)=g2(i,j)-xm2(j)
 END DO 
END DO 	

!     --------GRUPO 1 ---------------------
!	criando a matriz transposta g1T
!	-------------- -------------------
DO i=1,np1    !107 ! número de equações 
 DO j=1,ndim   !2
  g1T(j,i)=g1(i,j)
 END DO 
END DO 
!----------------------------------------------------
!	 - multiplicação de matrizes
!	   multiplicação de g1T por g1 

DO k=1,ndim
 DO j=1,ndim
  cov1(j,k)=0.d0
  DO i=1,np1	
   cov1(j,k)=cov1(j,k)+g1T(j,i)*g1(i,k)
  END DO
 END DO
END DO 

DO i=1,ndim
 DO j=1,ndim
  cov1(i,j)=cov1(i,j)/dfloat(np1)
 END DO
END DO

!	write(6,*) '======covari�ncia 1 ======'
!	write(6,*) cov1(1,1),cov1(1,2)
!	write(6,*) cov1(2,1),cov1(2,2)

!     --------GRUPO 2 ---------------------
!	criando a matriz transposta g2T

DO i=1,np2
 DO j=1,ndim
  g2T(j,i)=g2(i,j)
 END DO 
END DO 

!---------------------------------------------------
!	 - multiplicação de matrizes
!	   multiplicação de g2T por g2 

DO k=1,ndim
 DO j=1,ndim
  cov2(j,k)=0.d0
   DO i=1,np2
    cov2(j,k)=cov2(j,k)+g2T(j,i)*g2(i,k)
   END DO
  END DO 
END DO 

DO i=1,ndim
  DO j=1,ndim
   cov2(i,j)=cov2(i,j)/dfloat(np2)
  END DO 
END DO 

!	write(6,*) '======covariância 2 ======'
!	write(6,*) cov2(1,1),cov2(1,2)
!	write(6,*) cov2(2,1),cov2(2,2)


!	-------- covariância agrupada------

DO i=1,ndim
  DO j=1,ndim
   covag(i,j)=dfloat(np1)*cov1(i,j)/(dfloat(np1+np2))+dfloat(np2)*cov2(i,j)/(dfloat(np1+np2))
  END DO
END DO 

!	write(6,*) '======covariância agrupada ======'
!	write(6,*) covag(1,1),covag(1,2)
!	write(6,*) covag(2,1),covag(2,2)	

!	inversão da matriz covag - usando subrotina

 CALL invert(covag,ndim)

!	write(6,*) '====== inv covariância agrupada ======'
!	write(6,*) covag(1,1),covag(1,2)
!	write(6,*) covag(2,1),covag(2,2)

!	diferenicas médias

 DO i=1,ndim
  md(i,1)=xm1(i)-xm2(i)
 END DO 

!	write(6,*) '====== diferencias medias ======'
!	write(6,*) md(1,1)
!	write(6,*) md(2,1)

!	criando a matriz transposta mdT
!	---------------------------
 DO i=1,ndim
  DO j=1,1
   mdT(j,i)=md(i,j)
  END DO 
END DO 

!----------------------------------------------------
!	multiplicaçãoo de mdT por cov^-1 
!	 - multiplicação de matrizes

 DO k=1,ndim	
  DO j=1,1	
   alfa(j,k)=0.d0
    DO i=1,ndim
	 alfa(j,k)=alfa(j,k)+mdT(j,i)*covag(i,k)
    END DO 
  END DO 
END DO 

!----------------------------------------------------
!	multiplica��o de alfa por md 
!	 - multiplica��o de matrizes

DO k=1,1
 DO j=1,1
  d2(j,k)=0.d0
   DO i=1,ndim  !2	!
    d2(j,k)=d2(j,k)+alfa(j,i)*md(i,k)
   END DO 
  END DO 
END DO 

	dist=dsqrt(d2(1,1))

      return
END SUBROUTINE mahalanobis


!---------------------------------------------------------------

END MODULE metric
