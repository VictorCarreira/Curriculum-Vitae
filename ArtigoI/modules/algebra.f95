MODULE algebra
IMPLICIT NONE
  PUBLIC
  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(8,10)
  INTEGER(KIND=SP):: n
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: A, bt
  REAL(KIND=DP):: c,nn, tau, etaO
  

CONTAINS

!-------------------------------------------------------------------------------------------

!REAL(KIND=PD) FUNCTION randomic(idum)
!“Minimal” random number generator of Park and Miller combined with a Marsaglia shift
!sequence. Returns a uniform random deviate between 0.0 and 1.0 (exclusive of the endpoint
!values). This fully portable, scalar generator has the “traditional” (not Fortran 90) calling
!sequence with a random deviate as the returned function value: call with idum a negative
!integer to initialize; thereafter, do not alter idum except to reinitialize. The period of this
!generator is about 3.1×10^18 .
!IMPLICIT NONE
! INTEGER, PARAMETER :: K4B=selected_int_kind(9)
! INTEGER(K4B), INTENT(INOUT) :: idum
! REAL :: ran
! INTEGER(K4B), PARAMETER :: IA=16807,IM=2147483647,IQ=127773,IR=2836
! REAL, SAVE :: am
! INTEGER(K4B), SAVE :: ix=-1,iy=-1,k

! IF(idum <= 0 .or. iy < 0) THEN       !Initialize.
!  am=nearest(1.0,-1.0)/IM
!  iy=ior(ieor(888889999,abs(idum)),1)
!  ix=ieor(777755555,abs(idum))
!  idum=abs(idum)+1                      !Set idum positive.
! END IF

! ix=ieor(ix,ishft(ix,13))              !Marsaglia shift sequence with period 2 32 − 1.
! ix=ieor(ix,ishft(ix,-17))
! ix=ieor(ix,ishft(ix,5))
! k=iy/IQ                               !Park-Miller sequence by Schrage’s method,
! iy=IA*(iy-k*IQ)-IR*k                  !period 2 31 − 2. 
! IF(iy<0) iy=iy+IM
! ran=am*ior(iand(IM,ieor(ix,iy)),1)   !Combine the two generators with masking to ensure nonzero value.

!END FUNCTION randomic

!----------------------------------------------------------------------------------

 SUBROUTINE transp(n,m,A)
  !Nesta subrotina é preciso fornecer o número de linhas totais n.
  !E o número de colunas totais m.

  IMPLICIT NONE
  INTEGER(KIND=SP), INTENT(IN):: n,m  !n, linhas e m, colunas
  INTEGER(KIND=DP):: i, j
  REAL(KIND=DP), DIMENSION(n,m), INTENT(INOUT):: A !A é a matriz de entrada. Ela vai ser reescrita no processo


    DO i=1,n
      DO j=1,m
        A(j,i) = A(i,j)
      END DO
    END DO

END SUBROUTINE transp

!------------------------------------------------------------------------------------

!Desvio padrão 
SUBROUTINE media_desvio(x,n,media,desvio_padrao)
  IMPLICIT NONE
  INTEGER(KIND=DP), INTENT(IN)::n
  REAL(KIND=DP), INTENT(INOUT), ALLOCATABLE, DIMENSION(:):: x
  REAL(KIND=DP), INTENT(OUT):: media, desvio_padrao
  INTEGER(KIND=DP):: i,j
  REAL(KIND=DP):: soma, soma2

!n=30! voce escolhe

 ALLOCATE(x(n))
 !implicit real*8(a-h,o-z)
 !real*8 x(n),media,desvio_padrao
 soma = 0d0
 soma2 = 0d0

 DO j=1,n
  soma = soma + x(j)
  soma2 = soma2 + x(j)**2
 END DO 

 media = soma/n
 desvio_padrao=dsqrt((soma2-soma**2/n)/(n-1))

END SUBROUTINE media_desvio

!------------------------------------------------------------------------------------


!SUBROUTINE invert(A,i)      
! IMPLICIT NONE
! INTEGER:: i,im,j,k,l
!REAL(KIND=DP),DIMENSION(:,:):: A
!REAL(KIND=DP),DIMENSION(:):: B
 
 !real*8 A(i,i),B(i)
 !integer*4 i,im,j,k,l
!   IM=I-1
!   DO 5 K=1,I
!    DO 2 J=1,IM
!    2 B(J)=A(1,J+1)/A(1,1)
!    B(I)=1.d0/A(1,1)
!     DO 4 L=1,IM
!      DO 3 J=1,IM
!      3 A(L,J)=A(L+1,J+1)-A(L+1,1)*B(J)
!      4 A(L,I)=-A(L+1,1)*B(I)
!      DO 5 J=1,I
!       5 A(I,J)=B(J)
!RETURN

!END SUBROUTINE invert

!----------------------------------------------------------------

!SUBROUTINE transposta(n,A)
!!! TRANSPOSIÇÃO MATRICIAL
!  IMPLICIT NONE
!  INTEGER(KIND=SP), INTENT(IN):: n                                             !n, dimensão da matriz
!  INTEGER(KIND=SP):: i, j
!  REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT):: A                  !A é a matriz de entrada. Ela vai ser reescrita no processo
!  REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE:: aux

!  ALLOCATE(A(n,n), aux(n,n))

!    DO i=1,n
!      DO j=1,n
!        IF(i<j) THEN
!        aux = A(i,j)
!        A(i,j) = A(j,i)
!        A(j,i) = aux
!        END IF
!      ENDDO
!    ENDDO

!END SUBROUTINE transposta

!-----------------------------------------------------------------

!SUBROUTINE matriz_inversa(n, A, inversa)
!IMPLICIT NONE
!INTEGER(KIND=SP), INTENT(IN):: n
!REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT):: A
!REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT):: inversa
!LOGICAL:: invertivel=.true.
!INTEGER(KIND=SP):: i, j, k, l
!REAL(KIND=DP):: m
!REAL, DIMENSION(:,:), ALLOCATABLE:: matriz_aumentada

!ALLOCATE(A(n,n), inversa(n,n), matriz_aumentada(n,2*n))

!!! Matriz aumentada com uma matriz identidade
!DO i=1,n
!  DO j=1,2*n
!    IF(j<=n) THEN
!        matriz_aumentada(i,j) = A(i,j)
!      ELSE IF((i+n)==j) THEN
!        matriz_aumentada(i,j)=1
!      ELSE
!        matriz_aumentada(i,j)=0
!    ENDIF
!  ENDDO
!ENDDO

!!! Reduzir a matriz aumentada a uma matriz triangular superior pela eliminação gaussiana

!DO k=1,n−1
! Verifica se algum elemento da diagonal é zero
!      IF(ABS(matriz_aumentada(k,k))<=1.0E−6) THEN
!        invertivel=.false.
!        DO i=k+1,n
! Verifica se os elementos são maiores que zero
!          IF(ABS(matriz_aumentada(i,k))>1.0E−6)  THEN
!            DO j=1,2*n
!              matriz_aumentada(k,j)=matriz_aumentada(k,j)+matriz_aumentada(i,j)
!            END DO
!            invertivel=.true.
!            EXIT
!          END IF
! Se algum elemento da diagonal for zero, não podemos calcular a inversa
!        IF(invertivel==.false.) THEN
!          WRITE(*,'(/,x,A,/)')'** A matriz nao e inverstivel! ∗∗'
!          inversa = 0
!          STOP
!        END IF
!      END DO
!    END IF
! Eliminação gaussiana
!  DO j=k+1,n
!    m = matriz_aumentada(j,k) / matriz_aumentada(k,k)
!    DO i=k,2*n
!      matriz_aumentada(j,i) = matriz_aumentada(j,i) − m * matriz_aumentada(k,i)
!    END DO
!  END DO
!END DO

! Teste para invertibilidade

!DO i=1,n
! Elementos da diagonal não podem ser zero
!  IF(ABS(matriz_aumentada(i,i))<=1.0E−6) THEN
!    WRITE(*,'(/,x,A,/)')' ** A matriz nao e inverstivel! ∗∗'
!    inversa = 0
!    STOP
!  END IF
!END DO

! Elementos da diagonal iguais a 1
!DO i=1,n
!  m = matriz_aumentada(i,i)
!  DO j=i,2*n
!    matriz_aumentada(i,j) = matriz_aumentada(i,j) / m
!  END DO
!END DO

! Reduzir o lado esquerdo da matriz aumentada a matriz identidade
!DO k = n−1, 1, −1
!  DO i = 1, k
!    m = matriz_aumentada(i,k+1)
!    DO j = k, 2*n
!      matriz_aumentada(i,j) = matriz_aumentada(i,j) − matriz_aumentada(k+1,j) * m
!    END DO
!  END DO
!END DO
  ! Armazene o resultado
!  DO i = 1, n
!    DO j = 1, n
!      inversa(i,j) = matriz_aumentada(i,j+n)
!    END DO
!  END DO

!END SUBROUTINE matriz_inversa


END MODULE algebra
