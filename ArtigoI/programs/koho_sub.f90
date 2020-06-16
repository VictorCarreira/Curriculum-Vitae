! C     PROGRAMA - rede neural para identificação de litologia
! C     VARIAVEIS UTILIZADAS

! C	DEFINICAO DO TIPO DAS VARIAVEIS

 implicit real*8(a-h,o-z)
   real*8,allocatable::cl(:),tr(:,:),tclass(:,:),cld(:)
   real*8 menor,menorr,a(20,20,10),xx(810,3),k1,k2,res1(8),res2(8),xsub(20000),fsub(20000),a22(20,20,10)
   integer*4 v(20,20,8),cn
   character*11 L(4),tic
   character*80 cab, branco


! cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! c	ARQUIVOS 
! cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

  open(1,file='../inputs/dados_sint_T1.txt')
  open(2,file='../inputs/dados_sint_c1.txt')   ! ver linha 490
  open(3,file='../outputs/saida1.txt')
  open(4,file='../outputs/saida2.txt')
  open(5,file='../outputs/saida3.txt')
  open(7,file='../outputs/conv2000.txt')
  open(8,file='../outputs/dado_class_c1.txt')

! c       Leitura do arquivo de treinamento

    read(1,15) cab    ! cabeçalho
!	write(6,15) cab
    read(1,15) cab    ! linha em branco abaixo do cabeçalho
!	write(6,15) cab

  ij=1
   do while (.true.)
    read(1,*,end=6) branco,a1,a2,a3,a4,a5,a6
    ij=ij+1
   end do
  6 continue
  close(1)

  nt=ij-1
  write(6,*) "n de dados de treinamento",nt


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! ccc	leitura do arquivo de entrada
! ccc	litologias a serem determinadas

  read(2,15) cab    ! cabeçalho
!	write(6,15) cab
  read(2,15) cab    ! linha em branco abaixo do cabeçalho
!	write(6,15) cab

   ij=1
    do while (.true.)
     read(2,*,end=7) branco,a1,a2,a3,a4,a5,a6
     ij=ij+1
    end do
  7 continue
 close(2)

  nclass=ij-1
 write(6,*) "n de dados a serem classificados",nclass


  allocate (tr(1:nt,4),cl(1:nt),tclass(1:nclass,4),cld(1:nclass))

  open(1,file='../inputs/dados_sint_T1.txt')

 read(1,15) cab    ! cabeçalho
!	write(6,15) cab
 read(1,15) cab    ! linha em branco abaixo do cabeçalho
!	write(6,15) cab
 
  do i=1,nt
   read(1,*) branco,cl(i),prof,tr(i,1),tr(i,2),tr(i,3),tr(i,4)
  end do
 close(1)


! construção do tabuteiro - configuração inicial randômica

res1=minval(tr,dim=1)
res2=maxval(tr,dim=1)



 c1min=res1(1)   !2.05d0
 c1max=res2(1)   !3.66d0

 c2min=res1(2)   !5.11d-1
 c2max=res2(2)   !8.38d0

 c3min=res1(3)   !4.12d2
 c3max=res2(3)   !5.72d8

 c4min=res1(4)   !4.61d0
 c4max=res2(4)   !5.88d0

nta=20   !20 ! número de neurônios em cada lado do tabuleiro

print*, 'numero de neuronios da rede=',nta**2

 do i=1,nta
  do j=1,nta
   a(i,j,1)=rand()*(c1max-c1min)+c1min
   a(i,j,2)=rand()*(c2max-c2min)+c2min
   a(i,j,3)=rand()*(c3max-c3min)+c3min
   a(i,j,4)=rand()*(c4max-c4min)+c4min
   a(i,j,10)=0d0
  end do
end do

a22=a
! determinação das vizinhanças do tabuleiro/toro

! vizinhança da aresta horizontal superior

 do i=1,nta
   v(1,i,1)=nta ! acima 
   v(1,i,2)=i 
   v(1,i,3)=1 !direito
   v(1,i,4)=i+1
  if(i+1 == nta+1) then
   v(1,i,4)=1
  end if
   v(1,i,5)=2 ! abaixo
   v(1,i,6)=i
   v(1,i,7)=1  !esquerda
   v(1,i,8)=i-1
  if(i-1 == -1) then
   v(1,i,8)=nta
  end if
! vizinhança da aresta horizontal inferior
   v(nta,i,1)=nta-1 ! acima 
   v(nta,i,2)=i 
   v(nta,i,3)=nta   !direito
   v(nta,i,4)=i+1
  if(i+1 == nta+1) then
   v(nta,i,4)=1
  end if
   v(nta,i,5)=1   ! abaixo
   v(nta,i,6)=i
   v(nta,i,7)=nta   ! esquerda
   v(nta,i,8)=i-1
  if(i-1 == -1) then
   v(nta,i,8)=nta
  end if
! vizinhança da aresta lateral esquerda
   v(i,1,1)=i-1  ! acima
  if(i-1 == 0)then
   v(i,1,1)=nta
  end if
   v(i,1,2)=1 
   v(i,1,3)=i    ! a direita
   v(i,1,4)=2
   v(i,1,5)=i+1  ! abaixo
  if(i+1 == nta+1) then
   v(i,1,5)=1
  end if
   v(i,1,6)=1
   v(i,1,7)=i    ! a esquerda	
   v(i,1,8)=nta
! vizinhança da aresta lateral direita
   v(i,nta,1)=i-1  ! acima
  if(i-1 == 0)then
   v(i,nta,1)=nta
  end if
   v(i,nta,2)=nta 
   v(i,nta,3)=i    ! a direita
   v(i,nta,4)=1
   v(i,nta,5)=i+1  ! abaixo
  if(i+1 == nta+1) then
   v(i,nta,5)=1
  end if
   v(i,nta,6)=nta
   v(i,nta,7)=i    ! a esquerda	
   v(i,nta,8)=nta-1
  end do
! vizinhanças no miolo
  do i=2,nta-1
   do j=2,nta-1
     v(i,j,1)=i-1 !acima 
     v(i,j,2)=j 
     v(i,j,3)=i ! a direita
     v(i,j,4)=j+1
     v(i,j,5)=i+1 ! abaixo
     v(i,j,6)=j
     v(i,j,7)=i ! a esquerda
     v(i,j,8)=j-1
    end do
  end do
!! fim da detrminaçao das vizinhanças

!  Início do treinamento
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
k1=0.9d0
k2=0.1d0


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
n_repi=1000
n_rep=n_repi

do imin=1,2


! numero máximo de ciclos de treinamento                  
do 113, ih=1,n_rep       !tempo    !!   Loopp 2


k1=0.8d0*(1d0-dfloat(ih)/dfloat(n_repi+1))  ! taxa de aprendizagem
k2=k1/1.2d0   !1.2                              !   "  "     "  


  

!print*, 'k1=',k1

do ig=1,nt  !144  ! dados de treinamento   !!  Loop1

! busca pelo neuronio vitorioso

menor=1.d20  !deve ser um número grande
ij=1
do i=1,nta
 do j=1,nta
  a1=a(i,j,1)
  a2=a(i,j,2)
  a3=a(i,j,3)
  a4=a(i,j,4)

  b1=tr(ig,1)
  b2=tr(ig,2)
  b3=tr(ig,3)
  b4=tr(ig,4)

   call dist(a1,a2,a3,a4,b1,b2,b3,b4,d)
   !  print*, 'dist',i,j,ij,d    

  if(d.LT.menor) then
   menor=d
   imenor=i
   jmenor=j
   ijmenor=ij
  end if

   ij=ij+1
 end do
end do


!print*, 'menor',menor
!print*, 'indices do menor',imenor,jmenor,ijmenor

! atualização do neuronio vitorioso
a(imenor,jmenor,1)=a(imenor,jmenor,1)+k1*(tr(ig,1)-a(imenor,jmenor,1))
a(imenor,jmenor,2)=a(imenor,jmenor,2)+k1*(tr(ig,2)-a(imenor,jmenor,2))
a(imenor,jmenor,3)=a(imenor,jmenor,3)+k1*(tr(ig,3)-a(imenor,jmenor,3))
a(imenor,jmenor,4)=a(imenor,jmenor,4)+k1*(tr(ig,4)-a(imenor,jmenor,4))

a(imenor,jmenor,9)=cl(ig)
a(imenor,jmenor,10)=1d0

!print*, 'a(imenor,jmenor,9)',a(imenor,jmenor,9)


!atualização das vizinhancas

! vizinhança acima
il=v(imenor,jmenor,1)
ic=v(imenor,jmenor,2)


!print*, 'il=',il
!print*, 'ic=',ic


!if(il-1 == 0)then
!il=9
!end if

a(il,ic,1)=a(il,ic,1)+k2*(tr(ig,1)-a(il,ic,1))
a(il,ic,2)=a(il,ic,2)+k2*(tr(ig,2)-a(il,ic,2))
a(il,ic,3)=a(il,ic,3)+k2*(tr(ig,3)-a(il,ic,3))
a(il,ic,4)=a(il,ic,4)+k2*(tr(ig,4)-a(il,ic,4))

!a(il,ic,9)=cl(ig)  ! ????

!!!!!!!!!!!!!!!!!!!!!!!!!!!

! vizinhança a direita
il=v(imenor,jmenor,3)
ic=v(imenor,jmenor,4)


!print*, 'il=',il
!print*, 'ic=',ic


!if(ic == 10)then
!ic=1
!end if

a(il,ic,1)=a(il,ic,1)+k2*(tr(ig,1)-a(il,ic,1))
a(il,ic,2)=a(il,ic,2)+k2*(tr(ig,2)-a(il,ic,2))
a(il,ic,3)=a(il,ic,3)+k2*(tr(ig,3)-a(il,ic,3))
a(il,ic,4)=a(il,ic,4)+k2*(tr(ig,4)-a(il,ic,4))

!a(il,ic,9)=cl(ig)  ! ????
!!!!!!!!!!!!!!!!!!!!!!!!!!!

! vizinhança abaixo
il=v(imenor,jmenor,5)
ic=v(imenor,jmenor,6)


!print*, 'il=',il
!print*, 'ic=',ic


!if(il-1 == 9)then
!il=1
!end if

a(il,ic,1)=a(il,ic,1)+k2*(tr(ig,1)-a(il,ic,1))
a(il,ic,2)=a(il,ic,2)+k2*(tr(ig,2)-a(il,ic,2))
a(il,ic,3)=a(il,ic,3)+k2*(tr(ig,3)-a(il,ic,3))
a(il,ic,4)=a(il,ic,4)+k2*(tr(ig,4)-a(il,ic,4))

!a(il,ic,9)=cl(ig)  ! ????

! vizinhança a esquerda
il=v(imenor,jmenor,7)
ic=v(imenor,jmenor,8)


!print*, 'il=',il
!print*, 'ic=',ic


!if(ic == -1)then
!ic=9
!end if

a(il,ic,1)=a(il,ic,1)+k2*(tr(ig,1)-a(il,ic,1))
a(il,ic,2)=a(il,ic,2)+k2*(tr(ig,2)-a(il,ic,2))
a(il,ic,3)=a(il,ic,3)+k2*(tr(ig,3)-a(il,ic,3))
a(il,ic,4)=a(il,ic,4)+k2*(tr(ig,4)-a(il,ic,4))

!a(il,ic,9)=cl(ig)  ! ????


end do   ! loop do ig (dados de treinamento)

!UUUUUUUUUUUUUUUUUUUUUUUUUU
! Usando a rede para construir o arquivo com a covergência    
! do treinamento da rede                                    

somacon=0d0
do irr=1,nt

ind=irr
menor=1.d20  !deve ser um número grande
ij=1
do i=1,nta
do j=1,nta


a1=a(i,j,1)
a2=a(i,j,2)
a3=a(i,j,3)
a4=a(i,j,4)

b1=tr(ind,1)
b2=tr(ind,2)
b3=tr(ind,3)
b4=tr(ind,4)

call dist(a1,a2,a3,a4,b1,b2,b3,b4,d)


 if(d.LT.menor) then
  menor=d
  imenor=i
  jmenor=j
  ijmenor=ij
 end if


ij=ij+1

end do
end do

if(a(imenor,jmenor,9).ne.cl(ind)) then
somacon=somacon+1d0
end if

end do


!write(7,*)ih,somacon

xsub(ih)=dfloat(ih)
fsub(ih)=somacon






113 end do !loop do ih  (repetições do treinamento)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! critério de para pelo valor mínimo

if(imin .eq. 1)then
a=a22

!ixxx=minloc(fsub,1)

menorr=1d20
do k=1,n_rep
 if(fsub(k).LT.menorr) then
 menorr=fsub(k)
 imenorr=k
 end if
end do



print*, 'ixxx=',imenorr
n_rep=imenorr
print*, 'imin agora=',imenorr


xsub=0d0
fsub=0d0

end if


end do !!!!!!!! fim do loop do critério de parada pelo valor mínimo  imin (loop com 2 laços)




print*, 'numero de ciclos de treinamento (época) ótimo=', n_rep
print*, 'fsub(1)=', fsub(1)
print*, 'fsub(n_rep)', fsub(n_rep)

call ajuste_exp(xsub,fsub,fsub(1),fsub(n_rep),n_rep,A0,B0,C0)

do it=1,n_rep

v_aj=A0*exp(-B0*xsub(it))+C0

write(7,*) xsub(it),fsub(it),v_aj

end do




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! final do loop de treinamento



! contagem dos neurônios vitoriosos
soma=0d0
do i=1,nta
do j=1,nta
soma=soma+a(i,j,10)
end do
end do
print*, 'neurônios vitoriosos=', soma

! contagem dos neurônios sem uso
soma=0d0
do i=1,nta
do j=1,nta
if(a(i,j,1).eq. 0d0)then
soma=soma+1
end if

end do
end do
print*, 'neurônios sem uso =', soma

!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2

!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

! mapeamento dos neuronios vitoriosos
 cn=10  ! o elemento a(i,j,10) vale 1 se o neuronio for vitorioso
do i=1,nta
write (3,'(*(ES12.4E3,2x))') (a(i,j,cn),j=1,nta)
end do

!!!!!!!!!!!!!!!!!!!!!!!!!

! mapeamento das classes dos neuronios vitoriosos
 cn=9 ! classe do vitorioso 
do i=1,nta
write (4,'(*(ES12.4E3,2x))') (a(i,j,cn),j=1,nta)
end do

!!!!!!!!!!!!!!!!!!!!!!!!

! mapeamento dos valores das propriedades dos neuronios vitoriosos
 cn=1 !   classe do vitorioso Pode variar de 1 até 4
do i=1,nta
write (5,'(*(ES12.4E3,2x))') (a(i,j,cn),j=1,nta)
end do


!!!!!!!!!!!!!!!111 final do treinamento

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!    Usando a rede   fluxograma 3

open(2,file='../inputs/dados_sint_c1.txt')

 read(2,15) cab    ! cabeçalho
!	write(6,15) cab
 read(2,15) cab    ! linha em branco abaixo do cabeçalho
!	write(6,15) cab

 do i=1,nclass
  read(2,*) branco,cld(i),prof,tclass(i,1),tclass(i,2),tclass(i,3),tclass(i,4)
 end do
close(2)



ierro=0
do irr=1,nclass
ind=irr
menor=1.d20  !deve ser um número grande
ij=1
do i=1,nta
do j=1,nta


a1=a(i,j,1)
a2=a(i,j,2)
a3=a(i,j,3)
a4=a(i,j,4)

b1=tclass(ind,1)
b2=tclass(ind,2)
b3=tclass(ind,3)
b4=tclass(ind,4)

call dist(a1,a2,a3,a4,b1,b2,b3,b4,d)


 if(d.LT.menor) then
  menor=d
  imenor=i
  jmenor=j
  ijmenor=ij
 end if


ij=ij+1

end do
end do

 xxclasse=a(imenor,jmenor,9)
!!print*, ind,'classe=',xxclasse,'=',cl(ind)

If(xxclasse .eq. cld(ind)) then
tic="certo"
else
ierro=ierro+1
tic="errado"
end if

write(8,*) ind,'classe=',xxclasse,'=',cld(ind),' --',tic



if(a(imenor,jmenor,9).ne.cl(ind)) then
!!print*, ind,'erro'
end if


end do

print*,'numero de erros=',ierro,'  ',100d0*ierro/nclass,'%'



!	print*, 'a(4,4,1)=', a(4,4,1)
!	determinação da vizinhança acima
!	il=v(4,4,1)
!	ic=v(4,4,2)

! print*, 'il fim=',il
! print*, 'ic fim=',ic	

!	print*, ' vizinhança abaixo=', a(il,ic,1),a(3,4,1)





11 format(10(ES12.4E3,2x))
12 format(I3,2x,3(f6.2,2x))
13 format(4(ES12.4E3,2x))
14 format(4(ES9.2E2,2x))
15 format(A71)
16 format(A11,8(ES9.2E3))
17 format(A30,2x,ES12.4E3)
18 format(2(f6.2,2x),2x,A11,2x,ES12.4E3)




 print*,' ************ FIM *************'
 print*,''

! c	pause
 stop
 end

! ccccccccccccccccc
! ccccccccccccccccc

! ccccccccccccccccccccccccccccccccccccccccc

subroutine dist(a1,a2,a3,a4,b1,b2,b3,b4,d)
real*8 	a1,a2,a3,a4,b1,b2,b3,b4,d

d=dsqrt((a1-b1)**2+(a2-b2)**2+(a3-b3)**2+(a4-b4)**2)

	return
	end



!!!!  subrotinas

     subroutine parada(x,f,A00,delta,n_eq,flag,A0,B0,C0) 
     
!      real*8 x(n_eq),f(n_eq)

	implicit real*8(a-h,o-z)


	real*8 A(n_eq,3),AT(3,n_eq),ATA(3,3),C(3,n_eq),f(n_eq),&
             p(3),f1(n_eq),p0(3),x(n_eq)


! Função a ser ajustada: y=A.exp(-Bx)+C

	n_par=3   ! A,B e C





!	 valores iniciais

	A0=A00 !2d0
	B0=0.110d0  !0.5d0
	C0=1.1d0  !0.5d0


!	vetor de dados = f(ie)



!	 número de iteraçoes
	do jj=1,20 !loop das iterações


!	matriz jacobiana A (29,3)
	
	do i=1,n_eq

	A(i,1)=exp(-B0*x(i))
	A(i,2)=A0*exp(-B0*x(i))*(-x(i))
	A(i,3)=1d0

	end do

!	--------------AT (29,3)
	do i=1,n_eq
	do j=1,n_par
	AT(j,i)=A(i,j)

	end do
	end do

!	multiplicação de AT.A (3,3)

	do k=1,n_par
	do j=1,n_par
	ATA(j,k)=0.d0
	do i=1,n_eq
	ATA(j,k)=ATA(j,k)+AT(j,i)*A(i,k)
	end do
	end do
	end do
!	-----------------  multiplicador de Lagrange

	aaa=0.9d0
	do i=1,3
	ATA(i,i)=ATA(i,i)+aaa
	end do
!	----------------------------------

	call INVERT(ATA,n_par)

!	multiplica a inversa de ATA por AT = C (3,29)

	do k=1,n_eq
	do j=1,n_par
	C(j,k)=0.d0
	do i=1,n_par
	C(j,k)=C(j,k)+ATA(j,i)*AT(i,k)
	end do
	end do
	end do

!	-----------determinação da matriz f1
	

	do i=1,n_eq
	f1(i)=A0*exp(-B0*x(i))+C0 - f(i)
	end do

!	multiplica C por bz --- P = C.f

!!	do k=1,1
	do j=1,n_par
	p(j)=0.d0
	do i=1,n_eq
	p(j)=p(j)+C(j,i)*f1(i)
	end do
	end do
!!	end do

	A0=A0-p(1)
	B0=B0-p(2)
	C0=C0-p(3)


!	write(6,22) jj,A0,B0,C0
	
!	write(6,*) "===================="




!	pause
	end do			!fim do loop das iterações



	write(6,*) 'A=',A0
	write(6,*) 'B=',B0
	write(6,*) 'C=',C0



!print*,"diferença quadratica",soma 

! Cálculo da derivada

deriv=A0*EXP(-B0*x(n_eq))*(-B0)
deriv=abs(deriv)

print*,'deriv=',deriv
print*,'delta=',delta


if (deriv < delta) then
flag=1d0
else
flag=-1d0
end if

      RETURN
      END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     subroutine ajuste_exp(x,f,vi,vf,n_eq,A0,B0,C0) 
     
!      real*8 x(n_eq),f(n_eq)

	implicit real*8(a-h,o-z)


	real*8 A(n_eq,3),AT(3,n_eq),ATA(3,3),C(3,n_eq),f(n_eq),&
             p(3),f1(n_eq),p0(3),x(n_eq)


! Função a ser ajustada: y=A.exp(-Bx)+C

	n_par=3   ! A,B e C


!	 valores iniciais

	A0=vi-vf !2d0
	B0=0.110d0  !0.5d0
	C0=vf  !0.5d0


!	vetor de dados = f(ie)



!	 número de iteraçoes
	do jj=1,100 !loop das iterações


!	matriz jacobiana A (29,3)
	
	do i=1,n_eq

	A(i,1)=exp(-B0*x(i))
	A(i,2)=A0*exp(-B0*x(i))*(-x(i))
	A(i,3)=1d0

	end do

!	--------------AT (29,3)
	do i=1,n_eq
	do j=1,n_par
	AT(j,i)=A(i,j)

	end do
	end do

!	multiplicação de AT.A (3,3)

	do k=1,n_par
	do j=1,n_par
	ATA(j,k)=0.d0
	do i=1,n_eq
	ATA(j,k)=ATA(j,k)+AT(j,i)*A(i,k)
	end do
	end do
	end do
!	-----------------  multiplicador de Lagrange

	aaa=0.9d0
	do i=1,3
	ATA(i,i)=ATA(i,i)+aaa
	end do
!	----------------------------------

	call INVERT(ATA,n_par)

!	multiplica a inversa de ATA por AT = C (3,29)

	do k=1,n_eq
	do j=1,n_par
	C(j,k)=0.d0
	do i=1,n_par
	C(j,k)=C(j,k)+ATA(j,i)*AT(i,k)
	end do
	end do
	end do

!	-----------determinação da matriz f1
	

	do i=1,n_eq
	f1(i)=A0*exp(-B0*x(i))+C0 - f(i)
	end do

!	multiplica C por bz --- P = C.f

!!	do k=1,1
	do j=1,n_par
	p(j)=0.d0
	do i=1,n_eq
	p(j)=p(j)+C(j,i)*f1(i)
	end do
	end do
!!	end do

	A0=A0-p(1)
	B0=B0-p(2)
	C0=C0-p(3)


!	write(6,22) jj,A0,B0,C0
	
!	write(6,*) "===================="




!	pause
	end do			!fim do loop das iterações

	write(6,*) 'vi=',vi
	write(6,*) 'vf=',vf

	write(6,*) 'A=',A0
	write(6,*) 'B=',B0
	write(6,*) 'C=',C0

      RETURN
      END




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! subrotina de inversao de matriz

      subroutine INVERT(A,i)      
      real*8 A(i,i),B(i)
      integer i,im,j,k,l
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
 
      RETURN
      END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!	subroutine sort2(xx,n)

!! c	ordena xx com relação a coluna 3	
!	implicit real*8(a-h,o-z)
!	real*8 xx(n,3)


!	do i=1,n-1,1
!	do j=i+1,n,1
!	if(xx(i,3) .GT. xx(j,3)) then
!	a1=xx(i,1)
!	a2=xx(i,2)
!	a3=xx(i,3)

!	xx(i,1)=xx(j,1)
!	xx(i,2)=xx(j,2)
!	xx(i,3)=xx(j,3)

!	xx(j,1)=a1
!	xx(j,2)=a2
!	xx(j,3)=a3

!	end if
!	end do
!	end do

!	return
!	end
! cccccccccccccccccccccc



