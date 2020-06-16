!     PROGRAMA - geração de dados sintéi!os de poços
!     VARIAVEIS UTILIZADAS

!	DEFINI!AO DO TIPO DAS VARIAVEIS

	implicit real*8(a-h,o-z)

      real*8 ds(10000,5),x1(10000),x2(10000),vp(4,20),delta(4,20),vc(4,20)
	integer*4 Lito(20),nd(20)

	character*12 L(12)
	character*80 cab
!cccccccccccccccccccccccccccccccccccc
!c	ARQUIVOS 
!cccccccccccccccccccccccccccccccccccc

	open(1,file='../inputs/C1.txt')

	isemen=23  						
	do j=1,isemen
	xaa=rand()
	end do


!	nd - número de dados
!	vp - valor da propriedade física
!	delta - despersão da valor medidos (desvio padrão)



!! cabeçalho do arquivo dados_sint.txt

write(1,15) 'Lithology','Code','Depth(m)','RHOB','GR','SP','DT'
write(1,*) '   '   ! linha em branco embaixo do cabeçalho

!cccccc  camada 1  cccccccccccccccccccccc
print*,'**************** Poço C1 ********************'
	Lito(1)=1       !folhelho
	L(1)='shale2'

  vp(1,1)=2.50d0!densidade
  vp(2,1)=110d0!raio-gama
  vp(3,1)=70d0!potencial espontaneo 
  vp(4,1)=550d0!sonico


	delta(1,1)=vp(1,1)*0.02d0
	delta(2,1)=vp(2,1)*0.1d0
	delta(3,1)=vp(3,1)*0.1d0
	delta(4,1)=vp(4,1)*0.1d0

	cota0=0d0	
	cota1=0.6d0
	esp1=cota1-cota0
	T_amostra=1d-2
	nd(1)=Int(esp1/T_amostra)   !numero de medidas na camada 1
	write(6,*) 'nd(c1-folhelho2)=',nd(1)


!cccccc  camada 2  cccccccccccccccccccccc
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


	cota2=1.17d0
	esp2=cota2-cota1
	T_amostra=1d-2
	nd(2)=int(esp2/T_amostra)   !numero de medidas na camada 2
	write(6,*) 'nd(c1-dolomita)=',nd(2)

!cccccccccc  camada 3   ccccccccccccccccccccccc
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

	cota3=1.5d0
	esp3=cota3-cota2
	T_amostra=1d-2
	nd(3)=int(esp3/T_amostra)   !numero de medidas na camada 3
	write(6,*) 'nd(c1-diabasio)=',nd(3)


!ccccccccc   camada 4 cccccccccccccccccccccc

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

	cota4=2.4d0
	esp4=cota4-cota3
	T_amostra=1d-2
	nd(4)=int(esp4/T_amostra)   !numero de medidas na camada 4
	write(6,*) 'nd(c1-dolomita)=',nd(4)


!ccccccccc   camada 5 cccccccccccccccccccccc
	Lito(5)=4      !conglomerado
	L(5)='conglomerate'
 vp(1,5)=2.30d0!RHOB
 vp(2,5)=20d0!GR
 vp(3,5)=-40d0!SP
 vp(4,5)=110d0!DT


	delta(1,5)=vp(1,5)*0.02d0
	delta(2,5)=vp(2,5)*0.1d0
	delta(3,5)=vp(3,5)*0.1d0
	delta(4,5)=vp(4,5)*0.1d0


	cota5=6.3d0
	esp5=cota5-cota4
	T_amostra=1d-2
	nd(5)=int(esp5/T_amostra)   !numero de medidas na camada 5
	write(6,*) 'nd(c1-conglomerado)=',nd(5)



!ccccccccc   camada 6 Ultima camada cccccccccccccccccccccc
	Lito(6)=5      !embasamento

	L(6)='Crystalline'
 vp(1,6)=2.75d0!RHOB
 vp(2,6)=40d0!GR
 vp(3,6)=70d0!SP
 vp(4,6)=55d0!DT

	delta(1,6)=vp(1,6)*0.02d0
	delta(2,6)=vp(2,6)*0.1d0
	delta(3,6)=vp(3,6)*0.1d0
	delta(4,6)=vp(4,6)*0.1d0


	cota6=7d0
	esp6=cota6-cota5
	T_amostra=1d-2

	nd(6)=int(esp6/T_amostra)   !numero de medidas na camada 6
	write(6,*) 'nd(c1-embasamento)=',nd(6)



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1

	ic=0

	do j=1,6  ! numero de camadas
	do i=1,nd(j)
	ic=ic+1

	call n_rand(vp(1,j),delta(1,j),vc(1,j))
	call n_rand(vp(2,j),delta(2,j),vc(2,j))
	call n_rand(vp(3,j),delta(3,j),vc(3,j))
	call n_rand(vp(4,j),delta(4,j),vc(4,j))


	a2=vc(1,j)
	a3=vc(2,j)
	a4=vc(3,j)
	a5=vc(4,j)

	write(1,14) L(j),Lito(j),ic,a2,a3,a4,a5

	ia1=ic
	end do
	end do





11	format(4(ES12.4E3,2x))
12	format(I3,2x,3(f6.2,2x))

13	format(I2,3x,I10,2x,4(ES9.2E2,2x))
14	format(A12,2x,I3,2x,I10,2x,4(ES9.2E2,2x))

15	format(A9,5x,A6,6x,A4,2x,A4,7x,A4,7x,A3,8x,A3)
16	format(A11,8(ES9.2E3))





	print*,' ************ FIM *************'
	print*,''

!c	pause
	stop
	end

!ccccccccccccccccc
!ccccccccccccccccc

	subroutine media_desvio(x,n,media,desvio_padrao)

	implicit real*8(a-h,o-z)

	real*8 x(n),media,desvio_padrao

!c	n=i-1
	 
	soma = 0d0
	do i=1,n	
	soma = soma + x(i)
	end do
	media=soma/dfloat(n)

	soma2 = 0d0
	do i=1,n
	   soma2 = soma2 + (x(i)-media)**2
	end do
	
	soma2=soma2/(dfloat(n)-1d0)
	
	desvio_padrao=dsqrt(soma2)

	return
	end


!c#####################################
	subroutine soma(e,x,w)
	integer:: e,i
	real*8 :: x(e),w

	w=0
	do i=1,e
		w= w + x(i)
	end do

	end subroutine soma

!cccccccccccccccccccccccccccc
	subroutine n_rand(vp,dp,vc)
	real*8 vp,dp,vc,tq(100),vmax,vmin
	integer i

	do i=1,38
		vmax=vp+0.5d0*dp
		vmin=vp-0.5d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=39,62
		vmax=vp+1.5d0*dp
		vmin=vp+0.5d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=63,86
		vmax=vp-0.5d0*dp
		vmin=vp-1.5d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=87,92
		vmax=vp+2.5d0*dp
		vmin=vp+1.5d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=93,98
		vmax=vp-1.5d0*dp
		vmin=vp-2.5d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	vmax=vp+3.5d0*dp
	vmin=vp+2.5d0*dp
	tq(99)=rand()*(vmax-vmin)+vmin
	
	vmax=vp-2.5d0*dp
	vmin=vp-3.5d0*dp
	tq(100)=rand()*(vmax-vmin)+vmin
	
	vc=tq(int(100.d0*rand()+1.d0))
	
	return
	end

!cccccccccccccccccccccccccc

	subroutine n_rand2(vp,dp,vc)
	real*8 vp,dp,vc,tq(100),vmax,vmin
	integer i

	do i=1,22
		vmax=vp+0.28d0*dp
		vmin=vp-0.28d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=23,28
		vmax=vp+0.44d0*dp
		vmin=vp+0.28d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=29,34
		vmax=vp-0.28d0*dp
		vmin=vp-0.44d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=35,44
		vmax=vp+0.74d0*dp
		vmin=vp+0.44d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=45,54
		vmax=vp-0.44d0*dp
		vmin=vp-0.74d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=55,58
		vmax=vp+0.88d0*dp
		vmin=vp+0.74d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=59,62
		vmax=vp-0.74d0*dp
		vmin=vp-0.88d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=63,70
		vmax=vp+1.23d0*dp
		vmin=vp+0.88d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=71,78
		vmax=vp-0.88d0*dp
		vmin=vp-1.23d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=79,83
		vmax=vp+1.56d0*dp
		vmin=vp+1.23d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=84,88
		vmax=vp-1.23d0*dp
		vmin=vp-1.56d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=89,90
		vmax=vp+1.76d0*dp
		vmin=vp+1.56d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=91,92
		vmax=vp-1.56d0*dp
		vmin=vp-1.76d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=93,94
		vmax=vp+2.06d0*dp
		vmin=vp+1.76d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=95,96
		vmax=vp-1.76d0*dp
		vmin=vp-2.06d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=97,98
		vmax=vp+4.51d0*dp
		vmin=vp+2.06d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=99,100
		vmax=vp-2.06d0*dp
		vmin=vp-4.51d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

		
	vc=tq(int(100.d0*rand()+1.d0))

	
	return
	end
