module modu
	contains
	subroutine prog(A,B,C,D,X,Imax)
	implicit none
	integer :: Imax,i
	real(8) :: A(Imax), B(Imax), C(Imax), D(Imax), X(Imax),alf(Imax), bet(Imax)
	!____________________________________________________________Прямой ход
	alf(1)=-C(1)/B(1)
	bet(1)=D(1)/B(1)
	i=2
	do i=2,(Imax-1),1
		if ((B(i)+A(i)*alf(i-1)) .EQ. 0) then
			print*, 'wrong matrix'
			stop
		end if
		alf(i)=-(C(i)/(B(i)+A(i)*alf(i-1)))
		bet(i)=(D(i)-A(i)*bet(i-1))/(B(i)+A(i)*alf(i-1))
		!i = i + 1
	end do
	if ((B(Imax)+A(Imax)*alf(Imax-1)) .EQ. 0) then
		print*, 'wrong matrix'
		stop
	end if
	!____________________________________________________________Обратный ход
	X(Imax)=(D(Imax)-A(Imax)*bet(Imax-1))/(B(Imax)+A(Imax)*alf(Imax-1))
	i=Imax-1
	do i=(Imax-1),1,-1
		X(i)=alf(i)*X(i+1)+bet(i)
	end do
	end subroutine
end module
program metod_progonki
use modu
implicit none
integer, parameter :: Imax=5
integer :: i,j
real(8) :: A(Imax), B(Imax), C(Imax), D(Imax), X(Imax), MATRIX(Imax,IMAX)

call random_seed
call random_number(A)
call random_number(B)
call random_number(C)
call random_number(D)

if (B(1) .EQ. 0) then
	print*, 'wrong matrix'
	stop
end if
do i = 1, Imax-1
	MATRIX(i,i) = B(i)
	MATRIX(i+1,i) = A(i+1)
	MATRIX(i,i+1)=C(i)
end do

MATRIX(Imax,Imax)=B(Imax)


do i = 1,Imax
	write(*, '(a1, $)') '|'
	do j = 1,Imax
		write(*, '(f7.2, $)') MATRIX(i,j)  
	end do
	write(*,*)'|'
end do


i=1
A(1)=0
C(Imax)=0
i = 1

call prog(A,B,C,D,X,Imax)

do i =1,Imax
	write(*,'(a3,i2,a5,f8.5)')'X(',i,') = ',X(i)
end do

end program
