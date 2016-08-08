program NwtnDivDiff
! Calculates the coefficients a_0, a_1, ... a_n
! of the interpolatory polynomial
! P(x)=a_0+a_1(x-x_0) + ...+a_{n-1}(x-x0)...(x-x_{n-1})
! The numbers   x_0,...x_n 
! and values f(x_0),...f(x_n)
! are read from the first two lines of the file fort.7
implicit none
integer, parameter :: real8   = selected_real_kind(13,300)
real(kind=real8) tol,xx
integer, parameter :: n=4
real(kind=real8), dimension(0:n,0:n)::F
real(kind=real8), dimension(0:n):: x
integer i,j
open(unit=7,file='Table3.10')
read(7,*) (x(i),i=0,n)
read(7,*) (F(i,0),i=0,n)
do i=1,n
  do j=1,i
    F(i,j)=(F(i,j-1)-F(i-1,j-1))/(x(i)-x(i-j))
  enddo
enddo
write(*,*) '          i          a_i'
do i=0,n
  write(*,*) i,F(i,i)
enddo
write(*,*) '          i          b_i'
do i=0,n
  write(*,*) i,F(n,i)
enddo
end program NwtnDivDiff

function func(x)
integer, parameter :: real8   = selected_real_kind(13,300)
real(kind=real8) x,func
func=x*x
return
end function func

