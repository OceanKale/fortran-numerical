program NwtnBwdDiffForm
! Computes the value of the interpolating
! polynomial using Newton's forward difference formula 
! The numbers   x_0, h, x 
! and values f(x_0),...f(x_n)
! are read from the first two lines of the file named NwtnFwdInput 
implicit none
integer, parameter :: real8   = selected_real_kind(13,300)
real(kind=real8) tol,xx,s,h,x0,P,prod
integer, parameter :: n=3
real(kind=real8), dimension(0:n,0:n)::F
real(kind=real8), dimension(0:n):: x
integer i,j
open(unit=7,file='NwtnFwdInput')
read(7,*) x0,h,xx
read(7,*) (F(i,0),i=0,n)
s=(0.43-0.75)/h
do i=1,n
  do j=1,i
    F(i,j)=F(i,j-1)-F(i-1,j-1)
  enddo
enddo
prod=1.d0
P=F(0,0)
i=0
write(*,*) i,prod,P
do i=1,n
  prod=prod*(s-dfloat(i-1))/dfloat(i)
  P=P+prod*F(i,i)
  write(*,*) i,prod,P
enddo
write(*,*) 'P(x)=',P
end program NwtnBwdDiffForm


