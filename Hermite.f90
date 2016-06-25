program Hermite
implicit none
integer, parameter :: real8   = selected_real_kind(13,300)
integer, parameter :: n=2
real(kind=real8), dimension(0:2*n+1,0:2*n+1)::Q
real(kind=real8), dimension(0:n):: x,f,fp
real(kind=real8), dimension(0:2*n):: z
integer i,j
open(unit=7,file='Table3.15')
read(7,*) (x(i),i=0,n)
read(7,*)  (f(i),i=0,n)
read(7,*)  (fp(i),i=0,n)
do i=0,n
  z(2*i)=x(i)
  z(2*i+1)=x(i)
  Q(2*i,0)=f(i)
  Q(2*i+1,0)=f(i)
  Q(2*i+1,1)=fp(i)
  if (i .ge. 1) then
    Q(2*i,1)=(Q(2*i,0)-Q(2*i-1,0))/(z(2*i)-z(2*i-1))
  endif
enddo
do i=2,2*n+1
  do j=2,i
    Q(i,j)=(Q(i,j-1)-Q(i-1,j-1))/(z(i)-z(i-j))
  enddo
enddo
do i=0,2*n+1
  write(*,*) i,Q(i,i)
enddo
end program Hermite

