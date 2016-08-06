! This program uses the bisection algorithm to find a root
program bisect
implicit none
real a,b,p,f
integer i,imax
a=0.
b=3.
imax=20
if (f(a)*f(b) .gt. 0.) then
  write(*,*) 'root not bracketed'
  stop
endif
do i=1,imax
  p=(b+a)/2.
  if (f(a)*f(p) .lt. 0.) then
    b=p
  else
    a=p
  endif
  write(*,*) i,p,f(p)
  write(8,*) i,abs(p-1.)
enddo
stop
end program bisect

function f(x)
implicit none
real x,f
f=x**5-1.
return
end function f

