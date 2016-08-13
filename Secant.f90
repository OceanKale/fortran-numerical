program secant
implicit none
double precision p1,p0,p2,q1,q0,q2,tol,f
integer i,imax
imax=20
tol=1.d-7
write(*,*) 'tol=',tol
p0=0.1
p1=0.3
q0=f(p0)
q1=f(p1)
write(*,*) '           i        p_i             |p_i - p_i-1| '
do i=1,imax   
  p2=p1-q1*(p1-p0)/(q1-q0)
  q2=f(p2)
  write(*,*) i,p2,dabs(p2-p1)
  if (q1*q2 .lt. 0.) then
    write(8,*) i,'-'
  else
    write(8,*) i,'+'
  endif
  if (dabs(p2-p1) .lt. tol) then
     stop
  endif
  p0=p1
  p1=p2
  q0=q1
  q1=q2
enddo
stop
end program secant

function f(x)
double precision x,f
f= x + 1- 2 * dsin(pi * x)
return
end function f
