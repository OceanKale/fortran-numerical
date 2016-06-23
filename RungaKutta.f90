! Runga-Kutta method

program RK4
implicit none
double precision h,w,exact,diff,f,a,b,alpha,t,k1,k2,k3,k4
integer i,N
write(*,*) 'enter a,b'
read(*,*) a,b
write(*,*) 'enter N'
read(*,*) N
write(*,*) 'enter alpha'
read(*,*) alpha
h=(b-a)/dfloat(N)
t=a
w=alpha
do i=1,n
  k1=h*f(t,w)
  k2=h*f(t+h/2.d0,w+k1/2.d0)
  k3=h*f(t+h/2.d0,w+k2/2.d0)
  k4=h*f(t+h,w+k3)
  w=w+(k1+2.d0*(k2+k3)+k4)/6.d0
  t=a+dfloat(i)*h
  diff=dabs(exact(t)-w)
  write(8,*) t,diff
  write(10,*) t,w,exact(t),diff
enddo
write(9,*) h,diff
stop
end program RK4

function exact(t)
double precision t,exact,x
x=2.d0*t
exact=(.5d0)*(1/(t**2.d0))*(4+dcos(2.d0)-dcos(x))
return
end function exact

function f(t,y)
double precision t,y,f,z,x
x=2*t
z=x*y
f=(1/(t**2))*(dsin(x) - z)
return
end function f


