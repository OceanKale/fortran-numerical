program modEuler
implicit none
integer n,i
double precision a,b,alpha,h,t,y,e,f,g
e=dexp(1.d0)
write(*,*)'enter n'
read(*,*) n
write(*,*)'enter a'
read(*,*) a
write(*,*)'enter b'
read(*,*) b
write(*,*)'enter alpha'
read(*,*) alpha
h=(b-a)/n
t=a
y=alpha
write(8,*) t,y
write(*,*) h
do i=1,n
	y=y+ h/2.d0*(f(t,y)+f(t+h,y+h*f(t,y)))
	write(*,*)'YYYYYYY=', y
	t=a + i*h
	write(8,*) t,y,g(t),'error:', dabs(y - g(t))
enddo
write(9,*) h, dabs(y-g(t))
stop
end program modEuler

function f(t,y)
implicit none
double precision t,x,y,f,z
x=2*t
z=2*t*y
f=(1/(t**2))*(dsin(x) - z)
return
end function f

function g(t)
implicit none
double precision t,g,x
x=2.d0*t
g=(.5d0)*(1/(t**2.d0))*(4+dcos(2.d0)-dcos(x))
return
end function g
