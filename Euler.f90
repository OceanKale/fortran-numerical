program Euler
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
	y=y+h*f(t,y)
	write(*,*)'Y=', y
	t=a + i*h
	write(8,*) t,y,g(t),'error:', dabs(y - g(t))
enddo
write(9,*) h, dabs(y-g(t))
write(10,*) t, dabs(y-g(t))
stop
end program Euler

function f(x,y)
implicit none
double precision x,y,f,z
z=2.d0*x
f=(1/x**2)*(dsin(z) - 2.d0*x*y)
return
end function f

function g(t)
implicit none
double precision t,g,e,y
e=dexp(1.d0)
y=2.d0*t
g=(4.d0 + dcos(2.d0) - dcos(y))/(2*(t**2))
return
end function g
