! computes trapezoidal method
program CompTrap
implicit none
double precision x0,xn,h,sum,f,exact,diff,xj
integer n,j
write(*,*)'enter n'
read(*,*) n
x0=0.0d0
xn=2.d0
h=(xn-x0)/dfloat(n)
sum=f(x0)+f(xn)
do j=1,n-1
  xj=x0+dfloat(j)*h
  sum=sum+2.d0*f(xj)
enddo
sum=h/2.d0*sum
exact=dlog(1.5d0)
write(*,*) 'exact=',exact
diff=dabs(sum-exact)
write(*,*) 'Trapezoidal approx=',sum
write(*,*) 'diff=',diff
stop
end program CompTrap

function f(x)
double precision x,f
f=1/(x+4)
return
end function f
