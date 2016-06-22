! computes using Simpson's method
program CompSimp
implicit none
double precision x0,xn,x,h,Simp,exact,diff,f
integer j,n,m
write(*,*) 'enter n (must be even)'
read(*,*) n
m=n/2
x0=0.d0
xn=1.d0
h=(xn-x0)/dfloat(n)
Simp=f(x0)+f(xn)
do j=1,m-1
  x=x0+dfloat(2*j)*h
  Simp=Simp+2.d0*f(x)
enddo
do j=1,m
  x=x0+dfloat(2*j-1)*h
  Simp=Simp+4.d0*f(x)
enddo
Simp=h/3.d0 *Simp
exact=0.528408
diff=dabs(Simp-exact)
write(*,*)'exact=',exact
write(*,*)'Simp=',Simp
write(*,*)'|Simp-exact|=',diff
stop
end program CompSimp

!!!! 0.63629436111989061883446424291635313615100026

function f(x)
double precision x,f
f=x**(-1/4) *dsin(x)
return
end function f
