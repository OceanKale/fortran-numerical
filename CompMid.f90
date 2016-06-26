program CompMid
implicit none
double precision x0,xn,x,h,Mid,exact,diff,f,y
integer j,n,m
write(*,*) 'enter n (must be even)'
read(*,*) n
m=n/2
x0=1.d0
xn=2.d0
y = n + 2
h=(xn-x0)/dfloat(y)
Mid=0.d0
do j=0,m
  x=x0+dfloat(2*j+1)*h
  Mid=Mid+f(x)
enddo
Mid=2*h*Mid
exact=0.6362943611198906188344642429163531361500026
diff=dabs(Mid-exact)
write(*,*)'exact=',exact
write(*,*)'Mid=',Mid
write(*,*)'|Mid-exact|=',diff
stop
end program CompMid

function f(x)
double precision x,f
f=x*dlog(x)
return
end function f
