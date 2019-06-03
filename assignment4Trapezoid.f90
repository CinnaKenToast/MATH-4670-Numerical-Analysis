program trapezoid
implicit none
integer, parameter :: nt = 10
double precision :: a,b,f,trap,table(1,0:nt),exact
integer :: i, n
external f

n = 100
a = 0.0d0
b = 2.0d0
exact = 21d0/20d0

print*, '         n      ', '       trapezoid       ', '             error'
do i = 0,nt
    table(i,0) = trap(n,a,b,f)
    print*, n, table(i,0), table(i,0) - exact
    n = n+5
end do

end program 

double precision function f(x)
implicit none
double precision :: x

if (x <= 1) then
    f = 1-x**4.0d0
else if (x > 1) then
    f = (x-1)**3.0d0
end if
return 
end 

double precision function trap(n,a,b,f)
implicit none
double precision :: a,b,f,dx,sum
integer :: n,i

dx = (b-a)/dble(n)

sum = 0.0d0

do i = 1,n-1
        sum = sum + 2*f(dble(a+i*dx))
end do

trap= dx/dble(2)*(f(a)+f(b)+sum)

return
end

