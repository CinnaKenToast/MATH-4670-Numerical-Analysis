program simpson1d
implicit none
integer, parameter :: nt = 10
double precision :: a,b,f,simpson,table(1,0:nt),exact
integer :: i, n
external f

n=100
a = 0.0d0
b = 2.0d0
exact = 21d0/20d0

print*, '         n      ', '       simpson       ', '             error'
do i = 0, nt
    table(i,0) = simpson(n,a,b,f)
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

double precision function simpson(n,a,b,f)
implicit none
double precision :: a,b,f,dx,sum,fvalue
integer :: n,i

dx = (b-a)/dble(n)

sum = 0.0d0

do i = 1,n-1
    fvalue = f(dble(a+i*dx))
    if(mod(i,2) == 0) then
        sum = sum + 2*fvalue
    else 
        sum = sum + 4*fvalue
    end if

end do

simpson = dx/dble(3)*(f(a)+f(b)+sum)

return
end

