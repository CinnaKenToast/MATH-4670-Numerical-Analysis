program romberg
implicit none 
integer, parameter :: nt = 10
double precision :: a,b,f,trap,table(0:nt,0:nt),exact,bot
integer :: n,i,k
external f

n = 100
a = 0.0d0
b = 2.0d0
exact = 21d0/20d0
table = 0.0d0

print*, '         n      ', '       trapezoid       ', '             error'
do i = 0,nt
    table(i,0) = trap(n,a,b,f)
    print*, n, table(i,0), table(i,0) - exact
    n = n*2
end do

print*, 'Doing Romberg extrapolation'
print*
do k = 1,nt
    do i = k,nt
        bot = 4.0d0**k - 1.0d0
        table(i,k) = (4.0d0**k*table(i,k-1)-table(i-1,k-1))/bot
    end do
end do

print*,table(nt,nt), 'Romberg     ',table(nt,nt)-exact
print*
print*
print*,exact, 'is the "exact" value'

print*, 'Diagonal of Romberg table'
do i = 0,nt
    print*, table(i,i)
end do

end

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

do i = 1, n-1
    sum = sum + f(a + dble(i)*dx)
end do 

trap = dx/2.0d0*(f(a)+f(b)+2.0d0*sum)

return
end