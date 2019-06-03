program assignment4_1simpson
implicit none
double precision:: x, newtons

x= newtons()
print*, 'Simpson f(x) =', x

end program

double precision function newtons()
implicit none
double precision::f, fprime, x
integer::i

x = 0.5d0

do i = 1,5
    x = x - f(x)/fprime(x)
    print*, i, x, f(x)
end do

newtons = x
return
end

double precision function f(x)
implicit none
double precision :: x,y,a,b,inner,dx,sum,fvalue
integer :: i,n

a = 0.0d0
b = x
n = 100

dx = (b-a)/dble(n)

do i = 1,n-1
    fvalue = inner(dble(a+i*dx))
    if(mod(i,2) == 0) then
        sum = sum + 2*fvalue
    else 
        sum = sum + 4*fvalue
    end if

end do

f = dx/dble(3)*(inner(a)+inner(b)+sum) - 0.45d0
end

double precision function inner(x)
implicit none
double precision :: x, pi

pi = 4 * atan(1d0)

inner = (1/sqrt(2d0*pi))*exp(-x**2/2)

return
end

double precision function fprime(x)
implicit none
double precision :: x, pi

pi = 4*atan(1d0)

fprime = (1/sqrt(2d0*pi))*exp(-x**2/2)

return
end