program assignment2_2
double precision::x(0:25), f
integer::i

x(0) = 0.2d0
x(1) = 2.5d0

print*, 0, x(0), f(x(0))
print*, 1, x(1), f(x(1))

do i = 1,25
    x(i+1) = x(i) - f(x(i))*(x(i)-x(i-1))/(f(x(i))-f(x(i-1)))
    print*, i+1, x(i+1), f(x(i+1))
end do

end program

double precision function f(x)
implicit none
double precision::x

f=-32.12*((sinh(x)-sin(x))-3.4*x**2)

return
end function