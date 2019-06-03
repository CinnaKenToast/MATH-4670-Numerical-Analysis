program assignment2_3

double precision::x(0:15), f
integer::i

x(0) = 0.1d0
x(1) = 0.9d0

print*, 0, x(0), f(x(0))
print*, 1, x(1), f(x(1))

do i = 1,15
    x(i+1) = x(i) - f(x(i))*(x(i)-x(i-1))/(f(x(i))-f(x(i-1)))
    print*, i+1, x(i+1), f(x(i+1))
end do

end program

double precision function f(x)
implicit none
double precision::x

f=((1+x)/2)*((x/(1-x+x**2))**21)-0.5

return
end function