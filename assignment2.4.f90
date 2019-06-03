program assignment2_4

double precision::x(0:40), f
integer::i

x(0) = 2.0d0
x(1) = 10.0d0

print*, 0, x(0), f(x(0))
print*, 1, x(1), f(x(1))

do i = 1,40
    x(i+1) = x(i) - f(x(i))*(x(i)-x(i-1))/(f(x(i))-f(x(i-1)))
    print*, i+1, x(i+1), f(x(i+1))
end do

end program

double precision function f(x)
implicit none
double precision::x, term, sum
integer::i

term = -1d0
sum = 0d0

do i = 0,10
    term = term * (-x**2/((2*i+3)*(2*i+2)))
    sum = sum + term
end do

f = sum
return
end function