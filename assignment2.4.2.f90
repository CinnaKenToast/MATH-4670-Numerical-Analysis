program Assignment2_1
implicit none

double precision :: a,b,m, findM, f
integer :: i 

do
    print*, "choose a point A and a point B"
    read*, a,b
    print*, f(a),f(b)

    if(f(a) * f(b) < 0) then
        m = findM(a,b)
       exit
    end if

end do

do i = 1,25
    if(f(a)>0 .and. f(m)>0 .or. f(a)<0 .and. f(m)<0) then
        a = m
    else
        b = m
    end if

    m = findM(a,b)

    print*, i,m,f(m)
end do

end program Assignment2_1

double precision function findM(a,b)
implicit none
double precision :: a,b

findM = (a+b)/2

return
end function findM

double precision function f(x)
implicit none
double precision::x, term, sum
integer::i

term = -1d0
sum = 0d0

do i = 1,10000
    term = term * (-x**2/((2*i+3)*(2*i+2)))
    sum = sum + term
end do

f = sum
return
end function