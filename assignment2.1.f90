program Assignment2_1
implicit none

real :: a,b,m, findM, f
integer :: i 

do
    print*, "choose a point A and a point B"
    read*, a,b

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

real function findM(a,b)
implicit none
real :: a,b

findM = (a+b)/2

return
end function findM

real function f(x)
implicit none
real::x

f=2*sin(x)

end function