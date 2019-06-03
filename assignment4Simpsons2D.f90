module stuff
integer :: nx, ny
double precision :: a, b, inlower, inupper, f, exact, pi
external inlower, inupper, f
end module stuff

program simpson2d
use stuff
implicit none
double precision:: simpson2
pi = 4*atan(1.0d0)
nx = 12
ny = 14


!exact = 184d0/21d0

print*, simpson2()!, simpson2()-exact

end program

double precision function f(x,y)
implicit none
double precision :: x,y

f = (3*x+2*y)*exp(x**2+y**2)

return
end

double precision function simpson2()
use stuff
implicit none
double precision :: sum,dx, x, fxvalue, inner
integer :: i

a = 0.0d0
b = pi/4d0
sum = 0.0d0
dx = (b-a)/dble(nx)

do i = 1,nx-1
    fxvalue = inner(dble(a+i*dx))
    if(mod(i,2) == 0) then
        sum = sum + 2*fxvalue
    else 
        sum = sum + 4*fxvalue
    end if

end do

simpson2 = dx/dble(3)*(inner(a)+inner(b)+sum)
return
end

double precision function inner(x)
use stuff
implicit none
double precision :: x, c, d, dy,sum,fyvalue
integer :: i

sum = 0.0d0
c = inlower(x)
d = inupper(x)
dy = (d-c)/dble(ny)

do i = 1,ny-1
    fyvalue = f(x,dble(c+i*dy))
    if(mod(i,2) == 0) then
        sum = sum + 2*fyvalue
    else 
        sum = sum + 4*fyvalue
    end if

end do

inner = dy/dble(3)*(f(x,d)+sum)

return 
end

double precision function inupper(x)
implicit none
double precision :: x

inupper = cos(x)

return 
end

double precision function inlower(x)
implicit none
double precision :: x

inlower = sin(x)

return
end

