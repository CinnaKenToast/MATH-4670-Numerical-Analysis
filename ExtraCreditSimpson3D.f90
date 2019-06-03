module stuff
implicit none
integer :: nx, ny, nz
double precision :: a, b, inlower, inupper, func, exact, pi
external inlower, inupper, func
end module stuff

program simpson3d
use stuff
implicit none
double precision:: simpson3
pi = 4*atan(1.0d0)
nx = 1000
ny = 1000
nz = 1000

print*, simpson3()

end program

double precision function func(x,y,z)
implicit none
double precision :: x,y,z

func = (x+y+z)

return
end

double precision function simpson3()
use stuff
implicit none
double precision :: sum,dx, x, fxvalue, mid
integer :: i

a = 0.0d0
b = 3.0d0
sum = 0.0d0
dx = (b-a)/dble(nx)

do i = 1,nx-1
    fxvalue = mid(dble(a+i*dx))
    if(mod(i,2) == 0) then
        sum = sum + 2*fxvalue
    else 
        sum = sum + 4*fxvalue
    end if

end do

simpson3 = dx/dble(3)*(mid(a)+mid(b)+sum)
return
end

double precision function mid(x)
use stuff
implicit none
double precision :: x, c, d, dy,sum,fyvalue
double precision, external :: inner, midlower, midupper
integer :: i

sum = 0.0d0
c = midlower(x)
d = midupper(x)
dy = (d-c)/dble(ny)

do i = 1,ny-1
    fyvalue = inner(x,dble(c+i*dy))
    if(mod(i,2) == 0) then
        sum = sum + 2*fyvalue
    else 
        sum = sum + 4*fyvalue
    end if

end do

mid = dy/dble(3)*(inner(x,c)+inner(x,d)+sum)

return 
end

double precision function inner(x,y)
use stuff
implicit none
double precision :: x, y, e, f, dz,sum,fzvalue
integer :: i

sum = 0.0d0
e = inlower(x,y)
f = inupper(x,y)
dz = (f-e)/dble(nz)

do i = 1,nz-1
    fzvalue = func(x,y,dble(e+i*dz))
    if(mod(i,2) == 0) then
        sum = sum + 2*fzvalue
    else 
        sum = sum + 4*fzvalue
    end if

end do

inner = dz/dble(3)*(func(x,y,e)+func(x,y,f)+sum)
return 
end

double precision function midupper(x) 
implicit none
double precision :: x

midupper = 2.0d0

return 
end

double precision function midlower(x)
implicit none
double precision :: x

midlower = 0.0d0
return
end

double precision function inupper(x,y)
implicit none
double precision :: x,y

inupper = 1.0d0

return 
end

double precision function inlower(x,y)
implicit none
double precision :: x,y

inlower = 0.0d0

return
end

