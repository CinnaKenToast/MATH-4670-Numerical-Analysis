!interpolate (1,1)(3,-2)(5,4)(8,5)
!graph data and polynomial
program assignment3_2
implicit none
integer :: n,i
double precision, allocatable, dimension(:) :: xdata, fdata, a

open(unit = 9, file = 'input1.data', status = 'old')
read(9,*) n

allocate(xdata(0:n), fdata(0:n), a(0:n))

do i = 0, n
    read(9,*) xdata(i), fdata(i)
end do
close(9)

call showdata(n, xdata, fdata)
call compute_coefficients(n, xdata, fdata, a)
print*, 'Computed Newton form coefficients:'

do i = 0, n
    print*, a(i)
end do

call validate(n, xdata, fdata, a)

call makegraphs(n, xdata, fdata, a)
deallocate(a, xdata, fdata)
end

subroutine showdata(n, xdata, fdata)
implicit none
integer, intent(in) :: n
integer :: i
double precision, intent(in), dimension(0:n) :: xdata, fdata

print*, 'n= ', n
do i = 0,n
    print*, i, xdata(i), fdata(i)
end do

return
end

subroutine compute_coefficients(n, xdata, fdata, a)
implicit none
integer, intent(in) :: n
integer :: i, k

double precision, intent(in) :: xdata(0:n), fdata(0:n)
double precision :: a(0:n)
double precision, allocatable :: t(:,:)

allocate(t(0:n,0:n))

t(:,0) = fdata

do k = 1, n
    do i = k, n
        t(i,k) = (t(i,k-1) - t(i-1,k-1))/(xdata(i)-xdata(i-k))
    end do
end do

do i = 0,n
    a(i) = t(i,i)
end do

deallocate(t)
return
end

subroutine validate(n,xdata,fdata,a)
implicit none
integer,intent(in)::n
integer::i
double precision, intent(in),dimension(0:n)::xdata,fdata,a
double precision::p,x

print*,'Validating. Printing x, f(x) and f(x)-p(x)'
print*,'If all is well the third column should be zeros'
print*,'At least up to rounding errors'

do i = 0,n
    x = xdata(i)
    print*, xdata(i),fdata(i), fdata(i)-p(n,a,xdata,x)
end do

return 
end

double precision function p(n,a,xdata,x)
implicit none
integer,intent(in)::n
double precision, intent(in), dimension(0:n)::xdata,a
double precision, intent(in)::x
integer::i

p=a(n)
do i = n-1,0,-1
    p = p*(x-xdata(i)) + a(i)
end do

return 
end

subroutine makegraphs(n,xdata,fdata,a)
implicit none
integer,intent(in)::n
double precision, dimension(0:n),intent(in)::xdata,fdata,a
double precision::x,p,dx,big,small
integer::i,nplot

print*,'Creating a file "data" for plotting with gnuplot'
open(unit = 9, file = 'data21', status = 'replace')
do i = 0,n
    write(9,*) xdata(i),fdata(i)
end do
close(9)

big = xdata(0)
small = xdata(0)

do i = 1,n
    if(xdata(i) > big) big = xdata(i)
    if(xdata(i) < small) small = xdata(i)
end do

print*, 'Creating file "poly" a plot of the interpolating polynomial'
nplot = 1000
dx=(big-small)/dble(nplot)
open(unit=10,file='poly21',status='replace')
do i = 0,nplot
    x=small+dble(i)*dx
    write(10,*) x,p(n,a,xdata,x)
end do
close(10)

return 
end