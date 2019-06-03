program assignment3_2
implicit none
integer :: n,i,nd
double precision, allocatable, dimension(:) :: xdata, fdata, fprimedata, a, xdouble, fdouble, fpdouble

open(unit = 9, file = 'input.data', status = 'old')
read(9,*) n

nd=(n+1)*2-1

allocate(xdata(0:n), fdata(0:n), fprimedata(0:n), a(0:nd),xdouble(0:nd), fdouble(0:nd), fpdouble(0:nd))

do i = 0, n
    read(9,*) xdata(i), fdata(i), fprimedata(i)
end do
close(9)

call showdata(n, xdata, fdata, fprimedata)
do i = 0, nd
    if(mod(i,2) == 1) then
        xdouble(i) = xdata(i/2)
        fdouble(i) = fdata(i/2)
        fpdouble(i)= fprimedata(i/2)
    else
        xdouble(i) = xdata(i/2)
        fdouble(i) = fdata(i/2)
        fpdouble(i)= fprimedata(i/2) 
    end if
end do 

call compute_coefficients(nd, xdouble, fdouble, fpdouble, a)
print*, 'Computed Hermite coefficients:'

do i = 0, nd
    print*, a(i)
end do

do i = 0, nd
    if(mod(i,2) == 1) then
        xdouble(i) = xdata(i/2)
        fdouble(i) = fdata(i/2)
        fpdouble(i)= fprimedata(i/2)
    else
        xdouble(i) = xdata(i/2)
        fdouble(i) = fdata(i/2)
        fpdouble(i)= fprimedata(i/2) 
    end if
end do 

call validate(nd,xdouble, fdouble, a)

call makegraphs(nd, xdouble, fdouble, a)
deallocate(a, xdata, fdata, fprimedata, xdouble, fdouble, fpdouble)
end

subroutine showdata(n, xdata, fdata, fprimedata)
implicit none
integer, intent(in) :: n
integer :: i
double precision, intent(in), dimension(0:n) :: xdata, fdata, fprimedata

print*, 'n= ', n
do i = 0,n
    print*, i, xdata(i), fdata(i), fprimedata(i)
end do

return
end

subroutine compute_coefficients(n, xdata, fdata, fprimedata, a)
implicit none
integer, intent(in) :: n
integer :: i, k

double precision, intent(in) :: xdata(0:n), fdata(0:n), fprimedata(0:n)
double precision :: a(0:n)
double precision, allocatable :: t(:,:)

allocate(t(0:n,0:n))

t(:,0) = fdata

k=1
do i = k, n
    if(mod(i,2) == 1) then
        t(i,k) = fprimedata(i)
    else
        t(i,k) = (t(i,k-1) - t(i-1,k-1))/(xdata(i)-xdata(i-k))
    end if
end do

do k = 2, n
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
integer::i,nd
double precision, intent(in),dimension(0:n)::xdata,fdata,a
double precision::p,x

print*,'Validating. Printing x, f(x) and f(x)-p(x)'
print*,'If all is well the third column should be zeros'
print*,'At least up to rounding errors'

do i = 0,n
    x = xdata(i)
    if(mod(i,2)==0) then
        print*, xdata(i),fdata(i), fdata(i)-p(n,a,xdata,x)
    end if
end do

return 
end

double precision function p(n,a,xdata,x)
implicit none
integer,intent(in)::n
double precision, intent(in), dimension(0:n)::xdata,a
double precision, intent(in)::x
double precision xmult
integer::i



p=a(0)
xmult = 1
do i = 1, n

    xmult = xmult*(x-xdata(i-1))

    p = p + a(i) * xmult
    
    
end do
    
return 
end

subroutine makegraphs(n,xdata,fdata,a)
implicit none
integer,intent(in)::n
double precision, dimension(0:n),intent(in)::xdata,fdata,a
double precision::x,p,dx,big,small
integer::i,nplot

print*,'Creating a file "data2" for plotting with gnuplot'
open(unit = 9, file = 'data', status = 'replace')
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

print*, 'Creating file "poly2" a plot of the interpolating polynomial'
nplot = 1000
dx=(big-small)/dble(nplot)
open(unit=10,file='poly',status='replace')
do i = 0,nplot
    x=small+dble(i)*dx
    write(10,*) x,p(n,a,xdata,x)
end do
close(10)

return 
end