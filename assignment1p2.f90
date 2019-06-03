!Numerical Assignment 1 pt2
program sumCheck

real :: s, sum, oldSum
integer :: i
s = 0.0
sum = 0

do i = 1,1000000

oldSum = sum
sum = sum + 1/float(i**2)

    if(sum == oldsum) then

        print*, "An impossible thing is about to happen at the next index"
        print*, "I got through the series until the index", i
        print*, "The sum is", sum
        stop

    end if

end do

end program