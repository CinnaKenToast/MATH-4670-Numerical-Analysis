!Kenenth Shipley
!MATH 4670 
!Assignment 1

program test

real :: a,b,c,d,p1,p2,p3,q1,q2,q3
real :: dist,denom,num

print*, "Enter an a, b, c, and d for the plane given by the equation ax+by+cz=d:"
read*, a,b,c,d

print*, "Enter the x, y, and z coordinates of a point P:"
read*, p1,p2,p3

q1 = 1
q2 = 1
q3 = (d-a-b)/c

num = abs(a*(p1-q1)+b*(p2-q2)+c*(p3-q3))

denom = sqrt(a*a+b*b+c*c)

dist = num/denom

print*, "The distance from the point to the plane is ", dist

end program