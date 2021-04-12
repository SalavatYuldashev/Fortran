program Laba2
implicit none

100 real(4) :: x1, x2, x3
real(4) :: y1, y2, y3
real(4) :: x, y, midleY1, midleY2
print *, 'Введите x1, x2, x3 соответствующих условию x1<x2<x3'
read(*,*) x1, x2, x3
print *, 'Введите y1, y2, y3 соответствующих x1<x2<x3'
read(*,*) y1, y2, y3
if ((x1 < x2) .and. (x2<x3)) then
  midleY1 = y1 + (x-x1)/(x2-x1) * (y2 - y1)
  midleY2 = y2 + (x-x2)/(x3-x2) * (y3 - y2)
else 
  print *, 'Введенные x1, x2, x3  не соответствуют условию x1<x2<x3&
   попробуйте снова'
  goto 100
end if

!real(4) :: summand1=1, summand2, summand3, summand4
!character (len=7) :: cosX = "cos(x)=" 
!summand2 = x**2/factorial(2) 
!summand3 = x**4/factorial(4) 
!summand4 = x**6/factorial(6) 
!y = summand1 - summand2 + summand3 - summand4
!print *, y
!print *, cos(x)
!write(*,*) cosx, summand1, '-', summand2, '+', summand3, '-', summand4
if()
 
contains
		
integer(4) function factorial(n) 
implicit none

integer(4) :: n, fact, i
fact=1
do i=1, n
fact=fact*i
end do

factorial = fact

end function factorial
	
end program Laba2
