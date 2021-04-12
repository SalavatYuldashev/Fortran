program LabaNumberOne
implicit none

real(4) :: y, x=0.9
real(4) :: summand1=1, summand2, summand3, summand4
character (len=7) :: cosX = "cos(x)=" 
summand2 = x**2/factorial(2) 
summand3 = x**4/factorial(4) 
summand4 = x**6/factorial(6) 
y = summand1 - summand2 + summand3 - summand4
print *, y
print *, cos(x)
write(*,*) cosX, summand1, '-', summand2, '+', summand3, '-', summand4


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
	
end program LabaNumberOne





