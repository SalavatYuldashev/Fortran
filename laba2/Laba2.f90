program Laba2
implicit none

real(4) :: x1, x2, x3
real(4) :: y1, y2, y3
real(4) :: x, y
character(len=1):: option, yes, no

yes = 'y'
no = 'n'

100 print *, 'Введите x1, x2, x3 соответствующих условию x1<x2<x3'
read(*,*) x1, x2, x3, ERR 150

  if ((x1 < x2) .and. (x2 <x3)) then
    print *, 'Введите y1, y2, y3 соответствующих x1, x2, x3'
    read(*,*) y1, y2, y3, ERR 200
    print *, 'Введите значние аргумента x в промежутке от ', x1, ' до ', x3
    read(*,*) x, ERR 300
      if ((x >= x1) .and. (x<=x3))
	    piecewiseLinearInterpolation(x)
	  else 
	    goto 300
	  end if
  else 
    print *, 'Введенные значения x1, x2, x3  не соответствуют условию \"x1<x2<x3\"&
    попробуйте снова'
    print *, 'Для повтора введите \"y\", для завершения \"n\"'
    read(*,*) option ERR 400
      if (option .eq. yes)then
        goto 100
	  else
	    goto 400
      end if
  end if
  
contains
		
real(4) function piecewiseLinearInterpolation(x) 
    implicit none
    if((x >= x1) .and. (x <=x2)) then
	  y = y1 + (x-x1)/(x2-x1)*(y2-y1)
	  print *,'y= ', y
	elseif ((x >= x2) .and. (x <=x3)) then
	  y = y2 + (x-x2)/(x3-x2)*(y3-y2)
	  print *,'y= ', y
	end if
end function piecewiseLinearInterpolation
GOTO 400
150 print *, 'Значения x1, x2, x3 заданы не корректно &'
GOTO 400
200 print *, 'Значения y1, y2, y3 заданы не корректно &'
GOTO 400
300 print *, 'Значениe агрумента x задано не корректно&'
GOTO 400
400 print *, 'Программа завершена!'
end program Laba2
