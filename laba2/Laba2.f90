program Laba2
  include 'var.f90'
  include 'subroutineFPLI.f90'
  implicit none

  character(len=3):: option, yes, no

  yes = 'y'
  no = 'n'

  100 print *, 'Введите x1, x2, x3 соответствующих условию x1<x2<x3'
  read(*,*) x1, x2, x3
    if ((x1 < x2) .and. (x2 <x3)) then
      print *, 'Введите y1, y2, y3 соответствующих x1, x2, x3'
      read(*,*) y1, y2, y3
      print *, 'Введите значние аргумента x в промежутке от ', x1, ' до ', x3
      read(*,*) x
        if (x >= x1 .and. x<=x3) then
	       CALL FPLI
	    else 
	      goto 300
	    end if
    else 
      print *, 'Введенные значения x1, x2, x3  не соответствуют условию \"x1<x2<x3\"'&
      'попробуйте снова'
      print *, 'Для повтора введите \"y\", для завершения \"n\"'
      read(*,*) option
        if (option .eq. yes)then
          goto 100
	    else 
	      contunue 400
        end if
    end if
  
contains

contunue  400
150 contunue print *, 'Значения x1, x2, x3 заданы не корректно &'
!goto 400
200 contunue  print *, 'Значения y1, y2, y3 заданы не корректно &'
!goto 400
300 contunue print *, 'Значениe агрумента x задано не корректно&'
!goto 400
400 contunue print *, 'Программа завершена!'
end program Laba2
