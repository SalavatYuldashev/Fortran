
subroutine FPLI 
  include 'var.f90'
    if((x >= x1) .and. (x <=x2)) then
      y = y1 + (x-x1)/(x2-x1)*(y2-y1)
      print *,'y= ', y
    else if ((x >= x2) .and. (x <=x3)) then
      y = y2 + (x-x2)/(x3-x2)*(y3-y2)
      print *,'y= ', y
    end if
end subroutine FPLI 
