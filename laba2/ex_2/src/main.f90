program exercise_2
   use environment
   use IEEE_Arithmetic
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: x = 0, x1 = 0, x2 = 0, x3 = 0, fval = 0
   real(R_)                :: y1 = 0, y2 = 0, y3 = 0

   open (file=input_file, newunit=In)
      read (In, *) x, x1, y1, x2, y2, x3, y3
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(4(a, f0.2/))") "x = ", x, "x1 = ", x1, "y1 = ", y1, "x2 = ", x2, "y2 = ", y2, "x3 = ", x3, "y3 = ", y3
   close (Out)
   
   fval = F(x, x1, y1, x2, y2, x3, y3)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, "('f = ', f0.2)") fval
   close (Out)

   !fval = FImpure(x, y, z, w)

contains
   ! Чистая функция.
   pure function F(x, x1, y1, x2, y2, x3, y3)
      real(R_) x, x1, y1, x2, y2, x3, y3
      intent(in)  F, x, x1, y1, x2, y2, x3, y3
 
      if (x>0 .and. z<5) then
         F = x*x + w
      else if (x>0 .and. z>=5) then
         F = cos(z) + x*y
      else if (x<0 .and. z>5) then
         F = w + cos(y)*x
      else
         ! Присвоение не числа -- NaN.
         F = IEEE_Value(x, IEEE_Quiet_NaN)
      end if
   end function F
   
   ! Нечистая функция с вычислениями и с вводом/выводом одновременно.
   ! Так реализовывать не нужно.
   ! Демонстирует применение блока БЕЗ использования меток.
   !function FImpure(x, y, z, w) result(F)
      !real(R_) F, x, y, z, w
      !intent(in)  x, y, z, w
 
      !open (file=output_file, encoding=E_, newunit=Out, position='append')
         !eval: block
            !if (x>0 .and. z<5) then
      !         F = x*x + w
        !    else if (x>0 .and. z>=5) then
         !      F = cos(z) + x*y
          !  else if (x<0 .and. z>5) then
           !    F = w + cos(y)*x
           ! else
           !    write (Out, "('f is indetermined')")
           !   exit eval
           ! end if
           ! write (Out, "('f = ', f0.2)") F
        ! end block eval
      !close (Out)
   !end function FImpure
end program exercise_2
