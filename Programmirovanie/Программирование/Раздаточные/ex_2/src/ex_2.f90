program exercise_2
   implicit none
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0
   real(R_)                :: x = 0, y = 0, z = 0, w = 0, f = 0

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) x, y, z, w
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(4(a, f0.2/))") "x = ", x, "y = ", y, "z = ", z, "w = ", w
      eval: block
         if (x>0 .and. z<5) then
            f = x*x + w
         else if (x>0 .and. z>=5) then
            f = cos(z) + x*y
         else if (x<0 .and. z>5) then
            f = w + cos(y)*x
         else
            write (Out, "('f is indetermined')")
            exit eval
         end if
         write (Out, "('f = ', f0.2)") f
      end block eval
   close (Out)
end program exercise_2
