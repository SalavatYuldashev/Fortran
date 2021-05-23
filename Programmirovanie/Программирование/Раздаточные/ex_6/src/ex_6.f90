program exercise_6
   implicit none
   integer, parameter      :: R_ = 16
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, n = 0
   real(R_)                 :: sin_x = 0, new_sin_x = 0, x = 0, r = 0, q = 0, x_2 = 0

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) x
   close (In)
   
   r         = x
   new_sin_x = r
   n         = 1
   x_2       = x * x
   do
      q           = - x_2 / (2*n*(2*n + 1))
      r           = r * q
      sin_x       = new_sin_x
      new_sin_x   = sin_x + r
      n           = n + 1
      ! Продолжать, пока очередное приближение не изменится.
      if (new_sin_x == sin_x) &
         exit
   end do

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(4(a, T16, "= ", f9.6/))') 'x', x, "Sin(x)", sin_x, "Fortran Sin(x)", Sin(x), "Error", sin_x - Sin(x)
   close (Out)
end program exercise_6
