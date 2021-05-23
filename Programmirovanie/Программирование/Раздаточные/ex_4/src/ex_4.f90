program exercise_4
   implicit none
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, N = 0, i = 0
   real(R_)                :: x1 = 0, x2 = 0, h = 0, f = 0, x = 0
   !real(R_), allocatable   :: X(:), F(:)

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) x1, x2, h
   close (In)
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "x1", x1, "x2", x2, "h", h
   close (Out)
   
   N = Int((x2-x1) / h + 0.5)+ 1
  
   ! В императивном стиле:
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      x = x1
      write (Out, '("  X   |   f")')
      do i = 1, N
         f = (Sin(x) + 1) / Sin(x)
         write (Out, '(f0.4, T7, "| ", f0.4)') x, f
         x = x + h
      end do
   close (Out)
   
   ! В регулярном стиле:
   !allocate (X(N), F(N))
   !forall (i = 1:N) &
   !   X(i) = x1 + h*(i-1)
   !f = (Sin(x) + 1) / Sin(x)
   !open (file=output_file, encoding=E_, newunit=Out, position='append')
   !   write (Out, '("  X   |   f")')
   !   write (Out, '(f0.4, T7, "| ", f0.4)') (X(i), F(i), i = 1, N)
   !close (Out)
end program exercise_4
