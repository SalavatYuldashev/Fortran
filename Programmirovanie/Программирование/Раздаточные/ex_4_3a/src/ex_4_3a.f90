program exercise_4_3a
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, N = 0, j = 0
   real(R_)                :: a = 0, b = 0, h = 0, I = 0, x = 0
   !real(R_), allocatable   :: X(:)

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) a, b, h
   close (In)
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "a", a, "b", b, "h", h
   close (Out)
  
   N = Int((b - a) / h + .5_R_)

   ! В императивном стиле:
   x = a
   do j = 1, N
      I = I + .8_R_*x*Exp(-(x*x + .5_R_))
      x = x + h
   end do
   I = I * h

  ! ! В регулярном стиле:
  ! allocate (X(N))
  ! forall (j = 1:N) &
  !    X(j) = a + (j-1)*h
  ! I = Sum(.8_R_* X * Exp(-(X*X + .5_R_))) * h

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(a, T4, "= ", f0.4)') "I", I
   close (Out)
   
end program exercise_4_3a
