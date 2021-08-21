program exercise_4_2a
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, Nx = 0, i = 0
   real(R_)                :: x1 = 0, x2 = 0, dx = 0
   real(R_), allocatable   :: X(:), F(:)

   open (file=input_file, newunit=In)
      read (In, *) x1, x2, dx
   close (In)

   open (file=output_file, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "x1", x1, "x2", x2, "dx", dx
   close (Out)
   
   Nx = NInt((x2-x1) / dx) + 1
  
   ! Размещение данных в НАЧАЛЕ работы программы,
   ! а не внутри процедуры при КАЖДОМ её вызове.
   allocate (X(Nx), F(Nx))
   
   call TabF(x1, dx, X, F)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '("   x", T8, "|", T12, "f")')
      !write (Out, '(f0.4, T8, "| ", f0.4)') ((X(i), F(i)), i = 1, Nx)
      write (Out, '(f0.4, T8, "| ", f0.4)') X(1:5), F(1:5)
   close (Out)

contains
   pure subroutine TabF(x1, dx, X, F)
      real(R_)    x1, dx, X(:), F(:)
      intent(in)  x1, dx
      intent(out) X, F
      integer     i

      ! Формирование X = [x1, x2, x3, ...].
      X = [(x1 + dx*(i-1), i = 1, Nx)]
     
      ! Формирование F = [F(x1,y1), F(x1,y2), F(x1,y3), ..., F(x2,y1), F(x2,y2), F(x2,y3), ...].
      ! F(1) == F(x1, y1), F(2) == F(x1, y2), .... F(Ny) = F(x1, yNy), ...
      F = [(Sin(X(i)) / (Cos(X(i)) + 1), i = 1, Nx)]
   end subroutine TabF
end program exercise_4_2a
