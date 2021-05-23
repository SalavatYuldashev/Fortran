program exercise_4_2a
   implicit none
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, Nx = 0, Ny = 0, i = 0, j = 0!, N = 0
   real(R_)                :: x1 = 0, x2 = 0, dx = 0, y1 = 0, y2 = 0, dy = 0, f = 0, x = 0, y = 0
   !real(R_), allocatable   :: X(:), Y(:), Small_Y(:), F(:)

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) x1, x2, dx
      read (In, *) y1, y2, dy
   close (In)
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "x1", x1, "x2", x2, "dx", dx
      write (Out, '(3(a, T4, "= ", f0.4/))') "y1", y1, "y2", y2, "dy", dy
   close (Out)
   
   Nx = Int((x2-x1) / dx + 0.5) + 1
   Ny = Int((y2-y1) / dy + 0.5) + 1
  
   ! В императивном стиле:
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '("   x", T8, "|", T13, "y", T17, "|", T22, "f")')
      x = x1
      do i = 1, Nx
         y = y1
         do j = 1, Ny
            f = Sin(x-y) / (Cos(x)*Cos(y))
            write (Out, '(f0.4, T8, "| ", f0.4, T17, "| ", f0.4)') x, y, f
            y = y + dy
         end do
         x = x + dx
      end do
   close (Out)
   
  ! ! В регулярном стиле:
  ! N = Nx * Ny
  ! allocate (X(N), Y(N), Small_Y(Ny), F(N))
  ! 
  ! ! Формирование X = [x1, x1, x1, x2, x2, x2, ...].
  ! forall (i = 1:Nx) &
  !    X(1+Ny*(i-1):Ny*i) = x1 + dx*(i-1)
  ! 
  ! ! Формирование Y = [y1, y2, y3, y1, y2, y3, ...].
  ! forall (j = 1:Ny) &
  !    Small_Y(j) = y1 + dy*(j-1)
  ! forall (j = 1:Nx) &
  !    Y(1+Ny*(j-1):Ny*j) = Small_Y
  ! ! Вот так лучше не делать: тут присваивания проходят по не рядом расположенным в памяти элементам:
  ! !forall (j = 1:Ny) &
  ! !   Y(j::Ny) = y1 + dy*(j-1)
  ! 
  ! ! Формирование F = [F(x1,y1), F(x1,y2), F(x1,y3), F(x2,y1), F(x2,y2), F(x2,y3), ...].
  ! f = Sin(x-y) / (Cos(x)*Cos(y))
  ! 
  ! open (file=output_file, encoding=E_, newunit=Out, position='append')
  !    write (Out, '("   x", T8, "|", T13, "y", T17, "|", T22, "f")')
  !    write (Out, '(f0.4, T8, "| ", f0.4, T17, "| ", f0.4)') (x(i), y(i), f(i), i = 1, Nx*Ny)
  ! close (Out)
end program exercise_4_2a
