program exercise_5
   implicit none
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, N = 0, i = 0, M = 0
   integer                 :: S = 0
   integer, allocatable    :: Z(:)
   !logical, allocatable    :: Pos(:)

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) N
      allocate (Z(N))

      ! В регулярном стиле:
      read (In, *) Z
   close (In)

   ! В императивном стиле:
   do i = 1, N
      If (Z(i) > 0) then
         S = S + Z(i)
         M = M + 1
      end if
   end do

  ! ! В регулярном стиле:
  ! Pos = Z > 0
  ! S = Sum(Z, Pos)
  ! M = Count(Pos)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "(i0)") Z
      write (Out, *)
      write (Out, '(2(a, T12, "= ", i0/))') 'Pos. items', M, "Sum", S
   close (Out)
end program exercise_5
