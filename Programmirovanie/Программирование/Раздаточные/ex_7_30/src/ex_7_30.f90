program exercise_7_30
   implicit none
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
   real(R_), allocatable   :: A(:, :)
   logical, allocatable    :: NotNullRows(:, :), NotNullCols(:, :)
   character(10)           :: form

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) N, M
      allocate (A(N, M))
      ! В регулярном стиле:
      write (form, '(a, i0, a)') '(', M, 'f5.1)'
      read (In, form) (A(i, :), i = 1, N)
   close (In)
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, form) (A(i, :), i = 1, N)
   close (Out)
   
   ! В регулярном стиле:
   allocate (NotNullRows(N, 1), NotNullCols(1, M))
   NotNullRows(:, 1) = Any(A /= 0, dim=2)  ! Позиции ненулевых строк (N, 1). 
   NotNullCols(1, :) = Any(A /= 0, dim=1)  ! Позиции ненулевых столбцов (1, M).
   N = Count(NotNullRows) ! Ниже можно было сразу использовать Count, но N и M используются в выводе.
   M = Count(NotNullCols)
   A = Reshape( Pack(A, MatMul(NotNullRows, NotNullCols)), [N, M] )
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
     ! В регулярном стиле:
      write (Out, *)
      write (form, '(a, i0, a)') '(', M, 'f5.1)'
      write (Out, form) (A(i, :), i = 1, N)
   close (Out)
end program exercise_7_30
