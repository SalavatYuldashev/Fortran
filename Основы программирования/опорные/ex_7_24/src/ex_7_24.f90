program exercise_7_24
   implicit none
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0, j = 0, k = 0
   real(R_), allocatable   :: A(:, :), A_k(:)
   logical, allocatable    :: Mask(:, :)

   ! Ввод данных.
   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) N, M
      allocate (A(N, M))
      ! В императивном стиле:
      do i = 1, N
         do j = 1, M
            read (In, '(f5.1)', advance='no') A(i, j)
         end do
         read (In, *)
      end do

     ! ! В регулярном стиле:
     ! read (In, *) (A(i, :), i = 1, N)
     
     read (In, *) k
   close (In)

   ! Вывод данных.
   open (file=output_file, encoding=E_, newunit=Out)
      ! В императивном стиле:
      do i = 1, N
         do j = 1, M
            write (Out, '(f5.1)', advance='no') A(i, j)
         end do
         write (Out, *)
      end do

     ! ! В регулярном стиле:
     ! ! При изменении формы массива нужно поменять 4 или запрограммировать формат
     ! ! или взять из лаб. работ конкатенацию целого со строкой, тогда:
     ! ! '(' // N // 'f5.1)' будет интерпретироваться как '(Nf5.1)', что на прямую писать нельзя.
     ! write (Out, '(4f5.1)') (A(i, :), i = 1, N)
     
     write (Out, *) k
   close (Out)
      
   ! Вывод требуемых данных.
  ! ! В императивном стиле:
  ! open (file=output_file, encoding=E_, newunit=Out, position='append')
  !    ! Проход по столбцам -- как расположена матрица в памяти.
  !    do j = 1, M
  !       do i = 1, N
  !          if (Mod(i+j, k) == 0) &
  !             write (Out, '(f5.1)', advance='no') A(i, j)
  !       end do
  !    end do
  ! close (Out)

   ! В регулярном стиле:
   ! Формирование маски из необходимых элементов.
   allocate (Mask(N, M), source=.false.)
   ! 1 способ -- затратный для построения маски.
   forall (i = 1:N, j = 1:M, Mod(i+j, k) == 0) &
      Mask(i, j) = .true.
   ! 2 способ -- эффективный для построения маски.
   !forall (i = 1:k) &
   !   Mask(i::k, mod(k, i)::k) = .true.
   A_k = Pack(A, Mask)
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '(f5.1)') A_k
   close (Out)
end program exercise_7_24
