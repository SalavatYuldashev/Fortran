program exercise_7_14a
   implicit none
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, N = 0, i = 0, j = 0
   real(R_), allocatable   :: Z(:, :)
   !logical, allocatable    :: Upper(:, :)
   real(R_)                :: s = 0

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) N
      allocate (Z(N, N))
      ! В императивном стиле:
      do i = 1, N
         do j = 1, N
            read (In, '(f5.1)', advance='no') Z(i, j)
         end do
         read (In, *)
      end do

     ! ! В регулярном стиле:
     ! read (In, *) (Z(i, :), i = 1, N)
   close (In)

   ! В императивном стиле:
   ! Проход по столбцам -- как расположена матрица в памяти.
    
   do j = 2, N
      do i = 1, j-1
            s = s + Z(i, j)
      end do
   end do
   
   ! Так лучше не делать -- вводим избыточные итерации и проверки:
   !do j = 1, N
   !   do i = 1, N
   !      if (j > i) then
   !         s = s + Z(i, j)
   !      end if
   !   end do
   !end do
 
  ! ! В регулярном стиле:
  ! ! Формирование матрицы-маски, где верхняя треугольная матрица истина.
  ! allocate (Upper(N, N), source=.false.)
  ! ! Помечаем верхнюю треугольную матрицу.
  ! forall (i = 1:N, J = 1:N, j > i) &
  !    Upper(i, j) = .true.
  ! ! Находим сумму элементов по сформированной маске.
  ! s = Sum(Z, Upper)
   
   open (file=output_file, encoding=E_, newunit=Out)
      ! В императивном стиле:
      do i = 1, N
         do j = 1, N
            write (Out, '(f5.1)', advance='no') Z(i, j)
         end do
         write (Out, *)
      end do

     ! ! В регулярном стиле:
     ! ! При изменении формы массива нужно поменять 4 или запрограммировать формат
     ! ! или взять из лаб. работ конкатенацию целого со строкой, тогда:
     ! ! '(' // N // 'f5.1)' будет интерпретироваться как '(Nf5.1)', что на прямую писать нельзя.
     ! write (Out, '(4f5.1)') (Z(i, :), i = 1, N)
      
      write (Out, '(a, T5, "= ", f9.6)') "Sum", s
   close (Out)
end program exercise_7_14a
