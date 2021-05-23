program exercise_7_21
   implicit none
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0, j = 0, N_max_neg = 0, N_min_pos = 0
   real(R_), allocatable   :: C(:, :)
   real(R_)                :: max_neg = 0, min_pos = 0
   integer, allocatable    :: Ind(:, :), Ind_max_neg(:, :), Ind_min_pos(:, :)
   logical, allocatable    :: Mask_max_neg(:, :), Mask_min_pos(:, :)
   character(10)           :: format

   ! Ввод данных.
   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) N, M
      allocate (C(N, M))
     ! ! В императивном стиле:
     ! do i = 1, N
     !    do j = 1, M
     !       read (In, '(f5.1)', advance='no') A(i, j)
     !    end do
     !    read (In, *)
     ! end do

      ! В регулярном стиле:
      read (In, *) (C(i, :), i = 1, N)
   close (In)

   ! Вывод данных.
   open (file=output_file, encoding=E_, newunit=Out)
     ! ! В императивном стиле:
     ! do i = 1, N
     !    do j = 1, M
     !       write (Out, '(f5.1)', advance='no') C(i, j)
     !    end do
     !    write (Out, *)
     ! end do

      ! В регулярном стиле:
      write (format, '(a, i0, a)') "(", M, "f5.1)"
      write (Out, format) (C(i, :), i = 1, N)
   close (Out)
   
   ! В императивном стиле:
   ! Поиск наибольшего из отрицательных.
   max_neg = MaxVal(C, C < 0)
   ! Поиск наименьшего из положительных.
   min_pos = MinVal(C, C > 0)

   ! Формируем двумерный массив индексов:
   ! | 1 | 1 |
   ! | 2 | 1 |
   ! ...
   ! | N | 1 |
   ! ...
   ! | 1 | 2 |
   ! | 2 | 2 |
   ! ...
   ! | N | 2 |
   ! ...
   ! | 1 | M |
   ! | 2 | M |
   ! ...
   ! | N | M |
   allocate (Ind(N*M, 2))
   Ind(:, 1) = [((i, i = 1, N), j = 1, M)]
   Ind(:, 2) = [((j, i = 1, N), j = 1, M)]

   ! Получаем маску для элементов равных наибольшему из отрицательных.
   Mask_max_neg   = C == max_neg
   N_max_neg      = Count(Mask_max_neg)
   ! Получаем маску для элементов равных наименьшему из положительных.
   Mask_min_pos   = C == min_pos
   N_min_pos      = Count(Mask_min_pos)

   ! Формируем массив индексов, удовлктворяющих заданной маске:
   ! 1. Разворачиваем маску по столбцам:  One_dim_mask = Reshape(Mask_max_neg, [N*M]).
   ! 2. Добавляем второй столбец в маске: Two_dim_mask = Spread(One_dim_mask, 2, 2)).
   ! 3. Запаковываем индексы по столбцам: Ind_by_col   = Pack(Ind, Two_dim_mask).
   ! 4. формируем индексы в два столбца:  Ind_max_neg  = Reshape( Ind_by_col, [Count(Mask_max_neg), 2])
   ! Одним оператором:
   Ind_max_neg = Reshape( Pack(Ind, Spread(Reshape(Mask_max_neg, [N*M]), 2, 2)), [N_max_neg, 2])
   Ind_min_pos = Reshape( Pack(Ind, Spread(Reshape(Mask_min_pos, [N*M]), 2, 2)), [N_min_pos, 2])

   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      ! В императивном стиле:
      write (Out, '(a, f5.1)') "Наибольшие из отрицательных:", max_neg
      write (Out, '(2i2)') (Ind_max_neg(i, :), i = 1, N_max_neg)
      write (Out, '(a, f5.1)') "Наименьшие из положительных:", min_pos
      write (Out, '(2i2)') (Ind_min_pos(i, :), i = 1, N_min_pos)
   close (Out)
end program exercise_7_21
