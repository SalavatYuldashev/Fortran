program exercise_7_5a
   implicit none
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, i = 0, M = 0, MinInd = 0, N = 0
   real(R_), allocatable   :: A(:)
   real(R_)                :: tmp = 0
   logical, allocatable    :: Neg(:)
   character(8)            :: form

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) M
      allocate (A(M))
      ! В регулярном стиле:
      read (In, *) A
   close (In)
   open (file=output_file, encoding=E_, newunit=Out)
      write (form, '(a, i0, a)') '(', M, 'f5.1)'
      write (Out, form) A
   close (Out)
   
   ! В регулярном стиле:
   ! Проверять знак числа при сортировке очень дорого.
   ! Формируем маску для отрицательных эелементов.
   Neg = A < 0
   ! Помещаем отрицательные элементы в начало, остальные -- в конец.
   A = [Pack(A, Neg), Pack(A, .not. Neg)]
   ! Сортируем отрицательную часть массива A(1:N) методом выбора:
   ! 1. Находим в неотсортированной части A(i:N) индекс минимального элемента.
   ! 2. Меняем его с первым элементом в неотсортированной части -- A(i).
   ! 3. Отсортированная часть увеличилась. Повторяем п. 1.
   N = Count(Neg)
   do i = 1, N-1
      MinInd = MinLoc(A(i:N), 1) + i-1
      if (A(i) /= A(MinInd)) then
         tmp       = A(i)
         A(i)      = A(MinInd)
         A(MinInd) = tmp
      end if
   end do
   ! Распаковываем отрицательные элементы, а на другие ставим остальные.
   A = Unpack(A(:N), Neg, Unpack(A(N+1:), .not. Neg, A))
   ! Можно ещё так:
   ! A = Merge(Unpack(A(:N), Neg, A), Unpack(A(N+1:), .not. Neg, A), Neg)
   ! Или даже так:
   !where (Neg)
   !   A = Unpack(A(:N), Neg, A)
   !else
   !   A = Unpack(A(N+1:), .not. Neg, A)
   !end where
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
     ! В регулярном стиле:
      write (Out, *)
      write (Out, form) A
   close (Out)
end program exercise_7_5a
