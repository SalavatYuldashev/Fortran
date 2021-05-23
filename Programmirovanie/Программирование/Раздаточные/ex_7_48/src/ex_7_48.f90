program exercise_7_48
   implicit none
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, N = 0, i = 0, j = 0
   real(R_), allocatable   :: A(:, :), B(:)
   character(10)           :: form

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) N
      allocate (A(N, N))
      ! В регулярном стиле:
      write (form, '(a, i0, a)') '(', N, 'f5.1)'
      read (In, form) (A(i, :), i = 1, N)
   close (In)
   open (file=output_file, encoding=E_, newunit=Out)
      ! В регулярном стиле:
      write (Out, form) (A(i, :), i = 1, N)
   close (Out)
   
   ! В регулярном стиле:
   ! Размер результирующего вектора = общее число элементов за вычетом диагональных, делённое пополам.
   allocate (B((N*N - N) / 2))
   ! 1. Задача попроще - формирование вектора B из ВСЕХ элементов матрицы A, чтением по строкам.
   ! B = [A(1, 1), A(1, 2), ..., A(2, 1), A(2, 2), ...]
   ! k   | i, j | k(i,j)
   ! 1   | 1, 1 | j
   ! 2   | 1, 2 | j
   ! ... | ... (N штук)
   ! N+1 | 2, 1 | j + N
   ! N+2 | 2, 2 | j + N
   ! ... | ... (N штук)
   ! 
   ! Поэтому k = j + N*(i-1)
   ! 
   ! 2. Исходная задача - формирование вектора B из наддиагональных элементов матрицы A, чтением по строкам.
   ! Можно использовать предыдущую формулу, но вычитать из неё ко-во не вошедших элементов
   ! (из 1-ой строки не вошёл 1 элемент, из 2-ой -- 2 и т. д.):
   ! S(i) = 1 + 2, ..., i = (1+i)/i/2, поэтому k = j - N*(i-1) - (1+i)*i/2 ! делим на 2 в последнюю очередь.
   forall (i = 1:N, j = 1:N, j > i) &
      B(j + N*(i-1) - (1+i)*i/2) = A(i, j)
   ! или
   !B = [(Pack(A(i, i+1:), .true.), i = 1, N)]
   ! или
   !B = [(Reshape(A(i, i+1:), [N-i]), i = 1, N)]

   open (file=output_file, encoding=E_, newunit=Out, position='append')
     ! В регулярном стиле:
      write (Out, *)
      write (form, '(a, i0, a)') '(', Size(B), 'f5.1)'
      write (Out, form) B 
   close (Out)
end program exercise_7_48
