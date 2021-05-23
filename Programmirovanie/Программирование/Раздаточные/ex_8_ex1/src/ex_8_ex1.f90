program exercise_8_ex1
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, N = 0, N_2 = 0, i = 0
   integer, allocatable    :: Z(:, :), Y(:)
   character(10)           :: format

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) N
   close (In)
   
   ! В регулярном стиле:
   N_2 = N * N
   allocate (Y(N_2))
   forall (i = 1:N_2) &
      Y(i) = i * i

   Z = Form(Y)
   
   open (file=output_file, encoding=E_, newunit=Out)
      ! В регулярном стиле:
      write (Out, *) N
      write (format, '(a, i0, a)') '(', N_2, 'i4)'
      write (Out, format) Y
      write (format, '(a, i0, a)') '(', N, 'i4)'
      write (Out, format) (Z(i, :), i = 1, N)
   close (Out)
   

   open (file=output_file, encoding=E_, newunit=Out, position='append')
     ! В регулярном стиле:
      write (Out, *)
   close (Out)
contains
   pure function Form(B) result(A)
      integer, allocatable :: A(:, :)
      integer, intent(in)  :: B(:)

      integer i, j, k, N

      N = Int(SqRt(Real(Ubound(B, 1) + 0.5)))
      allocate (A(N, N))
      k = 1
      do j = 1, N
         do i = 1, N
            A(i, j) = B(k)
            k = k + 1
         end do
      end do
   end function Form
end program exercise_8_ex1
