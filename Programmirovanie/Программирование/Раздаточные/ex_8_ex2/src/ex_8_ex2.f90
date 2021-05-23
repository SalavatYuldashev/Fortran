program exercise_8_ex2
   implicit none
   integer, parameter      :: R_ = 4
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
   real(R_), allocatable   :: B(:, :), C(:)
   character(10)           :: form

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) N, M
      allocate (B(N, M))
      ! В регулярном стиле:
      write (form, '(a, i0, a)') '(', M, 'f5.1)'
      read (In, form) (B(i, :), i = 1, N)
   close (In)
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, form) (B(i, :), i = 1, N)
   close (Out)
   
   allocate (C(N))
   ! В регулярном стиле:
   forall (i = 1:N) &
      C(i) = BMax(B, i)

   open (file=output_file, encoding=E_, newunit=Out, position='append')
      ! В регулярном стиле:
      write (Out, '(f5.1)') C
   close (Out)

contains
   pure real(R_) function BMax(A, l)
      real(R_)    A(:, :)
      integer     l
      intent(in)  A, l

      integer i

      BMax = A(l, 1)
      do i = 2, UBound(A, 2)
         if (A(l, i) > BMax) &
            BMax = A(l, i)
      end do
   end function BMax
end program exercise_8_ex2
