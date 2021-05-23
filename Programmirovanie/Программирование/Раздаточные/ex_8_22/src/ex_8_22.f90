program exercise_8_22
   implicit none
   integer, parameter      :: R_ = 8
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                 :: In = 0, Out = 0, N = 0, j = 0
   real(R_)                :: p1 = 0, p2 = 0, delta_p = 0
   real(R_), allocatable   :: I(:), P(:)

   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) p1, p2, delta_p
   close (In)
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(3(a, T4, "= ", f0.4/))') "p1", p1, "p2", p2, "delta_p", delta_p
   close (Out)

   ! В регулярном стиле:
   N = Int((p2 - p1) / delta_p + 0.5) + 1
   allocate (P(N), I(N))
   forall (j = 1:N) &
      P(j) = p1 + delta_p*(j-1)
   I = Integral(P)
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '("  p    |   I")')
      write (Out, '(f0.4, T8, "| ", f0.4)') (P(j), I(j), j = 1, N)
   close (Out)

contains
   elemental real(R_) function Integral(p)
      real(4), intent(in) :: p

      real(R_), parameter  :: a = 0, b = 1
      integer, parameter   :: N = 100
      integer              :: i
      real(R_)             :: X(N), h

      h = (b - a) / N
      ! В регулярном стиле:
      forall (i = 1:N) &
         X(i) = a + (j-1)*h
      Integral = Sum(F(p, X)) * h
   end function Integral

   pure function F(p, X)
      real(R_)    p, X(:)
      real(R_)    F(UBound(X, 1))
      intent(in)  p, X
      
      real(R_), parameter  :: q = 2.75_R_

      F = SqRt(p + q * X**2) / (3 + p*X + q*X**2)
   end function F
end program exercise_8_22
