program exercise_1 
   implicit none
   integer, parameter         :: R_ = 4
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   integer                    :: In = 0, Out = 0
   complex(R_)                :: x = (0, 0), y = (0, 0), z = (0, 0)
   character(:), allocatable  :: form

   open (file=input_file, encoding=E_, newunit=In)
      read (In, '(2f5.2)') x, y
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      form = "(a, f0.2, ', ', f0.2, ')')"
      write (Out, form) "x = (", x
      write (Out, form) "y = (", y
   close (Out)

   z = x / y
   
   open (file=output_file, encoding=E_, newunit=Out, position="append")
      write (Out, form)
      write (Out, form) "z = (", z
   close (Out)
end program exercise_1
