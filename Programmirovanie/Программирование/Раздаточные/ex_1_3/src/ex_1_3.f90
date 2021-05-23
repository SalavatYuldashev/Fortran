program exercise_1_3
   implicit none
   integer, parameter         :: R_ = 8
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt", E_ = "UTF-8"
   character(:), allocatable  :: form
   integer                    :: In = 0, Out = 0
   real(R_)                   :: x = 0, x_1 = 0, ln_x = 0, item = 0

  
   open (file=input_file, encoding=E_, newunit=In)
      read (In, *) x
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      form = "(a, T7, '= ', f0.2)"
      write (Out, form) "x", x
      write (Out, form)
   
      x_1   = x - 1
      item  = x_1
      ln_x  = item
      
      write (Out, form) "item", item
      write (Out, form) "ln(x)", ln_x
      ! То же самое, но неэффектвно:
      !write (Out, "(/'item', T7, '=', f5.2)") item
      !write (Out, "('ln(x)', T7, '=', f5.2)") ln_x
      
      item  = - x_1 * x_1 / 2
      ln_x  = ln_x + item
      
      write (Out, form) "item", item
      write (Out, form) "ln(x)", ln_x
     
      item  = - item * x_1 * 2 / 3
      ln_x  = ln_x + item
      
      write (Out, form) "item", item
      write (Out, form) "ln(x)", ln_x
      
      item  = - item * x_1 * 3 / 4
      ln_x  = ln_x + item
      
      write (Out, form) "item", item
      write (Out, form) "ln(x)", ln_x
      ! Проверка:
      !write (Out, form) "error", log(x) - ln_x
   close (Out)
end program exercise_1_3
