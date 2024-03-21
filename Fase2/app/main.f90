program main
  use matrix_m
  implicit none
  
  type(matrix) :: m

  call m%insert(1, 14, "#000000")
  call m%insert(2, 14, "#000000")
  
  call m%insert(4, 14, "#000000")

  call m%insert(1, 15, "#000000")
  call m%insert(3, 15, "#000000")
  call m%insert(4, 15, "#000000")

  call m%insert(1, 16, "#000000")
  call m%insert(4, 16, "#000000")

  call m%insert(1, 17, "#000000")
  call m%insert(4, 17, "#000000")

  call m%insert(2, 18, "#000000")
  call m%insert(4, 18, "#000000")

  call m%insert(2, 19, "#000000")

  call m%insert(3, 20, "#000000")

  call m%insert(3, 21, "#000000")

  call m%insert(4, 22, "#000000")

  call m%insert(5, 10, "#000000")

  call m%insert(5, 11, "#000000")

  call m%insert(5, 16, "#000000")

  call m%insert(5, 17, "#000000")

  call m%insert(5, 19, "#000000")

  call m%insert(5, 20, "#000000")

  call m%insert(5, 23, "#000000")
  
  call m%graficar()
  call m%print()
end program main
