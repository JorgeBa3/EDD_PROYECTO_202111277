program main
  use json_module
  implicit none

  type(json_file) :: json
  logical :: found
  integer :: id, img_g, img_p, num_pasadas, i
  character(len=:),allocatable :: nombre, texto
  character(len=100) :: id_str, nombre_json  ! Assuming a maximum of 10 characters for id string

  ! initialize the class
  call json%initialize()

  ! read the file
  print *, 'Ingrese el nombre del  archivo JSON:'
  read(*, '(A)') nombre_json
  call json%load(filename = nombre_json)

  ! print the file to the console
  call json%print()

  ! Obtener la cantidad de registros en el JSON
  num_pasadas = 0
  do i = 1, 1000 ! Suponemos que hay como máximo 1000 registros para evitar bucles infinitos
    write(id_str, '(I10)') i  ! Convertir entero a cadena
    call json%get('['//trim(adjustl(id_str))//'].id', id, found)
    if (.not. found) exit ! Si no se encuentra el índice, salimos del bucle
    num_pasadas = num_pasadas + 1
  end do

  ! Imprimir la cantidad de registros
  print *, 'Cantidad de registros:', num_pasadas

 ! Realizar el bucle desde 1 hasta num_pasadas
  do i = 1, num_pasadas
    write(id_str, '(I10)') i  ! Convert integer to string
    call json%get('['//trim(adjustl(id_str))//'].id', id, found)
    print*, 'id: ', id, found
    if (.not. found) stop 11

    write(id_str, '(I10)') i  ! Convert integer to string
    call json%get('['//trim(adjustl(id_str))//'].nombre', nombre, found)
    print*, 'nombre: ', nombre, found
    if (.not. found) stop 12

    write(id_str, '(I10)') i  ! Convert integer to string
    call json%get('['//trim(adjustl(id_str))//'].img_g', img_g, found)
    print*, 'img_g: ', img_g, found
    if (.not. found) stop 13

    write(id_str, '(I10)') i  ! Convert integer to string
    call json%get('['//trim(adjustl(id_str))//'].img_p', img_p, found)
    print*, 'img_p: ', img_p, found
    if (.not. found) stop 14
  end do

  ! Limpiar
  call json%destroy()
  if (json%failed()) stop 4
end program main
