program main
  use json_module
  implicit none

  type(json_file) :: json
  logical :: found
  integer :: id, img_g, img_p, num_pasadas, i, opcion
  character(len=:),allocatable :: nombre, texto
  character(len=100) :: id_str, nombre_json  ! Assuming a maximum of 10 characters for id string

    ! initialize the class
  call json%initialize()

  do
    call mostrar_menu_principal()
    read(*,*) opcion

    select case(opcion)
        case(1)
            call submenu_parametros()
        case(2)
            call ejecutar_paso()
        case(3)
            call estado_memoria()
        case(4)
            call generar_reportes()
        case(5)
            call acerca_de()
        case(6)
            exit
        case default
            print *, 'Opcion no valida. Por favor, seleccione una opcion valida.'
    end select
end do

  

  ! Limpiar
  call json%destroy()
  if (json%failed()) stop 4




  contains

  subroutine mostrar_menu_principal()
      print *, '--- Menu ---'
      print *, '1. Parametros iniciales'
      print *, '2. Ejecutar paso'
      print *, '3. Estado en memoria de las estructuras'
      print *, '4. Reportes'
      print *, '5. Acerca de'
      print *, '6. Salir'
      print *, 'Ingrese su opcion:'
  end subroutine mostrar_menu_principal

  subroutine submenu_parametros()
      implicit none
      integer :: opcion_parametros

      do
          call mostrar_submenu_parametros()
          read(*,*) opcion_parametros

          select case(opcion_parametros)
              case(1)
                  call carga_masiva_clientes()
              case(2)
                  call cantidad_ventanillas()
              case(3)
                  exit
              case default
                  print *, 'Opcion no valida. Por favor, seleccione una opcion valida.'
          end select
      end do
  end subroutine submenu_parametros
  

      subroutine mostrar_submenu_parametros()
          print *, '--- Parametros iniciales ---'
          print *, '1. Carga masiva de clientes'
          print *, '2. Cantidad de ventanillas'
          print *, '3. Volver al menu principal'
          print *, 'Ingrese su opcion:'
      end subroutine mostrar_submenu_parametros

      subroutine carga_masiva_clientes()
          print *, 'Ha seleccionado Carga masiva de clientes'
          ! read the file
  print *, 'Ingrese el nombre del  archivo JSON:'
  read(*, '(A)') nombre_json
  call json%load(filename = nombre_json)

  ! print the file to the console
  call json%print()

  ! Obtener la cantidad de registros en el JSON
  num_pasadas = 0
  do i = 1, 1000 ! Suponemos que hay como maximo 1000 registros para evitar bucles infinitos
    write(id_str, '(I10)') i  ! Convertir entero a cadena
    call json%get('['//trim(adjustl(id_str))//'].id', id, found)
    if (.not. found) exit ! Si no se encuentra el indice, salimos del bucle
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
      end subroutine carga_masiva_clientes

      subroutine cantidad_ventanillas()
          print *, 'Ha seleccionado Cantidad de ventanillas'
          ! Aqui puedes incluir el codigo para establecer la cantidad de ventanillas
      end subroutine cantidad_ventanillas


  subroutine ejecutar_paso()
      print *, 'Ha seleccionado Ejecutar paso'
      ! Aqui puedes incluir el codigo para ejecutar un paso
  end subroutine ejecutar_paso

  subroutine estado_memoria()
      print *, 'Ha seleccionado Estado en memoria de las estructuras'
      ! Aqui puedes incluir el codigo para mostrar el estado en memoria
  end subroutine estado_memoria

  subroutine generar_reportes()
      print *, 'Ha seleccionado Reportes'
      ! Aqui puedes incluir el codigo para generar reportes
  end subroutine generar_reportes

  subroutine acerca_de()
      print *, 'Ha seleccionado Acerca de'
      print *, 'Jorge Alejandro De Leon Batres - 202111277'
  end subroutine acerca_de
end program main
