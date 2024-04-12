program MenuPrincipal
    use List_of_list_m
    use abb_m
    use avl_m
    use json_module
    use btree_m
    
    use matrix_m
    use cliente_m

    implicit none
    type(btree) :: mi_arbol
    type(cliente) :: cliente1
    type(List_of_list) :: lista_usuarios
    type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: listPointer, clientePointer, attributePointer, attributePointer2, capasPointer
        !Variable para arbol de capas
    integer, dimension(:), allocatable :: capas, imgs
    
    type(abb) :: arbol, nuevo_arbol
    type(matrix) :: matriz
    type(avl) :: arbolavl
    type(pixel) :: info, info2
  logical :: found
  type(json_value), pointer ::  capaPointer, pixelPointer
    
    integer :: id_capa, fila, columna, j,id
    character(len=:), allocatable ::color, nombre_album
  integer :: opcion, num_pasadas, i, size
  
  character(len=40) :: nombre_json, id_str
  
  ! Variables para el registro de usuarios
  character(len=:), allocatable :: nombre_completo, password_usuario, str_fila, str_columna, dpi
  character(len=:), allocatable :: nombre_cliente, password



  call json%initialize()
  
  do
      print *, "---- MENu PRINCIPAL ----"
      print *, "1. Inicio de Sesion"
      print *, "2. Registro de Usuarios"
      print *, "3. Acerca de"
      print *, "4. Salir"
      print *, "Seleccione una opcion: "
      read(*, *) opcion
      
      select case(opcion)
      case(1)
          call IniciarSesion()
      case(2)
          call RegistrarUsuario()
      case(3)
          call AcercaDe()
      case(4)
          exit
      case default
          print *, "Opcion no valida. Por favor, seleccione una opcion valida."
      end select
  end do
  
contains

function string_to_integer(str)
    character(len=*), intent(in) :: str
    integer :: string_to_integer
    integer(kind=8) :: i, num

    ! Inicializar num a cero
    num = 0

    do i = 1, len(str)
        if (str(i:i) >= '0' .and. str(i:i) <= '9') then
            num = num * 10 + ichar(str(i:i)) - ichar('0')
        else
            ! Si se encuentra un carácter no numérico, imprimir el mensaje de error y salir de la funcion
            print *, "Error: El string contiene caracteres no numéricos."
            string_to_integer = -1
            return
        end if
    end do

    ! Devolver el número solo si la conversion se realizo correctamente
    string_to_integer = num
end function string_to_integer

  subroutine carga_masiva_usuarios()
    integer(kind=8):: int_dpi
      print *, 'Ha seleccionado Carga masiva de clientes'
      ! read the file
      print *, 'Ingrese el nombre del  archivo JSON:'
      read(*, '(A)') nombre_json
      
      call json%load(filename=nombre_json)
      
      ! print the file to the console
      call json%print()
      
      call json%info('', n_children=size)
      
      call json%get_core(jsonc)
      call json%get('', listPointer, found)
      
      do i = 1, size
        call jsonc%get_child(listPointer, i, clientePointer, found)
        
        !imprimiendo el atributo nombre_cliente
        call jsonc%get_child(clientePointer, 'nombre_cliente', attributePointer, found)
        call jsonc%get(attributePointer, nombre_cliente)
        print *, "Nombre del cliente: ", trim(nombre_cliente)

        !imprimiendo el atributo dpi
        call jsonc%get_child(clientePointer, 'dpi', attributePointer, found)
        call jsonc%get(attributePointer, dpi)
        print *, "DPI del cliente: ", dpi
        
        !imprimiendo el atributo password
        call jsonc%get_child(clientePointer, 'password', attributePointer, found)
        call jsonc%get(attributePointer, password)
          print *, "Contrasena del cliente: ", trim(password)
        int_dpi = string_to_integer(dpi)

          print *, "DPI del cliente (entero): ", int_dpi
        cliente1 = cliente(dpi=int_dpi, nombre=nombre_cliente, contrasena=password)


        call mi_arbol%insert(cliente1)

      end do
  end subroutine carga_masiva_usuarios
  
  subroutine IniciarSesion()
    implicit none
    character(len=20) :: usuario, contrasena
    logical :: login_successful

    ! Logica para el inicio de sesion
    print *, "Ingrese el nombre de usuario: "
    read(*, *) usuario
    print *, "Ingrese la contrasena: "
    read(*, *) contrasena

    ! Comprobamos si el usuario es el administrador
    if (usuario == 'admin' .and. contrasena == 'EDD2024') then
        print *, "Inicio de sesion exitoso como administrador."
        call MenuAdministrador()
        return
    end if
    print *, "No es el administrador."
    ! Buscamos el usuario en la lista de usuarios
    login_successful = mi_arbol%buscar_usuario(usuario, contrasena)

    ! Comprobar si el inicio de sesion fue exitoso
    if (login_successful) then
        print *, "Inicio de sesion exitoso."
        call MenuUsuario(usuario)
        return
    else
        print *, "Nombre de usuario o contrasena incorrectos."
    end if
    ! Si no se encontro el usuario en la lista, se muestra un mensaje de error
    print *, "Nombre de usuario o contrasena incorrectos."
end subroutine IniciarSesion

  subroutine MenuUsuario(usuario)
    implicit none
    character(len=20), intent(in) :: usuario
    
    integer :: opcion

    do
        call MostrarMenuUsuario(usuario)
        read(*, *) opcion
        
        select case(opcion)
        case(1)
            call reportes_estructuras()
        case(2)
            call gestion_imagenes()
        case(3)
            call opciones_carga_masiva()
        case(4)
            exit
        case default
            print *, "Opcion no valida. Por favor, seleccione una opcion valida."
        end select
    end do
    end subroutine MenuUsuario

    subroutine MostrarMenuUsuario(usuario)
        implicit none
        character(len=20), intent(in) :: usuario
        print *, "---- MENu Usuario ----" // usuario
        print *, "1. Visualizar reportes de las estructuras"
        print *, "2. Navegacion y gestion de imagenes."
        print *, "3. Opciones de carga masiva."
        print *, "4. Salir."
        print *, "Seleccione una opcion: "
    end subroutine MostrarMenuUsuario

  subroutine MostrarMenuAdministrador()
      print *, "---- MENu ADMINISTRADOR ----"
      print *, "1. arbol B de usuarios (Grafico)"
      print *, "2. Operaciones sobre los usuarios (insertar, modificar y eliminar)"
      print *, "3. Operaciones de carga masiva de usuarios."
      print *, "4. Reportes Adminsitrador."
      print *, "5, Salir"
      print *, "Seleccione una opcion: "
  end subroutine MostrarMenuAdministrador
  
  subroutine grafico_arbol_usuarios()
      print *, "Grafico del arbol B de usuarios."
      call mi_arbol%graphTree()
      
  end subroutine grafico_arbol_usuarios
  
  subroutine MostrarOperaciones()
      print *, "---- Operaciones sobre los usuarios ----"
      print *, "1. Registrar un nuevo usuario"
      print *, "2. Modificar usuario"
      print *, "3. Eliminar usuario"
      print *, "4. Salir."
      print *, "Seleccione una opcion: "
  end subroutine MostrarOperaciones

  subroutine operaciones_usuarios()
    implicit none
      integer :: opcion
      
      do
          call MostrarOperaciones()
          read(*, *) opcion
          
          select case(opcion)
          case(1)
              call RegistrarUsuario()
          case(2)
              call ModificarUsuario()
          case(3)
              call EliminarUsuario()
          case(4)
              exit
          case default
              print *, "Opcion no valida. Por favor, seleccione una opcion valida."
          end select
      end do
  end subroutine operaciones_usuarios

  subroutine ModificarUsuario()
    implicit none
character(len=20) :: nombre, password
character(len=20) :: nombre_nuevo, password_nuevo
print *, "Modificar Usuario."
print *, "Ingrese el nombre completo del  usuario: "
read(*, '(A)') nombre
print *, "Ingrese la contraseña del usuario: "
read(*, '(A)') password
print *, "Ingrese el nuevo nombre del usuario: "
read(*, '(A)') nombre_nuevo
print *, "Ingrese la nueva contraseña del usuario: "
read(*, '(A)') password_nuevo
call mi_arbol%modificar_usuario(nombre, password, nombre_nuevo, password_nuevo)


end subroutine ModificarUsuario

subroutine EliminarUsuario()
    implicit none
character(len=20) :: nombre, password
print *, "Eliminar Usuario."
print *, "Ingrese el nombre completo del  usuario: "
read(*, '(A)') nombre
print *, "Ingrese la contraseña del usuario: "
read(*, '(A)') password
call mi_arbol%eliminar_usuario(nombre, password)
end subroutine EliminarUsuario

  subroutine MenuAdministrador()
      implicit none
      integer :: opcion
      
      do
          call MostrarMenuAdministrador()
          read(*, *) opcion
          
          select case(opcion)
          case(1)
              call grafico_arbol_usuarios()
          case(2)
              call operaciones_usuarios()
          case(3)
            call carga_masiva_usuarios()
        case(4)
            call reportes_Admin()
        case(5)
            exit
          case default
              print *, "Opcion no valida. Por favor, seleccione una opcion valida."
          end select
      end do
  end subroutine MenuAdministrador
  
  subroutine reportes_Admin()
    implicit none
        integer :: opcion
        do
          call Mostrar_reportes_Admin()
          read(*, *) opcion
          
          select case(opcion)
          case(1)
              call reporte_uno()
          case(2)
              call mi_arbol%mostrarClientes()
          case(3)
              exit
          case default
              print *, "Opcion no valida. Por favor, seleccione una opcion valida."
          end select
        end do
  end subroutine reportes_Admin

  subroutine reporte_uno()
    implicit none
    integer(kind=8):: int_dpi
    
    print *, "Ingrese el DPI del usuario a reportar: "
    read*,int_dpi 
    call mi_arbol%buscarCliente(int_dpi)
  end subroutine reporte_uno
  
  subroutine Mostrar_reportes_Admin()
    print *, "Reportes del Administrador."
      print *, "1. Reporte un usuario."
        print *, "2. Reporte todos los usuarios."
        print *, "3. Salir."
        print *, "Seleccione una opcion: "
    end subroutine Mostrar_reportes_Admin
  subroutine RegistrarUsuario()
    implicit none
    character(len=40) :: nombre_completo, password_usuario
    integer(kind=8):: int_dpi2
      ! Logica para el registro de usuarios
      print *, "Ingrese el nombre completo del nuevo usuario: "
      read(*, '(A)') nombre_completo
      print *, "Ingrese el DPI del nuevo usuario: "
      read*,int_dpi2 
      print *, "Ingrese la contrasena para el nuevo usuario: "
      read(*, '(A)') password_usuario
        
      cliente1 = cliente(dpi=int_dpi2, nombre=nombre_completo, contrasena=password_usuario)
          call mi_arbol%insert(cliente1)
  end subroutine RegistrarUsuario
  
  subroutine AcercaDe
      implicit none
      
      print *, "Jorge Alejandro De Leon Batres."
  end subroutine AcercaDe

  subroutine reportes_estructuras()
        implicit none
        integer :: opcion
        do
            call MostrarMenuReportes()
            read(*, *) opcion
            
            select case(opcion)
            case(1)
                call arbolavl%graficar()
            case(2)
                call arbol%graph_abb("capas")
            case(3)
                call listarCapas()
            case(4)
                exit
            case default
                print *, "Opcion no valida. Por favor, seleccione una opcion valida."
            end select
        end do
  end subroutine reportes_estructuras

    subroutine listarCapas()
        print *, "Listado inorder de capas:"
        call arbol%inorder_abb()
        print *, "Listado Posorden de capas."
        call arbol%posorder_abb()
        print *, "Listado Preorden de capas."
        call arbol%preorder_abb()
    end subroutine listarCapas

  subroutine MostrarMenuReportes()
        print *, "---- Reportes de las estructuras ----"
        print *, "1. Reporte de arbol AVL"
        print *, "2. Reporte de arbol de capas"
        print *, "3. Reporte de lista de capas"
        print *, "4. Salir."
        print *, "Seleccione una opcion: "
    end subroutine MostrarMenuReportes

    subroutine gestion_imagenes()
        implicit none
        integer :: opcion
        do
            call MostrarMenuImg()
            read(*, *) opcion
            
            select case(opcion)
            case(1)
                call verImagen()
            case(2)
                call MenuOrden()
            case(3)
                call Amplitud()
            case(4)
                call ImgCapa()
            case(5)
                call eliminar_imagen()
            case(6)
                exit
            case default
                print *, "Opcion no valida. Por favor, seleccione una opcion valida."
            end select
        end do

    end subroutine gestion_imagenes

    subroutine eliminar_imagen()
        implicit none
        character:: nombre_a
        integer  :: id_img
        print *, "Ingrese el nombre del album: "
        read(*, *) nombre_a
        print *, "Ingrese el ID de la imagen: "
        read(*, *) id_img
    end subroutine eliminar_imagen

    subroutine mostrarMenuImg()
        print *, "---- Gestion de imagenes ----"
        print *, "1. Ver imagen"
        print *, "2. Menu de ordenamiento"
        print *, "3. Recorrido en amplitud"
        print *, "4. Imagen por capa"
        print *, "5. Eliminar Imagen."
        print *, "6. Salir."
        print *, "Seleccione una opcion: "

    end subroutine mostrarMenuImg

    subroutine verImagen()
        implicit none
        integer :: id_capa
        print *, "Ingrese el ID de la capa a visualizar: "
        read(*, *) id_capa
        call arbol%buscarCapa_abb(id_capa)
    end subroutine verImagen

    subroutine MenuOrden()
        implicit none
        integer :: opcion
        do
            call MostrarMenuOrden()
            read(*, *) opcion
            
            select case(opcion)
            case(1)
                call InOrden()
            case(2)
                call PreOrden()
            case(3)
                call PostOrden()
            case(4)
                exit
            case default
                print *, "Opcion no valida. Por favor, seleccione una opcion valida."
            end select
        end do
    end subroutine MenuOrden

    subroutine MostrarMenuOrden()
        print *, "---- Menu de ordenamiento ----"
        print *, "1. InOrden"
        print *, "2. PreOrden"
        print *, "3. PostOrden"
        print *, "4. Salir."
        print *, "Seleccione una opcion: "
    end subroutine MostrarMenuOrden

    subroutine InOrden()
        print *, "Recorrido InOrden"
        call arbol%inOrder()
    end subroutine InOrden

    subroutine PreOrden()
        print *, "Recorrido PreOrden"
        call arbol%preOrder()
    end subroutine PreOrden

    subroutine PostOrden()
        print *, "Recorrido PostOrden"
        call arbol%posOrder()
    end subroutine PostOrden

    subroutine Amplitud()
        print *, "Recorrido en amplitud"
        call arbol%amplitudOrden()
    end subroutine Amplitud

    subroutine ImgCapa()
        implicit none
        integer :: id_capa
        print *, "Ingrese el ID de la capa a visualizar: "
        read(*, *) id_capa
        call arbol%buscar(id_capa)
    end subroutine ImgCapa



    subroutine opciones_carga_masiva()
        implicit none
      integer :: opcion
      
      do
          call MostrarMenuCargaMasiva()
          read(*, *) opcion
          
          select case(opcion)
          case(1)
              call CargaCapas()
          case(2)
              call CargaImagenes()
          case(3)
              call CargaAlbumes()
          case(4)
              exit
          case default
              print *, "Opcion no valida. Por favor, seleccione una opcion valida."
          end select
      end do
    end subroutine opciones_carga_masiva

    subroutine MostrarMenuCargaMasiva()
        print *, "---- MENu Carga Masiva ----"
        print *, "1. Carga de capas"
        print *, "2. Carga de imagenes"
        print *, "3. Carga de albumes"
        print *, "4. Salir."
        print *, "Seleccione una opcion: "
    end subroutine MostrarMenuCargaMasiva

    subroutine CargaCapas()
    integer :: int_id_capa

    print *, 'Ha seleccionado Carga masiva de capas'
    ! read the file
    print *, 'Ingrese el nombre del archivo JSON:'
    read(*, '(A)') nombre_json

    call json%load(filename=trim(nombre_json))

    ! print the file to the console
    call json%print()
    call json%info('', n_children=size)
      
    call json%get_core(jsonc)
    call json%get('', listPointer, found)
    
    

    do i = 1, size
        call jsonc%get_child(listPointer, i, capaPointer, found)
        
        ! Obtener el id_capa
        call jsonc%get_child(capaPointer, 'id_capa', attributePointer, found)
        call jsonc%get(attributePointer, id_capa)
        print *, "ID de Capa: ", id_capa
        ! Obtener los pixeles de la capa
        call jsonc%get_child(capaPointer, 'pixeles', pixelPointer, found)
        ! Obtener la cantidad de elementos en el arreglo de pixeles
        call jsonc%info(pixelPointer, n_children=num_pasadas)
        ! Iterar sobre cada pixel
        do j = 1, num_pasadas
            call jsonc%get_child(pixelPointer, j, attributePointer2, found)
            ! Obtener fila del pixel
            call jsonc%get_child(attributePointer2, 'fila', attributePointer, found)
            call jsonc%get(attributePointer, fila)
            ! Obtener columna del pixel
            call jsonc%get_child(attributePointer2, 'columna', attributePointer, found)
            call jsonc%get(attributePointer, columna)
            ! Obtener color del pixel
            call jsonc%get_child(attributePointer2, 'color', attributePointer, found)
            call jsonc%get(attributePointer, color)
            
            ! Imprimir informacion del pixel
            print *, "Pixel", id_capa,"- Fila:", fila, ", Columna:", columna, ", Color:", color
            info = pixel(fila, columna, color)
            call arbol%insert_abb(id_capa, info)
            
        end do
    end do
    call arbol%graph_abb("capas")
end subroutine CargaCapas

subroutine CargaImagenes()
    
    print *, 'Ha seleccionado Carga masiva de Imagenes'
    ! leer el nombre del archivo JSON
    print *, 'Ingrese el nombre del archivo JSON:'
    read(*, '(A)') nombre_json
    
    call json%load(filename=nombre_json)
    
    ! imprimir el archivo en la consola
    call json%print()
    
    call json%info('', n_children=size)
    
    call json%get_core(jsonc)
    call json%get('', listPointer, found)
    
    do i = 1, size
        call jsonc%get_child(listPointer, i, clientePointer, found)
        
        !imprimiendo el atributo id
        call jsonc%get_child(clientePointer, 'id', attributePointer, found)
        call jsonc%get(attributePointer, id)
        print *, "ID de la capa: ", id

        !imprimiendo el atributo capas
        call jsonc%get_child(clientePointer, 'capas', capasPointer, found)
        call jsonc%get(capasPointer, capas)
        print *, "Capas asociadas: ", capas
        call jsonc%info(capasPointer, n_children=num_pasadas)
        do j = 1, num_pasadas
            print *, "num capa: ", capas(j)
            info2 = arbol%buscar_abb(capas(j))
            call nuevo_arbol%insert_abb(capas(j), info2)
        end do

        call arbolavl%insert(id, nuevo_arbol)
        ! imprimir las capas asociadas
    end do
end subroutine CargaImagenes


    subroutine CargaAlbumes()
        
    print *, 'Ha seleccionado Carga masiva de albumes'
    ! leer el nombre del archivo JSON
    print *, 'Ingrese el nombre del archivo JSON:'
    read(*, '(A)') nombre_json
    
    call json%load(filename=nombre_json)
    
    ! imprimir el archivo en la consola
    call json%print()
    
    call json%info('', n_children=size)
    
    call json%get_core(jsonc)
    call json%get('', listPointer, found)
    
    do i = 1, size
        call jsonc%get_child(listPointer, i, clientePointer, found)
        
        !imprimiendo el atributo nombre_album
        call jsonc%get_child(clientePointer, 'nombre_album', attributePointer, found)
        call jsonc%get(attributePointer, nombre_album)
        print *, "nombre_album: ", nombre_album

        !imprimiendo el atributo imgs
        call jsonc%get_child(clientePointer, 'imgs', capasPointer, found)
        call jsonc%get(capasPointer, imgs)
        print *, "imgs: ", imgs
        call jsonc%info(capasPointer, n_children=num_pasadas)
        do j = 1, num_pasadas
            print *, "num imgs: ", imgs(j)
            call lista_usuarios%insert_album(nombre_album,imgs(j))
        end do
        ! imprimir las capas asociadas
        
    end do
    call lista_usuarios%printList()

    end subroutine CargaAlbumes



  
end program MenuPrincipal
