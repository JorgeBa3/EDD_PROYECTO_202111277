program main
    use hash_module
    use iso_fortran_env, only: int64
    use lista_adyacencia_m
    use abb_m
    use json_module
    implicit none
    character(20) :: username, contra
    integer :: opcion
    
    ! Definir el nombre de usuario y contrasena del administrador
    character(20) :: admin_username = 'a' !EDD1S2024
    character(20) :: admin_password = 'a' !ProyectoFase3
    type(abb) :: arbol_sucursales
    type(ListaAdyacencia) :: rutas
    type(hash) :: tabla

    call tabla%init(7, 13, 70)
    do
        print *, 'Ingrese su nombre de usuario:'
        read *, username
        print *, 'Ingrese su contrasena:'
        read *, contra
        
        if (username == admin_username .and. contra == admin_password) then
            print *, 'Bienvenido ', username
            call menu_principal()
            exit
        else
            print *, 'Usuario o contrasena incorrectos'
        end if
    end do
    
    contains
    function string_to_integer(str)
        character(len=*), intent(in) :: str
        integer(kind=16) :: string_to_integer
        integer :: i, num
    
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
    
    subroutine menu_principal()
    
        do
          print *, 'Menu principal'
          print *, '1. Carga de archivos'
          print *, '2. Sucursales'
          print *, '3. Reportes'
          print *, '4. Salir'
          print *, 'Seleccione una opcion:'
          read *, opcion
            select case(opcion)
                case(1)
                    call menu_carga_archivos()
                case(2)
                    call menu_sucursales()
                case(3)
                    call menu_reportes()
                case(4)
                    exit
                case default
                    print *, 'Opcion no valida'
            end select
        end do
    end subroutine menu_principal

    subroutine menu_carga_archivos()
        
        do
            print *, 'Seleccione una opcion:'
            print *, '1. Cargar archivo de sucursales'
            print *, '2. Cargar archivo de rutas'
            print *, '3. Regresar'
            read *, opcion
            
            select case(opcion)
                case(1)
                    call cargar_archivo_sucursales()
                case(2)
                    call cargar_archivo_rutas()
                case(3)
                    exit
                case default
                    print *, 'Opcion no valida'
            end select
        end do
    end subroutine menu_carga_archivos

    subroutine menu_sucursales()
        implicit none
        character(20) :: contras
        integer :: id
        type(sucursal) :: sucursal_info
        print *, 'Menu de sucursales'
        print *, 'Ingrese el ID de la sucursal'
        read *, id
        print *, 'Ingrese la contrasena de la sucursal'
        read *, contras

! Buscar la sucursal en el árbol ABB
        sucursal_info = arbol_sucursales%buscar_abb(id)
        print *, 'ID: ', sucursal_info%id
        print *, 'Password: ', sucursal_info%password
        if (sucursal_info%id /= -1 .and. sucursal_info%password == contras) then
            print *, 'Inicio de sesión exitoso para la sucursal ', sucursal_info%id
            call admin_sucursales()
            ! Aquí puedes continuar con las acciones que se pueden realizar después del login exitoso
        else
            print *, 'Inicio de sesión fallido. ID de sucursal o contraseña incorrectos.'
        end if

    end subroutine menu_sucursales

    subroutine admin_sucursales()
        implicit none
        integer :: opcion
        do
            print *, 'Menu de administrador de sucursales'
            print *, '1. Carga de Tecnicos'
            print *, '2. Generar recorrido mas optimo'
            print *, '3. Infomacion tecnico en especifico'
            print *, '4. Listar Tecnicos'
            print *, '5. Generar Reporte'
            print *, '6. Regresar'
            print *, 'Seleccione una opcion:'
            read *, opcion
            select case(opcion)
                case(1)
                    call carga_tecnicos()
                case(2)
                    print *, 'Recorrido mas optimo'
                case(3)
                    print *, 'Listar tecnico en especifico'
                case(4)
                    print *, 'Listar Tecnicos'
                case(5)
                    print *, 'Generar Reporte'
                case(6)
                    exit
                case default
                    print *, 'Opcion no valida'
            end select
        end do
    end subroutine admin_sucursales

    subroutine carga_tecnicos()
        implicit none
        type(persona) :: tecnico
        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: attributePointer, listPointer, tecnicoPointer
        character(100) :: nombre_json
        character(len=:), allocatable :: dpi
        character(len=:), allocatable :: nombre, apellido, genero, direccion, telefono
        integer :: size, i
        integer(kind=int64) :: dpi_int
        logical :: found
        
        call json%initialize()
        print *, 'Cargar archivo de Tecnicos'
        ! read the file
        print *, 'Ingrese el nombre del  archivo JSON:'
        read(*, '(A)') nombre_json
        
        call json%load(filename=nombre_json)
        print *, 'Cargando archivo JSON'
        ! print the file to the console
        call json%print()
        print *, 'Archivo cargado'
        call json%info('', n_children=size)
        
        call json%get_core(jsonc)
        call json%get('', listPointer, found)
        print *, "Size: ", size
        do i = 1, size
            call jsonc%get_child(listPointer, i, tecnicoPointer, found)
            !imprimiendo el atributo dpi
            call jsonc%get_child(tecnicoPointer, 'dpi', attributePointer, found)
            call jsonc%get(attributePointer, dpi)
            print *, "DPI tecnico: ", dpi

            !imprimiendo el atributo nombre
            call jsonc%get_child(tecnicoPointer, 'nombre', attributePointer, found)
            call jsonc%get(attributePointer, nombre)
            print *, "Nombre tecnico: ", nombre

            !imprimiendo el atributo apellido
            call jsonc%get_child(tecnicoPointer, 'apellido', attributePointer, found)
            call jsonc%get(attributePointer, apellido)
            print *, "Apellido tecnico: ", apellido

            !imprimiendo el atributo genero
            call jsonc%get_child(tecnicoPointer, 'genero', attributePointer, found)
            call jsonc%get(attributePointer, genero)
            print *, "Genero tecnico: ", genero

            !imprimiendo el atributo direccion
            call jsonc%get_child(tecnicoPointer, 'direccion', attributePointer, found)
            call jsonc%get(attributePointer, direccion)
            print *, "Direccion tecnico: ", direccion

            !imprimiendo el atributo telefono
            call jsonc%get_child(tecnicoPointer, 'telefono', attributePointer, found)
            call jsonc%get(attributePointer, telefono)
            print *, "Telefono tecnico: ", telefono

            read(dpi, *) dpi_int  
            tecnico = persona(dpi_int, nombre, apellido, genero, direccion, telefono)
            call tabla%insert_hash(tecnico)
            call tabla%show()
        end do
        print *, 'Tecnicos cargados exitosamente'
        call tabla%generate_dot_file()
        call tabla%show()
        call tabla%buscar_tecnico(dpi_int)
        call tabla%generate_dot_file_all_tecnicos()
    end subroutine carga_tecnicos

    subroutine menu_reportes()
      print *, 'Menu de reportes'
    end subroutine menu_reportes

    subroutine cargar_archivo_sucursales()
        implicit none
        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: attributePointer, listPointer, sucursalPointer
        type(sucursal) :: sucu
        character(100) :: nombre_json
        integer :: id
        character(len=:), allocatable :: departamento, direccion, password
        integer :: size, i
        logical :: found
        
        call json%initialize()
        print *, 'Cargar archivo de sucursales'
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
            call jsonc%get_child(listPointer, i, sucursalPointer, found)
            
            !imprimiendo el atributo id
            call jsonc%get_child(sucursalPointer, 'id', attributePointer, found)
            call jsonc%get(attributePointer, id)
            print *, "ID sucursal: ", id

            !imprimiendo el atributo departamento
            call jsonc%get_child(sucursalPointer, 'departamento', attributePointer, found)
            call jsonc%get(attributePointer, departamento)
            print *, "Departamento Sucursal: ", trim(departamento)
    
            !imprimiendo el atributo direccion
            call jsonc%get_child(sucursalPointer, 'direccion', attributePointer, found)
            call jsonc%get(attributePointer, direccion)
            print *, "Direccion Sucursal: ", trim(direccion)
            
            !imprimiendo el atributo password
            call jsonc%get_child(sucursalPointer, 'password', attributePointer, found)
            call jsonc%get(attributePointer, password)
            print *, "Password Sucursal: ", trim(password)
            sucu = sucursal(id, departamento, direccion, password)
            call arbol_sucursales%insert_abb(id, sucu)
            call arbol_sucursales%graph_abb("arbol_abb")
        end do
        
    end subroutine cargar_archivo_sucursales

    subroutine cargar_archivo_rutas()
        implicit none
        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: attributePointer, listPointer, rutaPointer
        character(100) :: nombre_json
        integer:: s1, s2, distancia, imp_mantenimiento
        integer :: size, i, num_pasadas, j
        logical :: found
        call json%initialize()
        print *, 'Cargar archivo de ruta'
        ! read the file
        print *, 'Ingrese el nombre del  archivo JSON:'
        read(*, '(A)') nombre_json
        
        call json%load(filename=nombre_json)
        
        ! print the file to the console
        call json%print()
        
        call json%info('grafo', n_children=size)
        
        call json%get_core(jsonc)
        call json%get('grafo', listPointer, found)
        print *, "Size: ", size
        do i = 1, size
            call jsonc%get_child(listPointer, i, rutaPointer, found)

            call jsonc%get_child(rutaPointer, 's1', attributePointer, found)
            call jsonc%get(attributePointer, s1)
            print *, "Nodo 1: ", s1

            call jsonc%get_child(rutaPointer, 's2', attributePointer, found)
            call jsonc%get(attributePointer, s2)
            print *, "Nodo 2: ", s2

            call jsonc%get_child(rutaPointer, 'distancia', attributePointer, found)
            call jsonc%get(attributePointer, distancia)
            print *, "Distancia: ", distancia

            call jsonc%get_child(rutaPointer, 'imp_mantenimiento', attributePointer, found)
            call jsonc%get(attributePointer, imp_mantenimiento)
            print *, "Impresora mantenimiento: ", imp_mantenimiento
            call rutas%insert(s1)
            call rutas%insert(s2)
            call rutas%crearConexion(s1, s2, distancia, imp_mantenimiento)
            call rutas%crearConexion(s2, s1, distancia, imp_mantenimiento)
        end do
        call rutas%crearGrafo()
    end subroutine cargar_archivo_rutas

end program main