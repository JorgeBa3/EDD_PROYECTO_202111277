program main
  use tabla_hash_m
  implicit none
    
    character(20) :: username, password
    integer :: opcion
    
    ! Definir el nombre de usuario y contrasena del administrador
    character(20) :: admin_username = 'EDD1S2024'
    character(20) :: admin_password = 'ProyectoFase3'
    
    do
        print *, 'Ingrese su nombre de usuario:'
        read *, username
        print *, 'Ingrese su contrasena:'
        read *, password
        
        if (username == admin_username .and. password == admin_password) then
            print *, 'Bienvenido ', username
            call menu_principal()
            exit
        else
            print *, 'Usuario o contrasena incorrectos'
        end if
    end do
    contains
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
            print *, '1. Cargar archivo de productos'
            print *, '2. Cargar archivo de sucursales'
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
      print *, 'Menu de sucursales'
    end subroutine menu_sucursales

    subroutine menu_reportes()
      print *, 'Menu de reportes'
    end subroutine menu_reportes

    subroutine cargar_archivo_sucursales()
      print *, 'Cargar archivo de sucursales'
    end subroutine cargar_archivo_sucursales

    subroutine cargar_archivo_rutas()
      print *, 'Cargar archivo de rutas'
    end subroutine cargar_archivo_rutas


end program main
