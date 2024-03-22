program MenuPrincipal
  use linked_list_m

  implicit none
  type(linked_list):: lista_usuarios
  character(len=20) :: usuario, password
  integer :: opcion

  ! Datos del usuario administrador
  character(len=20) :: usuario_admin = 'admin'
  character(len=20) :: password_admin = 'EDD2024'

  ! Variables para el registro de usuarios
  character(len=20) :: nombre_completo, dpi, password_usuario

  ! Variables para controlar el inicio de sesión
  logical :: sesion_iniciada
  sesion_iniciada = .false.

  do
      print *, "---- MENÚ PRINCIPAL ----"
      print *, "1. Inicio de Sesión"
      print *, "2. Registro de Usuarios"
      print *, "3. Acerca de"
      print *, "4. Salir"
      print *, "Seleccione una opción: "
      read(*,*) opcion

      select case(opcion)
      case(1)
          call IniciarSesion(usuario, password, sesion_iniciada, lista_usuarios)
      case(2)
          call RegistrarUsuario(nombre_completo, dpi, password_usuario)
      case(3)
          call AcercaDe
      case(4)
          exit
      case default
          print *, "Opción no válida. Por favor, seleccione una opción válida."
  end select
  
  end do

contains

subroutine IniciarSesion(usuario, password, sesion_iniciada, lista_usuarios)
  use linked_list_m
  implicit none
  character(len=20), intent(out) :: usuario
  character(len=20), intent(out) :: password
  logical, intent(out) :: sesion_iniciada
  class(linked_list), intent(in) :: lista_usuarios

  character(len=5) :: usuario_admin = 'admin'
  character(len=10) :: password_admin = 'EDD2024'

  type(user), pointer :: current_user

  ! Lógica para el inicio de sesión
  print *, "Ingrese el nombre de usuario: "
  read(*,*) usuario
  print *, "Ingrese la contraseña: "
  read(*,*) password

  sesion_iniciada = .false. ! Por defecto, la sesión no está iniciada

  ! Comprobamos si el usuario es el administrador
  if (usuario == usuario_admin .and. password == password_admin) then
      print *, "Inicio de sesión exitoso como administrador."
      sesion_iniciada = .true.
      return
  end if

  ! Si el usuario no es el administrador, buscamos en la lista de usuarios registrados
  current_user => lista_usuarios%get_head() ! Using get_head method to get the head of the linked list
  do while (associated(current_user))
    
      if (trim(current_user%nombre_completo) == trim(usuario) .and. &
          trim(current_user%contrasena) == trim(password)) then
          print *, "Inicio de sesión exitoso como ", trim(current_user%nombre_completo), " (usuario)."
          sesion_iniciada = .true.
          return
      end if
      current_user => current_user%next
  end do

  ! Si no se encontró el usuario en la lista, se muestra un mensaje de error
  print *, "Nombre de usuario o contraseña incorrectos."
end subroutine IniciarSesion

  subroutine RegistrarUsuario(nombre_completo, dpi, password_usuario)
      implicit none
      character(len=20), intent(out) :: nombre_completo
      character(len=20), intent(out) :: dpi
      character(len=20), intent(out) :: password_usuario

      ! Lógica para el registro de usuarios
      print *, "Ingrese el nombre completo del nuevo usuario: "
      read(*,*) nombre_completo
      print *, "Ingrese el DPI del nuevo usuario: "
      read(*,*) dpi
      print *, "Ingrese la contraseña para el nuevo usuario: "
      read(*,*) password_usuario

      ! Agregar el usuario a la lista enlazada
    call lista_usuarios%append_usuario(nombre_completo, dpi, password_usuario)

  end subroutine RegistrarUsuario

  subroutine AcercaDe
      implicit none

      ! Información acerca del programa
      print *, "Este es un programa de ejemplo para un menú en Fortran."
  end subroutine AcercaDe

end program MenuPrincipal
