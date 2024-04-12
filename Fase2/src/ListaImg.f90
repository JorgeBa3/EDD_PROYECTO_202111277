module linked_list_m
    implicit none
    private

    public :: user
    public :: linked_list
    public :: push_usuario, append_usuario, print_users, destructor, get_head
    
    type :: user

        
        character(len=20) :: nombre_completo
        character(len=20) :: dpi
        character(len=20) :: contrasena
        type(user), pointer :: next => null()
    end type user

    type, public :: linked_list
        private
        type(user), pointer :: head => null()

    contains
        procedure :: push_usuario
        procedure :: append_usuario
        ! procedure :: insert
        procedure :: print_users
        final :: destructor
        procedure :: get_head ! Método para obtener el head de la lista enlazada
    end type linked_list

contains
    subroutine push_usuario(self, nombre_completo, dpi, contrasena)
        class(linked_list), intent(inout) :: self
        character(len=20), intent(in) :: nombre_completo
        character(len=20), intent(in) :: dpi
        character(len=20), intent(in) :: contrasena

        type(user), pointer :: new_user
        allocate(new_user)

        new_user%nombre_completo = nombre_completo
        new_user%dpi = dpi
        new_user%contrasena = contrasena

        if(.not. associated(self%head)) then
            self%head => new_user
        else    
            new_user%next => self%head
            self%head => new_user
        end if
    end subroutine push_usuario

    subroutine append_usuario(self, nombre_completo, dpi, contrasena)
        class(linked_list), intent(inout) :: self
        character(len=*), intent(in) :: nombre_completo
        character(len=*), intent(in) :: dpi
        character(len=*), intent(in) :: contrasena

        type(user), pointer :: current_user
        type(user), pointer :: new_user
        allocate(new_user)

        new_user%nombre_completo = nombre_completo
        new_user%dpi = dpi
        new_user%contrasena = contrasena

        if(.not. associated(self%head)) then
            self%head => new_user
        else
            current_user => self%head
            do while(associated(current_user%next))
                current_user => current_user%next
            end do

            current_user%next => new_user
        end if

    end subroutine append_usuario

    subroutine print_users(self)
        class(linked_list), intent(in) :: self
        type(user), pointer :: current_user
        current_user => self%head

        do while(associated(current_user))
            print *, "Nombre completo: ", trim(current_user%nombre_completo)
            print *, "DPI: ", trim(current_user%dpi)
            print *, "Contraseña: ", trim(current_user%contrasena)
            print *
            current_user => current_user%next
        end do
    end subroutine print_users

    subroutine destructor(self)
        type(linked_list), intent(inout) :: self
        type(user), pointer :: aux_user

        do while(associated(self%head))
            aux_user => self%head%next
            deallocate(self%head)
            self%head = aux_user
        end do
    end subroutine destructor

    function get_head(self) result(head_ptr)
        class(linked_list), intent(in) :: self
        type(user), pointer :: head_ptr

        head_ptr => self%head
    end function get_head

end module linked_list_m