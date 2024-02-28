module pila_img_m
    implicit none
    private

    type :: node
        private
        integer :: id !id del cliente
        character(len=5) :: tipo !img_p o img_g
        type(node), pointer :: next => null()
    end type node

    type, public :: pila_i
        private
        type(node), pointer :: head => null()

    contains
        procedure :: push_i
        procedure :: vaciar_i
        procedure :: pop_i
        procedure :: print_i
        final :: destructor_i
    end type pila_i

contains
    subroutine vaciar_i(self)
        class(pila_i), intent(inout) :: self
        type(node), pointer :: aux

        do while(associated(self%head))
            aux => self%head%next
            deallocate(self%head)
            self%head => aux
        end do
    end subroutine vaciar_i
    subroutine pop_i(self)
        class(pila_i), intent(inout) :: self
        type(node), pointer :: temp

        if (.not. associated(self%head)) then
            print *, "La lista está vacía. No se puede realizar 'pop_i'."
            return
        else
            temp => self%head
            self%head => self%head%next
            deallocate(temp)
        end if
    end subroutine pop_i

    subroutine push_i(self, tipo, id)
        class(pila_i), intent(inout) :: self
        character(len=5) :: tipo
        integer :: id


        type(node), pointer :: new
        allocate(new)

        new%tipo = tipo
        new%id = id

        if(.not. associated(self%head)) then
            self%head => new
        else    
            new%next => self%head
            self%head => new
        end if
    end subroutine push_i

    subroutine print_i(self)
        class(pila_i), intent(in) :: self
        type(node), pointer :: current
        current => self%head

        do while(associated(current))
            print *, current%tipo, ","
            current => current%next
        end do
    end subroutine print_i

    subroutine destructor_i(self)
        type(pila_i), intent(inout) :: self
        type(node), pointer :: aux

        do while(associated(self%head))
            aux => self%head%next
            deallocate(self%head)
            self%head = aux
        end do
    end subroutine destructor_i


end module pila_img_m