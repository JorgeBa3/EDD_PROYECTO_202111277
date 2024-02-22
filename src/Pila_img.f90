module pila_img_m
    implicit none
    private

    type :: node
        private
        character(len=100) :: tipo
        type(node), pointer :: next => null()
    end type node

    type, public :: pila_i
        private
        type(node), pointer :: head => null()

    contains
        procedure :: push
        procedure :: vaciar
        procedure :: pop
        procedure :: print
        final :: destructor
    end type pila_i

contains
    subroutine vaciar(self)
        class(pila_i), intent(inout) :: self
        type(node), pointer :: aux

        do while(associated(self%head))
            aux => self%head%next
            deallocate(self%head)
            self%head => aux
        end do
    end subroutine vaciar
    subroutine pop(self)
        class(pila_i), intent(inout) :: self
        type(node), pointer :: temp

        if (.not. associated(self%head)) then
            print *, "La lista está vacía. No se puede realizar 'pop'."
            return
        else
            temp => self%head
            self%head => self%head%next
            deallocate(temp)
        end if
    end subroutine pop

    subroutine push(self, tipo)
        class(pila_i), intent(inout) :: self
        character(len=100) :: tipo

        type(node), pointer :: new
        allocate(new)

        new%tipo = tipo

        if(.not. associated(self%head)) then
            self%head => new
        else    
            new%next => self%head
            self%head => new
        end if
    end subroutine push

    subroutine print(self)
        class(pila_i), intent(in) :: self
        type(node), pointer :: current
        current => self%head

        do while(associated(current))
            print *, current%tipo, ","
            current => current%next
        end do
    end subroutine print

    subroutine destructor(self)
        type(pila_i), intent(inout) :: self
        type(node), pointer :: aux

        do while(associated(self%head))
            aux => self%head%next
            deallocate(self%head)
            self%head = aux
        end do
    end subroutine destructor


end module pila_img_m