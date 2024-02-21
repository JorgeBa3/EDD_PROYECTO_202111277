module pila_img_m
    implicit none
    private

    type :: node
        private
        integer :: value
        type(node), pointer :: next => null()
    end type node

    type, public :: pila_i
        private
        type(node), pointer :: head => null()

    contains
        procedure :: push
        procedure :: append
        procedure :: pop
        procedure :: clear
        ! procedure :: insert
        procedure :: print
        final :: destructor
    end type pila_i

contains
subroutine clear(self)
    class(pila_i), intent(inout) :: self
    type(node), pointer :: current
    type(node), pointer :: temp

    current => self%head
    do while(associated(current))
        temp => current%next
        deallocate(current)
        current => temp
    end do
    self%head => null() ! Asignar el puntero de la cabeza a nulo después de vaciar la lista
end subroutine clear
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
    subroutine push(self, value)
        class(pila_i), intent(inout) :: self
        integer, intent(in) :: value

        type(node), pointer :: new
        allocate(new)

        new%value = value

        if(.not. associated(self%head)) then
            self%head => new
        else    
            new%next => self%head
            self%head => new
        end if
    end subroutine push

    subroutine append(self, value)
        class(pila_i), intent(inout) :: self
        integer, intent(in) :: value

        type(node), pointer :: current
        type(node), pointer :: new
        allocate(new)

        new%value = value

        if(.not. associated(self%head)) then
            self%head => new
        else
            current => self%head
            do while(associated(current%next))
                current => current%next
            end do

            current%next => new
        end if

    end subroutine append

    subroutine print(self)
        class(pila_i), intent(in) :: self
        type(node), pointer :: current
        current => self%head

        do while(associated(current))
            print *, current%value, ","
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