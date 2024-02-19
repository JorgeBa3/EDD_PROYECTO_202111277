module cola_recepcion_m
    implicit none
    private

    type :: node
        private
        integer :: id
        integer :: img_g
        integer :: img_p
        type(node), pointer :: next => null()
    end type node

    type, public :: cola_r
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
    end type cola_r

contains
    subroutine clear(self)
        class(cola_r), intent(inout) :: self
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
        class(cola_r), intent(inout) :: self
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

    subroutine push(self, id, img_g, img_p)
        class(cola_r), intent(inout) :: self
        integer, intent(in) :: id, img_g, img_p

        type(node), pointer :: new
        allocate(new)

        new%id = id
        new%img_p = img_g
        new%img_g = img_p

        if(.not. associated(self%head)) then
            self%head => new
        else    
            new%next => self%head
            self%head => new
        end if
    end subroutine push

    subroutine append(self, id, img_g, img_p)
        class(cola_r), intent(inout) :: self
        integer, intent(in) :: id, img_g, img_p

        type(node), pointer :: current
        type(node), pointer :: new
        allocate(new)

        new%id = id
        new%img_p = img_g
        new%img_g = img_p

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
        class(cola_r), intent(in) :: self
        type(node), pointer :: current
        current => self%head

        do while(associated(current))
            print *, "ID:", current%id, ", img_g:", current%img_g, ", img_p:", current%img_p
            current => current%next
        end do
    end subroutine print

    subroutine destructor(self)
        type(cola_r), intent(inout) :: self
        type(node), pointer :: aux

        do while(associated(self%head))
            aux => self%head%next
            deallocate(self%head)
            self%head => aux
        end do
    end subroutine destructor

end module cola_recepcion_m
