module cola_impresoras_m
    use pila_img_m
    implicit none
    private

    type :: node
        private
        integer :: id
        character(len=17) :: nombre
        integer :: pasos_necesarios
        type(pila_i) :: stack
        type(node), pointer :: next => null() 
    end type node

    type, public :: cola_imp
        private
        type(node), pointer :: head => null()

        

    contains
        procedure :: push_imp
        procedure :: nueva_impresora
        procedure :: pop_imp
        procedure :: print_imp
        final :: destructor_imp

    end type cola_imp

contains
    subroutine nueva_impresora(self, id_impresora, nombre, pasos)
        implicit none
        class(cola_imp), intent(inout) :: self
        integer, intent(in) :: id_impresora
        integer, intent(in) :: pasos
        character(len=17) :: nombre
        type(node), pointer :: aux, nuevo
        allocate(nuevo)
        nuevo%id = id_impresora
        nuevo%nombre = nombre
        nuevo%pasos_necesarios = pasos
        nuevo%next => null()
        if(associated(self%head)) then
            aux => self%head
            do while(associated(aux%next))
                aux => aux%next
            end do
            aux%next => nuevo
        else
            self%head => nuevo
        end if

    end subroutine nueva_impresora

    subroutine push_imp(self, id, nombre, tipo_img)
        class(cola_imp), intent(inout) :: self
        integer :: id
        character(len=17) :: nombre
        character(len=5) :: tipo_img

        type(node), pointer :: new_node
        type(node), pointer :: current

        allocate(new_node)
        new_node%id = id
        new_node%nombre = nombre

        ! Llamamos al procedimiento push de la pila para insertar el tipo de imagen
        call new_node%stack%push_i(tipo_img, id)

        if (.not. associated(self%head)) then
            self%head => new_node
        else
            current => self%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => new_node
        end if
    end subroutine push_imp

    subroutine pop_imp(self)
        class(cola_imp), intent(inout) :: self
        type(node), pointer :: temp

        if (.not. associated(self%head)) then
            print *, "La cola está vacía. No se puede realizar 'pop'."
            return
        else
            temp => self%head
            self%head => self%head%next
            deallocate(temp)
        end if
    end subroutine pop_imp

    subroutine print_imp(self)
        class(cola_imp), intent(in) :: self
        type(node), pointer :: current
        current => self%head

        print *, "Elementos en la cola de impresoras:"
        do while (associated(current))
            print *, "ID:", current%id, "Nombre:", current%nombre
            ! Llamamos al procedimiento print de la pila para mostrar el tipo de imagen
            call current%stack%print_i()
            current => current%next
        end do
    end subroutine print_imp

    subroutine destructor_imp(self)
        type(cola_imp), intent(inout) :: self
        type(node), pointer :: aux

        do while(associated(self%head))
            aux => self%head%next
            deallocate(self%head)
            self%head => aux
        end do
    end subroutine destructor_imp

end module cola_impresoras_m
