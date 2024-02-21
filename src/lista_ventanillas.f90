module lista_ventanillas_m
    use cola_recepcion_m
    use pila_img_m

    implicit none
    private

    type :: node
        private
        integer :: value
        type(cola_r) :: queue
        type(pila_i) :: stack
        logical :: ocupada
        
        type(node), pointer :: next => null()
    end type node

    type, public :: lista_v
        private
        type(node), pointer :: head => null()

    contains
        procedure :: push
        procedure :: append
        procedure :: pop
        procedure :: clear
        procedure :: tiene_cola
        procedure :: print
        final :: destructor
    end type lista_v

contains


function tiene_cola(self, ventanilla) result(hay_cola)
    class(lista_v), intent(in) :: self
    integer, intent(in) :: ventanilla
    logical :: hay_cola
    type(node), pointer :: current

    current => self%head
    hay_cola = .false. ! Inicializa con falso, no tiene cola por defecto

    do while(associated(current))
        if (current%value == ventanilla) then
            ! Verifica si la cola asociada a la ventanilla no está vacía
            hay_cola = .not. current%queue%esta_vacia()
            return
        end if
        current => current%next
    end do
end function tiene_cola




    

    subroutine clear(self)
        class(lista_v), intent(inout) :: self
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
        class(lista_v), intent(inout) :: self
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
        class(lista_v), intent(inout) :: self
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
        class(lista_v), intent(inout) :: self
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
        class(lista_v), intent(in) :: self
        type(node), pointer :: current
        current => self%head
    
        do while(associated(current))
            print *, "Valor:", current%value
            ! Imprime los datos de la pila (stack) asociada al nodo actual
            print *, "Datos de la pila:"
            call current%stack%print()
            ! Imprime los datos de la cola (queue) asociada al nodo actual
            print *, "Datos de la cola:"
            call current%queue%print()
            current => current%next
        end do
    end subroutine print

    subroutine destructor(self)
        type(lista_v), intent(inout) :: self
        type(node), pointer :: aux

        do while(associated(self%head))
            aux => self%head%next
            deallocate(self%head)
            self%head => aux
        end do
    end subroutine destructor

end module lista_ventanillas_m
