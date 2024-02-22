module lista_ventanillas_m
    use cola_recepcion_m
    use pila_img_m

    implicit none

    private
    type, public :: cliente
        integer :: id
        integer :: img_g
        integer :: img_p

        contains
            procedure :: esta_vacia
            procedure :: crear_cliente
    end type cliente

    type :: node
        private
        integer :: num_ventanilla
        type(cliente) :: datos_cliente
        type(pila_i) :: stack
        type(node), pointer :: next => null()
    end type node

    type, public :: lista_v
        private
        type(node), pointer :: head => null()
    contains
        procedure :: tiene_cliente
        procedure :: toma_id
        procedure :: toma_img_g
        procedure :: toma_img_p
        procedure :: nueva_ventanilla
        procedure :: agregar_cliente
        procedure :: agregar_pila
        procedure :: limpiar_pila_cliente
        procedure :: print
        final :: destructor
    end type lista_v

contains


    integer function toma_id(self, ventanilla)
        class(lista_v), intent(in) :: self
        integer, intent(in) :: ventanilla
        type(node), pointer :: current
        current => self%head
        do while(associated(current))
            if (current%num_ventanilla == ventanilla) then
                toma_id = current%datos_cliente%id
                return
            end if
            current => current%next
        end do
    end function toma_id

    integer function toma_img_g(self, ventanilla)
        class(lista_v), intent(in) :: self
        integer, intent(in) :: ventanilla
        type(node), pointer :: current
        current => self%head
        do while(associated(current))
            if (current%num_ventanilla == ventanilla) then
                toma_img_g = current%datos_cliente%img_g
                return
            end if
            current => current%next
        end do
    end function toma_img_g
    integer function toma_img_p(self, ventanilla)
        class(lista_v), intent(in) :: self
        integer, intent(in) :: ventanilla
        type(node), pointer :: current
        current => self%head
        do while(associated(current))
            if (current%num_ventanilla == ventanilla) then
                toma_img_p = current%datos_cliente%img_p
                return
            end if
            current => current%next
        end do
    end function toma_img_p
    subroutine crear_cliente(self, id, img_g, img_p)
        class(cliente), intent(inout) :: self
        integer, intent(in) :: id
        integer, intent(in) :: img_g
        integer, intent(in) :: img_p
        self%id = id
        self%img_g = img_g
        self%img_p = img_p
    end subroutine crear_cliente

    logical function esta_vacia(self)
    class(cliente), intent(in) :: self
    esta_vacia = (self%id == 0)
end function esta_vacia


    ! Método para verificar si la ventanilla tiene una cliente
logical function tiene_cliente(self, ventanilla)
    class(lista_v), intent(in) :: self
    integer, intent(in) :: ventanilla
    logical :: hay_cola
    type(node), pointer :: current

    hay_cola = .false. ! Inicializa la variable a falso

    current => self%head

    do while(associated(current))
        if (current%num_ventanilla == ventanilla) then
            ! Verifica si la pila asociada a la ventanilla no está vacía
            hay_cola = .not. current%datos_cliente%esta_vacia()
            exit ! Sale del bucle una vez que se encuentra la ventanilla
        end if
        current => current%next
    end do

end function tiene_cliente






subroutine agregar_cliente(self, cliente_nuevo, num_ventanilla)
    implicit none
    class(lista_v), intent(inout) :: self
    type(cliente), intent(in) :: cliente_nuevo
    integer, intent(in) :: num_ventanilla
    type(node), pointer :: current
    logical :: found
    found = .false.

    current => self%head

    ! Buscar la ventanilla específica
    do while (associated(current))
        if (current%num_ventanilla == num_ventanilla) then
            found = .true.
            current%datos_cliente = cliente_nuevo
            exit
        else
            current => current%next
        end if
    end do

    ! Si no se encontró la ventanilla, imprimir mensaje de error
    if (.not. found) then
        print *, "La ventanilla especificada no existe."
    end if
end subroutine agregar_cliente



    ! Método para agregar una imagen a la pila de la ventanilla
    subroutine agregar_pila(self, tipo)
        implicit none
        class(lista_v), intent(inout) :: self
        character(len=100) :: tipo
        type(node), pointer :: aux
        if(associated(self%head)) then
            aux => self%head
            do while(associated(aux))
                call aux%stack%push(tipo)   ! Agregar la imagen a la pila utilizando el nuevo método
                aux => aux%next
            end do
        end if
    end subroutine agregar_pila

    subroutine nueva_ventanilla(self, num_ventanilla)
        implicit none
        class(lista_v), intent(inout) :: self
        integer, intent(in) :: num_ventanilla
        type(node), pointer :: aux, nuevo
        allocate(nuevo)
        nuevo%num_ventanilla = num_ventanilla
        nuevo%datos_cliente%id = 0
        nuevo%datos_cliente%img_g = 0
        nuevo%datos_cliente%img_p = 0
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
    end subroutine nueva_ventanilla
    
    
! Método para limpiar la pila y la cliente de la ventanilla
    subroutine limpiar_pila_cliente(self)
        implicit none
        class(lista_v), intent(inout) :: self
        type(node), pointer :: aux
        if(associated(self%head)) then
            aux => self%head
            do while(associated(aux))
                call aux%stack%vaciar()   ! Vaciar la pila utilizando el nuevo método
                print*, "Ventanilla:", aux%num_ventanilla
                aux => aux%next
            end do
        end if
    end subroutine limpiar_pila_cliente

    
    ! Método para imprimir información sobre la ventanilla


    subroutine print(self)
        implicit none
        type(pila_i) :: cola
        class(lista_v), intent(in) :: self
        type(node), pointer :: aux
        if(associated(self%head)) then
            aux => self%head
            do while(associated(aux))
                print*, "Ventanilla: ", aux%num_ventanilla
                print*, "Cliente: ", aux%datos_cliente%id, " Imagenes: ", aux%datos_cliente%img_g, " ", aux%datos_cliente%img_p
                print*, "Imagenes: "
                aux%stack=cola
                call aux%stack%print()
                aux => aux%next
            end do
        end if
    end subroutine print

    ! Destructor para liberar la memoria asignada a la lista de ventanillas
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
!smsmsmm