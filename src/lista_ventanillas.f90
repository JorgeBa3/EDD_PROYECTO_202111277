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
        procedure :: graficar_ventanilla
        procedure :: toma_img_g
        procedure :: toma_img_p
        procedure :: nueva_ventanilla
        procedure :: agregar_cliente
        procedure :: agregar_pila
        procedure :: limpiar_pila_cliente
        procedure :: print_ven
        procedure :: atender_cliente
        
        final :: destructor_ven
    end type lista_v

contains
function itoa(number) result(str)
    integer, intent(in) :: number
    character(len=14) :: str

    ! Convierte el entero a una cadena de caracteres
    write(str, '(I14)') number

end function itoa


subroutine graficar_ventanilla(self, nombre_archivo)
    implicit none
    class(lista_v), intent(in) :: self
    character(len=*), intent(in) :: nombre_archivo
    integer :: io

    integer :: index
    character(len=100), allocatable :: command
    character(:), allocatable :: connections
    character(:), allocatable :: firsts
    character(len=8) :: name
    type(node), pointer :: current

    current => self%head
    command = "dot -Tpng " // trim(nombre_archivo) // " -o " // trim(nombre_archivo) // ".png"
    io = 1
    index = 0

    connections = ""
    firsts = ""

    open(newunit=io, file=trim(nombre_archivo))
    write(io, *) "digraph ListaVentanillas {"
    write(io, *) "  rankdir=LR;"

    if (associated(self%head)) then
        do while (associated(current))
            write(name, '(I5)') current%num_ventanilla

            if (firsts == "") then
                firsts = trim(name)
            end if

            ! Generar etiqueta para el nodo del cliente
            write(io, '(A)') '"cliente' // trim(name) // '"[label="Cliente: ' // &
                merge(trim(itoa(current%datos_cliente%id)), "No hay cliente", current%datos_cliente%id /= 0) // &
                ', Img_g: ' // trim(itoa(current%datos_cliente%img_g)) // &
                ', Img_p: ' // trim(itoa(current%datos_cliente%img_p)) // &
                '", shape=box];'

            ! Generar etiqueta para el nodo de la ventana
            write(io, '(A)') '"nodo' // trim(name) // '"[label="Ventanilla: ' // trim(name) // '", shape=box];'

            ! Generar conexión entre el cliente y la ventana
            connections = connections // '"cliente' // trim(name) // '" -> ' // '"nodo' // trim(name) // '";'

! Generar etiqueta para el nodo de la pila de ventanilla
! Obtener los elementos de la pila de la ventanilla actual
            write(io, '(A)') '"pila' // trim(name) // '"[label="Pila de Ventanilla ' // trim(name) // &
            '& : ' // trim(current%stack%get_elements()) // '", shape=box];'
        
        ! Generar conexión entre la ventana y la pila
        connections = connections // '"nodo' // trim(name) // '" -> ' // '"pila' // trim(name) // '";'
        


            current => current%next
            index = index + 1
        end do
    end if

    write(io, *) connections
    write(io, *) "}"
    close(io)

    call execute_command_line(command, exitstat=io)

    if (io /= 0) then
        print *, "Ocurrió un error al generar la imagen."
    else
        print *, "Imagen generada satisfactoriamente."
    end if
end subroutine graficar_ventanilla





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
            print *, "Cliente", cliente_nuevo%id, "agregado a la ventanilla:", num_ventanilla
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
subroutine atender_cliente(self)
    implicit none
    class(lista_v), intent(inout) :: self
    
    type(node), pointer :: current
    current => self%head
    
    ! Recorrer todas las ventanillas
    do while (associated(current))
        if (current%datos_cliente%id /= 0) then
            ! Si la ventanilla tiene un cliente, procesarlo

            ! Procesar las imágenes de img_g y img_p del cliente
            do while (current%datos_cliente%img_g > 0 .or. current%datos_cliente%img_p > 0)
                if (current%datos_cliente%img_g > 0) then
                    call current%stack%push_i('img_g', current%datos_cliente%id) ! Agregar img_g a la pila de la ventanilla
                    current%datos_cliente%img_g = current%datos_cliente%img_g - 1
                    print *, "Ventanilla:", current%num_ventanilla, "recibió una imagen grande"
                    exit
                elseif (current%datos_cliente%img_p > 0) then
                    call current%stack%push_i('img_p', current%datos_cliente%id) ! Agregar img_p a la pila de la ventanilla 
                    current%datos_cliente%img_p = current%datos_cliente%img_p - 1
                    print *, "Ventanilla:", current%num_ventanilla, "recibió una imagen pequeña"
                    exit
                endif
            end do
            
        endif
        current => current%next
    end do 
end subroutine atender_cliente



    ! Método para agregar una imagen a la pila de la ventanilla
    subroutine agregar_pila(self, tipo)
        implicit none
        class(lista_v), intent(inout) :: self
        type(node), pointer :: current
        character(len=40) :: tipo
        type(node), pointer :: aux
        if(associated(self%head)) then
            aux => self%head
            do while(associated(aux))
                call aux%stack%push_i(tipo, current%datos_cliente%id)   ! Agregar la imagen a la pila utilizando el nuevo método
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
                call aux%stack%vaciar_i()   ! Vaciar la pila utilizando el nuevo método
                print*, "Ventanilla:", aux%num_ventanilla
                aux => aux%next
            end do
        end if
    end subroutine limpiar_pila_cliente

    
    ! Método para imprimir información sobre la ventanilla


    subroutine print_ven(self)
        implicit none
        class(lista_v), intent(in) :: self
        type(node), pointer :: aux
        if (associated(self%head)) then
            aux => self%head
            do while (associated(aux))
                print *, "Ventanilla: ", aux%num_ventanilla
                print *, "Cliente: ", aux%datos_cliente%id, " Imagenes: ", aux%datos_cliente%img_g, " ", aux%datos_cliente%img_p
                print *, "Imagenes: "
                call aux%stack%print_i()  ! Imprimir la pila de la ventanilla actual
                aux => aux%next
            end do
        end if
    end subroutine print_ven
    

    ! destructor_ven para liberar la memoria asignada a la lista de ventanillas
    subroutine destructor_ven(self)
        type(lista_v), intent(inout) :: self
        type(node), pointer :: aux

        do while(associated(self%head))
            aux => self%head%next
            deallocate(self%head)
            self%head => aux
        end do
    end subroutine destructor_ven

end module lista_ventanillas_m
!smsmsmm