module cola_recepcion_m
    implicit none
    private

    type :: node
        private
        integer :: id
        character(len=40) :: nombre
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
        procedure :: mostrar_top5_img_p
        procedure :: clear
        procedure :: vaciar
        procedure :: esta_vacia
        procedure :: toma_id
        procedure :: toma_img_g
        procedure :: toma_img_p
        procedure :: mostrar_top5_img_g
        ! procedure :: insert
        procedure :: print
        final :: destructor

    end type cola_r

contains
function itoa(number) result(str)
    integer, intent(in) :: number
    character(len=14) :: str

    ! Convierte el entero a una cadena de caracteres
    write(str, '(I14)') number

end function itoa

subroutine mostrar_top5_img_g(self)
    class(cola_r), intent(in) :: self
    type(node), pointer :: current
    type(node), pointer :: top_client
    character(len=100) :: file_name
    integer :: i

    ! Abrir un archivo DOT para escribir el grafo
    open(unit=101, file='top5_img_g.dot', status='replace', action='write', &
        access='sequential', form='formatted', iostat=i)
    if (i /= 0) then
        print *, 'Error al abrir el archivo DOT'
        return
    end if

    ! Escribir el encabezado del archivo DOT
    write(101, '(a)') 'digraph G {'
    write(101, '(a)') 'rankdir=LR;'
    write(101, '(a)') 'node [shape=box];'

    ! Encontrar el cliente con más imágenes grandes (Top 1)
    current => self%head
    top_client => current
    do
        if (.not. associated(current%next)) exit
        if (current%next%img_g > top_client%img_g) then
            top_client => current%next
        end if
        current => current%next
    end do

    ! Escribir los nodos para los top 5 clientes con más imágenes grandes
    current => top_client
    do i = 1, 5
        if (.not. associated(current)) exit
        write(file_name, '(a, i0)') 'Top', i
        write(101, '(a, i0,a, i0)') file_name, current%id, '[label="ID: ' &
            , current%id,  '\nNombre: ' // trim(current%nombre) // &
            '\nimg_g: ' // trim(adjustl(itoa(current%img_g))) // '"];'
        current => current%next
    end do

    ! Escribir las relaciones entre los nodos "Top1", "Top2", etc., y la información de los clientes
    current => top_client
    do i = 1, 5
        if (.not. associated(current) .or. .not. associated(current%next)) exit
        write(101, '(a, i0, a, i0, a)') 'Top', i, '->', current%id, ';'
        current => current%next
    end do

    ! Escribir el final del archivo DOT
    write(101, '(a)') '}'
    close(unit=101)

    ! Generar el archivo de imagen usando Graphviz
    call system('dot -Tpng top5_img_g.dot -o top5_img_g.png')

    ! Mostrar un mensaje de confirmación
    print *, 'Grafo generado con éxito en top5_img_g.png'
end subroutine mostrar_top5_img_g

subroutine mostrar_top5_img_p(self)
    class(cola_r), intent(in) :: self
    type(node), pointer :: current
    type(node), pointer :: top_client
    character(len=100) :: file_name
    integer :: i

    ! Abrir un archivo DOT para escribir el grafo
    open(unit=101, file='top5_img_p.dot', status='replace', action='write', &
        access='sequential', form='formatted', iostat=i)
    if (i /= 0) then
        print *, 'Error al abrir el archivo DOT'
        return
    end if

    ! Escribir el encabezado del archivo DOT
    write(101, '(a)') 'digraph G {'
    write(101, '(a)') 'rankdir=LR;'
    write(101, '(a)') 'node [shape=box];'

    ! Encontrar el cliente con la menor cantidad de imágenes pequeñas (Top 1)
    current => self%head
    top_client => current
    do
        if (.not. associated(current%next)) exit
        if (current%next%img_p < top_client%img_p) then
            top_client => current%next
        end if
        current => current%next
    end do

    ! Escribir los nodos para los top 5 clientes con menos imágenes pequeñas
    current => top_client
    do i = 1, 5
        if (.not. associated(current)) exit
        write(file_name, '(a, i0)') 'Top', i
        write(101, '(a, i0,a, i0)') file_name, current%id, '[label="ID: ' &
            , current%id,  '\nNombre: ' // trim(current%nombre) // &
            '\nimg_p: ' // trim(adjustl(itoa(current%img_p))) // '"];'
        current => current%next
    end do

    ! Escribir las relaciones entre los nodos "Top1", "Top2", etc., y la información de los clientes
    current => top_client
    do i = 1, 5
        if (.not. associated(current) .or. .not. associated(current%next)) exit
        write(101, '(a, i0, a, i0, a)') 'Top', i, '->', current%id, ';'
        current => current%next
    end do

    ! Escribir el final del archivo DOT
    write(101, '(a)') '}'
    close(unit=101)

    ! Generar el archivo de imagen usando Graphviz
    call system('dot -Tpng top5_img_p.dot -o top5_img_p.png')

    ! Mostrar un mensaje de confirmación
    print *, 'Grafo generado con éxito en top5_img_p.png'
end subroutine mostrar_top5_img_p



function toma_id(self) result(id)
    class(cola_r), intent(inout) :: self
    type(node), pointer :: current
    integer ::id
    current => self%head
    id = current%id
end function toma_id

function toma_img_g(self) result(img_g)
    class(cola_r), intent(inout) :: self
    type(node), pointer :: current
    integer ::img_g
    current => self%head
    img_g = current%img_g
end function toma_img_g

function toma_img_p(self) result(img_p)
    class(cola_r), intent(inout) :: self
    type(node), pointer :: current
    integer ::img_p
    current => self%head
    img_p = current%img_p
end function toma_img_p

subroutine vaciar(self)
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
end subroutine vaciar

function esta_vacia(self) result(is_empty)
    class(cola_r), intent(in) :: self
    logical :: is_empty
    ! busca si es 0 el id
    if(self%head%id==0)then
        is_empty= .false.
    else
        is_empty =.true.
    end if  

end function esta_vacia

    
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
        type(node), pointer :: current
        type(node), pointer :: temp

        current => self%head
        temp => current%next
        deallocate(current)
        self%head => temp
    end subroutine pop

    subroutine push(self, id, nombre, img_g, img_p)
        class(cola_r), intent(inout) :: self
        integer, intent(in) :: id, img_g, img_p
        character(len=*), intent(in) :: nombre
        type(node), pointer :: new, current
    
        allocate(new)
        new%id = id
        new%nombre = nombre
        new%img_p = img_p
        new%img_g = img_g
        new%next => null()
    
        if (.not. associated(self%head)) then
            self%head => new
        else
            current => self%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => new
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
            print *, "ID:", current%id, "Nombre:", current%nombre, ", img_g:", current%img_g, ", img_p:", current%img_p
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
