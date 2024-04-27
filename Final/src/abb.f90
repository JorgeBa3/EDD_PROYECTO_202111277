module abb_m
    use uuid_module
    implicit none
    
    type :: sucursal
        integer :: id
        character(len=50) :: departamento
        character(len=50) :: direccion
        character(len=20) :: password
    end type sucursal

    type :: node
        integer :: value
        integer :: height = 1
        type(node), pointer :: right => null()
        type(node), pointer :: left => null()
        type(sucursal), allocatable :: sucursales(:)
    end type node

    type, public :: abb
        type(node), pointer :: root => null()
    contains
        procedure :: insert_abb
        procedure :: delete_abb
        procedure :: preorder_abb
        procedure :: buscar_abb
        procedure :: inorder_abb
        procedure :: posorder_abb
        procedure :: graph_abb
        procedure :: buscarCapa_abb
        procedure :: inOrder
        procedure :: preOrder
        procedure :: posOrder
        procedure :: amplitudOrden
        procedure :: buscar
    end type abb
contains

    subroutine buscar(self, val)
        class(abb), intent(in) :: self
        integer, intent(in) :: val
        type(sucursal) :: info

        ! Inicializamos la información con valores por defecto
        info%id = -1
        info%departamento = 'Ninguno'
        info%direccion = 'Ninguna'
        info%password = 'Ninguna'

        ! Realizamos la búsqueda en el árbol
        if (associated(self%root)) then
            call buscarRec_abb(self%root, val, info)
        end if
    end subroutine buscar

    subroutine amplitudOrden(self)
        class(abb), intent(in) :: self
        print *, "Recorrido por amplitud", self%root%value
    end subroutine amplitudOrden

    subroutine posOrder(self)
        class(abb), intent(in) :: self
        call posordenRec_abb(self%root)
    end subroutine posOrder

    subroutine preOrder(self)
        class(abb), intent(in) :: self
        call preorderRec_abb(self%root)
    end subroutine preOrder

    subroutine inOrder(self)
        class(abb), intent(in) :: self
        call inordenRec_abb(self%root)
    end subroutine inOrder

    subroutine buscarCapa_abb(self, val)
        class(abb), intent(in) :: self
        integer, intent(in) :: val
        print *, "Buscando capa ", val
    end subroutine buscarCapa_abb

    function buscar_abb(self, val) result(info)
        class(abb), intent(in) :: self
        integer, intent(in) :: val
        type(sucursal) :: info

        ! Inicializamos la información con valores por defecto
        info%id = -1
        info%departamento = 'Ninguno'
        info%direccion = 'Ninguna'
        info%password = 'Ninguna'

        ! Realizamos la búsqueda en el árbol
        if (associated(self%root)) then
            call buscarRec_abb(self%root, val, info)
        else
            print *, "No se encontró el valor"
        end if
    end function buscar_abb

    recursive subroutine buscarRec_abb(root, val, info)
        type(node), pointer :: root
        integer, intent(in) :: val
        type(sucursal), intent(inout) :: info

        print *, "Buscando ", val
        if (associated(root)) then
            if (val == root%value) then
                ! Si encontramos el valor, actualizamos la información
                if (size(root%sucursales) > 0) then
                    info = root%sucursales(1)
                end if
            else if (val < root%value) then
                ! Si el valor buscado es menor, buscamos en el subárbol izquierdo
                call buscarRec_abb(root%left, val, info)
            else
                ! Si el valor buscado es mayor, buscamos en el subárbol derecho
                call buscarRec_abb(root%right, val, info)
            end if
        end if
    end subroutine buscarRec_abb

    subroutine insert_abb(self, val, info)
        class(abb), intent(inout) :: self
        integer, intent(in) :: val
        type(sucursal), intent(in) :: info

        if (.not. associated(self%root)) then
            allocate(self%root)
            self%root%value = val
            allocate(self%root%sucursales(1))
            self%root%sucursales(1) = info
        else
            call insertRec_abb(self%root, val, info)
        end if
    end subroutine insert_abb

    recursive subroutine insertRec_abb(root, val, info)
        type(node), pointer, intent(inout) :: root
        integer, intent(in) :: val
        type(sucursal), intent(in) :: info

        if (val < root%value) then
            if (.not. associated(root%left)) then
                allocate(root%left)
                root%left%value = val
                allocate(root%left%sucursales(1))
                root%left%sucursales(1) = info
            else
                call insertRec_abb(root%left, val, info)
            end if
        else if (val > root%value) then
            if (.not. associated(root%right)) then
                allocate(root%right)
                root%right%value = val
                allocate(root%right%sucursales(1))
                root%right%sucursales(1) = info
            else
                call insertRec_abb(root%right, val, info)
            end if
        end if
    end subroutine insertRec_abb

    subroutine delete_abb(self, val)
        class(abb), intent(inout) :: self
        integer, intent(inout) :: val

        self%root => deleteRec_abb(self%root, val)
    end subroutine delete_abb

    recursive function deleteRec_abb(root, key) result(res)
        type(node), pointer :: root
        integer, intent(in) :: key
        type(node), pointer :: res
        type(node), pointer :: temp

        if (.not. associated(root)) then
            res => root
            return
        end if

        if (key < root%value) then
            root%left => deleteRec_abb(root%left, key)
        else if (key > root%value) then
            root%right => deleteRec_abb(root%right, key)
        else
            if (.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp
                return
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
                return
            else
                call getMajorOfMinors_abb(root%left, temp)
                root%value = temp%value
                root%left => deleteRec_abb(root%left, temp%value)
            end if
        end if

        res => root
    end function deleteRec_abb

    recursive subroutine getMajorOfMinors_abb(root, major)
        type(node), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinors_abb(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinors_abb

    subroutine preorder_abb(self)
        class(abb), intent(in) :: self

        call preorderRec_abb(self%root)
    end subroutine preorder_abb

    subroutine inorder_abb(self)
        class(abb), intent(in) :: self

        call inordenRec_abb(self%root)
    end subroutine inorder_abb

    subroutine posorder_abb(self)
        class(abb), intent(in) :: self

        call posordenRec_abb(self%root)
    end subroutine posorder_abb

    recursive subroutine preorderRec_abb(root)
        type(node), pointer, intent(in) :: root

        if(associated(root)) then
            print *, root%value
            call preorderRec_abb(root%left)
            call preorderRec_abb(root%right)
        end if
    end subroutine preorderRec_abb

    recursive subroutine inordenRec_abb(root)
        type(node), pointer, intent(in) :: root

        if(associated(root)) then
            call inordenRec_abb(root%left)
            print *, root%value
            call inordenRec_abb(root%right)
        end if
    end subroutine inordenRec_abb

    recursive subroutine posordenRec_abb(root)
        type(node), pointer, intent(in) :: root

        if(associated(root)) then
            call posordenRec_abb(root%left)
            call posordenRec_abb(root%right)
            print *, root%value
        end if
    end subroutine posordenRec_abb

    subroutine graph_abb(self, filename)
        class(abb), intent(in) :: self
        character(len=*), intent(in) :: filename
        integer :: io
        character(len=100) :: dot_filename, dot_command, png_filename

        ! Agregar extensiones
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"

        ! Construir comandos para generar la imagen
        dot_command = "dot -Tpng " // trim(dot_filename) // " -o " // trim(png_filename)

        io = 1
        open(newunit=io, file=trim(dot_filename))
        write(io, *) "digraph G {"
        if(associated(self%root)) then
            call printRec_abb(self%root, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)

        ! Ejecutar el comando para generar la imagen
        call execute_command_line(trim(dot_command))
    end subroutine graph_abb

    recursive subroutine printRec_abb(root, name, io)
        type(node), pointer :: root
        character(len=36) :: name
        integer :: io

        character(len=36) :: right
        character(len=36) :: left

        right = generate_uuid()
        left = generate_uuid()

        if(associated(root)) then
            write(io, *)'"Nodo'//name//'"[label = "', root%value, '"]'
            if(associated(root%left)) then
                write(io, *)'"Nodo'//name//'"->"Nodo'//left//'"'
            end if
            if(associated(root%right)) then
                write(io, *)'"Nodo'//name//'"->"Nodo'//right//'"'
            end if
            call printRec_abb(root%left, left, io)
            call printRec_abb(root%right, right, io)
        end if
    end subroutine printRec_abb
end module abb_m
