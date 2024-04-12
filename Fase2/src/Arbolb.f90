module btree_m
    use page_m
    implicit none
    
    type, public :: btree
        private
        type(page), pointer :: root
        integer :: orden = 5
    contains 
        procedure :: insert
        procedure :: insertInLeaf
        procedure :: buscar_usuario
        procedure :: modificar_usuario
        procedure :: eliminar_usuario
        procedure :: graphTree
        procedure :: mostrarClientes
        procedure :: buscarCliente
    end type btree

contains 

subroutine buscarCliente(self, dpi)
    class(btree), intent(in) :: self
    integer(kind=8), intent(in) :: dpi
    type(key), pointer :: current_key
    current_key => self%root%first
    do while (associated(current_key))
        if (current_key%data%dpi == dpi) then
            print *, current_key%data
            exit
        end if
        current_key => current_key%next
    end do
    print *, 'Cliente no encontrado'
end subroutine buscarCliente

subroutine mostrarClientes(self)
    class(btree), intent(in) :: self
    type(key), pointer :: current_key
    current_key => self%root%first
    do while (associated(current_key))
        print *, current_key%data
        current_key => current_key%next
    end do
end subroutine mostrarClientes

subroutine graphTree(this)
    class(btree), intent(in) :: this
    integer :: unit, count = 0
    open(unit, file='btree.dot', status='replace')
    write(unit, '(A)') 'digraph G {'
    call graphBtree(this%root, unit, count)
    write(unit, '(A)') '}'
    close(unit)
    call execute_command_line('dot -Tpng btree.dot -o btree.png')
    call execute_command_line('start btree.png')
end subroutine graphTree

recursive subroutine graphBtree(myNode, unit, count)
    type(page), pointer :: myNode
    integer, intent(in) :: unit
    integer, intent(inout) :: count
    integer :: temp
    type(key), pointer :: current_key
    
    if (associated(myNode)) then
        count = count + 1
        ! Escribir la etiqueta del nodo
        write(unit, '(A,I0,A)', advance='no') 'node', count, ' [label="'
        
        ! Escribir las claves del nodo
        if (associated(myNode%first)) then
            current_key => myNode%first
            do while (associated(current_key))
                write(unit, '(I0,A,A)', advance='no') current_key%data%dpi, ', ', trim(current_key%data%nombre)
                if (associated(current_key%next)) then
                    write(unit, '(A)', advance='no') '|'
                end if
                current_key => current_key%next
            end do
        end if
        
        ! Cerrar la etiqueta del nodo
        write(unit, '(A)', advance='no') '"];'
        
        temp = count
        
        ! Recorrer los enlaces a los nodos hijos
        if (associated(myNode%first)) then
            current_key => myNode%first
            do while (associated(current_key))
                if (current_key%hasKids()) then
                    write(unit, '(A,I0,A,I0,A)', advance='no') 'node', temp, ' -> node', count + 1, ';'
                    call graphBtree(current_key%left, unit, count)
                    write(unit, '(A,I0,A,I0,A)', advance='no') 'node', temp, ' -> node', count + 1, ';'
                    call graphBtree(current_key%right, unit, count)
                end if
                current_key => current_key%next
            end do
        end if
    end if
end subroutine graphBtree





subroutine modificar_usuario(self, usuario, contrasena, nuevo_usuario, nueva_contrasena)
    class(btree), intent(inout) :: self
    character(len=20), intent(in) :: usuario, contrasena
    character(len=20), intent(in) :: nuevo_usuario, nueva_contrasena

    type(key), pointer :: current_key

    ! Buscar el usuario que se desea modificar
    current_key => self%root%first
    do while (associated(current_key))
        if (trim(current_key%data%nombre) == trim(usuario) .and. trim(current_key%data%contrasena) == trim(contrasena)) then
            ! Actualizar los datos del usuario
            current_key%data%nombre = nuevo_usuario
            current_key%data%contrasena = nueva_contrasena
            print *, 'Usuario modificado'
            exit
        end if
        current_key => current_key%next
    end do
    
end subroutine modificar_usuario

subroutine eliminar_usuario(self, usuario, contrasena)
    class(btree), intent(inout) :: self
    character(len=20), intent(in) :: usuario, contrasena

    type(key), pointer :: current_key, previous_key

    ! Buscar el usuario que se desea eliminar
    current_key => self%root%first
    previous_key => null()
    do while (associated(current_key))
        if (trim(current_key%data%nombre) == trim(usuario) .and. trim(current_key%data%contrasena) == trim(contrasena)) then
            ! Eliminar el usuario encontrado
            if (associated(previous_key)) then
                previous_key%next => current_key%next
            else
                self%root%first => current_key%next
            end if
            deallocate(current_key)
            print *, 'Usuario eliminado'
            exit
        end if
        previous_key => current_key
        current_key => current_key%next
    end do
    
end subroutine eliminar_usuario


function buscar_usuario(self, usuario, contrasena) result(found)
    class(btree), intent(in) :: self
    character(len=20), intent(in) :: usuario, contrasena
    logical :: found
    type(key), pointer :: current_key

    found = .false.
    current_key => self%root%first

    ! Iterar a través de las claves del árbol
    do while (associated(current_key))
        ! Comprobar si el usuario y la contraseña coinciden
        if (trim(current_key%data%nombre) == trim(usuario) .and. trim(current_key%data%contrasena) == trim(contrasena)) then
            found = .true.
            exit
        end if
        ! Mover al siguiente nodo del árbol
        current_key => current_key%next
    end do
end function buscar_usuario
subroutine insert(self, client)
    class(btree), intent(inout) :: self
    type(cliente), intent(in) :: client

    type(key), pointer :: newRoot
    type(key), pointer :: newKey
    if(.not. associated(self%root)) then
        allocate(self%root)
        self%root = page()
    end if

    allocate(newKey)
    newKey%data = client

    newRoot => self%insertInLeaf(newKey, self%root)
    if(associated(newRoot)) then
        self%root = page()
        call self%root%insertKey(newRoot)
        self%root%leaf = .false.
    end if
end subroutine insert


    recursive function insertInLeaf(self, newKey, root) result(res)
        class(btree), intent(inout) :: self
        type(key), pointer :: newKey
        type(page), pointer, intent(inout) :: root
        type(key), pointer :: res
        type(key), pointer :: p
        
        if(root%leaf) then
            call root%insertKey(newKey)

            if(root%numberKeys == self%orden) then
                res => divide(root)
            else
                res => null()
            end if
            return
        else
            p => root%first
            if(newKey%data%dpi == p%data%dpi) then
                res => null()
                return
            else if (newKey%data%dpi < p%data%dpi) then
                call root%insertKey(newKey)
                if(root%numberKeys == self%orden) then
                    res => divide(root)
                    return
                else
                    res => null()
                    return
                end if
            else
                do while (.true.)
                    if(newKey%data%dpi == p%data%dpi) then
                        res => null()
                        return

                    else if(newKey%data%dpi < p%data%dpi) then
                        call root%insertKey(newKey)
                        if(root%numberKeys == self%orden) then
                            res => divide(root)
                            return
                        else
                            res => null()
                            return
                        end if
                    else if(.not. associated(p%next)) then
                        call root%insertKey(newKey)
                        if(root%numberKeys == self%orden) then
                            res => divide(root)
                            return
                        else 
                            res => null()
                            return
                        end if
                    end if

                    p => p%next
                    if(.not. associated(p)) exit
                end do
            end if
        end if
        res => null()
        return
    end function insertInLeaf

    function divide(n) result(res)
        type(page), pointer :: n
        type(key), pointer :: res
        type(key), pointer :: temp
        type(page), pointer :: left
        type(page), pointer :: right
        type(key), pointer :: new

        integer :: cont
        integer :: rootValue
        allocate(left)
        allocate(right)
        allocate(res)
        temp => n%first
        cont = 1

        do while(associated(temp))
            if(cont < 3) then
                allocate(new)
                new%data = temp%data
                new%right => temp%right
                new%left => temp%left
                call left%insertKey(new)
                if(new%hasKids()) then
                    left%leaf = .false.
                end if

            else if(cont == 3) then
                rootValue = temp%data%dpi

            else
                allocate(new)
                new%data = temp%data
                new%right => temp%right
                new%left => temp%left
                call right%insertKey(new)
                if(new%hasKids()) then
                    right%leaf = .false.
                end if
            end if
            cont = cont + 1
            temp => temp%next
        end do

        allocate(res)
        res%data%dpi = rootValue
        res%right => right
        res%left => left
    end function divide
end module btree_m
