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
    end type btree

contains 
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
