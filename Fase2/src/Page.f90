module cliente_m
    implicit none
    
    ! Definición del tipo cliente
    type :: cliente
        integer :: dpi
        character(len=50) :: nombre
        character(len=20) :: contrasena
    end type cliente
end module cliente_m

module page_m
    use cliente_m
    implicit none

    type, public :: page
        logical :: leaf = .true.
        type(key), pointer :: first => null()
        integer :: numberKeys = 0

    contains
        procedure :: insertKey
    end type page

    type :: key 
        type(cliente) :: data
        type(key), pointer :: prev => null()
        type(key), pointer :: next => null()
        type(page), pointer :: right => null()
        type(page), pointer :: left => null()
    contains
        procedure :: hasKids
    end type key

contains
    !Funciones del tipo page
    subroutine insertKey(self, k)
        class(page), intent(inout) :: self
        type(key), pointer :: k
        type(key), pointer :: p

        p => self%first
        if(.not. associated(self%first)) then
            self%first => k
            self%numberKeys = self%numberKeys + 1

        else if(k%data%dpi < self%first%data%dpi) then
            k%next => self%first
            self%first%left => k%right
            self%first%prev => k
            self%first => k
            self%numberKeys = self%numberKeys + 1

        else
            do while(associated(p))
                if(k%data%dpi == p%data%dpi) then
                    print *, "El DPI ", k%data%dpi, " no se puede insertar ya que es un valor repetido"
                    exit

                else if(k%data%dpi < p%data%dpi) then
                    p%left => k%right
                    p%prev%right => k%left

                    k%next => p
                    k%prev => p%prev
                    p%prev%next => k
                    p%prev => k
                    self%numberKeys = self%numberKeys + 1
                    exit

                else if(.not. associated(p%next)) then
                    p%next => k
                    k%prev => p
                    p%right => k%left
                    self%numberKeys = self%numberKeys + 1
                    exit
                end if
                p => p%next
            end do
        end if
    end subroutine insertKey

    function getLast(self) result(last)
        class(page), intent(in) :: self
        type(key), pointer :: last

        last => self%first
        do while(associated(last%next)) 
            last => last%next
        end do
    end function getLast

    !Funciones del tipo key
    function hasKids(self) result(kids)
        class(key), intent(inout) :: self
        logical :: kids

        kids = associated(self%right) .and. associated(self%left)
    end function hasKids
end module page_m
