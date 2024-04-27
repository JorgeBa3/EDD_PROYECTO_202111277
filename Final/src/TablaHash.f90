module tabla_hash_m
    implicit none
    private
    integer :: M = 7
    real :: R = 0.618034
    integer, parameter :: long = selected_int_kind(4)
    integer, parameter :: dp = selected_real_kind(15)
  
    type, public :: Persona
        integer(kind=8) :: dpi
        character(len=50) :: nombre
        character(len=50) :: apellido
        character(len=50) :: genero
        character(len=50) :: direccion
        character(len=15) :: telefono
    end type Persona
  
    type, public :: TablaHash
        integer :: n = 0
        type(Persona), allocatable :: tabla(:)
    contains
        procedure :: insert
        procedure :: print
        procedure :: search
        procedure :: rehashing
        procedure, private :: pruebaLineal
    end type TablaHash
  
  contains
    subroutine insert(self, p)
        class(TablaHash), intent(inout) :: self
        type(TablaHash) :: ret
        type(Persona), intent(in) :: p
        integer :: pos
  
        if(.not. allocated(self%tabla)) then
            allocate(self%tabla(0:M-1))
            self%tabla(:) = persona(-1, "", "", "", "", "")
        end if
  
        pos = funcion_hash(p%dpi)
  
        if(self%tabla(pos)%dpi /= -1 .and. self%tabla(pos)%dpi /= p%dpi) then
            call self%pruebaLineal(pos)
        end if
  
        self%tabla(pos) = p
        self%n = self%n + 1
  
        if(self%n * 1.0_dp/M > 0.75) then
            call self%rehashing()
        end if
    end subroutine
  
    subroutine rehashing(self)
        class(TablaHash), intent(inout) :: self
        type(TablaHash) :: temp
        integer :: i
  
        M = M*2
        allocate(temp%tabla(0:M-1))
        temp%tabla(:) = persona(-1, "", "", "", "", "")
  
        do i = 0, size(self%tabla)-1
            if(self%tabla(i)%dpi /= -1) then
                call temp%insert(self%tabla(i))
            end if
        end do
  
        deallocate(self%tabla)
        self%tabla = temp%tabla
    end subroutine
  
    subroutine pruebaLineal(self, pos)
        class(TablaHash), intent(inout) :: self
        integer, intent(inout) :: pos
  
        do while(self%tabla(pos)%dpi /= -1)
            pos = pos + 1
            pos = mod(pos, M)
        end do
    end subroutine pruebaLineal
  
    subroutine search(self, val)
        class(TablaHash), intent(in) :: self
        integer(kind=8), intent(in) :: val
        integer :: pos
  
        pos = funcion_hash(val)
        print *, "Nombre:", self%tabla(pos)%nombre
        print *, "Apellido:", self%tabla(pos)%apellido
        print *, "Género:", self%tabla(pos)%genero
        print *, "Dirección:", self%tabla(pos)%direccion
        print *, "Teléfono:", self%tabla(pos)%telefono
    end subroutine search
  
    subroutine print(self)
        class(TablaHash), intent(in) :: self
        integer :: i
        do i = 0, size(self%tabla)-1
            if(self%tabla(i)%dpi /= -1) then
                print *, "DPI:", self%tabla(i)%dpi
                print *, "Nombre:", self%tabla(i)%nombre
                print *, "Apellido:", self%tabla(i)%apellido
                print *, "Género:", self%tabla(i)%genero
                print *, "Dirección:", self%tabla(i)%direccion
                print *, "Teléfono:", self%tabla(i)%telefono
                print *, "------------------------"
            end if
        end do
    end subroutine print
  
    function funcion_hash(x) result(v)
        integer(kind=8), intent(in) :: x
        real :: d
        integer :: v
  
        d = R*x - floor(R*x)
        v = floor(M*d)
    end function funcion_hash
  end module tabla_hash_m
  