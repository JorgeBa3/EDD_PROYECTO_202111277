module hash_module
    implicit none

    type persona
        integer(kind=16) :: dpi
        character(len=20) :: nombre
        character(len=20) :: apellido
        character(len=20) :: genero
        character(len=50) :: direccion
        character(len=20) :: telefono
    end type persona

    type hash
        integer :: n ! elements
        integer :: m ! size table
        integer :: mini, maxi ! percentages
        type(persona), dimension(:), allocatable :: personas ! table
    contains
        procedure :: init
        procedure :: division
        procedure :: linear
        procedure :: insert_hash
        procedure :: rehashing
        procedure :: show
    end type hash

contains

    subroutine init(this, m, mini, maxi)
        class(hash), intent(inout) :: this
        integer, intent(in) :: m, mini, maxi
        this%n = 0
        this%m = m
        this%mini = mini
        this%maxi = maxi
        if (allocated(this%personas)) then
            deallocate(this%personas)
        end if
        allocate(this%personas(m))
        this%personas = persona(-1, "", "", "", "", "") ! Inicializar la tabla con valores predeterminados
    end subroutine init

    integer function division(this, k)
        class(hash), intent(inout) :: this
        integer(kind=16), intent(in) :: k
        division = mod(k, this%m)
    end function division

    integer function linear(this, k)
        class(hash), intent(inout) :: this
        integer, intent(in) :: k
        linear = mod(k + 1, this%m)
    end function linear

    subroutine insert_hash(this, persona_obj)
        class(hash), intent(inout) :: this
        type(persona), intent(in) :: persona_obj
        integer :: i
        i = this%division(persona_obj%dpi)
        do while (this%personas(i)%dpi /= -1)
            i = this%linear(i)
        end do
        this%personas(i) = persona_obj
        this%n = this%n + 1
        call this%rehashing()
    end subroutine insert_hash

    subroutine rehashing(this)
        class(hash), intent(inout) :: this
        integer :: i, mprev
        type(persona), dimension(:), allocatable :: temp
        if (this%n * 100 / this%m >= this%maxi) then
            allocate(temp(this%m))
            temp = this%personas
            mprev = this%m
            this%m = this%n * 100 / this%mini
            call this%init(this%m, this%mini, this%maxi)
            do i = 1, mprev
                if (temp(i)%dpi /= -1) then
                    call this%insert_hash(temp(i))

                end if
            end do
        end if
    end subroutine rehashing

    subroutine show(this)
        class(hash), intent(inout) :: this
        integer :: i
        write (*, '(a)', advance='no') '['
        do i = 1, this%m
            write (*, '(1I13)', advance='no') this%personas(i)%dpi
        end do
        write(*, '(A, I0, A)') '] ', (this%n * 100 / this%m), '%'
    end subroutine show

end module hash_module
