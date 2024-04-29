module hash_module
    implicit none
    integer, parameter :: long = selected_int_kind(18)
    
    integer, parameter :: dp = selected_real_kind(15)
    
type hash
    integer :: n ! elements
    integer :: m ! size table
    integer :: mini, maxi ! percentages
    integer, dimension(:), allocatable :: h ! table
contains
    procedure :: init
    procedure :: division
    procedure :: linear
    procedure :: insert
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
    if (allocated(this%h)) then
        deallocate(this%h)
    end if
    allocate(this%h(m))
    this%h = -1
end subroutine init

integer function division(this, k)
class(hash), intent(inout) :: this
    integer(kind=long), intent(in) :: k
    division = mod(k, this%m)
end function division

integer function linear(this, k)
class(hash), intent(inout) :: this
    integer, intent(in) :: k
    linear = mod(k + 1, this%m)
end function linear

subroutine insert(this, k)
class(hash), intent(inout) :: this
    integer(kind=long), intent(in) :: k
    integer :: i
    i = this%division(k)
    do while (this%h(i) /= -1)
        i = this%linear(i)
    end do
    this%h(i) = k
    this%n = this%n + 1
    call this%rehashing()
end subroutine insert

subroutine rehashing(this)
class(hash), intent(inout) :: this
    integer :: i, mprev
    integer(kind=long), dimension(:), allocatable :: temp
    if (this%n * 100 / this%m >= this%maxi) then
        allocate(temp(this%m))
            temp = this%h
            call this%show()
            mprev = this%m
            this%m = this%n * 100 / this%mini
            call this%init(this%m, this%mini, this%maxi)
            do i = 1, mprev
                if (temp(i) /= -1) then
                        call this%insert(temp(i))
                end if
            end do
    else
            call this%show()
    end if
end subroutine rehashing

subroutine show(this)
    class(hash), intent(inout) :: this
    integer :: i
    write (*, '(a)', advance='no') '['
    do i = 1, this%m
        write (*, '(I13)', advance='no') this%h(i) ! Cambio de '1I3' a 'I4'
    end do
    write(*, '(A, I0, A)') '] ', (this%n * 100 / this%m), '%'
end subroutine show


end module hash_module

program hash_main
use hash_module
    implicit none
    integer:: a = 1234567890
    integer :: m = 7 !size table
    integer :: mini = 13, maxi = 70 ! min and max percentage
    type(hash) :: h
    call h%init(m, mini, maxi)
    print *, 'Hash table with division method and linear probing'
    print *, 'Initial size table: ', m
print *, 'Minimum percentage: ', mini
print *, 'Maximum percentage: ', maxi
    call h%insert(a)
    call h%insert(int(5, kind=long))
    call h%insert(int(15, kind=long))
    call h%insert(int(25, kind=long))
    call h%insert(int(35, kind=long))
    call h%insert(int(45, kind=long))
    call h%show()
end program hash_main