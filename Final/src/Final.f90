module Final
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, Final!"
  end subroutine say_hello
end module Final
