module Proyecto
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, Proyecto!"
  end subroutine say_hello
end module Proyecto
