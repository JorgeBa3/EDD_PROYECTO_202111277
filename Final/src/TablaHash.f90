module hash_module
    use iso_fortran_env, only: int64
    implicit none
    type persona
        integer(kind=int64) :: dpi
        character(len=20) :: nombre
        character(len=20) :: apellido
        character(len=20) :: genero
        character(len=50) :: direccion
        character(len=20) :: telefono
        !integer :: cantidad_pedidos
        !integer :: cantidad_realizados
    end type persona

    type hash
        integer :: n ! elements
        integer :: m ! size table
        integer :: mini, maxi ! percentages
        type(persona), dimension(:), allocatable :: personas ! table
    contains
        procedure :: init
        procedure :: division
        procedure :: generate_dot_file
        procedure :: linear
        procedure :: insert_hash
        procedure :: rehashing
        procedure :: show
        procedure :: buscar_tecnico
        procedure :: generate_dot_file_all_tecnicos
    end type hash

contains
    
subroutine generate_dot_file_all_tecnicos(this)
    class(hash), intent(in) :: this
    integer :: i
    character(len=50) :: filename

    ! Abre el archivo para escribir
    filename = 'hash_table_all_tecnicos.dot'
    open(unit=10, file=filename, status='unknown')

    ! Escribe el encabezado del archivo DOT
    write(10, '(A)') 'digraph HashTable {'
    write(10, '(A)') '    node [shape=record];'

    ! Escribe los nodos de la tabla hash para todos los técnicos almacenados
    do i = 1, this%m
        if (this%personas(i)%dpi /= -1) then
            write(10, '(A, I0, A, I16, A, A, A, A, A, A, A, A, A, A, A)') '    node', i, ' [label="{<dpi>', this%personas(i)%dpi,&
            '|<nombre>', trim(this%personas(i)%nombre), '|<apellido>', trim(this%personas(i)%apellido),& 
            '|<genero>', trim(this%personas(i)%genero), '|<direccion>', trim(this%personas(i)%direccion),& 
            '|<telefono>', trim(this%personas(i)%telefono), '}"];'
        end if
    end do

    ! Cierra el archivo
    write(10, '(A)') '}'
    close(10)

    ! Ejecuta el comando dot para generar la imagen
    call execute_command_line('dot -Tpng hash_table_all_tecnicos.dot -o hash_table_all_tecnicos.png')
end subroutine generate_dot_file_all_tecnicos


    subroutine buscar_tecnico(this, dpi)
        class(hash), intent(inout) :: this
        integer(kind=int64), intent(in) :: dpi
        integer :: i
        i = this%division(dpi)
        do while (this%personas(i)%dpi /= dpi)
            i = this%linear(i)
        end do
        write(*, '(A, I0, A, A, A, A, A, A, A, A, A, A)') 'DPI: ', this%personas(i)%dpi, &
        ',Nombre: ', trim(this%personas(i)%nombre), ', Apellido: ', trim(this%personas(i)%apellido),&
        ', Genero: ', trim(this%personas(i)%genero), ', Direccion: ', trim(this%personas(i)%direccion),&
        ', Telefono: ', trim(this%personas(i)%telefono)
    end subroutine buscar_tecnico
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
        integer(kind=int64), intent(in) :: k
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
            write (*, '(1I15)', advance='no') this%personas(i)%dpi
        end do
        write(*, '(A, I0, A)') '] ', (this%n * 100 / this%m), '%'
    end subroutine show

    subroutine generate_dot_file(this)
        class(hash), intent(in) :: this
        integer :: i
        character(len=50) :: filename
    
        ! Abre el archivo para escribir
        filename = 'hash_table.dot'
        open(unit=10, file=filename, status='unknown')
    
        ! Escribe el encabezado del archivo DOT
        write(10, '(A)') 'digraph HashTable {'
        write(10, '(A)') '    node [shape=record];'
    
        ! Escribe los nodos de la tabla hash
        do i = 1, this%m
            if (this%personas(i)%dpi /= -1) then
                write(10, '(A, I0, A, I16, A, A)') '    node', i, ' [label="{<dpi>', this%personas(i)%dpi,&
                '|<nombre>', trim(this%personas(i)%nombre), '}"];'
            end if
        end do
    
        ! Cierra el archivo
        write(10, '(A)') '}'
        close(10)
    
        ! Ejecuta el comando dot para generar la imagen
        call execute_command_line('dot -Tpng hash_table.dot -o hash_table.png')
    end subroutine generate_dot_file
    
    
    

end module hash_module
