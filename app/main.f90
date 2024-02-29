program main
    use json_module
    use lista_ventanillas_m
    use cola_recepcion_m
    use cola_impresoras_m
    use pila_img_m

    implicit none
    type(cola_imp) :: impresora_grande
    type(cola_imp) :: impresora_pequena
    type(json_file) :: json
    type(cliente) :: cliente_nuevo
    logical :: found
    integer :: id, img_g, img_p, num_pasadas, i, opcion, n_ventanillas, j,k, num_paso
    character(len=:),allocatable :: nombre, texto
    character(len=40) :: id_str, nombre_json, nombre_completo
    type(lista_v) :: lista_ventanillas
    type(cola_r) :: cola_recepcion
    type(pila_i) :: pila_imagenes
    

    !Clientes aleatorios
    integer :: new_img_p, new_img_g, num_clientes_aleatorios
    integer, parameter :: num_nombres = 7
integer, parameter :: num_apellidos = 7
real :: rnd1(1), rnd2(1), rnd3(1), rnd4(1), rnd5(1)
character(len=40), dimension(num_nombres) :: nombres
character(len=40), dimension(num_apellidos) :: apellidos
integer :: values(8) ! Array para almacenar los valores de fecha y hora
character(len=8) :: date_string, time_string ! Cadenas para almacenar la fecha y la hora  


!asignacion impresoras
call impresora_grande%nueva_impresora(1, "Impresora Grande ", 2)
call impresora_pequena%nueva_impresora(2, "Impresora Pequena", 1)
! Asignar nombres
nombres = [ "Jorge ", "Jose  ", "Juan  ", "Maria ", "Carlos", "Luis  ", "Ana   " ]

! Asignar apellidos
apellidos = ["De Leon  ", "Batres   ", "Gonzalez ", "Lopez    ", "Martinez ", "Perez    ", "Sanchez  " ]
num_paso = 1
! Llamar a date_and_time para obtener la fecha y la hora actual
call date_and_time(values=values)

! Inicializar generador de números aleatorios con el tiempo actual
call srand(seed=int(40*values(7))) ! Usamos los segundos como semilla
call json%initialize()

do
    call mostrar_menu_principal()
    read(*,*) opcion

    select case(opcion)
        case(1)
            call submenu_parametros()
        case(2)
            call ejecutar_paso()
        case(3)
            call estado_memoria()
        case(4)
            call generar_reportes()
        case(5)
            call acerca_de()
        case(6)
            exit
        case default
            print *, 'Opcion no valida. Por favor, seleccione una opcion valida.'
    end select
end do

! Limpiar
call json%destroy()
if (json%failed()) stop 4




contains

subroutine mostrar_menu_principal()
      print *, '--- Menu ---'
      print *, '1. Parametros iniciales'
      print *, '2. Ejecutar paso'
      print *, '3. Estado en memoria de las estructuras'
      print *, '4. Reportes'
      print *, '5. Acerca de'
      print *, '6. Salir'
      print *, 'Ingrese su opcion:'
end subroutine mostrar_menu_principal

subroutine submenu_parametros()
    implicit none
    integer :: opcion_parametros

    do
        call mostrar_submenu_parametros()
        read(*,*) opcion_parametros

        select case(opcion_parametros)
            case(1)
                call carga_masiva_clientes()
            case(2)
                call cantidad_ventanillas()
            case(3)
                exit
            case default
                  print *, 'Opcion no valida. Por favor, seleccione una opcion valida.'
        end select
    end do
end subroutine submenu_parametros

    subroutine mostrar_submenu_parametros()
          print *, '--- Parametros iniciales ---'
          print *, '1. Carga masiva de clientes'
          print *, '2. Cantidad de ventanillas'
          print *, '3. Volver al menu principal'
          print *, 'Ingrese su opcion:'
    end subroutine mostrar_submenu_parametros

    subroutine carga_masiva_clientes()
          print *, 'Ha seleccionado Carga masiva de clientes'
        ! read the file
    print *, 'Ingrese el nombre del  archivo JSON:'
    read(*, '(A)') nombre_json
call json%load(filename = nombre_json)

! print the file to the console
call json%print()

! Obtener la cantidad de registros en el JSON
num_pasadas = 0
do i = 1, 400 ! Suponemos que hay como maximo 400 registros para evitar bucles infinitos
    write(id_str, '(I10)') i  ! Convertir entero a cadena
    call json%get('['//trim(adjustl(id_str))//'].id', id, found)
    if (.not. found) exit ! Si no se encuentra el indice, salimos del bucle
    num_pasadas = num_pasadas + 1
end do

! Imprimir la cantidad de registros
print *, 'Cantidad de registros:', num_pasadas

! Realizar el bucle desde 1 hasta num_pasadas
do i = 1, num_pasadas
    write(id_str, '(I10)') i  ! Convert integer to string
    call json%get('['//trim(adjustl(id_str))//'].id', id, found)
    if (.not. found) stop 11

    write(id_str, '(I10)') i  ! Convert integer to string
    call json%get('['//trim(adjustl(id_str))//'].nombre', nombre, found)
    if (.not. found) stop 12

    write(id_str, '(I10)') i  ! Convert integer to string
    call json%get('['//trim(adjustl(id_str))//'].img_g', img_g, found)
    if (.not. found) stop 13

    write(id_str, '(I10)') i  ! Convert integer to string
    call json%get('['//trim(adjustl(id_str))//'].img_p', img_p, found)
    if (.not. found) stop 14
    call cola_recepcion%push(id, nombre, img_g, img_p)
    end do
    call cola_recepcion%print()
    end subroutine carga_masiva_clientes

    subroutine cantidad_ventanillas()
         print *, 'Ha seleccionado Cantidad de ventanillas'
        print *, 'Ingrese la cantidad  de ventanillas:'
        read(*, *) n_ventanillas
        do i = 1, n_ventanillas
            call lista_ventanillas%nueva_ventanilla(i)
        end do
        call lista_ventanillas%print_ven()
    end subroutine cantidad_ventanillas


    subroutine ejecutar_paso()

        
        integer :: nuevo_id, nuevo_img_g, nuevo_img_p
        print *, 'Ha seleccionado Ejecutar paso'
        WRITE(*, '(A, I0, A)') ' ------------------------ Paso ', num_paso, ' ------------------------'
        num_paso = num_paso + 1
        call random_number(rnd5)
        num_clientes_aleatorios = mod(int(rnd5(1) * 400), 4)
        print *, 'Numero ventanillas', n_ventanillas
        ! Buscar ventanillas disponibles
        do i = 1, num_clientes_aleatorios
            call cliente_aleatorio()
        end do
        
        do i = 1, n_ventanillas
            if (lista_ventanillas%tiene_cliente(i)) then
                print *, 'Ventanilla', i, 'tiene cliente'
                
            else
                print *, 'Ventanilla', i, 'no tiene cliente'
                nuevo_id = cola_recepcion%toma_id()
                nuevo_img_g = cola_recepcion%toma_img_g()
                nuevo_img_p = cola_recepcion%toma_img_p()
                call cliente_nuevo%crear_cliente(nuevo_id, nuevo_img_g, nuevo_img_p)
                call lista_ventanillas%agregar_cliente(cliente_nuevo,i)
                call cola_recepcion%pop()
                exit !sale del samsara 
                
                
            end if
        end do
        call lista_ventanillas%atender_cliente()
        call cola_recepcion%print()
        call lista_ventanillas%print_ven()
    end subroutine ejecutar_paso


    subroutine cliente_aleatorio
        call random_number(rnd1)
        call random_number(rnd2)
        call random_number(rnd3)
        call random_number(rnd4)
        k = 1 + mod(int(rnd1(1) * 400), num_nombres)
        j = 1 + mod(int(rnd2(1) * 400), num_apellidos)
        new_img_g = mod(int(rnd3(1) * 400), 5)
        new_img_p = mod(int(rnd4(1) * 400), 5)
        ! Compose the full name
        write(nombre_completo, '(A," ",A)') trim(nombres(k)), trim(apellidos(j))
    
        ! Display the generated full name
        print *, 'Nombre generado:', nombre_completo

        num_pasadas = num_pasadas + 1
        call cola_recepcion%push(num_pasadas, nombre_completo, new_img_g, new_img_p)
        
        
    end subroutine cliente_aleatorio
    

    subroutine estado_memoria()
        print *, 'Ha seleccionado Estado en memoria de las estructuras'
        
        
        call impresora_grande%generar_grafo_cola_imp("cola_impresora_grande")
        call impresora_pequena%generar_grafo_cola_imp("cola_impresora_pequeña")
        call lista_ventanillas%graficar_ventanilla("ventanillas")
    end subroutine estado_memoria

    subroutine generar_reportes()
      print *, 'Ha seleccionado Reportes'
    ! Aqui puedes incluir el codigo para generar reportes
    end subroutine generar_reportes

    subroutine acerca_de()
      print *, 'Ha seleccionado Acerca de'
      print *, 'Jorge Alejandro De Leon Batres - 202111277'
    end subroutine acerca_de
end program main
