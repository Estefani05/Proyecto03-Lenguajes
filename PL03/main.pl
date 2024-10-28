:- discontiguous procesar_opcion_administrativa/1.



% Cargar relaciones desde un archivo
% Propósito: Cargar relaciones desde un archivo de texto y asociarlas al sistema.
% Entrada: Ninguna.
% Salida: Crea hechos en la base de datos a partir de las relaciones leídas del archivo.
cargar_relaciones :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\relaciones.txt', read, Stream),
    cargar_relaciones_aux(Stream),  % Llama a la función auxiliar para cargar relaciones
    close(Stream).  % Cierra el flujo de entrada

% Auxiliar para cargar relaciones
% Propósito: Procesar cada relación en el archivo.
% Entrada: Stream - Flujo de entrada desde el cual se leen las relaciones.
% Salida: Crea hechos en la base de datos a partir de las relaciones leídas.
cargar_relaciones_aux(Stream) :-
    repeat,
    read(Stream, Relacion),  % Lee una relación del flujo
    (   Relacion == end_of_file  % Verifica si se ha llegado al final del archivo
    ->  !
    ;   assert(Relacion),  % Si es una relación válida, se asocia al sistema
        fail  % Continúa leyendo
    ).

% Cargar destinos desde un archivo
% Propósito: Cargar destinos desde un archivo de texto y asociarlos al sistema.
% Entrada: Ninguna.
% Salida: Crea hechos en la base de datos a partir de los destinos leídos del archivo.
cargar_destino :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\destino.txt', read, Stream),
    cargar_destino_aux(Stream),  % Llama a la función auxiliar para cargar destinos
    close(Stream).  % Cierra el flujo de entrada

% Auxiliar para cargar destinos
% Propósito: Procesar cada destino en el archivo.
% Entrada: Stream - Flujo de entrada desde el cual se leen los destinos.
% Salida: Crea hechos en la base de datos a partir de los destinos leídos.
cargar_destino_aux(Stream) :-
    read_term(Stream, Term, []),  % Lee un término del flujo
    (   Term == end_of_file  % Verifica si se ha llegado al final del archivo
    ->  true
    ;   Term = destino(Nombre_del_destino, Descripcion_del_destino),  % Si es un destino válido
        assert(destino(Nombre_del_destino, Descripcion_del_destino)),  % Se asocia al sistema
        cargar_destino_aux(Stream)  % Continúa leyendo
    ;   writeln('Error procesando la linea de destinos.')  % Muestra un mensaje de error en caso de fallo
    ).

% Cargar actividades desde un archivo
% Propósito: Cargar actividades desde un archivo de texto y asociarlas al sistema.
% Entrada: Ninguna.
% Salida: Crea hechos en la base de datos a partir de las actividades leídas del archivo.
cargar_actividades :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\actividad.txt', read, Stream),
    cargar_actividades_aux(Stream),  % Llama a la función auxiliar para cargar actividades
    close(Stream).  % Cierra el flujo de entrada

% Auxiliar para cargar actividades
% Propósito: Procesar cada actividad en el archivo.
% Entrada: Stream - Flujo de entrada desde el cual se leen las actividades.
% Salida: Crea hechos en la base de datos a partir de las actividades leídas.
cargar_actividades_aux(Stream) :-
    read(Stream, Term),  % Lee un término del flujo
    (   Term == end_of_file  % Verifica si se ha llegado al final del archivo
    ->  true
    ;   Term =.. [actividad, Nombre, CostoNum, DuracionNum, DescripcionAtom, ListaTipo],  % Si es una actividad válida
        assert(actividad(Nombre, CostoNum, DuracionNum, DescripcionAtom, ListaTipo)),  % Se asocia al sistema
        cargar_actividades_aux(Stream)  % Continúa leyendo
    ;   writeln('Error procesando la linea de actividades.')  % Muestra un mensaje de error en caso de fallo
    ).
% Cargar asociaciones desde asociar_actividad.txt
% Propósito: Cargar asociaciones de actividades desde un archivo de texto y almacenarlas en la base de datos.
% Entrada: Ninguna (el archivo se especifica directamente en el código).
% Salida: Crea dinámicamente hechos en la base de datos Prolog a partir del contenido del archivo.

cargar_asociaciones :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\asociar_actividad.txt', read, Stream),
    leer_asociaciones(Stream),
    close(Stream).

% leer_asociaciones(+Stream)
% Propósito: Leer términos del flujo de entrada y asertarlos en la base de datos.
% Entrada: Stream - un flujo de entrada abierto desde donde se leerán los términos.
% Salida: Crea hechos asociar_actividad(Destino, Actividad) en la base de datos.
% Si se encuentra end_of_file, termina la lectura. Si hay un error de procesamiento, se imprime un mensaje.

leer_asociaciones(Stream) :-
    read_term(Stream, Term, []),
    (   Term == end_of_file
    ->  true  % Termina la lectura si se alcanza el final del archivo
    ;   Term = asociar_actividad(Destino, Actividad),
        assert(asociar_actividad(Destino, Actividad)),  % Asegura el término en la base de datos
        leer_asociaciones(Stream)  % Llama recursivamente para leer el siguiente término
    ;   writeln('Error procesando la linea de asociaciones.')  % Manejo de errores
    ).

% Cargar todos los datos desde los archivos
:- cargar_destino.
:- cargar_actividades.
:- cargar_asociaciones.

% Menu principal
% Propósito: Proporcionar un menú interactivo para que el usuario elija opciones administrativas o salir del programa.
% Entrada: Ninguna (la opción se lee de la entrada estándar).
% Salida: Llama al menú administrativo si la opción es 1, o termina el programa si la opción es 2.
% Si la opción no es válida, solicita al usuario que intente de nuevo.

menu_principal :-
  repeat,
  writeln('Menu Principal'),
  writeln('1. Opciones administrativas'),
  writeln('2. Salir'),
  write('Seleccione una opcion: '),
  read(Opcion),
  (   Opcion = 1
  ->  menu_administrativo  % Llama al menú administrativo si la opción es 1
  ;   Opcion = 2
  ->  writeln('Saliendo del programa.'), true  % Mensaje de salida si la opción es 2
  ;   writeln('Opcion no valida, por favor intente de nuevo.')  % Mensaje de error para opción no válida
  ),
  Opcion = 2, true,  % Asegura que el programa se detenga si se selecciona la opción 2
  halt.  % Finaliza el programa


% Menu administrativo
% Propósito: Proporcionar un menú interactivo para que el usuario elija opciones administrativas diversas.
% Entrada: Ninguna (la opción se lee de la entrada estándar).
% Salida: Llama a diferentes predicados según la opción seleccionada, permitiendo agregar hechos, consultar información,
% generar itinerarios, mostrar estadísticas, entre otras opciones.

menu_administrativo :-
  repeat,
  writeln('\nMenu Administrativo'),
  writeln('1. Agregar hechos'),
  writeln('2. Consulta destino'),
  writeln('3. Actividades por tipo'),
  writeln('4. Consulta por precio'),
  writeln('5. Generar itinerario por monto'),
  writeln('6. Generar itinerario por dias'),
  writeln('7. Recomendar por frase'),
  writeln('8. Estadisticas'),
  writeln('9. Volver'),
  write('Seleccione una opcion: '),
  read(Opcion),
  ( Opcion = 9
  ->  writeln('Volviendo al Menu Principal.'),
      menu_principal  % Vuelve al menú principal si la opción es 9
  ;   Opcion = 1
    ->  menu_agregar_hechos  % Llama al menu para agregar hechos si la opción es 1
  ;   Opcion = 2
    ->  pre_consultar_actividades_destino  % Inicia la consulta de actividades por destino si la opcion es 2
  ;   Opcion = 3
    ->  consultar_actividades_tipo  % Consulta actividades por tipo si la opción es 3
  ;  Opcion = 4
    ->  consultar_actividades_precio  % Consulta actividades por precio si la opción es 4
  ;  Opcion = 5
    -> generar_itinerario
  ; Opcion = 6
    -> itinerario_por_dias
  ; Opcion = 7
  -> solicitar_frase
  ;  Opcion = 8
    ->  mostrar_estadisticas,  % Muestra estadisticas si la opción es 8
        mostrar_categoria_mas_frecuente  % Muestra la categoria mas frecuente si la opción es 8
  ).

% Menu agregar hechos
% Proposito: Proporcionar un menu interactivo para que el usuario elija entre agregar destinos, actividades o asociar actividades a destinos.
% Entrada: Ninguna (la opcion se lee de la entrada estandar).
% Salida: Llama a diferentes predicados segun la opcion seleccionada, permitiendo agregar destinos, actividades o asociar actividades.

menu_agregar_hechos :- 
    repeat,
    writeln('\nMenu Agregar Hechos'),
    writeln('1. Agregar destino'),
    writeln('2. Agregar actividad'),
    writeln('3. Asociar actividad a destino'),
    writeln('4. Volver'),
    write('Seleccione una opcion: '),
    read(Opcion),
    ( Opcion = 1
        ->  agregar_destino  % Llama al predicado para agregar un destino si la opcion es 1
    ;
    Opcion = 2
    ->  agregar_actividad  % Llama al predicado para agregar una actividad si la opcion es 2
    ;
    Opcion = 3
    ->  asociar_actividad_destino  % Llama al predicado para asociar una actividad a un destino si la opcion es 3
    ;
      Opcion = 4
    ->  writeln('Volviendo al Menu Administrativo.'),  % Mensaje de salida si la opcion es 4
        menu_administrativo  % Vuelve al menu administrativo
    ).

% --------------------------------------% Agregar destino - opcion 1--------------------------------
% Agregar destino
% Proposito: Permitir al usuario agregar un nuevo destino al sistema, incluyendo su nombre y descripcion.
% Entrada: El nombre del destino y su descripcion se leen de la entrada estandar.
% Salida: Agrega el destino a la base de datos y escribe la informacion en un archivo.

agregar_destino :- 
    write('Debe separarse por _ si es necesario, de lo contrario abra un error en sistema.'),
    nl,
    write('Ingrese el nombre del destino, usa comillas simples para ingresar el texto: '),
    read(Nombre),  % Captura el nombre con espacios
    validar_nombre(Nombre),  % Llamada a la validacion del nombre
    write('Ingrese la descripcion del destino: '),
    read(DescripcionInput),  % Leemos la descripcion ingresada
    format(atom(Descripcion), "'~w'", [DescripcionInput]),  % Formatea la descripcion como un atomo
    assert(destino(Nombre, Descripcion)),  % Agrega el destino a la base de datos
    writeln('Destino agregado exitosamente.'),  % Mensaje de confirmacion
    escribir_destinoTxt(Nombre, Descripcion),  % Llamada para escribir en el archivo
    writeln('Regresando al menu de agregar hechos...').  % Mensaje de regreso al menu

% Escribir destino en archivo
% Proposito: Escribe la informacion de un destino en el archivo destino.txt.
% Entrada: Nombre y descripcion del destino.
% Salida: Escribe la informacion en el archivo destino.txt.

escribir_destinoTxt(Nombre, Descripcion) :-
    % Abrir el archivo destino.txt en modo append
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\destino.txt', append, Stream),
    % Escribir la informacion del destino en el archivo
    write(Stream, destino(Nombre, Descripcion)),
    % Agregar un punto al final de la linea
    write(Stream, '.'),
    % Agregar un salto de linea
    nl(Stream),
    % Cerrar el archivo
    close(Stream).




% --------------------------------------% Agregar actividad - opcion 2--------------------------------

% Agregar actividad
% Proposito: Permitir al usuario agregar una nueva actividad al sistema, incluyendo su nombre, costo, duracion, descripcion y tipos.
% Entrada: Nombre, costo, duracion, descripcion y tipos de la actividad se leen de la entrada estandar.
% Salida: Agrega la actividad a la base de datos y escribe la informacion en un archivo.

agregar_actividad :- 
    write('Ingrese el nombre de la actividad: '),
    read(Nombre),  % Captura el nombre de la actividad
    validar_nombre(Nombre),  % Llamada a la validacion del nombre
    write('Ingrese el costo de la actividad: '),
    read(Costo),  % Captura el costo de la actividad
    validar_costo(Costo),  % Llamada a la validacion del costo
    write('Ingrese la duracion de la actividad (en dias): '),
    read(Duracion),  % Captura la duracion de la actividad
    validar_duracion(Duracion),  % Llamada a la validacion de la duracion
    write('Ingrese la descripcion de la actividad: '),
    read(DescripcionInput),  % Leemos la descripcion ingresada
    format(atom(Descripcion), "'~w'", [DescripcionInput]),  % Formatea la descripcion como un atomo
    write('Ingrese los tipos de la actividad (ej. [\'aventura\',\'naturaleza\']): '),
    read(TiposInput),  % Leemos la entrada del usuario como lista
    validar_tipos(TiposInput),  % Llamamos a la validacion de tipos
    assert(actividad(Nombre, Costo, Duracion, Descripcion, TiposInput)),  % Agrega la actividad a la base de datos
    writeln('Actividad agregada exitosamente.'),  % Mensaje de confirmacion
    escribir_actividadTxt(Nombre, Costo, Duracion, Descripcion, TiposInput),  % Llamada para escribir en el archivo
    writeln('Regresando al menu de agregar hechos...').  % Mensaje de regreso al menu

% Validar tipos
% Proposito: Verifica que la entrada sea una lista de strings o átomos.
% Entrada: Tipos (debe ser una lista).
% Salida: Verifica que cada elemento de la lista sea un string o un átomo.

validar_tipos(Tipos) :-
    is_list(Tipos),  % Verifica que Tipos sea una lista
    forall(member(Tipo, Tipos), (atom_string(Tipo, _) ; string(Tipo))),  % Verifica que cada elemento sea un string o átomo
    !.

% Escribir actividad en archivo
% Proposito: Escribe la informacion de una actividad en el archivo actividad.txt.
% Entrada: Nombre, costo, duracion, descripcion y tipos de la actividad.
% Salida: Escribe la informacion en el archivo actividad.txt.

escribir_actividadTxt(Nombre, Costo, Duracion, Descripcion, Tipos) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\actividad.txt', append, Stream),  % Abrir el archivo en modo append
    write(Stream, actividad(Nombre, Costo, Duracion, Descripcion, Tipos)),  % Escribir la informacion de la actividad en el archivo
    write(Stream, '.'),  % Agregar un punto al final de la linea
    nl(Stream),  % Agregar un salto de linea
    close(Stream).  % Cerrar el archivo


% --------------------------------------% asociar actividad-opcion 3--------------------------------
% Asociar actividad a destino
% Proposito: Permite al usuario asociar una actividad existente a un destino existente.
% Entrada: Nombre del destino y nombre de la actividad se leen de la entrada estandar.
% Salida: Asocia la actividad al destino y escribe la información en un archivo.

asociar_actividad_destino :- 
    write('Ingrese el nombre del destino: '),
    read(Destino),  % Captura el nombre del destino
    verificar_destino(Destino),  % Verifica si el destino existe
    !,  % Continuar solo si el destino es válido
    write('Ingrese el nombre de la actividad: '),
    read(Actividad),  % Captura el nombre de la actividad
    verificar_actividad(Actividad),  % Verifica si la actividad existe
    assert(asociar_actividad(Destino, Actividad)),  % Asocia la actividad al destino
    escribir_asosiarTxt(Destino, Actividad),  % Llama a la función para escribir en el archivo
    writeln('Actividad asociada exitosamente.'),  % Mensaje de confirmación
    writeln('Regresando al menu de agregar hechos...').  % Mensaje de regreso al menú


% Escribir asociación en archivo
% Proposito: Escribe la información de la asociación entre actividad y destino en un archivo.
% Entrada: Nombre del destino y nombre de la actividad.
% Salida: Escribe la información en el archivo asociar_actividad.txt.

escribir_asosiarTxt(Destino, Actividad) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\asociar_actividad.txt', append, Stream),  % Abrir el archivo en modo append
    write(Stream, asociar_actividad(Destino, Actividad)),  % Escribir la asociación en el archivo
    write(Stream, '.'),  % Agregar un punto al final de la línea
    nl(Stream),  % Agregar un salto de línea
    close(Stream).  % Cerrar el archivo


% Verificar actividad
% Proposito: Comprueba si la actividad ingresada existe en el archivo actividad.txt.
% Entrada: Nombre de la actividad.
% Salida: Verdadero si la actividad existe, falso en caso contrario.

verificar_actividad(Actividad) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\actividad.txt', read, Stream),  % Abrir el archivo en modo lectura
    buscar_actividad(Stream, Actividad, Existe),  % Llamar a la función que busca la actividad
    close(Stream),  % Cerrar el archivo
    (   Existe == true
    ->  true  % Continuar si la actividad existe
    ;   writeln('Error: La actividad ingresada no existe.'),  % Mensaje de error
        fail  % Detener el proceso si no existe
    ).


% Verificar destino
% Proposito: Comprueba si el destino ingresado existe en el archivo destino.txt.
% Entrada: Nombre del destino.
% Salida: Verdadero si el destino existe, falso en caso contrario.

verificar_destino(Destino) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\destino.txt', read, Stream),  % Abrir el archivo en modo lectura
    buscar_destino(Stream, Destino, Existe),  % Llamar a la función que busca el destino
    close(Stream),  % Cerrar el archivo
    (   Existe == true
    ->  true  % Continuar si el destino existe
    ;   writeln('Error: El destino ingresado no existe.'),  % Mensaje de error
        fail  % Detener el proceso si no existe
    ).

% Predicado para buscar un destino en el archivo destino.txt
buscar_destino(Stream, DestinoBuscado, Existe) :-
    read(Stream, Term),
    (   Term == end_of_file
    ->  Existe = false
    ;   (   Term = destino(DestinoActual, _),  % Ignorar el segundo argumento
            DestinoActual == DestinoBuscado
        ->  Existe = true
        ;   buscar_destino(Stream, DestinoBuscado, Existe)
        )
    ).


% Propósito:
% El predicado buscar_actividad/3 busca una actividad específica en un archivo de texto llamado actividad.txt.
% El archivo se supone que contiene términos en el formato actividad(Actividad, _, _, _, _), donde Actividad es el nombre de la actividad y los otros argumentos se ignoran.
% Entrada:
% - Stream: el flujo de entrada del archivo actividad.txt.
% - ActividadBuscada: la actividad que se busca en el archivo.
% - Existe: una variable que se unificará con true si la actividad se encuentra en el archivo, o false en caso contrario.

% Salida:
% - Existe: una variable que indica si la actividad se encuentra en el archivo.

buscar_actividad(Stream, ActividadBuscada, Existe) :-
    % Leer un término del archivo
    read(Stream, Term),
    % Si se llega al final del archivo, la actividad no existe
    (   Term == end_of_file
    ->  Existe = false
    ;   % Si el término es una actividad, verificar si es la buscada
        (   Term = actividad(ActividadActual, _, _, _, _)  % Ignorar los otros argumentos
        ->  % Si es la actividad buscada, unificar Existe con true
            (   ActividadActual == ActividadBuscada
            ->  Existe = true
            ;   % Si no es la actividad buscada, seguir buscando
                buscar_actividad(Stream, ActividadBuscada, Existe)
            )
        ;   % Si el término no es una actividad, seguir buscando
            buscar_actividad(Stream, ActividadBuscada, Existe)
        )
    ).


% ----------------------consultar actividades por destino--------------------------------------------------------


% Predicado principal para consultar actividades por destino.
% Propósito:
% El predicado pre_consultar_actividades_destino/0 permite al usuario ingresar el nombre de un destino
% y luego llama al predicado consultar_actividades_destino/1 para realizar la consulta de actividades
% asociadas a dicho destino.
% Entrada:
% - No tiene parámetros de entrada.
% Salida:
% - Llama al predicado consultar_actividades_destino/1 con el destino ingresado por el usuario.

pre_consultar_actividades_destino :-
    write('Ingrese el nombre del destino: '),  % Solicita al usuario el nombre del destino
    read(Destino),                            % Lee el destino ingresado
    consultar_actividades_destino(Destino).  % Llama al predicado para consultar actividades


% Predicado para consultar actividades asociadas a un destino.
% Propósito:
% El predicado consultar_actividades_destino/1 busca obtener las actividades
% asociadas a un destino específico desde un archivo y muestra sus detalles.
% Entrada:
% - Destino: el nombre del destino para el cual se desean consultar las actividades.
% Salida:
% - Muestra las actividades disponibles y sus detalles, así como el costo total
%   y la duración total de las actividades.

consultar_actividades_destino(Destino) :-
    % Obtener la actividad asociada al destino desde el archivo asociar_actividad.txt
    obtener_actividad_por_destino(Destino, ActividadesDestino),
    (   ActividadesDestino = [] 
    ->  writeln('No hay actividades para este destino.')
    ;   % Buscar los detalles de las actividades en el archivo actividad.txt
        obtener_detalles_actividades(ActividadesDestino, DetallesActividades),
        (   DetallesActividades = []
        ->  writeln('No hay actividades con detalles disponibles para este destino.')
        ;   writeln('Actividades disponibles en '), write(Destino), writeln(':'),
            listar_actividades(DetallesActividades), % Mostrar actividades una vez
            % Calcular totales solo una vez
            calcular_totales(DetallesActividades, CostoTotal, DuracionTotal),
            write('Costo total: '), writeln(CostoTotal),
            write('Duracion total (en dias): '), writeln(DuracionTotal)
        )
    ),
    writeln('Regresando al menu administrativo...').

% Predicado para obtener las actividades asociadas a un destino desde asociar_actividad.txt
obtener_actividad_por_destino(Destino, ActividadesDestino) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\asociar_actividad.txt', read, Stream),
    read_actividades_destino(Stream, Destino, ActividadesDestino),
    close(Stream).

read_actividades_destino(Stream, Destino, Actividades) :-
    read(Stream, Term),
    (   Term \= end_of_file
    ->  (   Term = asociar_actividad(DestinoActual, Actividad),
            DestinoActual = Destino
        ->  Actividades = [Actividad | Resto],
            read_actividades_destino(Stream, Destino, Resto)
        ;   read_actividades_destino(Stream, Destino, Actividades)
        )
    ;   Actividades = []).


% Predicado para obtener los detalles de las actividades desde actividad.txt
obtener_detalles_actividades([], []).
obtener_detalles_actividades([Actividad | Resto], DetallesActividades) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\actividad.txt', read, Stream),
    buscar_detalles_actividad(Stream, Actividad, Detalles),
    close(Stream),
    (   Detalles = []
    ->  DetallesActividades = RestoDetalles
    ;   DetallesActividades = [Detalles | RestoDetalles]
    ),
    obtener_detalles_actividades(Resto, RestoDetalles).

% Predicado auxiliar para buscar los detalles de una actividad en actividad.txt
buscar_detalles_actividad(Stream, Actividad, Detalles) :-
    read(Stream, Term),
    (   Term \= end_of_file
    ->  (   Term = actividad(ActividadActual, Costo, Duracion, Descripcion, Tipos),
            ActividadActual = Actividad
        ->  Detalles = [ActividadActual, Costo, Duracion, Descripcion, Tipos]
        ;   buscar_detalles_actividad(Stream, Actividad, Detalles)
        )
    ;   Detalles = []).


% Predicado auxiliar para mostrar la lista de actividades con detalles.
% Propósito:
% El predicado listar_actividades/1 se encarga de imprimir en la consola
% los detalles de cada actividad contenida en una lista.
% Entrada:
% - Lista de actividades, donde cada actividad es una lista que contiene
%   [Actividad, Costo, Duracion, Descripcion, Tipos].
% Salida:
% - Muestra en la consola los detalles de cada actividad de forma estructurada.

listar_actividades([]).  % Caso base: si la lista está vacía, no se hace nada.
listar_actividades([[Actividad, Costo, Duracion, Descripcion, Tipos] | Resto]) :-
    write('___________________________________'), nl,
    write('Actividad: '), writeln(Actividad),
    write('Descripcion: '), writeln(Descripcion),
    write('Costo: '), writeln(Costo),
    write('Duracion (en dias): '), writeln(Duracion),
    write('Tipos: '), writeln(Tipos),
    write('___________________________________'), nl,
    listar_actividades(Resto).  % Llamada recursiva para procesar el resto de la lista.

% Predicado auxiliar para calcular el costo total y la duración total de las actividades
calcular_totales([], 0, 0).
calcular_totales([[_, Costo, Duracion, _, _] | Resto], CostoTotal, DuracionTotal) :-
    calcular_totales(Resto, CostoResto, DuracionResto),
    CostoTotal is CostoResto + Costo,
    DuracionTotal is DuracionResto + Duracion.

% ----------------------Consultar actividades---------------------------------------------------------

% Predicado para consultar actividades por tipo.
% Propósito:
% El predicado consultar_actividades_tipo/0 permite al usuario ingresar un tipo de actividad
% y luego busca y muestra las actividades asociadas a ese tipo.
% Entrada:
% - No tiene parámetros de entrada.
% Salida:
% - Muestra las actividades disponibles de un tipo específico y sus detalles,
%   así como el costo total y la duración total de las actividades.

consultar_actividades_tipo :-
    write('Ingrese el tipo de actividad: '),  % Solicita al usuario el tipo de actividad
    read(Tipo),                              % Lee el tipo ingresado
    obtener_actividades_por_tipo(Tipo, Actividades),  % Obtiene actividades por tipo
    (   Actividades = []
    ->  writeln('No hay actividades de este tipo.')  % Mensaje si no hay actividades
    ;   writeln('Actividades de tipo '), write(Tipo), writeln(':'),
        listar_actividades(Actividades),  % Lista las actividades encontradas
        calcular_totales(Actividades, CostoTotal, DuracionTotal),  % Calcula totales
        write('Costo total: '), writeln(CostoTotal),  % Muestra costo total
        write('Duracion total (en dias): '), writeln(DuracionTotal)  % Muestra duración total
    ),
    writeln('Regresando al menú administrativo...').  % Mensaje final




% Predicado para obtener actividades por tipo desde actividad.txt
obtener_actividades_por_tipo(Tipo, Actividades) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\actividad.txt', read, Stream),
    read_actividades_por_tipo(Stream, Tipo, Actividades),
    close(Stream).

% Predicado para leer actividades desde un flujo de datos filtrando por tipo.
% Propósito:
% El predicado read_actividades_por_tipo/3 lee actividades desde un flujo de datos
% y filtra aquellas que coinciden con un tipo específico, almacenándolas en una lista.
% Entrada:
% - Stream: el flujo de datos desde el cual se leerán las actividades.
% - Tipo: el tipo de actividad a filtrar.
% Salida:
% - Actividades: una lista de actividades que coinciden con el tipo especificado.

read_actividades_por_tipo(Stream, Tipo, Actividades) :-
    read(Stream, Term),  % Lee el siguiente término del flujo
    (   Term \= end_of_file  % Verifica si no se ha llegado al final del archivo
    ->  (   Term = actividad(Actividad, Costo, Duracion, Descripcion, Tipos),  % Comprueba si el término es una actividad
            member(Tipo, Tipos)  % Verifica si el tipo está en la lista de tipos de la actividad
        ->  Actividades = [[Actividad, Costo, Duracion, Descripcion, Tipos] | Resto],  % Si coincide, agrega a la lista
            read_actividades_por_tipo(Stream, Tipo, Resto)  % Llama recursivamente para leer el resto
        ;   read_actividades_por_tipo(Stream, Tipo, Actividades)  % Si no coincide, continúa leyendo
        )
    ;   Actividades = []  % Si se llega al final del archivo, la lista de actividades es vacía
    ).

% -----------------Consultar actividades por monto---------------------------

% Predicado para consultar actividades según un monto especificado.
% Propósito:
% El predicado consultar_actividades_precio/0 permite al usuario ingresar un monto y
% elegir si desea consultar actividades que son mas baratas o mas caras que ese monto.
% Entrada:
% - No tiene parámetros de entrada.
% Salida:
% - Muestra las actividades que cumplen con el criterio de precio elegido, junto con sus detalles,
%   así como el costo total y la duración total de las actividades.

consultar_actividades_precio :-
    write('Ingrese el monto (numero entero): '),  % Solicita al usuario el monto
    read(Monto),                                 % Lee el monto ingresado
    write('¿Desea consultar actividades mas baratas o mas caras? (b/c): '),  % Pregunta la opción
    read(Opcion),                                % Lee la opción elegida
    (   Opcion = b  % Si la opción es 'b' para mas baratas
    ->  findall([Actividad, Costo, Duracion, Descripcion, Tipos], 
                (actividad(Actividad, Costo, Duracion, Descripcion, Tipos),
                 Costo =< Monto),  % Filtra actividades con costo menor o igual al monto
                Actividades)
    ;   Opcion = c  % Si la opción es 'c' para mas caras
    ->  findall([Actividad, Costo, Duracion, Descripcion, Tipos], 
                (actividad(Actividad, Costo, Duracion, Descripcion, Tipos),
                 Costo > Monto),  % Filtra actividades con costo mayor al monto
                Actividades)
    ),
    (   Actividades = []  % Verifica si hay actividades encontradas
    ->  writeln('No hay actividades dentro de ese rango de precio.')  % Mensaje si no hay actividades
    ;   writeln('Actividades dentro del rango de precio:'),
        listar_actividades(Actividades),  % Lista las actividades encontradas
        calcular_totales(Actividades, CostoTotal, DuracionTotal),  % Calcula totales
        write('Costo total: '), writeln(CostoTotal),  % Muestra costo total
        write('Duración total (en dias): '), writeln(DuracionTotal)  % Muestra duración total
    ),
    writeln('Regresando al menu administrativo...').  % Mensaje final


% ---------------------------Crear mobiliario por monto --------------
% Filtrar actividades según las preferencias del usuario
filtrar_actividades_monto(MontoMaximo, Categoria, CantidadPersonas, PreferenciaDuracion, ActividadesSeleccionadas) :-
    findall(Actividad, actividad(Actividad, _, _, _, _), TodasLasActividades),
    seleccionar_actividades(TodasLasActividades, MontoMaximo, Categoria, CantidadPersonas, PreferenciaDuracion, ActividadesSeleccionadas),
    (   ActividadesSeleccionadas = []
    ->  writeln('Error: No hay actividades disponibles para la categoría y el monto especificados.')
    ;   true
    ).

% Seleccionar actividades de acuerdo a las reglas
seleccionar_actividades([], _, _, _, _, []).  % No hay mas actividades

seleccionar_actividades([Actividad | Resto], MontoMaximo, Categoria, CantidadPersonas, [ActividadDetalles | ActividadesSeleccionadas]) :-
    actividad(Actividad, Costo, Duracion, Descripcion, Tipos),
    CostoTotal is Costo * CantidadPersonas,
    CostoTotal =< MontoMaximo,
    (   (PreferenciaDuracion == largo, Duracion >= 3) ; (PreferenciaDuracion == corto, Duracion =< 2) ),
    (   member(Categoria, Tipos) ; verificar_categoria_afines(Categoria, Tipos) ),
    ActividadDetalles = [Actividad, CostoTotal, Duracion, Descripcion, Tipos],
    seleccionar_actividades(Resto, MontoMaximo - CostoTotal, Categoria, CantidadPersonas, PreferenciaDuracion, ActividadesSeleccionadas).

% Verificar si la categoría es afín a alguna de las categorías de la actividad
verificar_categoria_afines(Categoria, Tipos) :-
    relacion(Categoria, CategoriaAfines),
    member(CategoriaAfines, Tipos).

% Función para generar itinerario
generar_itinerario :-
    write('Ingrese el monto máximo: '),
    read(MontoMaximo),
    write('Ingrese la categoría preferida: '),
    read(Categoria),
    write('Ingrese la cantidad de personas: '),
    read(CantidadPersonas),
    write('Prefiere estancias largas o cortas? (l/c): '),
    read(PreferenciaDuracion),
    cargar_relaciones,  % Cargar relaciones antes de filtrar actividades
    cargar_actividades,  % Cargar actividades
    filtrar_actividades_monto(MontoMaximo, Categoria, CantidadPersonas, PreferenciaDuracion, ActividadesSeleccionadas),
    writeln('Itinerario generado:'),
    listar_actividades_monto(ActividadesSeleccionadas).

% Función para listar actividades mostrando solo el nombre, la duración y el costo total
listar_actividades_monto([]).
listar_actividades_monto([[Actividad, CostoTotal, Duracion, _, _] | Resto]) :-
    write('_'), nl,
    write('Actividad: '), writeln(Actividad),
    write('Duración (en dias): '), writeln(Duracion),
    write('Costo Total: '), writeln(CostoTotal),
    write('_'), nl,
    listar_actividades_monto(Resto).


% --------------------------7.Crear mobiliario por dias-------------------------------------

itinerario_por_dias :- 
    cargar_actividades,  % Cargar actividades desde el archivo al iniciar
    write('Ingrese la cantidad maxima de dias: '),
    read(MaxDias),
    write('Ingrese la categoria de preferencia: '),
    read(Categoria),
    generar_itinerario_dias(MaxDias, Categoria, Itinerario),
    mostrar_itinerario(Itinerario),
    write('¿Desea generar otro itinerario? (s/n): '),
    read(Respuesta),
    (Respuesta = s ->
        itinerario_por_dias  % Si la respuesta es sí, llamar de nuevo a itinerario_por_dias
    ; 
        menu_administrativo  % Si la respuesta es no, ir al menú principal
    ).
% Generar itinerario basado en la duracion igual a la entrada
generar_itinerario_dias(MaxDias, Categoria, Itinerario) :-
    findall(act(Nombre, Costo, Duracion, Descripcion, Tipos),
            actividad(Nombre, Costo, Duracion, Descripcion, Tipos),
            TodasActividades),
    % Usar setof para evitar duplicados
    setof(act(Nombre, Costo, Duracion, Descripcion, Tipos),
          ( member(act(Nombre, Costo, Duracion, Descripcion, Tipos), TodasActividades),
            Duracion =< MaxDias, % Solo incluir actividades que se pueden hacer en los dias solicitados
            member(Categoria, Tipos) % Asegurarse de que la actividad sea de la categoria solicitada
          ),
          Itinerario).

% Mostrar el itinerario
mostrar_itinerario(Itinerario) :-
    write('Itinerario generado:'), nl,
    (   Itinerario = []
    ->  write('No se encontraron actividades que coincidan con los criterios.')
    ;   forall(member(act(Nombre, Costo, Duracion, Descripcion, Tipos), Itinerario),
               (format('Actividad: ~w, Precio: ~2f, Duracion: ~d, Descripcion: ~w, Categoria(s): ~w~n',
                       [Nombre, Costo, Duracion, Descripcion, Tipos])))).
% =====================================Opcion 7 Frase=======================================================================================

% Cargar las actividades desde el archivo
cargar_actividades_frase :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\actividad.txt', read, Stream),
    leer_actividades_frase(Stream),
    close(Stream).

% Leer actividades del archivo
leer_actividades_frase(Stream) :-
    read(Stream, Line),
    (   Line == end_of_file
    ->  true
    ;   assert(Line), % Almacena la actividad leída
        leer_actividades_frase(Stream)
    ).

% Solicitar frase al usuario
solicitar_frase :-
    cargar_actividades_frase,
    write('Por favor, ingrese una frase: '),
    read(Frase),  % Usa read para obtener la frase
    downcase_atom(Frase, FraseLower),  % Convertir a minúsculas
    buscar_actividad_frase(FraseLower).

% Normalizar la frase eliminando artículos y palabras irrelevantes
normalizar_frase(Frase, FraseNormalizada) :- 
    split_string(Frase, " ", "", Palabras),  
    exclude(articulo_o_irrelevante, Palabras, PalabrasFiltradas),  
    format('Palabras filtradas: ~w~n', [PalabrasFiltradas]),  % Imprimir palabras filtradas
    atomic_list_concat(PalabrasFiltradas, ' ', FraseNormalizada). 


% Predicado para identificar artículos y palabras irrelevantes
articulo_o_irrelevante(Palabra) :-
    downcase_atom(Palabra, PalabraLower),
     member(PalabraLower, [
    'este', 'esta', 'esto', 'ese', 'esa', 'eso', 
    'aquello', 'toda', 'todo', 'cualquier', 
    'cada', 'mucho', 'poco', 'más', 'menos', 
    'sobre', 'tras', 'ante', 'bajo', 'hacia', 
    'hasta', 'entre', 'según', 'durante', 
    'contra', 'sin', 'excepto', 'más allá', 
    'además', 'aunque', 'sin embargo', 'pero', 
    'ni', 'o', 'ya', 'si', 'cuando', 'donde', 
    'como', 'quien', 'qué', 'cuál', 'cuándo',
    'mientras', 'para', 'antes', 'después', 
    'de', 'a través', 'junto', 'fuera', 'delante',
      'a', 'el', 'la', 'y', 'de', 'en', 
    'que', 'por', 'con', 'un', 'una', 
    'los', 'las', 'como', 'quisiera', 
    'ir', 'me', 'gustaria', 'encantaria', 
    'quiero', 'vamos', 'vayamos', 'gusta', 
    'tener', 'actividad', 'relacion', 'relacionada', 
    'al', 'ir']).


% Buscar la actividad que coincide con la frase ingresada
buscar_actividad_frase(Frase) :-
    normalizar_frase(Frase, FraseNormalizada),  % Normaliza la frase
    format('Frase normalizada: ~w~n', [FraseNormalizada]),  
    findall(Resultado, 
            (   actividad(Nombre, Costo, Tipo, Descripcion, Dias),
                atom_string(Descripcion, DescString),
                downcase_atom(DescString, DescStringLower),  % Convertir descripción a minúsculas
                downcase_atom(FraseNormalizada, FraseNormalizadaLower),  % Convertir frase normalizada a minúsculas
                (   sub_string(DescStringLower, _, _, _, FraseNormalizadaLower)  % Compara la frase normalizada con la descripción
                ->  Resultado = (Descripcion, Nombre, Costo, Tipo, Dias)  % Guardar la descripción encontrada
                ;   member(Categoria, Dias),  % Buscar por categorías
                    downcase_atom(Categoria, CategoriaLower),
                    sub_string(CategoriaLower, _, _, _, FraseNormalizadaLower)  % Compara la categoría con la frase normalizada
                ->  Resultado = (Descripcion, Nombre, Costo, Tipo, Dias)  % Guardar la categoría encontrada
                )
            ), 
            Resultados),  % Acumula todas las coincidencias

    % Filtrar resultados duplicados
    list_to_set(Resultados, ResultadosUnicos),  % Eliminar duplicados

    (   ResultadosUnicos \= []
    ->  mostrar_resultados(ResultadosUnicos)
    ;   format('Error: No se encontró ninguna actividad que coincida con: ~w~n', [Frase])
    ).

    

% Mostrar todos los resultados encontrados
mostrar_resultados([]).
mostrar_resultados([(Descripcion, Nombre, Costo, Tipo, Dias) | Resto]) :-
    write("-----------------------------------------------"), nl,
    write('Descripcion encontrada: '), write(Descripcion), nl,
    write('Nombre de actividad: '), write(Nombre), nl,
    write('Costo: '), write(Costo), nl,
    write('Duracion en dias: '), write(Tipo), nl,
    write('Categoria(s): '), write(Dias), nl,
    write("-----------------------------------------------"), nl,
    write("-----------------------------------------------"), nl,
    mostrar_resultados(Resto).


% Mostrar todos los destinos encontrados
mostrar_resultados_destinos([]).
mostrar_resultados_destinos([(NombreDestino, DescripcionDestino) | Resto]) :-
    write('Destino encontrado: '), write(NombreDestino), nl,
    write('Descripcion del destino: '), write(DescripcionDestino), nl,
    write("-----------------------------------------------"), nl,
    mostrar_resultados_destinos(Resto).
% --------------------------Opcion 8 estadisticas.---------------------------------------

% Cargar todos los datos necesarios para las estadísticas
cargar_datos_estadisticas :- 
    cargar_actividades_estadistica,
    cargar_asociacion_estadistica.


% Predicado para obtener las 3 ciudades con mas actividades.
% Propósito:
% El predicado ciudades_mas_actividades/1 busca las ciudades que tienen mas actividades
% asociadas y devuelve las tres ciudades con la mayor cantidad de actividades.
% Entrada:
% - CiudadesMasActividades: una lista que contendrá las tres ciudades con mas actividades.
% Salida:
% - Devuelve una lista con las tres ciudades que tienen mas actividades.

ciudades_mas_actividades(CiudadesMasActividades) :- 
    findall(Ciudad, asociar_actividad(Ciudad, _), TodasLasCiudades),  % Obtiene todas las ciudades asociadas a actividades
    list_to_set(TodasLasCiudades, CiudadesUnicas),  % Elimina duplicados para obtener ciudades únicas
    contar_actividades_por_ciudad(CiudadesUnicas, Contadores),  % Cuenta las actividades por ciudad
    keysort(Contadores, ContadoresOrdenados),  % Ordena los contadores de actividades
    reverse(ContadoresOrdenados, CiudadesOrdenadas),  % Invierte el orden para tener las mas activas primero
    % Aquí se obtiene solo las 3 ciudades con mas actividades
    findall(Ciudad, (nth1(Index, CiudadesOrdenadas, Ciudad-Count), Index =< 3, Count > 0), CiudadesMasActividades).

contar_actividades_por_ciudad([], []).
contar_actividades_por_ciudad([Ciudad | Resto], [Ciudad-Count | Contadores]) :- 
    findall(Actividad, asociar_actividad(Ciudad, Actividad), ActividadesCiudad), 
    length(ActividadesCiudad, Count),
    contar_actividades_por_ciudad(Resto, Contadores).


% Predicado para obtener la actividad mas cara.
% Propósito:
% El predicado actividad_mas_cara/1 busca y devuelve la actividad con el costo masmás alto
% entre todas las actividades disponibles.
% Entrada:
% - ActividadMasCara: una variable que contendrá la actividad mas cara encontrada.
% Salida:
% - Devuelve la actividad con el costo mas alto.

actividad_mas_cara(ActividadMasCara) :- 
    findall(Costo-Actividad, actividad(Actividad, Costo, _, _, _), CostosActividades),  % Obtiene todos los costos y actividades
    max_member(Costo-ActividadMasCara, CostosActividades).  % Encuentra el maximo costo y la actividad correspondiente


% Predicado para obtener la actividad de menor duración.
% Propósito:
% El predicado actividad_menor_duracion/1 busca y devuelve la actividad con la duración mas corta
% entre todas las actividades disponibles.
% Entrada:
% - ActividadMenorDuracion: una variable que contendrá la actividad con la menor duración encontrada.
% Salida:
% - Devuelve la actividad con la duración mas corta.

actividad_menor_duracion(ActividadMenorDuracion) :- 
    findall(Duracion-Actividad, actividad(Actividad, _, Duracion, _, _), DuracionesActividades),  % Obtiene todas las duraciones y actividades
    min_member(Duracion-ActividadMenorDuracion, DuracionesActividades).  % Encuentra la duración minima y la actividad correspondiente



% Predicado para obtener todas las categorías presentes en las actividades.
% Propósito:
% El predicado obtener_categorias/1 busca y devuelve todas las categorías asociadas
% a las actividades disponibles, incluyendo duplicados.
% Entrada:
% - Categorias: una lista que contendrá todas las categorías encontradas.
% Salida:
% - Devuelve una lista de categorías, que puede incluir duplicados.

% Funcion principal para mostrar la categoria mas frecuente
mostrar_categoria_mas_frecuente :-
    findall(Categoria, (actividad(_, _, _, _, Categorias), member(Categoria, Categorias)), ListaCategorias),
    contar_frecuencias(ListaCategorias, Frecuencias),
    encontrar_maximo(Frecuencias, MaxCategoria),
    write('La categoria mas frecuente es: '),
    write(MaxCategoria), nl, menu_administrativo.
 
    

% Contar las frecuencias de cada categoria
contar_frecuencias(Categorias, Frecuencias) :-
    sort(Categorias, CategoriasUnicas),
    findall((Categoria, Cuenta), (member(Categoria, CategoriasUnicas), 
                                    aggregate(count, member(Categoria, Categorias), Cuenta)), Frecuencias).

% Encontrar la categoria con la maxima frecuencia
encontrar_maximo(Frecuencias, MaxCategoria) :-
    Frecuencias \= [],
    sort(2, @>=, Frecuencias, [(MaxCategoria, _)|_]).

% Manejo de errores en caso de que no haya categorias
encontrar_maximo([], 'No hay categorias disponibles.').


% Función principal para mostrar estadísticas
mostrar_estadisticas :- 
    cargar_datos_estadisticas,  % Cargar los datos antes de calcular las estadísticas
    ciudades_mas_actividades(Ciudades),
    writeln('Las 3 ciudades con mas actividades:'), writeln(Ciudades),
    actividad_mas_cara(ActividadCara),
    writeln('La actividad mas cara es:'), writeln(ActividadCara),
    actividad_menor_duracion(ActividadCorta),
    writeln('La actividad de menor duracion es:'), 
    writeln(ActividadCorta).





    % Cargar actividades desde actividad.txt
cargar_actividades_estadistica :- 
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\actividad.txt', read, Stream),
    cargar_actividades_auxa(Stream),
    close(Stream).

cargar_actividades_auxa(Stream) :- 
    read(Stream, Term),
    (   Term == end_of_file
    ->  true
    ;   ( Term = actividad(Nombre, CostoNum, DuracionNum, DescripcionAtom, ListaTipo)
        ->  assert(actividad(Nombre, CostoNum, DuracionNum, DescripcionAtom, ListaTipo)),
            cargar_actividades_auxa(Stream)
        ;   writeln('Error procesando la línea de actividades: '), writeln(Term),
            cargar_actividades_auxa(Stream)
        )
    ).

% Cargar asociaciones desde asociar_actividad.txt
cargar_asociacion_estadistica :- 
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\asociar_actividad.txt', read, Stream),
    leer_asociaciones_estadistica(Stream),
    close(Stream).

% Leer asociaciones desde el flujo
leer_asociaciones_estadistica(Stream) :-
    read_term(Stream, Term, []),
    (   Term == end_of_file
    ->  true
    ;   (Term = asociar_actividad(Destino, Actividad) ->
            assert(asociar_actividad(Destino, Actividad)),
            leer_asociaciones_estadistica(Stream)
        ;   writeln('Error procesando la línea de asociaciones: '), writeln(Term),
            leer_asociaciones_estadistica(Stream)
        )
    ).

% ---------------------------auxiliares para validaciones------------------------

% Valida que el nombre solo contenga letras y no espacios al inicio ni al final
validar_nombre(Nombre) :-
    string_chars(Nombre, Chars),  % Convierte el nombre en una lista de caracteres
      forall(member(Char, Chars), (char_type(Char, alpha); Char = ' '; Char = '_')),  % Verifica que solo contenga letras o espacios
    !.

validar_nombre(_) :-
    writeln('Error: El nombre solo debe contener letras y espacios.'),
    fail.  % Falla si hay caracteres no permitidos


% Valida que el costo solo sea un numero de tipo float y que sea positivo
validar_costo(Costo) :-
    number(Costo),        % Verifica que sea un numero
    integer(Costo),         % Verifica que sea un numero
    Costo >= 0,           % Verifica que sea positivo (mayor o igual a 0)
    !.

validar_costo(_) :-
    writeln('Error: El costo debe ser un numero entero positivo (e.g., 1 v 2 v 3).'),
    fail.  % Falla si no es un número flotante positivo


% Valida que la duracion solo sea un número entero no negativo
validar_duracion(Duracion) :-
    number(Duracion),  % Verifica que sea un número
    integer(Duracion), 
    Duracion >= 0,           % Verifica que sea positivo (mayor o igual a 0)
    !.

validar_duracion(_) :-
    writeln('Error: la duracion debe ser un numero no negativo (e.g., 1 o 2).'),
    fail. 



procesar_opcion_administrativa(9).
% Iniciar la aplicacion
inicio :-
  menu_principal.

% Ejecuta la aplicacion
:- inicio.




