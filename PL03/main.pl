:- discontiguous procesar_opcion_administrativa/1.

cargar_destino :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\destino.txt', read, Stream),
    cargar_destino_aux(Stream),
    close(Stream).

cargar_destino_aux(Stream) :-
    read_term(Stream, Term, []),
    (   Term == end_of_file
    ->  true
    ;   Term = destino(Nombre_del_destino, Descripcion_del_destino),
        assert(destino(Nombre_del_destino, Descripcion_del_destino)),
        cargar_destino_aux(Stream)
    ;   writeln('Error procesando la linea de destinos.')
    ).

cargar_actividades :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\actividad.txt', read, Stream),
    cargar_actividades_aux(Stream),
    close(Stream).

cargar_actividades_aux(Stream) :-
    read(Stream, Term),
    (   Term == end_of_file
    ->  true
    ;   Term =.. [actividad, Nombre, CostoNum, DuracionNum, DescripcionAtom, ListaTipo],
        assert(actividad(Nombre, CostoNum, DuracionNum, DescripcionAtom, ListaTipo)),
        cargar_actividades_aux(Stream)
    ;   writeln('Error procesando la linea de actividades.')
    ).

% Cargar asociaciones desde asociar_actividad.txt
cargar_asociaciones :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\asociar_actividad.txt', read, Stream),
    leer_asociaciones(Stream),
    close(Stream).

leer_asociaciones(Stream) :-
    read_term(Stream, Term, []),
    (   Term == end_of_file
    ->  true
    ;   Term = asociar_actividad(Destino, Actividad),
        assert(asociar_actividad(Destino, Actividad)),
        leer_asociaciones(Stream)
    ;   writeln('Error procesando la linea de asociaciones.')
    ).


% Cargar todos los datos desde los archivos
:- cargar_destino.
:- cargar_actividades.
:- cargar_asociaciones.


% Menu principal
menu_principal :-
  repeat,
  writeln('Menu Principal'),
  writeln('1. Opciones administrativas'),
  writeln('2. Salir'),
  write('Seleccione una opcion: '),
  read(Opcion),
  (   Opcion = 1
  ->  menu_administrativo
  ;   Opcion = 2
  ->  writeln('Saliendo del programa.'), true
  ;   writeln('Opcion no valida, por favor intente de nuevo.')
  ),
  Opcion = 2,true,
  halt.

% Menu administrativo
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
      menu_principal
  ;   Opcion = 1
    ->  menu_agregar_hechos
  ;   Opcion = 2
    ->  pre_consultar_actividades_destino
  ;   Opcion = 3
    ->  consultar_actividades_tipo
  ;  Opcion = 4
    ->  consultar_actividades_precio
  ).


% Agregar destino - opcion 1
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
        ->  agregar_destino
    ;
    Opcion = 2
    ->  agregar_actividad
    ;
    Opcion = 3
    ->  asociar_actividad_destino
    ;
      Opcion = 4
    ->  writeln('Volviendo al Menu Administrativo.'),
        menu_administrativo
    ).

% Agregar destino - opcion 1
agregar_destino :- 
    write('Debe separarse por _ si es necesario, de lo contrario abra un error en sistema.'),
    nl,
    write('Ingrese el nombre del destino: '),
    read(Nombre),  % Captura el nombre con espacios
    validar_nombre(Nombre),  % Llamada a la validación
    write('Ingrese la descripcion del destino: '),
    read(Descripcion),  % Captura la descripción con espacios
    assert(destino(Nombre, Descripcion)),
    writeln('Destino agregado exitosamente.'),
    escribir_destinoTxt(Nombre, Descripcion),  % Llamada para escribir en el archivo
    writeln('Regresando al menu de agregar hechos...').



% Función para escribir un destino en el archivo destino.txt
escribir_destinoTxt(Nombre, Descripcion) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\destino.txt', append, Stream),
    write(Stream, destino(Nombre, Descripcion)),
    write(Stream, '.'),
    nl(Stream), % Escribir en el archivo destino agrega un . al final
    close(Stream).




% Agregar actividad - opcion 2
agregar_actividad :- 
    write('Ingrese el nombre de la actividad: '),
    read(Nombre),
    validar_nombre(Nombre),  % Llamada a la validación
    write('Ingrese el costo de la actividad: '),
    read(Costo),
    validar_costo(Costo),
    write('Ingrese la duracion de la actividad (en dias): '),
    read(Duracion),
    validar_duracion(Duracion),
    write('Ingrese la descripcion de la actividad: '),
   read(DescripcionInput),  % Leemos la descripción ingresada
    format(atom(Descripcion), '"~w"', [DescripcionInput]),  % Agregamos comillas a la descripción
    write('Ingrese los tipos de la actividad (ej. aventura,naturaleza): '),
    read(TiposInput),  % Leemos la entrada del usuario
    format(atom(TiposFormateados), '[~w]', [TiposInput]),  % Formateamos añadiendo corchetes
    assert(actividad(Nombre, Costo, Duracion, Descripcion, TiposFormateados)),
    writeln('Actividad agregada exitosamente.'),
    escribir_actividadTxt(Nombre,Costo,Duracion,Descripcion,TiposFormateados),  % Llamada para escribir en el archivo
    writeln('Regresando al menu de agregar hechos...').


% Función para escribir una actividad en el archivo actividad.txt
escribir_actividadTxt(Nombre,Costo,Duracion,Descripcion,Tipos) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\actividad.txt', append, Stream),
    write(Stream, actividad(Nombre,Costo,Duracion,Descripcion,Tipos)), % Escribir en el archivo
    write(Stream, '.'),
    nl(Stream), 
    close(Stream).

% ------------------------asociar-------------------------------------------------------------------------------------
asociar_actividad_destino :- 
    write('Ingrese el nombre del destino: '),
    read(Destino),
    verificar_destino(Destino), % Verificar si el destino existe
    !, % Continuar solo si el destino es válido
    write('Ingrese el nombre de la actividad: '),
    read(Actividad),
    verificar_actividad(Actividad), % Verificar si la actividad existe
    assert(asociar_actividad(Destino, Actividad)),
    escribir_asosiarTxt(Destino, Actividad),
    writeln('Actividad asociada exitosamente.'),
    writeln('Regresando al menu de agregar hechos...').

% Función para escribir una asociación en el archivo asociar_actividad.txt
escribir_asosiarTxt(Destino, Actividad) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\asociar_actividad.txt', append, Stream),
    write(Stream, asociar_actividad(Destino, Actividad)), % Escribir en el archivo
    write(Stream, '.'),
    nl(Stream), 
    close(Stream).


% Predicado para verificar si la actividad existe en actividad.txt
verificar_actividad(Actividad) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\actividad.txt', read, Stream),
    buscar_actividad(Stream, Actividad, Existe),
    close(Stream),
    (   Existe == true
    ->  true  % Continuar si la actividad existe
    ;   writeln('Error: La actividad ingresada no existe.'),
        fail  % Detener el proceso si no existe
    ).


% Predicado para verificar si el destino existe en destino.txt (solo verifica el nombre del destino)
verificar_destino(Destino) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\destino.txt', read, Stream),
    buscar_destino(Stream, Destino, Existe),
    close(Stream),
    (   Existe == true
    ->  true  % Continuar si el destino existe
    ;   writeln('Error: El destino ingresado no existe.'),
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



% Predicado para buscar una actividad en el archivo actividad.txt
buscar_actividad(Stream, ActividadBuscada, Existe) :-
    read(Stream, Term),
    (   Term == end_of_file
    ->  Existe = false
    ;   (   Term = actividad(ActividadActual, _, _, _, _),  % Ignorar los otros argumentos
            ActividadActual == ActividadBuscada
        ->  Existe = true
        ;   buscar_actividad(Stream, ActividadBuscada, Existe)
        )
    ).


% ----------------------consultar actividades por destino - opcion 2--------------------------------------------------------
% Predicado principal para consultar actividades por destino
pre_consultar_actividades_destino :-
    write('Ingrese el nombre del destino: '),
    read(Destino),
    consultar_actividades_destino(Destino).

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

% Predicado auxiliar para mostrar la lista de actividades con detalles
listar_actividades([]).
listar_actividades([[Actividad, Costo, Duracion, Descripcion, Tipos] | Resto]) :-
    write('___________________________________'), nl,
    write('Actividad: '), writeln(Actividad),
    write('Descripcion: '), writeln(Descripcion),
    write('Costo: '), writeln(Costo),
    write('Duracion (en dias): '), writeln(Duracion),
    write('Tipos: '), writeln(Tipos),
    write('___________________________________'), nl,
    listar_actividades(Resto).

% Predicado auxiliar para calcular el costo total y la duración total de las actividades
calcular_totales([], 0, 0).
calcular_totales([[_, Costo, Duracion, _, _] | Resto], CostoTotal, DuracionTotal) :-
    calcular_totales(Resto, CostoResto, DuracionResto),
    CostoTotal is CostoResto + Costo,
    DuracionTotal is DuracionResto + Duracion.

% ----------------------Agregar asociacion actividad a destino - opcion 3 FIN--------------------------------------------------------
% Consultar actividades por tipo - opción 3
consultar_actividades_tipo :-
    write('Ingrese el tipo de actividad: '),
    read(Tipo),
    obtener_actividades_por_tipo(Tipo, Actividades),
    (   Actividades = []
    ->  writeln('No hay actividades de este tipo.')
    ;   writeln('Actividades de tipo '), write(Tipo), writeln(':'),
        listar_actividades(Actividades),
        calcular_totales(Actividades, CostoTotal, DuracionTotal),
        write('Costo total: '), writeln(CostoTotal),
        write('Duracion total (en dias): '), writeln(DuracionTotal)
    ),
    writeln('Regresando al menú administrativo...').

% Predicado para obtener actividades por tipo desde actividad.txt
obtener_actividades_por_tipo(Tipo, Actividades) :-
    open('C:\\Users\\joses\\Desktop\\PY01-Lenguajes\\Proyecto03-Lenguajes\\PL03\\actividad.txt', read, Stream),
    read_actividades_por_tipo(Stream, Tipo, Actividades),
    close(Stream).

read_actividades_por_tipo(Stream, Tipo, Actividades) :-
    read(Stream, Term),
    (   Term \= end_of_file
    ->  (   Term = actividad(Actividad, Costo, Duracion, Descripcion, Tipos),
            member(Tipo, Tipos)
        ->  Actividades = [[Actividad, Costo, Duracion, Descripcion, Tipos] | Resto],
            read_actividades_por_tipo(Stream, Tipo, Resto)
        ;   read_actividades_por_tipo(Stream, Tipo, Actividades)
        )
    ;   Actividades = []).


% -----------------por monto------------------------
% Consultar actividades por precio - opcion 4
consultar_actividades_precio :-
    write('Ingrese el monto (numero flotante): '),
    read(Monto),
    write('¿Desea consultar actividades mas baratas o mas caras? (b/c): '),
    read(Opcion),
    (   Opcion = b
    ->  findall([Actividad, Costo, Duracion, Descripcion, Tipos], 
                (actividad(Actividad, Costo, Duracion, Descripcion, Tipos),
                 Costo =< Monto), 
                Actividades)
    ;   Opcion = c
    ->  findall([Actividad, Costo, Duracion, Descripcion, Tipos], 
                (actividad(Actividad, Costo, Duracion, Descripcion, Tipos),
                 Costo > Monto), 
                Actividades)
    ),
    (   Actividades = [] 
    ->  writeln('No hay actividades dentro de ese rango de precio.')
    ;   writeln('Actividades dentro del rango de precio:'),
        listar_actividades(Actividades),
        calcular_totales(Actividades, CostoTotal, DuracionTotal),
        write('Costo total: '), writeln(CostoTotal),
        write('Duración total (en dias): '), writeln(DuracionTotal)
    ),
    writeln('Regresando al menu administrativo...').



% ---------------------------auxiliares para validaciones------------------------

% Validar que el nombre solo contenga letras y no espacios al inicio ni al final
validar_nombre(Nombre) :-
    string_chars(Nombre, Chars),  % Convierte el nombre en una lista de caracteres
      forall(member(Char, Chars), (char_type(Char, alpha); Char = ' '; Char = '_')),  % Verifica que solo contenga letras o espacios
    !.

validar_nombre(_) :-
    writeln('Error: El nombre solo debe contener letras y espacios.'),
    fail.  % Falla si hay caracteres no permitidos


% Validar que el costo solo sea un numero de tipo float y que sea positivo
validar_costo(Costo) :-
    number(Costo),        % Verifica que sea un numero
    float(Costo),         % Verifica que sea un numero flotante (con decimales)
    Costo >= 0,           % Verifica que sea positivo (mayor o igual a 0)
    !.

validar_costo(_) :-
    writeln('Error: El costo debe ser un numero flotante positivo (e.g., 123.45).'),
    fail.  % Falla si no es un número flotante positivo


% Validar que la duracion solo sea un número entero no negativo
validar_duracion(Duracion) :-
    number(Duracion),  % Verifica que sea un número
     Duracion >= 0,           % Verifica que sea positivo (mayor o igual a 0)
    !.

validar_duracion(_) :-
    writeln('Error: la duracion debe ser un numero no negativo (e.g., 1 o 2).'),
    fail. 







procesar_opcion_administrativa(9).
% Iniciar la aplicacion
inicio :-
  menu_principal.

% Ejecutar la aplicacion
:- inicio.




