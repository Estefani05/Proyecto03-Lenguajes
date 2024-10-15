:- discontiguous procesar_opcion_administrativa/1.

cargar_destino :-
    open('C:/Users/estef/OneDrive/Escritorio/PL03/destino.txt', read, Stream),
    cargar_destino_aux(Stream),
    close(Stream).

cargar_destino_aux(Stream) :-
    read_term(Stream, Term, []),
    (   Term == end_of_file
    ->  true
    ;   Term = destino(Nombre_del_destino, Descripcion_del_destino),
        assert(destino(Nombre_del_destino, Descripcion_del_destino)),
        cargar_destino_aux(Stream)
    ;   writeln('Error procesando la linea.')
    ).

cargar_actividades :-
    open('C:/Users/estef/OneDrive/Escritorio/PL03/actividad.txt', read, Stream),
    cargar_actividades_aux(Stream),
    close(Stream).

cargar_actividades_aux(Stream) :-
    read(Stream, Term),
    (   Term == end_of_file
    ->  true
    ;   Term =.. [actividad, Nombre, CostoNum, DuracionNum, DescripcionAtom, ListaTipo],
        assert(actividad(Nombre, CostoNum, DuracionNum, DescripcionAtom, ListaTipo)),
        cargar_actividades_aux(Stream)
    ;   writeln('Error procesando la linea.')
    ).

% Cargar asociaciones desde asociar_actividad.txt
cargar_asociaciones :-
    open('C:/Users/estef/OneDrive/Escritorio/PL03/asociar_actividad.txt', read, Stream),
    leer_asociaciones(Stream),
    close(Stream).

leer_asociaciones(Stream) :-
    read_term(Stream, Term, []),
    (   Term == end_of_file
    ->  true
    ;   Term = asociar_actividad(Destino, Actividad),
        assert(asociar_actividad(Destino, Actividad)),
        leer_asociaciones(Stream)
    ;   writeln('Error procesando la linea.')
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

agregar_destino :- 
    write('Ingrese el nombre del destino: '),
    read(Nombre),
    write('Ingrese la descripcion del destino: '),
    read(Descripcion),
    assert(destino(Nombre, Descripcion)),
    writeln('Destino agregado exitosamente.'),
    writeln('Regresando al menu de agregar hechos...').

% Agregar actividad - opcion 2
agregar_actividad :- 
    write('Ingrese el nombre de la actividad: '),
    read(Nombre),
    write('Ingrese el costo de la actividad: '),
    read(Costo),
    write('Ingrese la duracion de la actividad (en dias): '),
    read(Duracion),
    write('Ingrese la descripcion de la actividad: '),
    read(Descripcion),
    write('Ingrese los tipos de la actividad (separados por coma): '),
    read(Tipos),
    atomic_list_concat(Tipos, ',', TiposAtom),
    assert(actividad(Nombre, Costo, Duracion, Descripcion, TiposAtom)),
    writeln('Actividad agregada exitosamente.'),
    writeln('Regresando al menu de agregar hechos...').

% Agregar asociacion actividad a destino - opcion 3
asociar_actividad_destino :- 
    write('Ingrese el nombre del destino: '),
    read(Destino),
    write('Ingrese el nombre de la actividad: '),
    read(Actividad),
    assert(asociar_actividad(Destino, Actividad)),
    writeln('Actividad asociada exitosamente.'),
    writeln('Regresando al menu de agregar hechos...').


%consultar actividades por destino - opcion 2
pre_consultar_actividades_destino :-
    write('Ingrese el nombre del destino: '),
    read(Destino),
    consultar_actividades_destino(Destino).


consultar_actividades_destino(Destino) :-
    findall([Actividad, Costo, Duracion, Descripcion, Tipos], 
            (asociar_actividad(Destino, Actividad),
             actividad(Actividad, Costo, Duracion, Descripcion, Tipos)), 
            Actividades),
    (   Actividades = [] 
    ->  writeln('No hay actividades para este destino.')
    ;   writeln('Actividades disponibles en '), write(Destino), writeln(':'),
        listar_actividades(Actividades),
        calcular_totales(Actividades, CostoTotal, DuracionTotal),
        write('Costo total: '), writeln(CostoTotal),
        write('Duracion total (en dias): '), writeln(DuracionTotal)
    ),
    writeln('Regresando al menu administrativo...').


% Predicado auxiliar para mostrar la lista de actividades
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

% Predicado auxiliar para calcular el costo y la duracion totales de las actividades
calcular_totales(Actividades, CostoTotal, DuracionTotal) :-
    findall(Costo, member([_, Costo, _, _, _], Actividades), Costos),
    findall(Duracion, member([_, _, Duracion, _, _], Actividades), Duraciones),
    sumlist(Costos, CostoTotal),
    sumlist(Duraciones, DuracionTotal).

% Consultar actividades por tipo - opcion 3
consultar_actividades_tipo :-
    write('Ingrese el tipo de actividad: '),
    read(Tipo),
    findall([Actividad, Costo, Duracion, Descripcion, Tipos], 
            (actividad(Actividad, Costo, Duracion, Descripcion, Tipos),
             member(Tipo, Tipos)), 
            Actividades),
    (   Actividades = [] 
    ->  writeln('No hay actividades de este tipo.')
    ;   writeln('Actividades de tipo '), write(Tipo), writeln(':'),
        listar_actividades(Actividades),
        calcular_totales(Actividades, CostoTotal, DuracionTotal),
        write('Costo total: '), writeln(CostoTotal),
        write('Duracion total (en dias): '), writeln(DuracionTotal)
    ),
    writeln('Regresando al menu administrativo...').

% Consultar actividades por precio - opcion 4
consultar_actividades_precio :-
    write('Ingrese el monto: '),
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
        write('Duracion total (en dias): '), writeln(DuracionTotal)
    ),
    writeln('Regresando al menu administrativo...').


procesar_opcion_administrativa(9).
% Iniciar la aplicacion
inicio :-
  menu_principal.

% Ejecutar la aplicacion
:- inicio.
