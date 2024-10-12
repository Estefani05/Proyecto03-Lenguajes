:- discontiguous procesar_opcion_administrativa/1.

cargar_destino :-
    open('C:/Users/estef/OneDrive/Escritorio/PL03/destino.txt', read, Stream),
    leer_destinos(Stream),
    close(Stream).

leer_destinos(Stream) :-
    read_line_to_string(Stream, Linea),
    (   Linea == end_of_file
    ->  true
    ;   split_string(Linea, ',', '', [nombre_del_destino, descripcion_del_destino ]),
        atom_string(NombredelDestinoAtom, nombre_del_destino),
        atom_string(DescripciondelDestinoAtom, descripcion_del_destino),
        assert(destino(NombredelDestinoAtom, DescripciondelDestinoAtom)),
        leer_destinos(Stream)
    ).

% Cargar todos los datos desde los archivos
:- cargar_destino.

% Menú principal
menu_principal :-
  repeat,
  writeln('Menú Principal'),
  writeln('1. Opciones administrativas'),
  writeln('2. Salir'),
  write('Seleccione una opción: '),
  read(Opcion),
  (   Opcion = 1
  ->  menu_administrativo
  ;   Opcion = 2
  ->  writeln('Saliendo del programa.'), true
  ;   writeln('Opción no válida, por favor intente de nuevo.')
  ),
  Opcion = 2,true,
  halt.

% Menú administrativo
menu_administrativo :-
  repeat,
  writeln('\nMenú Administrativo'),
  writeln('1. Agregar hechos'),
  writeln('2. Consulta destino'),
  writeln('3. Actividades por tipo'),
  writeln('4. Consulta por precio'),
  writeln('5. Generar itinerario por monto'), 
  writeln('6. Generar itinerario por dias'),
  writeln('7. Recomendar por frase'),
  writeln('8. Estadísticas'),
  writeln('9. Volver'),
  write('Seleccione una opción: '),
  read(Opcion),
  procesar_opcion_administrativa(Opcion),
  ( Opcion = 2
  -> mostrar_destino,
     procesar_opcion_administrativa(2);  
    Opcion = 9
  ->  writeln('Volviendo al Menú Principal.'),
        menu_principal
  ),
  Opcion = 9.


% Predicado para mostrar personas
mostrar_destino :-
  (   catch(
          findall([Nombre, Descripcion], destino(Nombre, Descripcion) , destinos),
          _Exception,
          false
      ),
      Destinos \= false ->
      writeln('Destinos:'),
      forall(member([Nombre, Descripcion], Destinos),
          (
              write('___________________________________'),
              write('Nombre: '), writeln(Nombre),
              write('Descripcion: '), writeln(Descripcion),
              %tareas en las que ha sido asignado el usuario
              findall([Nombre, Descripcion], destino(Nombre,Descripcion)),
              write('___________________________________'),
              nl
          )
      )
  ;   write('No hay.'), procesar_opcion_administrativa(1)
  ).



procesar_opcion_administrativa(9).
% Iniciar la aplicación
inicio :-
  menu_principal.

% Ejecutar la aplicación
:- inicio.