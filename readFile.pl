head([L|_],L).
pertenece_letra([],_).
pertenece_letra([L | LS],Alfa):-
    member(L,Alfa),
    pertenece_letra(LS,Alfa).

pertenece_palabras([],_).
pertenece_palabras([_|_],[]):-false.
pertecene_palabras([L | LS],Alfa):-
    atom_chars(L,Letras),
    pertenece_letra(Letras,Alfa),
    pertenece_palabras(LS,Alfa).


cargarListaPalabra(Archivo):-
    open(Archivo,read,Str1),
    read(Str1,Palabras),
    write([Palabras]), nl.
    
main :- 
    open('prueba',read,Str),
    read(Str,Tamano),
    read(Str,Alfabeto),
    read(Str,Aceptadas),
    read(Str,Rechazadas),
    close(Str),
   
    cargarListaPalabra(Aceptadas),
    write([Tamano,Alfabeto,Aceptadas,Rechazadas]), nl.
