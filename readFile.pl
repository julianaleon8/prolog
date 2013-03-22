head([L|_],L).
pertenece_letra([],_).
pertenece_letra([L|LS],Alfa):-
    member(L,Alfa),
    pertenece_letra(LS,Alfa).
    
pertenece_palabras([],_).
    % pertenece_palabras([_|_],[]):-false.
pertecene_palabras([L|LS],Alfa):-
    atom_chars(L,Letras),
    pertenece_letra(Letras,Alfa),
    pertenece_palabras(LS,Alfa).


elimina(H,[H|T],T).
elimina(X,[H|T],[H|T2]):- elimina(X,T,T2).

permuta([],[]).
permuta(X,[H|T]):- 
    elimina(H,X,Z),
    permuta(Z,T).

% Llena una lista con todas las letras del alfabeto 

llenaSopa([],_).
llenaSopa([H|T],N):-
    N > 0,
    N1 is N - 1,
    lista(H),
    llenaSopa(T,N1).

% Genera un tablero vacio en forma de lista
generaTablero(X,Tam):-
    N is Tam * Tam,
    length(X,N),
    llenaSopa(X,N).

% Pasa de lista a lista de listas

    
    
% Las letras del alfabeto son hechos

factSopa([]).
factSopa([H|T]):-
    assert(lista(H)),
    factSopa(T).
   

% generaSopa(Alfabeto,Tam):-
%    length(X,Tam),
%    permuta(Alfabeto,Y),
%    split(Y,Tam,L1,L2),
%    llena(X,L1,Tam).


generaListaDiagonales(Tam,Lista):-
    N is (2 * Tam) - 1,
    length(Lista,N).    

listaDiagonales([H|T],Tam,Lista):-
    

split(L,0,[],L).
split([H|T],N,[H|T1],T2) :- 
    N > 0, 
    N1 is N - 1, 
    split(T,N1,T1,T2).


llena([],_,_).
llena([H|T],Y,Tam):-
    Tam > 0,
    N1 is Tam - 1,
    llenaFila(H,Y),
    llena(T,Y,N1).
    
    

main :- 
    open('prueba',read,Str),
    read(Str,Tamano),
    read(Str,Alfabeto),
     read(Str,Rechazadas),
    close(Str),
  
    cargarListaPalabra(Aceptadas),
    write([Tamano,Alfabeto,Aceptadas,Rechazadas]), nl.
