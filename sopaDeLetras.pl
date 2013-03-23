% (Taken from clpfd.pl and slightly modified).
transpose([], []).
transpose([F|Fs], Ts) :-
	transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).
% --------------------------------- %
% Verifica que si cada letra de la palabra esta en el alfabeto
pertenece_letra([],_).
pertenece_letra([L|LS],Alfa):-
	member(L,Alfa),
	pertenece_letra(LS,Alfa).
% Verifica si la palabra esta en el Alfabeto
pertenece_palabras([],_).
pertenece_palabras([L|LS],Alfa):-
	atom_chars(L,Letras),
	pertenece_letra(Letras,Alfa),
	pertenece_palabras(LS,Alfa).

% cargarListaPalabra('aceptadas.txt', [o,h,l,a,c]).
cargarListaPalabra(Archivo,Alfa):-
	open(Archivo,read,Str1),
	read(Str1,Palabras),
	write([Palabras]), nl,
	pertenece_palabras(Palabras,Alfa).

crea_listaDiag1(Entrada,Lista,Tam):-
	reverse(Entrada,Aux),
	N is (2 * Tam) - 1,
	length(Lista,N),
	lista_diagonales(Aux,Lista,N),
	!.

crea_listaDiag(Entrada,Lista,Tam):-
	N is (2 * Tam) - 1,
	length(Lista,N),
	lista_diagonales(Entrada,Lista,N),
	!.

lista_diagonales(_,_,0).
lista_diagonales(Entrada,[H|T],Tam):-
	Tam > 0,
	N1 is Tam - 1,
	diagonales(0,0,Tam,Entrada,H,Tam),
	lista_diagonales(Entrada,T,N1).

lista_columnas(Sopa,Lista):-
	transpose(Sopa,Lista).

% Esta es la que usaras para pegar las diagonales el resultado lo guarda en 5 argumento "Diagonal"
% diagonales(0,0,2,[[a,b],[c,d]],X,2).
diagonales(Tam,J,Tam,Sopa,[],N).
diagonales(I,Tam,Tam,Sopa,Diagonal,N):-
	AuxI is I + 1,
	diagonales(AuxI,0,Tam,Sopa,Diagonal,N).

diagonales(I,J,Tam,Sopa,Diagonal,N):-
	NumListas is (Tam*2)-1,
	ListaAc is NumListas - N,
	ListaAc is I + J,
	nth0(I, Sopa, X),
	nth0(J, X, Elem),
	
	AuxJ is J + 1,
	diagonales(I,AuxJ,Tam,Sopa,Resto,N),
	Diagonal = [Elem|Resto].

diagonales(I,J,Tam,Sopa,Diagonal,N):-
	AuxJ is J + 1,
	diagonales(I,AuxJ,Tam,Sopa,Diagonal,N).
% verifica que una palabra este en una de las diagonales
verifica_diago1(Palabra,Tablero,N,Tam):-
	Nu is N+1,
	crea_listaDiag1(Tablero,Lista,Tam),
	nth1(Nu,Lista,Elem),
	pertenece_palabras(Palabra,Elem),!.
% verifica que una palabra este en una de las diagonales

verifica_diago(Palabra,Tablero,Tam):-
	%Nu is N+1,
	crea_listaDiag(Tablero,Lista,Tam),
	%nth1(Nu,Lista,Elem),
	verifica_Diagonales(Lista,Palabra).

verifica_Diagonales1(Tablero,Palabra,Tam):-
	crea_listaDiag1(Tablero,Lista,Tam),
	verifica_filas(Palabra,Lista).


verifica_Diagonales(Tablero,Palabra,Tam):-
	crea_listaDiag(Tablero,Lista,Tam),
	verifica_filas(Palabra,Lista).
	

% verifica si la palabra esta en alguna de las columnas
verifica_columnas(Y,Tablero):-
	lista_columnas(Tablero,Lista),
	verifica_filas(Y,Lista).
% verifica si la palabra esta en algunas de las filas

verifica_filas(Y,[Ta|Blero]):-
	atom_chars(Y,X),
	reverse(X,Z),
	(sub_secuencia(Ta,Z,Z);
	(sub_secuencia(Ta,X,X));
	    verifica_filas(Y,Blero)).


% Palabra es la lista de letras de la palabra
sub_secuencia(_,[],_).
sub_secuencia([S|Opa], [S|Labra],Palabra):-
	sub_set(Opa,Labra,Palabra).

sub_secuencia([S|Opa], H,Palabra):-
	sub_secuencia(Opa,H,Palabra).
sub_set(_,[],_).

sub_set([Y|YY],[Y|YS],Palabra):-
	
	sub_set(YY,YS,Palabra).

sub_set([S|SS],[L|Ls],P):-
	write(S),
	sub_secuencia(SS,P,P).
%sub_set([Y|Pa],Y,Palabra).
	
% Llena una lista con todas las letras del alfabeto 

llenaFila([],_).
llenaFila([H|T],N):-
    N > 0,
    N1 is N - 1,
    lista(H),
    llenaFila(T,N1).


generaFilas([],_).
generaFilas([H|T],Tam):-
    f(H,Tam),
    generaFilas(T,Tam).

f(Lista,Tam):-
    length(Lista,Tam),
    llenaFila(Lista,Tam).

% Esto crea el tablero y como lista de listas. Antes de hacer eso deberias llamar a factSopa([a,b,c,d]) o lo que quieras como alfabeto, y luego haces crea_tablero(X,Tam), donde Tam es el tamano de las filas y X es unavariable sin instanciar, donde se va a guardar el tablero.

crea_tablero(X,Tam):-
    length(X,Tam),
    generaFilas(X,Tam).

factSopa([]).
factSopa([H|T]):-
    assert(lista(H)),
    factSopa(T).

% Esto es lo que vas a usar para obtener la lista de diagonales. Entrada es la lista de listas que pasas en diagonales, Lista es la lista donde vas a devolver tus diagonales, y Tam es el tamano de Entrada.

imprime([]).
imprime([H|T]):- 
    write(H),
    write(' '),
    imprime(T).

mostrarSopa(X,Y):-
    imprime(X),
    nl,
    imprime(Y).

verifica_filas_rever(Y,[Ta|Blero]):-
	atom_chars(Y,Z),
	reverse(Z,X),
	(sub_secuencia(Ta,X,X);
	    verifica_filas(X,Blero)).

verificar_todo_acep(Tablero,[],Tam).
verificar_todo_acep(Tablero,[A|Cepta],Tam):-
	(verifica_filas(A,Tablero);
	    verifica_columnas(A,Tablero);
	    verifica_Diagonales(Tablero,A,Tam);
	    verifica_Diagonales1(Tablero,A,Tam)),
	    verificar_todo_acep(Tablero,Cepta,Tam).

%verifica_palabra(PaLabra,Tablero,Tam,N):-
%	NO is N +1, 
%	verifica_fila(Palabra,Tablero,NO),
	

%verificar_palabras(Palabras,Tablero)
    
%% main :- 
%%     open('prueba',read,Str),
%%     read(Str,Tamano),
%%     read(Str,Alfabeto),
%%     read(Str,Rechazadas),
%%     close(Str),
  
%%     cargarListaPalabra(Aceptadas),
%%     write([Tamano,Alfabeto,Aceptadas,Rechazadas]), nl.

main :-
	open('prueba',read,Str),
	write('Tamano ?'),
	read(Tamano),
	nl,
	write('Alfabeto ?'),
	read(Alfabeto),
	nl,
	write('Aceptadas ?'),	
	read(Aceptadas),
	nl,
	write('Rechazadas ?'),	
	read(Rechazadas),
	%close(Str),
	%cargarListaPalabra(Aceptadas,Alfabeto),
	write('Introduzca Comando'),
	read(Dato),
	Dato = mas,
	write('hola'),
	fail.
	%mas(Dato,X),
	% Hay que llamar a generar sopa 
	% El comando MAS o no 
	%read(Comando),
	
	%write([Tamano,Alfabeto,Aceptadas,Rechazadas]), nl.
