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


lista_columnas(Sopa,Lista):-
	transpose(Sopa,Lista).

% lista_diagonalesPosi(Sopa,Tam,Lista,Tam).
% lista_diagonalesPosi(Sopa,N,Lista,Tam):-


%lista_diagonalesNega(Sopa,Tam,Lista,Tam).
%lista_diagonalesNega(Sopa,N,Lista,Tam):-	
%	AuxN is N +1,
%	diagonales(0,0,Tam,Sopa,Lista,AuxN),
%	lista_diagonalesNega(Sopa,N,[Lista],Tam).	
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

%verificar_Aceptadas([So|Pa],Aceptadas,Tam):-
%	%atom_chars(So,Fila),
%	pertenece_palabras(Aceptadas,So),
%	!,
%	verificar_Aceptadas(Pa,Aceptadas,Tam).

% Palabra es la lista de letras de la palabra
sub_secuencia(_,[],_).
sub_secuencia([S|Opa], [S|Labra],Palabra):-
	sub_set(Opa,Labra,Palabra).

sub_secuencia([S|Opa], [Pala],Palabra):-
	sub_set(Opa,Pala,Palabra).
sub_set(_,[],_).
sub_set([So|Pa],[L|Ls],Palabra):-
	sub_secuencia(Pa,Palabra,Palabra).

sub_set([L|Pa],[L|Ls],Palabra):-
	sub_set(Pa,Ls,Palabra).


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
%open('prueba',read,Str),
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
	cargarListaPalabra(Aceptadas,Alfabeto),
	% Hay que llamar a generar sopa 
	% El comando MAS o no 
	%read(Comando),
	
	write([Tamano,Alfabeto,Aceptadas,Rechazadas]), nl.
