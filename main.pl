% in terminal : set_prolog_flag(answer_write_options,[max_depth(0)]).


% Alege perechea [Nod, Prioritate] cu Prioritatea mai mica
min_priority([X1,P1],[X2,P2],M) :- P1 > P2, M = [X2,P2],!.
min_priority(X,_,M) :-  M = X.


% Gaseste root - Perechea [Nod, Prioritate] cu prioritatea cea mai mica dintr-o lista
root([H], H).
root([H1,H2|L],M) :- min_priority(H1, H2, M1 ), root([M1|L], M), !.


% Desparte Reteaua in Prioritati si Muchii
retea([Prioritati, Muchii], Prioritati, Muchii).


% Extrage Nodurile dintr-o lista de perechi sub forma [Nod, Prioritate]
noduri([], Nodes ).
noduri([[N,P]|L], Nodes) :- noduri(L, Nod1), Nodes = [N|Nod1].
	

% Construieste o Lista de muchii candidat 
% Criteriul fiind : unul din capetele muchiei sa apartina listei de noduri
% muchii_de_ales(Noduri,Muchii, MDA ).
mda([],Muchii,[]).
mda(Noduri,[],[]).
mda(Noduri,[[N1,N2,Cost]|L],M) :- member(N1,Noduri), member(N2,Noduri), mda(Noduri,L,M),!.
mda(Noduri,[[N1,N2,Cost]|L],M) :- member(N1,Noduri), mda(Noduri,L,N), M = [[N1,N2,Cost]|N],!.
mda(Noduri,[[N1,N2,Cost]|L],M) :- member(N2,Noduri), mda(Noduri,L,N), M = [[N2,N1,Cost]|N],!.
mda(Noduri,[[N1,N2,Cost]|L],M) :- mda(Noduri, L,M),!.


% Cauta prioritatea unui Nod intr-o Lista de Prioritati
% priority(Prioritati, Nod, Prioritate ).
priority([[N,P]|L],Nod,Prioritate ):- Nod == N, Prioritate = P,!.
priority([[N,P]|L],Nod,Prioritate ) :- priority(L,Nod,Prioritate).


% Cauta Costul unui Nod intr-o Lista de costuri
% cost(Costuri, Nod, Cost ).
cost([[N,C]|L],Nod,Cost ):- Nod == N, Cost = C,!.
cost([[N,C]|L],Nod,Cost ) :- cost(L,Nod,Cost).



% Aduna Cost la costul tuturor muchiilor ce contin Nodul
% add_cost(Muchii,Nod,Cost,Muchii_cost).
add_cost([],N,Cost,[]).
add_cost([[N1,N2,C]|L],N,Cost,M) :- N == N1, NewCost is C+Cost, M = [[N1,N2,NewCost]|M2], add_cost(L,N,Cost,M2),!.
add_cost([[N1,N2,C]|L],N,Cost,M) :- add_cost(L,N,Cost,M2), M=[[N1,N2,C]|M2],!.


% Prelucreaza o lista de muchii cu costuri intr-o noua lista cu costuri actualizate
% Cost actualizat = cost muchie + cost nod
% muchii_cu_cost(Muchii,Costuri,Mcc).
mcc(Muchii, [], Muchii ).
mcc(Muchii, [[N,C]|L], Mcc ) :- add_cost(Muchii,N,C,MC), mcc( MC, L, Mcc ),!. 


% Alege dintr-o lista de muchii candidat - muchia pentru care nodul are prioritate minima
% minimul_cu_prioritate_minima
% min_p ( Muchii, Prioritati, Muchie ).
min_p( [M], _, M ).
min_p([[N11,N12,C1],[N21,N22,C2]|L], Prioritati, Muchie ) :- C1 > C2, min_p([[N21,N22,C2]|L],Prioritati, Muchie),!.
min_p([[N11,N12,C1],[N21,N22,C2]|L], Prioritati, Muchie ) :- C1 < C2, min_p([[N11,N12,C1]|L],Prioritati, Muchie),!.
min_p([[N11,N12,C1],[N21,N22,C2]|L], Prioritati, Muchie ) :- priority(Prioritati, N11, P1),priority(Prioritati, N21, P2),
							   P1 < P2, min_p([[N11,N12,C1]|L],Prioritati, Muchie),!.
min_p([[N11,N12,C1],[N21,N22,C2]|L], Prioritati, Muchie ) :- min_p([[N21,N22,C2]|L],Prioritati, Muchie),!. 



% Construieste arborele - alege la fiecare pas muchia de cost minim (cost muchie + cost nod )
% Combina pasii de mai sus
% alege(N_alese, Costuri, Muchii, Prioritati , Muchii_alese ). 
alege(N_alese, Costuri, Muchii, Prioritati , Muchii_alese ) :- mda( N_alese, Muchii, MDA), mcc( MDA, Costuri, MCC), 
							       min_p( MCC, Prioritati, [Nod1,Nod2,Cost] ),
							       alege([Nod2|N_alese],[[Nod2,Cost]|Costuri],Muchii,Prioritati, Muchii_noi) , 
							       Muchii_alese = [[Nod1,Nod2]|Muchii_noi],!. 
alege(N_alese, Costuri, Muchii, Prioritati , Muchii_alese ) :- mda( N_alese, Muchii, []), Muchii_alese = [].


% Prelucreaza elementele retelei si apeleaza "alege" pentru a construi arborele
% stp(Retea, Root, Edges).
stp( Retea, Root, Edges ) :- retea(Retea, Prioritati, Muchii), root(Prioritati, [Root,PRoot]), alege([Root], [[Root,0]], Muchii, Prioritati , Edges ).



% Prelucreaza elementele retelei, construieste arborele folosind apelul "stp" 
% Folosind [Noduri, Muchii], cauta un path intre sursa si destinatie
drum( Retea, Src, Dest, Root, Edges, Path ) :- stp(Retea, Root, Edges),retea(Retea, Prioritati, Muchii), noduri(Prioritati, Nodes),
					path( Dest, Src, [Nodes, Edges], [Dest], Path).	 



% Verifica daca o muchie exista in lista de muchii
member_edges(E, [E|_]).
member_edges(E, [_|Tail]) :- member_edges(E, Tail).

edge(X,Y,[V,E]) :- member_edges([X,Y], E).
edge(X,Y,[V,E]) :- member_edges([Y,X], E).



% Cauta un path intre X si Y in graful dat de [Noduri, Muchii]
path(X,Y,[V,E],Visited,P) :- edge(X,Y,[V,E]), 
                             not(member_edges(Y, Visited)), 
                             P = [Y|Visited],!.


path(X,Y,[V,E],Visited,P) :- edge(X,Z,[V,E]),
                             not(member_edges(Z, Visited)),
                             path(Z,Y,[V,E],[Z|Visited],P),
                             !.

