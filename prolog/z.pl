:- use_module(library(lists)).

g0g([ [v0, e, v1, v2, v3] ]).
g0w([ [v0, e, v1] ]).

g1g([   [v0, a, v1, v3],
        [v1, e, v2, v3],
        [v2, a],
        [v3, a]         ]).

g1w([   [v0, a, v1, v3],
        [v1, e, v3],
        [v2, a],
        [v3, a]         ]).

jestWyborem([], []).

jestWyborem(    [ [V, a|DzieciAE]|ResztaAE ],
                [ [V, a|DzieciAE]|ResztaG  ] ) :-
        jestWyborem(ResztaAE, ResztaG).

jestWyborem(    [ [V, e|DzieciAE]|ResztaAE ],
                [ [V, e, DzieckoG]|ResztaG ]) :-
        member(DzieckoG, DzieciAE),
        jestWyborem(ResztaAE, ResztaG).

% jestDFS([], []).
%
% jestDFS(Graf, OdwiedzeniPo) :-
%         head([V0, _|_], Graf),
%         jestDFSWierzcholek(Graf, OdwiedzeniPo, V0, []). % Reverse! TODO

% Sukces, jeżeli istnieje kanoniczny DFS zgodny z argumentami.
% Graf - oczywiste,
% OdwiedzeniPo - lista wierzchołków, które trzeba przejść, żeby obejść podgraf zaczynający się w tym wierzchołku
% V - wierzchołek do odwiedzenia
% OdwiedzeniPrzed - lista wierzchołków, które trzeba przejść, by dojść do tego miejsca zaczynając od v0.
% jestDFSWierzcholek(Graf, [V|ResztaOdwiedzeniPo], V, OdwiedzeniPrzed, OdwiedzeniWPoddrzewie) :-
%         dzieci(Graf, V, Dzieci),
%         jestDFSDzieci(Graf, ResztaOdwiedzeniPo, Dzieci, [V|OdwiedzeniPrzed]).
%
% % Kiedy dojdziemy do liscia, odnosimy sukces, kiedy lista wierzchołków, które trzeba odwiedzić w przejściu DFS jest pusta.
% jestDFSDzieci(Graf, [], [], OdwiedzeniPrzed).
%
% jestDFSDzieci(Graf, [Vd|ResztaOdwiedzeniPo], [Vd|ResztaDzieci], OdwiedzeniPrzed) :-
%         not(member(Vd, OdwiedzeniPrzed)),
%         % Odwiedź pierwsze dziecko,
%         jestDFSWierzcholek(Graf, [Vd|ResztaOdwiedzeniPo], Vd, OdwiedzeniPrzed),
%         jestDFSDzieci(Graf, Lista, ResztaDzieci, [Vd|OdwiedzeniPrzedDzieckiem], OdwiedzeniPoDziecku),
%         =(OdwiedzeniPo, OdwiedzeniPrzed++)
%
% jestDFSWierzcholek(Graf, )

% dfsWierzcholka(G, V, Odwie)
%
% dfsDzieci(G, [Vd|Td], OdwiedzeniUDziecka+OdwiedzeniUDzieci, OdwiedzeniWczesniej) :-
%         dfsWierzcholka(G, V, OdwiedzeniUDziecka, OdwiedzeniWczesniej),
%         dfsDzieci(G, Td, OdwiedzeniuUDzieci, OdwiedzeniWczesniej)

jestDFS(Graf, Lista) :-
        head([V0, _|_], Graf),
        jestDFSWierzcholek(Graf, Lista, V0)

% Wymaga, aby V byl w G
jestDFSWierzcholek(G, V, [V|ResztaDoOdwiedzenia]) :-
        dzieci(G, V, Dzieci),
        jestDFSDzieci(G, ResztaDoOdwiedzenia, V).


jestDFSDzieci(G, [Vd|Td], DoOdwiedzenia) :-
        jestDFSWierzcholek(G, Vd, DoOdwiedzenia), % Odwiedza pierwsze dziecko




% jestDFS([], []).
%
% jestDFSAux(Graf, Wierzcholek, Lista)
%
% jestDFS(Graf, Lista) :-
%         select([v0,T|DzieciV0],)

% Relacja zaklada, ze wejsciowe listy sa posortowane.
% Listy wierzchołków z grafu AE i jego wyboru powinny być identyczne (pomijając dzieci)
% Dziecko wierzchołka z grafu będącego wyborem powinno być jedno i być wśród TODO
% jestWyboremVe([], []).
% jestWyboremVe([ [V, e|[]]|_ ], [ [V, e|[]]|_ ]).
% jestWyboremVe([ [V, e|AE_V_Dzieci]|AE_Vs ], [ [V, e, G_V_Wyjscie]|G_Vs ]) :-
%         member(G_V_Wyjscie, AE_V_Dzieci),
%         jestWyboremVe(AE_Vs, G_Vs).
%
% jestWyboremVa(X, X).
%
% jestA([_, a|_]).
% jestE([_, e|_]).
%
% jestPodzialemVaVe([], [], []).
% jestPodzialemVaVe([A|AT], ListaE, [A|VT]) :-
%         jestA(A),
%         jestPodzialemVaVe(AT, ListaE, VT).
% jestPodzialemVaVe(ListaA, [E|ET], [E|VT]) :-
%         jestE(E),
%         jestPodzialemVaVe(ListaA, ET, VT).
%
% wierzcholekOK([ [V, a|DzieciV]|Vs ], [])
%
% jestWyborem(AEgraf, Graf) :-
%         jestPodzialemVaVe(AE_a, AE_e, AEgraf),
%         jestPodzialemVaVe(G_a, G_e, Graf),
%         sort(AE_a, AE_a_sorted), sort(G_a, G_a_sorted),
%         sort(AE_e, AE_e_sorted), sort(G_e, G_e_sorted),
%         jestWyboremVa(AE_a_sorted, G_a_sorted),
%         jestWyboremVe(AE_e_sorted, G_e_sorted).
%
% jestWyborem([ [V, a|DzieciV]|ResztaV ], Ws) :-
%         member([V, a|DzieciV], Ws),
%         jestWyborem(ResztaV, Ws).
% jestWyborem([ [V, e|DzieciV]|ResztaV ], Ws) :-
%         member(V, e|DzieciV], Ws)
%         select([C, DzieciV, _),
%
%
% listaDzieci([ [V, _|Dzieci]|_ ], V, Dzieci).
% listaDzieci([ [_, _|_]|ListaWierzcholkow ], V, Dzieci) :- listaDzieci(ListaWierzcholkow, V, Dzieci).
