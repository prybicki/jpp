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

g2g([   [v0, a, v1],
        [v1, a, v2],
        [v2, a]     ]).

g3g([   [v0, a]         ]).

g4g([   [v0, a, v1],
        [v1, a]         ]).

g5g([   [v0, a, v1, v2],
        [v1, a],
        [v2, a]        ]).

g6g([ [v1, a, v5, v6, v1],
        [v2, a, v1],
        [v3, a],
        [v4, a, v2, v4],
        [v5, a, v1, v2, v3, v4],
        [v6, a, v5]     ]).

g7g([ [v0, a, v0] ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jestWyborem([], []).

jestWyborem(    [ [V, a|DzieciAE]|ResztaAE ],
                [ [V, a|DzieciAE]|ResztaG  ] ) :-
        jestWyborem(ResztaAE, ResztaG).

jestWyborem(    [ [V, e|DzieciAE]|ResztaAE ],
                [ [V, e, DzieckoG]|ResztaG ]) :-
        member(DzieckoG, DzieciAE),
        jestWyborem(ResztaAE, ResztaG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jestDFS(Graf, Lista) :-
        head([V0, _|_], Graf),
        jestDFSWierzcholek(V0, Lista, [], Graf).

% Argumenty:
% 1) Wierzchołek startowy
% 2) Lista wierzchołków w drzewie DFS (włącznie ze startowym) (w kolejnosci odwiedzania)
% 3) Lista wierzchołków, które zostały odwiedzone *przed* wejściem do V (ostatnie najblizej)
jestDFSWierzcholek(V,[V|OdwiedzeniPozniej], OdwiedzeniWczesniej, Graf) :-
        member([V, _|Dzieci], Graf),
        jestDFSDzieci(Dzieci, OdwiedzeniPozniej, [V|OdwiedzeniWczesniej], Graf).

% Pusta lista dzieci do przejscia i pusta lista wierzcholkow do odwiedzenia to truizm.
jestDFSDzieci([], [], _, _).

% Argumenty:
% 1) Lista dzieci, na których DFS jest wykonywany.
% 2) Lista wierzchołków, które są zostaną odwiedzone podczas DFS wszystkich dzieci. (w kolejnosci odwiedzania)
% 3) Lista wierzchołków, które zostały odwiedzone *przed* wejściem do pierwszego dziecka. (ostatnie najblizej)
jestDFSDzieci([Vd|ResztaDzieci], [Vd|OdwiedzeniPozniej], OdwiedzeniWczesniej, Graf) :-
        \+(member(Vd, OdwiedzeniWczesniej)),
        jestDFSWierzcholek(Vd, PoddrzewoDziecka, OdwiedzeniWczesniej, Graf),
        append([PoddrzewoDziecka, OdwiedzeniWczesniej], OdwiedzeniPoPierwszymDziecku),
        jestDFSDzieci(ResztaDzieci, PoddrzewaResztyDzieci,OdwiedzeniPoPierwszymDziecku, Graf),
        append([PoddrzewoDziecka, PoddrzewaResztyDzieci], [Vd|OdwiedzeniPozniej]).

jestDFSDzieci([Vd|ResztaDzieci], OdwiedzeniPozniej, OdwiedzeniWczesniej, Graf) :-
        member(Vd, OdwiedzeniWczesniej),
        jestDFSDzieci(ResztaDzieci, OdwiedzeniPozniej, OdwiedzeniWczesniej, Graf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
