% Autor: Piotr Rybicki
% Indeks: 360957

:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jestWyborem([], []).

% Implementacja zaklada taka sama kolejnosc wierzcholkow w AEGrafie i jego wyborze.
% Dla każdego wierzcholka typu a spodziewamy się, że będzie on niezmieniony w obu grafach.
jestWyborem(    [ [V, a|DzieciAE]|ResztaAE ],
                [ [V, a|DzieciAE]|ResztaG  ] ) :-
        jestWyborem(ResztaAE, ResztaG).

% Dla wierzcholka typu e spodziewamy się, że w wyborze będzie miał on tylko jedno dziecko z pierwotnego grafu.
jestWyborem(    [ [V, e|DzieciAE]|ResztaAE ],
                [ [V, e, DzieckoG]|ResztaG ]) :-
        member(DzieckoG, DzieciAE),
        jestWyborem(ResztaAE, ResztaG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jestDFS(Graf, Lista) :-
        head(Graf, [V0, _|_]),
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
jestDFSDzieci(Dzieci, [Vd|OdwiedzeniPozniej], OdwiedzeniWczesniej, Graf) :-
        select(Vd, Dzieci, ResztaDzieci),
        \+(member(Vd, OdwiedzeniWczesniej)),
        jestDFSWierzcholek(Vd, PoddrzewoDziecka, OdwiedzeniWczesniej, Graf),
        append([PoddrzewoDziecka, OdwiedzeniWczesniej], OdwiedzeniPoPierwszymDziecku),
        jestDFSDzieci(ResztaDzieci, PoddrzewaResztyDzieci,OdwiedzeniPoPierwszymDziecku, Graf),
        append([PoddrzewoDziecka, PoddrzewaResztyDzieci], [Vd|OdwiedzeniPozniej]).

% Ten predykat odpowiada za przypadek, kiedy wybrane dziecko zostalo wczesniej odwiedzone.
jestDFSDzieci(Dzieci, OdwiedzeniPozniej, OdwiedzeniWczesniej, Graf) :-
        select(Vd, Dzieci, ResztaDzieci),
        member(Vd, OdwiedzeniWczesniej),
        jestDFSDzieci(ResztaDzieci, OdwiedzeniPozniej, OdwiedzeniWczesniej, Graf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jestADFS(AEGraf, Lista) :-
        jestWyborem(AEGraf, Wybor),
        jestDFS(Wybor, Lista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


jestADFS1(Graf, Lista) :-
        head(Graf, [V0, _|_]),
        jestADFS1Wierzcholek(V0, Lista, [], Graf).

% Implementacja ADFS1 to copy-paste jestDFS, rozniaca sie na wejsciu do wierzcholka
% Wchodzac do a postepujemy tak jak przy zwyklym DFS.
% Wchodzac do e wywolujemy DFS dla jednego wybranego dziecka.
jestADFS1Wierzcholek(V,[V|OdwiedzeniPozniej], OdwiedzeniWczesniej, Graf) :-
        (
                member([V, a|Dzieci], Graf),
                jestADFS1Dzieci(Dzieci, OdwiedzeniPozniej, [V|OdwiedzeniWczesniej], Graf)
        ); % <<< OR
        (
                member([V, e|Dzieci], Graf),
                select(Vd, Dzieci, _),
                jestADFS1Dzieci([Vd], OdwiedzeniPozniej, [V|OdwiedzeniWczesniej], Graf)
        ).

jestADFS1Dzieci([], [], _, _).

jestADFS1Dzieci(Dzieci, [Vd|OdwiedzeniPozniej], OdwiedzeniWczesniej, Graf) :-
        select(Vd, Dzieci, ResztaDzieci),
        \+(member(Vd, OdwiedzeniWczesniej)),
        jestADFS1Wierzcholek(Vd, PoddrzewoDziecka, OdwiedzeniWczesniej, Graf),
        append([PoddrzewoDziecka, OdwiedzeniWczesniej], OdwiedzeniPoPierwszymDziecku),
        jestADFS1Dzieci(ResztaDzieci, PoddrzewaResztyDzieci,OdwiedzeniPoPierwszymDziecku, Graf),
        append([PoddrzewoDziecka, PoddrzewaResztyDzieci], [Vd|OdwiedzeniPozniej]).

jestADFS1Dzieci(Dzieci, OdwiedzeniPozniej, OdwiedzeniWczesniej, Graf) :-
        select(Vd, Dzieci, ResztaDzieci),
        member(Vd, OdwiedzeniWczesniej),
        jestADFS1Dzieci(ResztaDzieci, OdwiedzeniPozniej, OdwiedzeniWczesniej, Graf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Dane test&debug:

% AE graf z tresci zadania
g1g([   [v0, a, v1, v3],
        [v1, e, v2, v3],
        [v2, a],
        [v3, a]         ]).

% Wybor z tresci zadania
g1w([   [v0, a, v1, v3],
        [v1, e, v3],
        [v2, a],
        [v3, a]         ]).

% v0 -> v1 -> v2
g2g([   [v0, a, v1],
        [v1, a, v2],
        [v2, a]     ]).

% W. izolowany
g3g([   [v0, a]     ]).

% v0 -> v1
g4g([   [v0, a, v1],
        [v1, a]         ]).

g5g([   [v0, a, v1, v2],
        [v1, a],
        [v2, a]        ]).

% Maly losowy graf
g6g([ [v1, a, v5, v6, v1],
        [v2, a, v1],
        [v3, a],
        [v4, a, v2, v4],
        [v5, a, v1, v2, v3, v4],
        [v6, a, v5]     ]).

% Petla
g7g([ [v0, a, v0] ]).

% Duzy losowy graf
g8g([   [v1, a, v7, v14, v15, v18],
        [v2, a, v10],
        [v3, a, v13],
        [v4, a, v2, v14],
        [v5, a, v10, v16],
        [v6, a], % Wierzcholek izolowany
        [v7, a, v5, v12],
        [v8, a, v17],
        [v9, a, v12],
        [v10, a],
        [v11, a, v3],
        [v12, a],
        [v13, a, v4],
        [v14, a],
        [v15, a, v8],
        [v16, a, v13],
        [v17, a, v11, v16],
        [v18, a, v9]    ]).

% Gwiazda (DFS daje wszystkie permutacje)
g9g([   [v0, a, v1, v2, v3],
        [v1, a],
        [v2, a],
        [v3, a] ]).

% Maly graf z wierzcholkami typu e
g10g([  [v0, e, v1, v2, v3],
        [v1, a, v10, v11, v12],
        [v10, e, v0],
        [v11, e, v1],
        [v12, a]        ]).

adfsRownowazne(G, L) :-
        jestADFS(G, L),
        jestADFS1(G, L).
