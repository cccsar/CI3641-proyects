readKakuro(Kakuro) :- open("kakuro.txt", read, Strm), read(Strm, Kakuro).
/*
    Descripciones:
        blank(Col,Row)

        clue(Col, Row, Sum, Blanks)

        kakuro(lista de clues)

        fill(Blank, val)

        Custom structures:
            + cell(i,j)
        Predicados auxiliares
            + cluePositions(clues, cells): Posiciones de una lista de clues representados en forma de cells
            + frec(element, list): frecuencia de un elemento en una lista
            + checkFrec(L1, L2)> revisa que los elementos de L1  no aparezcan más de 2 veces en L2
*/

% clues positions as cells
cluePositions([], []).
cluePositions([clue(Col, Row, _, _) | CluesTail], [blank(Col, Row) | CellsTail ]) :- cluePositions(CluesTail, CellsTail). 

% frecuencia de un elemento en una lista
frec(_, [], 0).
frec(X, [X | T], N) :- frec(X, T, M), N is M + 1.
frec(X, [_| T], N)  :- frec(X, T, N).

% Indica los elementos de la primera lista aparecen menos de dos veces en la lista L
checkFrec([]).
checkFrec([X | T], L) :- frec(X, L, N), N =< 2, checkFrec([T], L).

% obtiene los blanks de una lista de clues
blanks([], []).
blanks([clue(_,_,_, Blanks) | MoreClues], AllBlanks) :- blanks(MoreClues, SomeBlanks), append(Blanks, SomeBlanks, AllBlanks). 

% Indica que cada celda tenga máximo 2 clues
noMoreThanTwoCluesSameCell([]).
noMoreThanTwoCluesSameCell(Clues) :- cluePositions(Clues, Positions), checkFrec(Positions, Positions).

% Indica que no existen clues repetidas en los blanks de otras clues
noClueInSomeBlanks(Clues) :- blanks(Clues, Blanks), cluePositions(Clues, Cells), intersection(Cells, Blanks, []).

% Obtiene las filas de una lista de blank
rows([], []).
rows([blank(_, Row) | MoreBlanks], [Row | MoreRows]) :- rows(MoreBlanks, MoreRows).

% Obtiene las columnas de una lista de blank
cols([], []).
cols([blank(Col, _) | MoreBlanks], [Col | MoreCols]) :- rows(MoreBlanks, MoreCols).

% Revisa si todos los numeros de una lista estan en un rango
allInRange([], _, _).
allInRange([X | L], Lower, Upper) :- Lower =< X, X =< Upper, allInRange(L,Lower, Upper).

% get sum of numbers in range [l, r]
sumInRange(L, R, Sum) :- S1 is R * (R + 1) / 2, S2 is (L - 1) * (L) / 2, Sum is S1 - S2.

% Indica si los blanks de una clue estan en linea recta respecto a la clue 
blanksInRow(clue(Col,Row,_, Blanks)) :- rows(Blanks, Rows),             % get rows
                                        union([Row], Rows, [Row]),      % check that they're all in the same row
                                        cols(Blanks, Cols),             % Get cols
                                        allInRange(Cols, Lower, Upper), % check that all elements are in range
                                        length(Blanks, NBlanks),        % get length
                                        Lower is Col + 1,               % get lower bound
                                        Upper is Col + NBlanks,         % get upper bound
                                        sumInRange(Lower, Upper, Sum),  % Calcula suma entre inferior y superior
                                        sum_list(Cols, Sum).            % Revisa que todos los elementos sean distintos

% Indica si los blanks de una clue estan en linea recta respecto a la clue 
blanksInRow(clue(Col,Row,_, Blanks)) :- rows(Blanks, Rows),             % get rows
                                        union([Col], Cols, [Col]),      % check that they're all in the same Col
                                        cols(Blanks, Cols),             % Get cols
                                        allInRange(Rows, Lower, Upper), % check that all elements are in range
                                        length(Blanks, NBlanks),        % get length
                                        Lower is Row + 1,               % get lower bound
                                        Upper is Row + NBlanks,         % get upper bound
                                        sumInRange(Lower, Upper, Sum),  % Calcula suma entre inferior y superior
                                        sum_list(Rows, Sum).            % Revisa que todos los elementos sean distintos

% Revisa si un kakuro es valido y la solucion dada resuelve el kakuro
valid(Kakuro, Solution) :- noMoreThanTwoCluesSameCell(Kakuro), noClueInSomeBlanks(Kakuro).


